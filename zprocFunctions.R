#Determine start,end time and location ID
end_time = end_time_low_level + (60*60*18) - (60*60*24) #normally 18
#For calculation of accurate bed time we start from the day before at 16:00
start_time = end_time - (60*60*24*28) + (60*60*1) #normally 1  # 1 hour difference can happen due to time change. It shouldn't affect anything

get_data_zproc <- function(con2, location_id = NULL,
                           start_time = NULL, end_time = NULL, 
                           tz = 'America/New_York',tables = 'all') {
  flog.debug("Import data for location = %d between %s and %s.", location_id, start_time, end_time) 
  if (tables[1] == 'all') {
    tables <- c('location', 'longterm', 'sensor_metadata')
  }
  
  start_int = as.double(start_time)*1000  
  end_int = as.double(end_time)*1000   
  output <- list() 
  
  if('location' %in% tables){
    flog.debug("Getting location table...")
    dl <- DBI::dbGetQuery(con2, paste('select * from location'))
    if(nrow(dl) == 0){
      flog.error("Fail to get location table")
    }    
    output[['location']] <- dl
    rm(dl)
  }
  
  if('sensor_metadata' %in% tables){
    flog.debug('Getting sensor_metadata table...')
    sm <- DBI::dbGetQuery(con2, paste('select * from sensor_metadata'))
    if(nrow(sm) == 0){
      flog.error("Fail to get sensor_metadata table")
    }
    output[['sensor_metadata']] <- sm
    sid_aid = get_sid_aid_map(sensor_info = sm, location_id = location_id)
    if(nrow(sid_aid) == 0){
      flog.error("Fail to get sid_aid for location_id = %s", location_id)
    }
    output[['sid_aid']] <- sid_aid
    rm(sm)
  }
  
  if('longterm' %in% tables){
    flog.debug("Getting longterm table...")
    st <- DBI::dbGetQuery(con2, paste('select * from longterm where', date_filter_query_zproc(start_int, end_int), 
                                      ' and is_original = 1', 'and sensor_id in (', paste(sid_aid$sensor_id, collapse = ', '), ')'))
    if(nrow(st) == 0){
      flog.error("Fail to get longterm table")
    }
    output[['longterm']] <- st
    rm(st)
  }
  return(output)
}


###
get_sid_aid_map <- function(sensor_info, location_id){
  # Using alias to get sensor_ids
  alias_need = c("In Living Room", "In Front Door Area", 
                 "In Bedroom", "In Bedroom, Master",
                 "In Bathroom, Master", "In Bathroom", "In Shower",
                 "In Kitchen", "In Dining Area",
                 "In Bedroom, Guest", "In Guest Bedroom", "In Bathroom, Guest", "In Bath, Guest", 
                 "In Office",
                 "In Study",
                 "In Sewing Room",
                 "Opened Front Door", "Opened Garage Door")
  
  alias_door = c("Opened Front Door", "Opened Garage Door")
  sensor_need_info <- sensor_info[which(sensor_info$alias %in% alias_need & sensor_info$location_id == location_id), ]
  door_id <- sensor_need_info[which(sensor_need_info$alias %in% alias_door), 'id']
  
  lookup_motion <- c('Living Room' = "1", 'Entrance' =  "1",         
                     'Bedroom, Master' = "2", 
                     "Bathroom, Master" = "3", 
                     'Kitchen' = "4", "Dining Room" = "4", "Dining Area" = "4",
                     'Bedroom, Guest' = "5", 'Bathroom, Guest' = "5",
                     'Office' = "7",
                     'Study' = "8",
                     'Sewing Room' = "9")
  
  sensor_need_info <- mutate(sensor_need_info, area_id = as.numeric(unname(lookup_motion[sensor_need_info$install_room])))  
  sensor_need_info[which(sensor_need_info$id %in% door_id), 'area_id'] = as.numeric(6)
  
  sid_aid = data.frame(location_id = sensor_need_info$location_id,
                       sensor_id = sensor_need_info$id,
                       area_id = sensor_need_info$area_id)
  if(is.null(sid_aid)){
    flog.error("No sid_aid information generated")
    return(NULL)
  }else{
    return(sid_aid)
  }
}

###
date_filter_query_zproc <- function(start_int = NULL, end_int = NULL, date_var = 'start_int') {
  if (length(c(start_int, end_int)) == 2) {
    query <- sprintf('%s between "%s" and "%s"', date_var, start_int, end_int)
  } else if (!is.null(start_int)){
    query <- sprintf('%s >= "%s"', date_var, start_int)
  } else {
    query <- sprintf('%s <= "%s"', date_var, end_int)
  }
  return(query)
}

###
check_conversion <- function(x){
  to2from = 0 # number of 'to' doesn't equal to 'from'
  bigger = 0 # number of 'from' bigger than 'from
  for(i in 1:(nrow(x)-1)){
    if(x$to[i] != x$from[i+1]){
      to2from = to2from + 1
    }
    
    if(x$from[i] > x$to[i]){
      bigger = bigger + 1
    }
  }
  if(to2from != 0 | bigger != 0){
    flog.error("Number of 'to' not equal to 'from' = %d", to2from)
    flog.error("Number of 'from' bigger than 'to' = %d", bigger)
    return(FALSE)
  }else{
    return(TRUE)
  }
}

###

generate_stst_data <- function(x, start_time = NULL, end_time = NULL,
                               table = NULL){
  
  if(is.null(x)){
    flog.error("The input for generate_stst_data() is NULL")
  }
  start_int = as.double(start_time)*1000  # In generating stst data and the applications based on ststs data,
  end_int = as.double(end_time)*1000     # millisecond are used. when generate time, second are used.    
  track_position = NULL
  position = NULL
  from = NULL
  to = NULL
  label = NULL
  id_index_next = NULL
  all_on_event = filter(x, x$value == 1)
  if(nrow(all_on_event) == 0){
    flog.warn("No ON events between %s and %s",start_time, end_time)
    return(NULL)
  }
  for(i in 1:nrow(all_on_event)){
    if(i < nrow(all_on_event)){
      data_between_on = filter(x, x$start_int > all_on_event[i, 'start_int'] 
                               & x$start_int < all_on_event[i + 1, 'start_int'])
      index_off_later = which(data_between_on$sensor_id == all_on_event[i + 1, 'sensor_id'] 
                              & data_between_on$value == 0)
      
      if(length(index_off_later) > 1){
        index_off_later = index_off_later[1]
      }
      
    }else{
      data_between_on = filter(x, x$start_int > all_on_event[i, 'start_int'] 
                               & x$start_int < end_int)
    }
    index_off = which(data_between_on$sensor_id == all_on_event[i, 'sensor_id'] 
                      & data_between_on$value == 0)
    if(length(index_off) > 1){
      index_off = index_off[1]
    }
    
    if(length(index_off) == 0){
      # No A_OFF found between A_ON and B_ON
      position = c(position, all_on_event[i, 'sensor_id'])
      from = c(from, all_on_event[i, 'start_int'])
      if(i < nrow(all_on_event)){
        to = c(to, all_on_event[i + 1, 'start_int'])
      }else{
        to = c(to, end_int)
      }
      label = c(label, 'AT')
    }else{
      # An A_OFF event is found between A_ON and B_ON
      position = c(position, all_on_event[i, 'sensor_id'])
      from = c(from, all_on_event[i, 'start_int'])
      to = c(to, data_between_on[index_off, 'start_int'])
      label = c(label, 'AT')
      
      if(length(index_off_later) == 0) {
        position = c(position, 0)
        from = c(from, data_between_on[index_off, 'start_int'])
        if(i < nrow(all_on_event)){
          to = c(to, all_on_event[i + 1, 'start_int'])
        }else{
          to = c(to, end_int)
        }
        label = c(label, 'UT')
      }else if(length(index_off_later) != 0){
        if(index_off > index_off_later){
          position = c(position, 0)
          from = c(from, data_between_on[index_off, 'start_int'])
          if(i < nrow(all_on_event)){
            to = c(to, all_on_event[i + 1, 'start_int'])
          }else{
            to = c(to, end_int)
          }
          label = c(label, 'UT')
        }else{
          position = c(position, all_on_event[i + 1, 'sensor_id'])
          from = c(from, data_between_on[index_off, 'start_int'])
          if(i < nrow(all_on_event)){
            to = c(to, all_on_event[i + 1, 'start_int'])
          }else{
            to = c(to, end_int)
          }
          label = c(label, 'ST')
        }
      } 
    }
  }
  
  # Construct track_position 
  track_position = data.frame(cbind(position, from, to), stringsAsFactors = FALSE)
  if(!check_conversion(track_position)){
    flog.error("STST data are generated in wrong time sequence order")  
    return(NULL)
  }
  track_position$area = track_position$position
  for(j in 1:nrow(table)){
    track_position[which(track_position$area == table[j, 'sensor_id']), 'area'] = table[j, 'area_id']
  }
  track_position$dur = (track_position$to - track_position$from)/1000
  track_position$label = label
  return(track_position)
}


###
sleep_estimate <- function(track_position, idle_time = 20*60, 
                           total_sleep = 60*60*4, between_window = 20*60,
                           area_set = c(2, 3), need_total = FALSE, need_longest = FALSE){
  if(is.null(track_position)){
    flog.debug("The input STST data is NULL in sleep_estimate()")     
    return(NULL)
  }
  bed_time = NULL
  rise_time = NULL
  num_bathroom = NULL
  sleep_data_temp = NULL
  sleep_data = NULL
  if(nrow(track_position) > 1){
    # Find potential sleep data named by sleep_data_temp
    for (j in 1:(nrow(track_position) - 1)){
      if(track_position[j, 'label'] == 'UT' && track_position[j, 'dur'] > idle_time){
        if(track_position[j + 1, 'area'] %in% area_set){
          sleep_data_temp = rbind(sleep_data_temp, track_position[j, ])
        }
      }else if(track_position[j, 'area'] %in% area_set && track_position[j, 'dur'] > idle_time &
               track_position[j, 'label'] == 'ST'){
        sleep_data_temp = rbind(sleep_data_temp, track_position[j, ])
      }else{
        next
      }
    }
    # Divide sleep_data_temp into different sleep windows
    window_index = 1
    if(!is.null(sleep_data_temp)){
      if(nrow(sleep_data_temp) >= 2){
        for(k in 1:(nrow(sleep_data_temp)-1)){
          from_to_dur = (sleep_data_temp[k+1, 'from'] - sleep_data_temp[k, 'to'])/1000
          if(from_to_dur < between_window){
            sleep_data_temp[k, 'window'] = window_index
            if(k + 1 == nrow(sleep_data_temp)){
              sleep_data_temp[k+1, 'window'] = window_index  
            }
          }else{
            sleep_data_temp[k, 'window'] = window_index
            window_index = window_index + 1
            if(k + 1 == nrow(sleep_data_temp)){
              sleep_data_temp[k+1, 'window'] = window_index
            }
          }
        }
      }else{
        sleep_data_temp$window = 1
      }
    }else{
      flog.debug("No satisfied sleep data is found for this day")
      return(NULL)
    }
    # Construct sleep_data with the first time of a sleep window as the rise time, and the last time of this 
    # sleep window as the rise time. Filter out sleep windows with duration smaller than total_sleep (4*60*60)
    max_window = max(sleep_data_temp$window)
    for(kk in 1:max_window){
      sleep_data_window = filter(sleep_data_temp, window == kk)
      bed_time = c(bed_time, sleep_data_window[1, 'from'])
      rise_time = c(rise_time, sleep_data_window[nrow(sleep_data_window), 'to'])
      summary_data_temp = track_position[which(track_position$from >= bed_time[length(bed_time)] 
                                               & track_position$to <= rise_time[length(rise_time)]), ] 
      num_bathroom = c(num_bathroom, sum(summary_data_temp$area == 3))
      sleep_data = data.frame(cbind(bed_time, rise_time, num_bathroom))
    }
    sleep_data$dur = sleep_data$rise_time - sleep_data$bed_time
    if(need_total == TRUE){ # need_total is used to control the total sleep time
      sleep_data = filter(sleep_data, dur/1000 >= total_sleep)
    }
    if(need_longest == TRUE){ # when need_longest is TRUE, return sleep estimation with longest dur
      sleep_data <- filter(sleep_data, dur == max(dur))
    }
    
    if(nrow(sleep_data) != 0){
      return(sleep_data)
    }else{
      return(NULL)
    }
    
  }else{
    flog.error("The input STST data only has one row")
    return(NULL)
  }
}

#

out_estimate <- function(track_position, idle_time = 20*60){
  if(is.null(track_position)){
    flog.error("STST data is NULL")
    return(NULL)
  }
  out_data = NULL
  if(nrow(track_position) > 1){
    #for(i in 1:(nrow(track_position)-1)){
    for(i in 2:(nrow(track_position)-1)){
      if(track_position[i, 'label'] == 'UT' & track_position[i, 'dur'] >= idle_time){
        if((track_position[i + 1, 'area'] == 6) & (track_position[i - 1, 'area'] == 6)){
          out_data = data.frame(rbind(out_data, track_position[i, c('from','to')]))
        }
        
      }else if(track_position[i, 'label'] == 'ST' & track_position[i, 'dur'] >= idle_time & track_position[i, 'area'] == 6){
        out_data = data.frame(rbind(out_data, track_position[i, c('from','to')]))
      }else{
        next
      }
    }
    return(out_data)
  }else{
    flog.error("The input STST data only has one row in sleep_estimation()")     
    return(NULL)
  }
}