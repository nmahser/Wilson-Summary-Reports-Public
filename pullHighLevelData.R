source("/home/nmahser/Wilson_Summary_Reports/zprocFunctions.R")


#data base connection
con2 <- NULL

flog.debug("Connecting to Zproc for highLevelSensorData %s",location_id)
con2 <- dbConnect(drv = RMySQL::MySQL(), default.file = '~/.my.zproc')

if(is.null(con2)) {
  flog.error("coulnd't connect to the database")
  
  #try only one more time
  con2 <- dbConnect(drv = RMySQL::MySQL(), default.file = '~/.my.zproc')
  
}

#Runs the query 30 times in case of returning Null
highLevelPull <- function() {
  iter = 0
  maxIter = 30
  query <- NULL
 
 while(is.null(query) & iter < maxIter) {
   query <- tryCatch(get_data_zproc(con2,location_id = location_id,start_time = start_time, end_time = end_time ,tables = 'all'),
                 error = function(e) {query=NULL})
   iter=iter + 1
   Sys.time()
   
   if(!is.null(query) | iter > maxIter) break
   flog.debug(paste("highLevel query is Null, trial:",iter))
   Sys.sleep(7)
  
 }
  return(query)
}

#Get sleep and outhome data sets and combined them into one dataframe
flog.debug("Getting highLevelData...")
zproc_initial <- highLevelPull()

if(nrow(zproc_initial[[1]])==0){
  flog.error("Failed to get highLevelSensor data")
}

#Disconnect from Database
dbDisconnect(con2)

events_data <- zproc_initial$longterm[, c('sensor_id', 'value', 'start_time', 'start_int')] %>% arrange(start_int)

flog.debug("Getting track_position...")
track_position <- generate_stst_data(events_data, start_time, end_time, table = zproc_initial$sid_aid)

#reset to NULL for the next house
zproc_initial <- NULL

flog.debug("Getting out_estimate")
#Pull OutHome and Sleep data sets
outHome <- out_estimate(track_position)

flog.debug("Getting sleep_estimate")
#Sleep data
sleepDataOrginal <- sleep_estimate(track_position)

#To be able to merge sleep data with outhome, change the time format.
for(i in 1:2) {
  sleepDataOrginal[,i] <- round(sleepDataOrginal[,i]/1000)
  sleepDataOrginal[,i] <- as.POSIXct(sleepDataOrginal[,i], origin = "1970-01-01", tz = "America/New_York")
}

#Create DateFormatout
#what if participant wakes up before midnight
####house008<-sleepDataOrginal

sleepDataOrginal$dateFormatOut <- as.Date(as.character(sleepDataOrginal$rise_time))

over14.00 <- format(as.POSIXct(strptime(sleepDataOrginal$rise_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

sleepDataOrginal <- cbind.data.frame(sleepDataOrginal,over14.00)


#Convert string time to chron 
sleepDataOrginal$over14.00 <- times(sleepDataOrginal$over14.00) 


# If there is rise time over 14:30:00 count that as next day. This will help us for total duration in a day and if there is rise_time before midnight
for(i in 1:nrow(sleepDataOrginal)) {
  if(sleepDataOrginal$over14.00[i] > times("14:30:00")) {
    sleepDataOrginal$dateFormatOut[i] <- (sleepDataOrginal$dateFormatOut[i] + 1)
  }
}

#Create two instances of sleep table. This will help us to determine  earliest bed time and latest wake up time
BED_TIME <- sleepDataOrginal
WAKEUP_TIME <- sleepDataOrginal
TOTAL_DURATION <- sleepDataOrginal


#Earliest Bed Time, particant can take a nap during the day so that's why we can't blindly choose the earliest bed time
#We delete times between 13:00 and 19:00, meaning that we make assumptions of a participant do not sleep between 13:00 and 19:00
#However we take nap times into count while calculating the total duration

hoursBed <- format(as.POSIXct(strptime(BED_TIME$bed_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

bed_time <- cbind.data.frame(BED_TIME,hoursBed)


#Convert string time to chron 
bed_time$hoursBed <-times(bed_time$hoursBed) 

#delete times greater than 13:00 less than 19:00
bed_time <- bed_time %>%
  group_by(dateFormatOut) %>%
  filter(!(hoursBed > times("13:00:00") & hoursBed < times("19:00:00"))) 

#Earliest Bed Time

bed_time <- bed_time %>%
  group_by(dateFormatOut)%>%
  filter(bed_time == min(bed_time)) %>%
  filter(!(dateFormatOut < as.Date(start_time)+1)) #Delete if we have bed time less than as.Date(start_time) + 1

#Latest Wake up Time
#Since a participant can wake up during the same day, we can get wake up times  like 23:00. So we can't simply say get the latest wake up time
#as we did for bed time. Split date and time into different cols. Filter times greater 13:00:00. Then get the latest.
#Split Date and Time

hoursWakeUp <- format(as.POSIXct(strptime(WAKEUP_TIME$rise_time,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")

wakeup_time <- cbind.data.frame(WAKEUP_TIME,hoursWakeUp)

#Convert string time to chron 
wakeup_time$hoursWakeUp <-times(wakeup_time$hoursWakeUp) 

#filter out times greater than 13:00:00
wakeup_time <- wakeup_time %>%
  group_by(dateFormatOut) %>%
  filter(hoursWakeUp < times("13:00:00")) %>%
  filter(hoursWakeUp == max(hoursWakeUp))  #latest wake up time

#Total duration
total_duration <- TOTAL_DURATION %>%
  group_by(dateFormatOut) %>%
  filter(!(dateFormatOut < as.Date(start_time)+1)) %>%
  summarise(dur = sum(dur))



#SleepDist
sleepDist <- sleepDataOrginal %>% 
  group_by(dateFormatOut) %>% 
  mutate(sleepDist = 1:n()) %>%
  top_n(n=1) %>%
  mutate(sleepDist = sleepDist - 1)


#Combine bed_time,wakeup time, total_duration and sleepDist based on dateFormatOut
#use cbind.fill in case nrows are not the same.

colNames <- c("bed_time", "rise_time", "num_bathroom", "dur", "sleepDist","dateFormatOut")

sleepDataOrginal <- rowr::cbind.fill(bed_time=bed_time$bed_time,rise_time=wakeup_time$rise_time,num_bathroom=wakeup_time$num_bathroom,
                                     dur=total_duration$dur,sleepDist=sleepDist$sleepDist, dateFormatOut=wakeup_time$dateFormatOut,  fill = NA)

colnames(sleepDataOrginal) <- colNames

sleepDataOrginal$dateFormatOut <- as.Date(sleepDataOrginal$dateFormatOut)

#Convert factor to Posixct
sleepDataOrginal$bed_time <- as.POSIXct(sleepDataOrginal$bed_time, origin = "1970-01-01", tz = "America/New_York")
sleepDataOrginal$rise_time <- as.POSIXct(sleepDataOrginal$rise_time, origin = "1970-01-01", tz = "America/New_York")

#add dateFormatOut col to make sure nrow(outHome) matches nrow(sleepDataOriginal) 
#sleepDataOrginal <- cbind(sleepDataOrginal,"dateFormatOut" = as.Date(sleepDataOrginal$bed_time - 86400)) #minus 24

#Convert from double to POSIXCT
for(i in 1:2) {
  outHome[,i] <- round(outHome[,i]/1000)
  outHome[,i] <- as.POSIXct(outHome[,i],origin = "1970-01-01", tz = "America/New_York")
}

outHome <- outHome %>%
  mutate("timeSpentOutside" = to - from,
         "dateOriginal" = from) %>%  #The date represents the date participant left the house
  select(c(timeSpentOutside,dateOriginal))


outHome <- outHome %>%
  mutate(dateFormatOut = as.Date(as.character(dateOriginal))) %>%
  group_by(dateFormatOut) %>%
  summarise(timeSpentOutside = sum(timeSpentOutside)) %>%
  as.data.frame()

#Merge sleepDataOrginal and outHome
sleepAndOut <- merge(sleepDataOrginal,outHome,by = "dateFormatOut", all=TRUE)

#Re-order columns. Fix here later on ####
sleepAndOut <- sleepAndOut[,c(2,3,4,5,6,1,7)]

#Make sure there isn't any sleep time for the upcoming day
sleepAndOut <- sleepAndOut %>%
  filter(!(dateFormatOut > as.Date(end_time)))

