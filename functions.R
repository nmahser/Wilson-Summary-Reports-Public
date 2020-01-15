#FUNCTIONS
# cleanSensorData(), filterCountDaily(), filterTimeDaily(), overallDailyCountAverage(), overallDailyTimeAverage()
#combineTables(), filterCountWeekly(), filterTimeWeekly(), OverallDailyCountAverage(), 
#sleepOrganizer(), filterSleepDaily(), filterSleepWeekly(), filterDailyAvg, filterWeeklyAvg, thresSleepDaily(), 
#secToHr(), hoursToHr(), minToHr(),secToFormat()
#styleAbove, styleBelow variables for coloring operation
##Seconds to hr:m:s
secToFormat <- function(seconds){
 
    hours <- seconds/60/60
    min <- seconds/60
    h <- hours%/% 1
    m <- ((hours%% 1)*60) %/% 1
    s <- ((min%% 1)* 60) %/% 1
    resultH <- paste0(h,":", sep = "")
    resultM <- paste0(m,":", sep = "")
    resultS <- paste0(s, sep = "")
    result <- paste(resultH,resultM,resultS,sep = "")
    return(result)
  
}

###Clean and Organizing Sensor Data
cleanSensorData <- function(sensorData) {
  sensorData <- sensorData %>%
    select(-c("id","on_value")) %>% ####3 Would you need this when you have database connection?
    filter(value==1) %>% #only on values
    dplyr::mutate(dateFormat=as.Date(start_time, "%Y-%m-%d")) %>% #dateFormat for weekly data separatio
    select(-c("report_unit","start_time","end_time"))
  
  #Proper names for the aliases. If alias doesn't exist, it doesn't return NA value which is good!Becase some houses do not have the exact sensors installed (Coffee Pot, Toaster, etc.)
  sensorData$alias <- recode(sensorData$alias,`In Bath, Guest` = "Time Spent In Shower",
                            `In Shower` = "Time Spent In Shower",
                            `In Kitchen`="Time Spent In Kitchen",
                            `In Bathroom, Master`="Time Spent In Master Bathroom",
                            `In Bathroom, Guest`="Time Spent In Guest Bathroom",
                            `In Living Room`="Time Spent In Living Room",
                            `In Shower`="Time Spent In Shower",
                            `In Office Area`="Time Spent In Office Area",
                            `Opened Fridge` ="Fridge Opening(s)",
                            `Opened Pantry` ="Pantry Opening(s)",
                            `Toilet Flushed, Master`="Toilet Flush(es), Master",
                            `Toilet Flushed, Guest` ="Toilet Flush(es), Guest",
                            `Using Toaster` ="Toaster Usage(s)",
                            `Using Microwave` ="Microwave Usage(s)",
                            `Using Coffee Pot` ="Coffee Pot Usage(s)",
                            `Using Washing Machine` ="Time Using Washing Machine",
                            `In Office` = "Time Spent In Office",
                            `Using Television` = "Time Spent Using TV",
                            `Using Television (In Bedroom)` = "Time Spent Using TV In Bedroom")
  
  #add rows for "Time Using Washing Machine","Time Spent In Kitchen", "Time Spent In Guest Bathroom" to make sure 
  #these aliases are in the 28 days data because we use them to label the groups (Hygiene, Nutrition, etc)
  sensorData <- add_row(sensorData, alias = c("Time Using Washing Machine","Time Spent In Kitchen","Time Spent In Guest Bathroom"), value=0, duration=0,dateFormat=as.Date(as.character(end_time_low_level))) 
  
  return(sensorData)
}

####What if an activity didn't happen in last 7 days but happen in  before that. Average col will be problematic
###Count&Time sensor data functions for Daily Report
#Count
filterCountDaily <- function(cleanedData) { ###you wanna have to option daily and weekly?
  cleanedData <- cleanedData %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for 7 days
    #This for aliases to display
    add_row(alias = unique(cleanedData$alias),value=0,duration=0,dateFormat=as.Date(as.character(end_time_low_level))) %>%
    #This is for days to display even tho there wasn't any activitiy in the day
    add_row(alias = "Count",value=0,duration=0,dateFormat= seq(as.Date(as.character(end_time_low_level)) 
                                                              - 6,as.Date(as.character(end_time_low_level)), by=1)) %>%
    filter(!str_detect(alias,"^Time")) %>% #not starts with "Time"
    filter(dateFormat >= as.Date(as.character(end_time_low_level)) - 6) %>% #gives last 7 days
    droplevels

  cleanedData <- dcast(cleanedData,alias ~ dateFormat) #Make sure to use dateFormat. Otherwise col order can be wrong!

  #Delete the dumy column
  cleanedData <- cleanedData[- grep("Count", cleanedData$alias),]
  
  # if one of the count daily variable is 0 which means there wasn't any activity for any of the count sensors for the latest col. 
  if(cleanedData[,ncol(cleanedData)][1] != 0) {
    #Substract -1 to exclude unique(cleanedData$alias) dummy row
    cleanedData[,ncol(cleanedData)] <- cleanedData[,ncol(cleanedData)] - 1
  }
  #### For some reason format(., "%b %d, %A") has to have one less space compare to other simirlar functions.
  colnames(cleanedData)[2:ncol(cleanedData)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  cleanedData[is.na(cleanedData)] <- 0 #returns NA when there isn't any event recorded
  
  row.names(cleanedData) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(cleanedData) 
}

#Time
filterTimeDaily <- function(cleanedData) {
  cleanedData <- cleanedData %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for 7 days
    add_row(alias = unique(cleanedData$alias),value=0,duration=0,dateFormat=as.Date(as.character(end_time_low_level))) %>%
    #This is for days to display even tho there wasn't any activitiy in the day
    add_row(alias = "Dummy",value=0,duration=0,dateFormat= seq(as.Date(as.character(end_time_low_level)) 
                                                          - 6,as.Date(as.character(end_time_low_level)), by=1))  %>%
    filter(str_detect(alias,"^Time|Dummy")) %>%
    filter(dateFormat >= as.Date(as.character(end_time_low_level)) - 6) %>%
    group_by(alias,dateFormat) %>%
    summarise(duration = round(sum(duration)))
  
    cleanedData <- dcast(cleanedData,alias ~ dateFormat)
    #Delete the dumy column
    cleanedData <- cleanedData[- grep("Dummy", cleanedData$alias),]
    
    colnames(cleanedData)[2:ncol(cleanedData)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  
  cleanedData[is.na(cleanedData)] <- 0 #returns NA when there isn't any event recorded
  
  row.names(cleanedData) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(cleanedData)
}

###Average functions
#Daily
#might need this function in the future because we might want to get the average of larger sample

overallDailyCountAvg <- function(cleanedData,filteredCountDaily) {
  cleanedData <- cleanedData %>%
    filter(alias %in% filteredCountDaily$alias) %>% #use the alias that were exist in the last 7 days
    droplevels %>%
    group_by(alias) %>%
    summarise("Historical Average" = round(sum(value)/length(unique(cleanedData$dateFormat)),1)) %>% #%>% #unique days = numbers of different days in the data set
    data.frame()
  
  return(cleanedData)
}


#Time
overallDailyTimeAvg <- function(cleanedData,filteredTimeDaily) {
  cleanedData <- cleanedData %>%
    filter(alias %in% filteredTimeDaily$alias) %>%
    filter(str_detect(alias,"^Time")) %>%
    droplevels %>%
    group_by(alias)%>%
    summarise('Historical Average'= round(sum(duration)/length(unique(cleanedData$dateFormat)),1)) %>% #unique days = numbers of different days
    data.frame()    ####?
  
  return(cleanedData)
}

###
#Dividing daily data into weeks
first  = end_time_low_level - (60*60*24*7)
second = first -(60*60*24*7) 
third = second - (60*60*24*7)


###Count&Time sensor data functions for Weekly Report
#Count Weekly
filterCountWeekly <- function(cleanedData) { ###you wanna have to option daily and weekly?
  cleanedData <- cleanedData %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for a week
    add_row(alias = "Dummy",value=0,duration=0,dateFormat = seq(as.Date(as.character(third)),as.Date(as.character(end_time_low_level)),
                                                                by=7)) %>%
    filter(!str_detect(alias,"^Time")) %>%
    droplevels %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                        ifelse(dateFormat <= second, as.Date(as.character(second)),
                        ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level))))))
  
    
  cleanedData$dateFormat <- as.Date(cleanedData$dateFormat, origin = "1970-01-01", tz = "America/New_York") #change numeric to date.

  cleanedData <- dcast(cleanedData,alias ~ dateFormat) #dateFormat for weekly, we need date format 
  
  #Delete the dumy column
  cleanedData <- cleanedData[- grep("Dummy", cleanedData$alias),]
  
  #Change colname
  colnames(cleanedData)[2:ncol(cleanedData)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  row.names(cleanedData) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(cleanedData)
}

#Time
filterTimeWeekly <- function(cleanedData){
  
  cleanedData <- cleanedData %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for a week
    add_row(alias = "Dummy",value=0,duration=0,dateFormat = seq(as.Date(as.character(third)),as.Date(as.character(end_time_low_level)),
                                                                by=7)) %>%
    filter(str_detect(alias,"^Time|Dummy")) %>%
    droplevels %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                        ifelse(dateFormat <= second, as.Date(as.character(second)),
                        ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(alias,dateFormat) %>%
    
    summarise(duration = round(sum(duration)))
  # ifelse returns numeric so convert dateFormat to Date format
  cleanedData$dateFormat <- as.Date(cleanedData$dateFormat, origin = "1970-01-01", tz = "America/New_York")
    
  
  cleanedData <- dcast(cleanedData,alias ~ dateFormat)
  
  #Delete the dumy column
  cleanedData <- cleanedData[- grep("Dummy", cleanedData$alias),]
  
  cleanedData[is.na(cleanedData)] <- 0 #### 0 DOESN'T MEAN IT HAS BEEN USED!
  
  #Change colname
  colnames(cleanedData)[2:ncol(cleanedData)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  row.names(cleanedData) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(cleanedData)
}  


###Weekly Average
#Count Weekly Average
#length!!! ####
overallWeeklyCountAvg <- function(cleanedData) {
  
  cleanedData <- cleanedData %>%
    filter(!str_detect(alias,"^Time")) %>%
    droplevels %>%
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                        ifelse(dateFormat <= second, as.Date(as.character(second)),
                        ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(alias,dateFormat) %>%
    summarise(value = sum(value))
  
  # ifelse returns numeric so convert dateFormat to Date format
  cleanedData$dateFormat <- as.Date(cleanedData$dateFormat, origin = "1970-01-01", tz = "America/New_York")
  
  #Group by alias again to get the average
  countWeeklyAvg <- cleanedData %>%
    group_by(alias) %>%
    summarise("Historical Average" = round(sum(value)/length(unique(cleanedData$dateFormat)),1)) #if there is any activity in a week we take whole week into account.
  
  return(countWeeklyAvg)
}


#Time Weekly Avg
overallWeeklyTimeAvg <- function(cleanedData) {
  
  cleanedData <- cleanedData %>%
    filter(str_detect(alias,"^Time")) %>%
    droplevels %>%
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                        ifelse(dateFormat <= second, as.Date(as.character(second)),
                        ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(alias,dateFormat) %>%
    summarise(duration = sum(duration))
  
  #Grouped by alias and date and then calculate the sum
  cleanedData <- cleanedData  %>%
    group_by(alias,dateFormat) %>%
    summarise(duration = round(sum(duration)))
  
  #Group by alias again to get the weekly average
  cleanedData <- cleanedData %>%
    group_by(alias) %>%
    summarise("Historical Average" = round(sum(duration)/length(unique(cleanedData$dateFormat)),1))
              
  return(cleanedData)

}


###Combine Tables Functions
combineTables <- function(time,count) {
  
  combinedTable <- rbind(time["Time Spent In Shower",],time["Time Using Washing Machine",],
                         count["Fridge Opening(s)",],count["Pantry Opening(s)",],
                         count["Coffee Pot Usage(s)",],count["Microwave Usage(s)",],
                         count["Toaster Usage(s)",],time["Time Spent In Kitchen",],
                         count["Toilet Flush(es), Master",],count["Toilet Flush(es), Guest",],
                         time["Time Spent In Master Bathroom",],time["Time Spent In Guest Bathroom",],
                         time["Time Spent In Living Room",],time["Time Spent In Office",],time["Time Spent Using TV",],time["Time Spent Using TV In Bedroom",])
  
  return(combinedTable)
}


###Sleep funcs( bed time, wake time and sleeping hours)
sleepOrganizer <- function(sleepAndOut) {
  end_time
  #create an alias for bed and wakeup times, and numBathroom
  aliasBed <- rep(c("Bed Time"), times = length(sleepAndOut$bed_time))
  aliasTime <- rep(c("Wake up Time"), times = length(sleepAndOut$rise_time))

  #create data frame for each alias
  `Bed Time` <- cbind.data.frame("alias" = aliasBed,"time" = sleepAndOut$bed_time,"dateFormat" = sleepAndOut$dateFormatOut,"duration" = sleepAndOut$dur,"sleepDist"=sleepAndOut$sleepDist)
  `Wake up Time` <- cbind.data.frame("alias" = aliasTime,"time" = sleepAndOut$rise_time,"dateFormat" = sleepAndOut$dateFormatOut,"duration" = sleepAndOut$dur,"sleepDist"=sleepAndOut$sleepDist)
  
  #combine all the dataframes
  combinedSleepData <- rbind.data.frame(`Bed Time`,`Wake up Time`)
  
  return(combinedSleepData)
}


filterSleepDaily <- function(sleepData) {
  #Need to convert Posixct to double first. Otherwise group by doesn't work. Dummy aliases don't get grouped.
  #So we had to sum dummy aliases based on alias and dateformat. ####This needs further explanation
  sleepData$time <- as.double(sleepData$time) * 1000
  
  sleepData <- sleepData %>%
    select(-c(duration)) %>%
    #This for aliases to display
    add_row(alias = unique(sleepData$alias),time=0,dateFormat=as.Date(as.character(end_time_low_level))) %>%
    #This is for days to display even tho there wasn't any activitiy in the day
    add_row(alias = "Dummy",time=0,dateFormat= seq(as.Date(as.character(end_time_low_level))
                                              - 6,as.Date(as.character(end_time_low_level)), by=1)) %>%
    filter(dateFormat >= as.Date(as.character(end_time_low_level)) - 6) %>%
    group_by(alias,dateFormat) %>%
    summarise(time = sum(time))
  
  #before casting time should be at last col index  
  sleepData <- sleepData[,c("alias","dateFormat","time")]
 
  sleepData <- dcast(sleepData,alias ~ dateFormat)
  
  #Delete the dumy column
  sleepData <- sleepData[- grep("Dummy", sleepData$alias),]

  #This is for to display NA instead of 0. related to add_row in the function above
  for(i in 1:nrow(sleepData)) {
    for(j in 2:ncol(sleepData)) {
      if(is.na(sleepData[i,j])) {
        sleepData[i,j] == NA
      } else if(sleepData[i,j] == 0) {
        sleepData[i,j] = NA
      }
    }
  }
  
  #Convert numeric to Posict
  for(i in 2:ncol(sleepData)) {
    sleepData[,i] <- round(sleepData[,i]/1000)
    sleepData[,i] <- as.POSIXct(sleepData[,i], origin = "1970-01-01", tz = "America/New_York")
  }
  
  #delete row names before using colum to rownames
  row.names(sleepData) <- NULL
  
  #alias col to row name
  sleepData <- column_to_rownames(sleepData, var = "alias")
  
  #Change col name format
  colnames(sleepData)[1:ncol(sleepData)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  return(sleepData)
}


#Sleep Overall Avg for Daily Table # Used median
sleepDailyAvg <- function(sleepData) {
  
  sleepData$time <- times(strftime(sleepData$time,"%H:%M:%S"))
  
  sleepData <- sleepData %>% drop_na() #tidyr
  
  #convert times to numeric
  sleepData$time <- period_to_seconds(hms(as.character(sleepData$time)))

  bedTimeAvg <- sleepData %>%
    filter(alias == "Bed Time") %>%
    summarise("Historical Average" = median(time))
  
  #If data has odd number of row for bed time, median selects the number in the middle. Otherwise there is an issue because there will 2 numbers in the middle. Average of them will be median
  #If one of the middle numbers value is over 11:59 then mean of them will give a very small bed time. So we'll asume that Median value of wake up should be always between 18:00 and 02:00. Other wise we'll add 12 hours. Also,
  #when you added 12 hours if total is greater than 24 hours substract 12 hours
  #To get the "actual" bedtime
  #If median returns between 6:00 (21600) and 12:00 (43200) add 12 hours
  tryCatch(if(bedTimeAvg[1,1] > 7200 & bedTimeAvg[1,1] < 64800) {  
    
    bedTimeAvg[1,1] <- bedTimeAvg[1,1] + 43200   #12 hours
      
      if(bedTimeAvg[1,1] > 86400) {
        
        bedTimeAvg[1,1] <- bedTimeAvg[1,1] - 86400
        
      }
    
    }, error = function(e){'no sleep data for some week(s)'})
  
  #Change to times format
  bedTimeAvg$`Historical Average` <- times(secToFormat(bedTimeAvg$`Historical Average`))
  
  wakeUpAvg <- sleepData %>%
    filter(alias == "Wake up Time") %>%
    summarise("Historical Average" = median(time))
  
  #Change to times format
  wakeUpAvg$`Historical Average` <- times(secToFormat(wakeUpAvg$`Historical Average`))
  
  combinedAvg <- rbind.data.frame(bedTimeAvg,wakeUpAvg)
  
  
  row.names(combinedAvg) <- c("Bed Time","Wake up Time")
  
  return(combinedAvg)
}



###Time In Bed
### duration in sleepData table gives Time In Bed
### The reason we don't basicall do wake up tim - bed time is that people do not always sleep straight 7 hours or etc.
### So going forward we can display timeInbed as we want, for example we can display total timeinbed or longest duration as timeinbed, etc.
timeInBedDaily <- function(sleepData) {
  
  timeInBed <- sleepData %>%
    select(dateFormat,duration)
  
  #Remove duplicate values
  timeInBed <- unique(timeInBed)
  
  #create an alias for TimeInBed
  alias <- rep(c("Time Spent In Bed"), times = length(timeInBed$dateFormat))
  
  #create an alias for TimeInBed
  timeInBed <- cbind.data.frame(alias,timeInBed)
  
  timeInBed <- timeInBed %>%
    #This for aliases to display
    add_row(alias = as.character(unique(timeInBed$alias)),dateFormat=as.Date(as.character(end_time_low_level)),duration = 0) %>%
    #This is for days to display even tho there wasn't any activitiy in the day. To display 0 
    add_row(alias = "Dummy",dateFormat=seq(as.Date(as.character(end_time_low_level))-6,as.Date(as.character(end_time_low_level)), by=1),duration=0) %>%
    filter(dateFormat >= as.Date(as.character(end_time_low_level)) - 6) %>%
    group_by(alias,dateFormat) %>%
    summarise(duration = sum(duration))
  
  #msec to hr
  timeInBed$duration <- round(timeInBed$duration /1000,1)
  
  timeInBed <- dcast(timeInBed,alias ~ dateFormat)
  
  timeInBed <- timeInBed[- grep("Dummy", timeInBed$alias),]
  
  #This is for to display NA instead of 0. related to add_row in the function above
  for(i in 2:ncol(timeInBed)) {
   
      if(is.na(timeInBed[,i])) {
        timeInBed[,i] == NA
      } else if(timeInBed[,i] == 0) {
        timeInBed[,i] = NA
      }
  }
  
  colnames(timeInBed)[2:ncol(timeInBed)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  row.names(timeInBed) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(timeInBed)
}


#Time in bed average

timeBedDailyAvg <- function(sleepData) {
  timeAvg <- sleepData %>%
    select("dateFormat", "duration")
  
  timeAvg <- unique(timeAvg)
  
  timeAvg <- timeAvg %>% drop_na() #tidyr 
  
  timeAvg <- round(mean(timeAvg$duration/1000),1)
  
  return(timeAvg)
}

#SleepDist Daily

sleepDistruptionDaily <- function (sleepData) {
  
  distruption <- sleepData %>%
    select("dateFormat","sleepDist")
  
  #remove duplicate values
  distruption <- unique(distruption)
  
  #create an alias for Sleep Distruption
  alias <- rep(c("Sleep Distruption"), times = length(distruption$dateFormat))
  
  #create an alias for TimeInBed
  distruption <- cbind.data.frame(alias,distruption)
  
  distruption <- distruption %>%
    #This for aliases to display
    add_row(alias = as.character(unique(distruption$alias)),dateFormat=as.Date(as.character(end_time_low_level)),sleepDist = 0) %>%
    #This is for days to display even tho there wasn't any activitiy in the day. To display 0 
    add_row(alias = "Dummy",dateFormat=seq(as.Date(as.character(end_time_low_level))-6,as.Date(as.character(end_time_low_level)), by=1),sleepDist=0) %>%
    filter(dateFormat >= as.Date(as.character(end_time_low_level)) - 6) %>%
    group_by(alias,dateFormat) %>%
    summarise(sleepDist = sum(sleepDist))
  
  
  
  distruption <- dcast(distruption,alias ~ dateFormat)
  
  #Delete the dumy column
  distruption <- distruption[- grep("Dummy", distruption$alias),]
  
  distruption[is.na(distruption)] <- 0 
  
  colnames(distruption)[2:ncol(distruption)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  row.names(distruption) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(distruption)
  
}

#SleepDist Daily Avg


sleepDistDailyAvgFun <- function(sleepData) {
  
  distDaily <- sleepData %>%
    select("dateFormat", "sleepDist")
  
  distDaily <- unique(distDaily)
  
  distDaily <- distDaily %>% drop_na() #tidyr 
  
  distDaily <- round(mean(distDaily$sleepDist),1)
  
  return(distDaily)  
  
}


###OUTHOME DAILY

filterOutHomeDaily <- function(sleepAndOut) {
  outHomeDaily <- sleepAndOut %>%
    select(dateFormatOut,timeSpentOutside)

  outHomeDaily$timeSpentOutside <- round(as.numeric(outHomeDaily$timeSpentOutside),1)
  alias <- rep(c("Time Spent Outside"), times = length(outHomeDaily$dateFormatOut))
  
  #combine alias and outhome
  outHomeDaily <- cbind.data.frame(alias,outHomeDaily)
  
  outHomeDaily[is.na(outHomeDaily)] <- 0 

  outHomeDaily <- outHomeDaily %>%
    #This for aliases to display
    add_row(alias = unique(outHomeDaily$alias),dateFormatOut=as.Date(as.character(end_time_low_level)),timeSpentOutside=0) %>%
    #This is for days to display even tho there wasn't any data recorded for a day. To display 0 
    add_row(alias = "Dummy",dateFormatOut=seq(as.Date(as.character(end_time_low_level))-6,as.Date(as.character(end_time_low_level)), by=1),timeSpentOutside=0) %>%
    filter(dateFormatOut >= as.Date(as.character(end_time_low_level)) - 6) %>%
    group_by(alias,dateFormatOut) %>%
    summarise(timeSpentOutside = sum(timeSpentOutside))
  
  #min to sec. we ll use secTomin func
  outHomeDaily$timeSpentOutside <- round(outHomeDaily$timeSpentOutside*60,1)
  
  outHomeDaily <- dcast(outHomeDaily,alias ~ dateFormatOut)
  
  outHomeDaily[is.na(outHomeDaily)] <- 0 
  
  outHomeDaily <- outHomeDaily[- grep("Dummy", outHomeDaily$alias),]
  
  # #Convert all 0s to NA because we don't know if they are NA or didn't go out
  # 
  # for(i in 2:ncol(outHomeDaily)) {
  #   if(outHomeDaily[,i] == 0 | is.na(outHomeDaily[,i]) ) {
  #     outHomeDaily[,i] <- NA
  #   }
  # }
  
  
  colnames(outHomeDaily)[2:ncol(outHomeDaily)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  row.names(outHomeDaily) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(outHomeDaily)
  
}

#Outhome daily Avg

outHomeDailyAvgFunc <- function(sleepAndOut) {
  
  outHomeDaily <- sleepAndOut %>%
    select(dateFormatOut,timeSpentOutside)
  
  outHomeDaily[is.na(outHomeDaily)] <- 0 
  #dif to numberic
  outHomeDaily$timeSpentOutside <- round(as.numeric(outHomeDaily$timeSpentOutside),1)
  
  outHomeAvg <- round(mean(outHomeDaily$timeSpentOutside*60),1)
  
  return(outHomeAvg)
  
}



filterSleepWeekly <- function(sleepData) {
  
  sleepData$time <- times(strftime(sleepData$time,"%H:%M:%S"))
  
  sleepData <- sleepData %>% drop_na() #tidyr
  
  #convert times to numeric
  sleepData$time <- period_to_seconds(hms(as.character(sleepData$time)))
  
  sleepData <- sleepData %>%
    select(-c(duration)) %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for a week
    add_row(alias = "Dummy",time = 0,dateFormat = seq(as.Date(as.character(third)),as.Date(as.character(end_time_low_level)),
                                                               by=7)) %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                               ifelse(dateFormat <= second, as.Date(as.character(second)),
                               ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(alias,dateFormat) %>%
    summarise(time = median(time))
  
  #ifelse returns numeric so convert dateFormat to Date format
  sleepData$dateFormat <- as.Date(sleepData$dateFormat, origin = "1970-01-01", tz = "America/New_York")
  
  #before casting time should be at last col index  
  sleepData <- sleepData[,c("alias","dateFormat","time")]

  #Cast
  sleepData <- dcast(sleepData,alias~dateFormat)
  
  #Delete the dumy column
  sleepData <- sleepData[- grep("Dummy", sleepData$alias),]
  
  #If data has odd number of row for bed time, median selects the number in the middle. Otherwise there is an issue because there will 2 numbers in the middle. Average of them will be median
  #If one of the middle numbers value is over 11:59 then mean of them will give a very small bed time. So we'll asume that Median value of wake up should be always between 04:00 and 1800. Other wise we'll add 12 hours. Also,
  #when you added 12 hours if total is greater than 24 hours substract 12 hours
  #To get the "actual" bedtime
  #If median returns between 4:00 (21600) and 6:00 (64800) add 12 hours
  
  for(i in 2:ncol(sleepData)) {
  
  tryCatch(if(sleepData[1,i] > 14400 & sleepData[1,i] < 64800) {  
    
    sleepData[1,i] <- sleepData[1,i] + 43200   #12 hours
    
    if(sleepData[1,i] > 86400) {
      
      sleepData[1,i] <- sleepData[1,i] - 86400
      
    }
    
  }, error = function(e){'no sleep data for some week(s)'})
  
}
  

  #seconds to hms. When there is NA value in bed/wakeup times, times() returns days not hms. Tried looping thru each cell but ...
  for(i in 2:ncol(sleepData)) {
    
    if(is.na(sleepData[,i])) {
      sleepData[,i] = NA
    } else {
      sleepData[,i] <- times(secToFormat(sleepData[,i]))
      }
  }  
  
  #delete row names before using colum to rownames
  row.names(sleepData) <- NULL
  
  sleepData <- column_to_rownames(sleepData, var = "alias")

  #Change col name format
  colnames(sleepData)[1:ncol(sleepData)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  return(sleepData)
}

#Sleep Weekly Avg
##### Average col
sleepWeeklyAvg <- function(sleepData) {
  
  sleepData$time <- times(strftime(sleepData$time,"%H:%M:%S"))
  sleepData <- sleepData %>% drop_na() #tidyr # I need this. otherwise median value of a week can be NA even if there is only 1 NA in a week
  
  #convert times to numeric
  sleepData$time <- period_to_seconds(hms(as.character(sleepData$time)))
  
   sleepData <- sleepData %>%
    select(-c(duration)) %>%
   #divide 28 days into 4 weeks  
   mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                          ifelse(dateFormat <= second, as.Date(as.character(second)),
                          ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
     group_by(alias,dateFormat) %>%
     summarise(time = median(time))

  ####
  bedTimeAvg <- sleepData %>%
    filter(alias == "Bed Time") 
  
  #To get the correct median when passes midnight
  
  for(i in 1:nrow(bedTimeAvg)) {
  
    tryCatch(if(bedTimeAvg[i,"time"] < 18000 ) {  #18000 is 5 AM
    
    bedTimeAvg[i,"time"] <- bedTimeAvg[i,"time"] + 86400   #24 hours
    
  }, error = function(e){'no sleep data for some week(s)'})
}
  
  bedTimeAvg <- bedTimeAvg %>%
    summarise("Historical Average" = median(time))
  
  #if it is over one day substract one day, 1 day = 86400 secs
  if(bedTimeAvg[1,2] >= 86400) {
    bedTimeAvg[1,2] <- bedTimeAvg[1,2] - 86400
  }
  
  #If data has odd number of row for bed time, median selects the number in the middle. Otherwise there is an issue because there will 2 numbers in the middle. Average of them will be median
  #If one of the middle numbers value is over 11:59 then mean of them will give a very small bed time. So we'll asume that Median value of wake up should be always between 18:00 and 04:00. Other wise we'll add 12 hours. Also,
  #when you added 12 hours if total is greater than 24 hours substract 12 hours
  #To get the "actual" bedtime
  #If median returns between 6:00 (21600) and 12:00 (43200) add 12 hours
  tryCatch(if(bedTimeAvg[1,2] > 14400 & bedTimeAvg[1,2] < 64800) {  
    
    bedTimeAvg[1,2] <- bedTimeAvg[1,2] + 43200   #12 hours
    
    if(bedTimeAvg[1,2] > 86400) {
      
      bedTimeAvg[1,2] <- bedTimeAvg[1,2] - 86400
      
    }
    
  }, error = function(e){'no sleep data for some week(s)'})
  
  #Change to times format
  bedTimeAvg$`Historical Average` <- times(secToFormat(bedTimeAvg$`Historical Average`))
  
  wakeUpAvg <- sleepData %>%
    filter(alias == "Wake up Time") %>%
    summarise("Historical Average" = median(time))
  
  #Change to times format
  wakeUpAvg$`Historical Average` <- times(secToFormat(wakeUpAvg$`Historical Average`))
  
  combinedAvg <- rbind.data.frame(bedTimeAvg,wakeUpAvg)

  return(combinedAvg)
}


timeInBedWeeklyFun <- function(sleepData) {
  
  timeInBed <- sleepData %>%
    select(dateFormat,duration)
  
  #Remove duplicate values
  timeInBed <- unique(timeInBed)
  
  #create an alias for TimeInBed
  alias <- rep(c("Time Spent In Bed"), times = length(timeInBed$dateFormat))
  
  #create an alias for TimeInBed
  timeInBed <- cbind.data.frame(alias,timeInBed)
  
  timeInBed <- timeInBed %>% drop_na()
  
  timeInBed <- timeInBed %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for a week
    add_row(alias = "Dummy",dateFormat = seq(as.Date(as.character(third)),as.Date(as.character(end_time_low_level)),
                                             by=7),duration=0) %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                        ifelse(dateFormat <= second, as.Date(as.character(second)),
                        ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(alias,dateFormat) %>%
    summarise(duration = sum(duration))
  
  #ifelse returns numeric so convert dateFormat to Date format
  timeInBed$dateFormat <- as.Date(timeInBed$dateFormat, origin = "1970-01-01", tz = "America/New_York")
  
  #msec to hr
  timeInBed$duration <- round(timeInBed$duration /1000,1)
  
  timeInBed <- dcast(timeInBed,alias ~ dateFormat)
  
  timeInBed <- timeInBed[- grep("Dummy", timeInBed$alias),]
  
  colnames(timeInBed)[2:ncol(timeInBed)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  row.names(timeInBed) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(timeInBed)
}

#Time in bed average

timeBedWeeklyAvgFun <- function(sleepData) {
  
  
  timeAvg <- sleepData %>%
    select("dateFormat", "duration") 
  
  timeAvg <- unique(timeAvg)
  
  timeAvg <- timeAvg %>% drop_na() #tidyr
  
  timeAvg <- timeAvg %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                               ifelse(dateFormat <= second, as.Date(as.character(second)),
                                      ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(dateFormat) %>%
    summarise(duration = sum(duration))
  
  timeAvg <- sum(timeAvg$duration)/length(unique(timeAvg$dateFormat))
  
  timeAvg <- round(mean(timeAvg/1000),1)
  
  return(timeAvg)
}

###Sleep Distruption
sleepDistruptionWeekly <- function(sleepData) {
  
  distruption <- sleepData %>%
    select("dateFormat","sleepDist")
  
  #remove duplicate values
  distruption <- unique(distruption)
  
  #create an alias for Sleep Distruption
  alias <- rep(c("Sleep Distruption"), times = length(distruption$dateFormat))
  
  #create an alias for TimeInBed
  distruption <- cbind.data.frame(alias,distruption)
  
  distruption <- distruption %>% drop_na()
  
  distruption <- distruption %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for a week
    add_row(alias = "Dummy",dateFormat = seq(as.Date(as.character(third)),as.Date(as.character(end_time_low_level)),
                                             by=7),sleepDist=0) %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                               ifelse(dateFormat <= second, as.Date(as.character(second)),
                                      ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(alias,dateFormat) %>%
    summarise(sleepDist = sum(sleepDist))
  
  #ifelse returns numeric so convert dateFormat to Date format
  distruption$dateFormat <- as.Date(distruption$dateFormat, origin = "1970-01-01", tz = "America/New_York")
  
  
  distruption <- dcast(distruption,alias ~ dateFormat)
  
  #Delete the dumy column
  distruption <- distruption[- grep("Dummy", distruption$alias),]
  
  colnames(distruption)[2:ncol(distruption)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  row.names(distruption) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(distruption)
  
  
}

sleepDistWeeklyAvgFun <- function(sleepData) {
  
  distWeekly <- sleepData %>%
    select("dateFormat", "sleepDist")
  
  distWeekly <- unique(distWeekly)
  
  distWeekly <- distWeekly %>% drop_na() #tidyr 
  
  distWeekly <- distWeekly %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormat = ifelse(dateFormat <= third, as.Date(as.character(third)),
                               ifelse(dateFormat <= second, as.Date(as.character(second)),
                                      ifelse(dateFormat <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(dateFormat) %>%
    summarise(sleepDist = sum(sleepDist))
  
  distWeekly <- sum(distWeekly$sleepDist)/length(unique(distWeekly$dateFormat))
  
  distWeekly <- round(mean(distWeekly),1)
  
  return(distWeekly)
  
}


filterOutHomeWeekly <- function(sleepAndOut) {
  outHomeWeekly <- sleepAndOut %>%
    select(dateFormatOut,timeSpentOutside)
  
  outHomeWeekly$timeSpentOutside <- round(as.numeric(outHomeWeekly$timeSpentOutside),1)
  alias <- rep(c("Time Spent Outside"), times = length(outHomeWeekly$dateFormatOut))
  
  #combine alias and outhome
  outHomeWeekly <- cbind.data.frame(alias,outHomeWeekly)
  
  outHomeWeekly[is.na(outHomeWeekly)] <- 0 #returns NA when there isn't any event recorded
  
  outHomeWeekly <- outHomeWeekly %>%
    # add dummy rows to make sure all the activities are displayed even tho they didn't occur for a week
    add_row(alias = "Dummy",dateFormatOut = seq(as.Date(as.character(third)),as.Date(as.character(end_time_low_level)),
                                                by=7),timeSpentOutside=0) %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormatOut = ifelse(dateFormatOut <= third, as.Date(as.character(third)),
                                  ifelse(dateFormatOut <= second, as.Date(as.character(second)),
                                         ifelse(dateFormatOut <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(alias,dateFormatOut) %>%
    summarise(timeSpentOutside = sum(timeSpentOutside))
  
  #ifelse returns numeric so convert dateFormat to Date format
  outHomeWeekly$dateFormatOut <- as.Date(outHomeWeekly$dateFormatOut, origin = "1970-01-01", tz = "America/New_York")
  
  #min to sec. we ll use secTomin func
  outHomeWeekly$timeSpentOutside <- round(outHomeWeekly$timeSpentOutside*60,1)
  
  outHomeWeekly <- dcast(outHomeWeekly,alias ~ dateFormatOut)
  
  outHomeWeekly <- outHomeWeekly[- grep("Dummy", outHomeWeekly$alias),]
  
  colnames(outHomeWeekly)[2:ncol(outHomeWeekly)] %<>% strptime(., "%Y-%m-%d") %>% format(., "%b %d, %A")
  
  
  row.names(outHomeWeekly) = NULL # To be able use column_to_row() there shouldn't be any rownames
  
  return(outHomeWeekly)
}


#Outhome weekly avg. ####no need for weekly avg. Moving forward you can add an average column in outHomeWeekly

outHomeWeeklyAvgFun <- function(sleepAndOut) {
  
  outHomeWeekly <- sleepAndOut %>%
    select(dateFormatOut,timeSpentOutside)
  
  outHomeWeekly[is.na(outHomeWeekly)] <- 0 
  #dif to numberic
  outHomeWeekly$timeSpentOutside <- round(as.numeric(outHomeWeekly$timeSpentOutside),1)
  
  outHomeWeekly <- outHomeWeekly %>%
    #divide 28 days into 4 weeks  
    mutate(dateFormatOut = ifelse(dateFormatOut <= third, as.Date(as.character(third)),
                                  ifelse(dateFormatOut <= second, as.Date(as.character(second)),
                                         ifelse(dateFormatOut <= first, as.Date(as.character(first)), as.Date(as.character(end_time_low_level)))))) %>%
    group_by(dateFormatOut) %>%
    summarise(timeSpentOutside = sum(timeSpentOutside))
  
  outHomeAvg <- round(mean(outHomeWeekly$timeSpentOutside*60),1)
  
  return(outHomeAvg)
  
}



###Time Conversion and Displayed data functions

#Seconds to hours and mins (hr min)
secToHr <- function(seconds) {
  hours <- seconds/60/60
  h <- hours%/% 1
  m <- ((hours%% 1)*60) %/% 1
  resultH <- paste(h,"hr",sep = "")
  resultM <- paste(m,"min",sep = "")
  result <- paste(resultH,resultM,sep =" ")
  return(result)
}


#For sleep data hours to hr min
hoursToHr <- function(hours) {
  h <- hours%/% 1
  m <- ((hours%% 1)*60) %/% 1
  resultH <- paste(h,"hr",sep = "")
  resultM <- paste(m,"min",sep = "")
  result <- paste(resultH,resultM,sep =" ")
  return(result)
}

#func for min to hr
minToHr <- function(min) {
  h <- min%/% 60
  m <- min%% 60
  resultH <- paste(h,"hr",sep = "")
  resultM <- paste(m,"min",sep = "")
  result <- paste(resultH,resultM,sep =" ")
  return(result)
}



#change time format for duration (seconds) from numeric to hr min for display purpose
numericToHrM <- function(timeData) {
  
  for(i in 1:nrow(timeData)) {
    
    for(j in 1:ncol(timeData)) {
      
      timeData[i,j] <- secToHr(as.numeric(timeData[i,j]))
    }
    
  }
  
  return(timeData)
}

#numeric to char for count data
numericToChar <- function(filteredCount) {
  
  filteredCount[,1:ncol(filteredCount)] <- apply(filteredCount[,1:ncol(filteredCount)],1,function(x) as.character(x))
  
  return(filteredCount)
}



# Table coloring cell style
styleAbove <- c('black; background-color: #BEE6FE; border-radius: 30px; padding: 3px')
styleBelow <- c('black; background-color: #FDDCDD; border-radius: 30px; padding: 3px')