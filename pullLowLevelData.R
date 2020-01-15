
#today() - 1 day?
#Determine start,end time and location ID
#end_time_low_level = as.POSIXct(paste(config.location[[houseID]]$params$end_time_low_level,"23:59:59"))
flog.debug("Requested date is %s",myDate)
end_time_low_level = as.POSIXct(paste(myDate,"23:59:59"))

start_time_low_level = as.POSIXct(paste(as.character(as.Date(end_time_low_level - (60*60*24*28))), "00:00:00"))  

location_id = as.numeric(config.location[[houseID]]$location_id)

#data base connection
flog.debug("Connecting zproc for lowLevelData..., LOCATION: %s",location_id)

con <- dbConnect(drv = RMySQL::MySQL(), default.file = '~/.my.zproc')

if(is.null(con)) {
  flog.error("Couldn't connect to the database for lowLevelData,LOCATION: %s",location_id)
}

#Sensor Aliasses
sensor_search_regexp <- "'Television|Pantry|Machine|Fridge|Microwave|Kitchen|Living|Bathroom|Office|Toaster|Coffee Pot|Washing|Bath|Shower'"

# Function to get sensor data from longterm table for report
generate_report_sensordata <- function(search_exp=sensor_search_regexp, start_time, end_time) {
  #get sensor metadata
  query<-paste('select id, alias, report_unit, on_value from sensor_metadata where location_id=', location_id, ' and report_unit != "value" and alias REGEXP ', search_exp)
  sensor_metadata<-dbGetQuery(con, query)
  report_sensors<-sensor_metadata$id
  
  # using list of sensor ids from metadata, obtain sensor data from longterm table
  report_query_results<-data.frame()
  query<-paste('select sensor_id, value, start_time, end_time, duration from longterm where is_original=1 and sensor_id in (', paste(report_sensors, collapse = ', '), ')  and start_time >="', start_time_low_level, '" and end_time <="', end_time_low_level,'"')
  report_query_results<- dbGetQuery(con, query)
  
  #join metadata with sensor data
  query_results<-merge(sensor_metadata, report_query_results, by.x="id", by.y="sensor_id")
  return(query_results)
}

flog.debug("Getting lowLevelData..., LOCATION: %s",location_id)
sensorData <- generate_report_sensordata(,start_time_low_level, end_time_low_level)
if(nrow(sensorData)==0) {
  flog.error("Failed to get the lowLevelSensor data, LOCATION: %s",location_id)
}
#disconnect from dbase
dbDisconnect(con)
