#Libriaries
library(gmailr)
library(httr)
library(yaml)
library(rmarkdown)
###Pull low level Data
library(sqrrl)
library(dplyr)
library(DBI)
library(reshape2)
library(futile.logger)
library(lubridate)
library(RMySQL)
library(stringr)
library(tibble)
library(tidyr)
#Function
library(knitr)
library(chron)
library(kableExtra)
library(rowr)

flog.threshold('DEBUG')

flog.appender(appender.file(paste0("/home/nmahser/Wilson_Summary_Reports/log/WilsonSumRep-",as.character(today()),".log")))

#if you want to generate the report for the specific day
#myDate <- as.character("2019-05-25")
#myDate <- as.character(as.Date(today()))

#load yaml file
config <- yaml.load_file("/home/nmahser/Wilson_Summary_Reports/config/config.yaml")
config.location <<- config$location

#Create new folder in the main directory
try(dir.create(paste0('/home/nmahser/Wilson_Summary_Reports/reports/',as.character(today()))))

#This function calls generateReport.Rmd
runRmd <- function(file = "generateReport.Rmd") {
  
  houseID <<- 0
  
  for(i in 1:length(config.location)) {
    
    houseID <<- houseID + 1 #global variable
    
    myDate <<- config.location[[houseID]]$params$end_date
    
    
    
    if(myDate == "today()") {
       myDate <<- as.character(as.Date(today()) -1)
      } else{
        myDate <<- config.location[[houseID]]$params$end_date
    }
    
    tryCatch(render("/home/nmahser/Wilson_Summary_Reports/generateReport.Rmd", output_format = "html_document",output_file = paste0("House"," ",as.numeric(config.location[[houseID]]$location_id),".html"),
                    output_dir=paste0("./Wilson_Summary_Reports/reports/",as.character(today()))),
             error = function(e) 
             {print(paste0("House"," ",as.numeric(config.location[[houseID]]$location_id)," 's report wasn't generated"))})
    
  }
  
}

runRmd(generateReport.Rmd)

setwd("Wilson_Summary_Reports/reports")

folderName <- paste0('Reports-',as.character(today()))

zip(zipfile = folderName, files = paste0(as.character(today())))


#zip(zipfile = paste0('/home/nmahser/homesense-summaryReport/reports/',as.character(today()),'/SummaryReports'), files = paste0('/home/nmahser/homesense-summaryReport/reports/',as.character(today())))


#Number of file in today()'s directory

numberOfFiles <- list.files(path = paste0('/home/nmahser/Wilson_Summary_Reports/reports/',as.character(today())))

###Send e-mails
## edit line below to reflect YOUR json credential filename
use_secret_file("/home/nmahser/Wilson_Summary_Reports/autoReport.json")

body_text <- paste("Please see the attached file to view HomeSense Summary Reports, which were generated on",as.character(today()),".   

Thank You,                  
HomeSense")

#CREATE A CHECK BEFORE SENDING THE E-MAILS
if(length(numberOfFiles) > 6) {
  mime() %>%
    to(as.character(config.location[[1]]$actions)) %>%  #
    from("alwaystrial123@gmail.com") %>%
    subject("HomeSense Summary Reports") %>%
    html_body(body_text) %>%
    attach_part(body_text) %>%
    attach_file(paste0('/home/nmahser/Wilson_Summary_Reports/reports/',folderName,".zip")) %>%
    send_message()
} else {
  
  mime() %>%
    to("nmahser@mail.usf.edu") %>%  #
    from("alwaystrial123@gmail.com") %>%
    subject("Report wasn't generated") %>%
    html_body(body_text) %>%
    attach_part(body_text) %>%
    #attach_file(paste0('/home/nmahser/homesense-summaryReport/reports/',folderName,".zip")) %>%
    send_message()
}

#Remove the generated file
file.remove(paste0(folderName,".zip"))

setwd("..")
setwd("..")

rm(list = ls())
