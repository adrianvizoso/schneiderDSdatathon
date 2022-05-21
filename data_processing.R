source("utilities.R")
library(httr)
library(jsonlite)
library(pdftools)
library(readr)
library(data.table)
library(caret)
library(VGAM)

###############################################################
#                   Data reading and preparation 
###############################################################

# Read data from csv

train1 <- read.csv2("train1.csv",header = T,sep = ",")
train2 <- read.csv2("train2.csv",header = T,sep = ";")

pred_df <- read.csv2("test_x.csv",header = T,sep = ",")

data_csv <- data.table(rbind(train1,train2))


# Read data from apis

links <- c("http://schneiderapihack-env.eba-3ais9akk.us-east-2.elasticbeanstalk.com/first",
           "http://schneiderapihack-env.eba-3ais9akk.us-east-2.elasticbeanstalk.com/second",
           "http://schneiderapihack-env.eba-3ais9akk.us-east-2.elasticbeanstalk.com/third")

list_data_API <- lapply(X = links,function(x){readAPI(x)})

data_api <- data.table(do.call(rbind,list_data_API))

data_api[, V1 := NULL]

# Standardize names in order to bind rows

data.table::setnames(data_csv,setdiff(names(data_csv),names(data_api)),c("DAY WITH FOGS","REPORTER NAME","CITY ID"))

# Remove EPRTRSectorCode code (enough with names) and treat EPRTRAnnexIMainActivityCode 

data_api[, EPRTRSectorCode := NULL]

eprtr_master <- unique(data_api[,.(EPRTRAnnexIMainActivityCode,EPRTRAnnexIMainActivityLabel)])

data_csv <- merge.data.table(x = data_csv ,y = eprtr_master,by = "EPRTRAnnexIMainActivityLabel",all.x = T)

data_csv_api <- rbind(data_csv,data_api)


#Read data from pdfs

files <- list.files("C:/Users/adria/Downloads/train6/train6",all.files = T,pattern = ".pdf",full.names = T)

list_data_pdfs <- lapply(X = files,function(x){readPDF(pdftools::pdf_text(pdf = x))})

data_pdfs <- do.call(rbind,list_data_pdfs)

data_pdfs <- merge.data.table(x = data_pdfs,y = eprtr_master,
                              by.x = "main_activity_code", 
                              by.y = "EPRTRAnnexIMainActivityCode",all.x = T)

setnames(data_pdfs,old = "EPRTRAnnexIMainActivityLabel",new = "main_activity_label")

# Standardize names

setnames(data_csv_api,old = names(data_csv_api),new = c("main_activity_label","country","sector_name","facility_id","facility_name",
                                                        "city","target_realase","pollutant","year","month","day","continent","max_wind_speed",
                                                        "avg_wind_speed","min_wind_speed","max_temp","avg_temp","min_temp","fog_days",
                                                        "reporter_name","city_id","main_activity_code") )

all_data <- na.omit(rbind(data_csv_api,data_pdfs))

all_data[,target_realase := NULL] # All air

all_data[,continent := NULL] # All Europe

all_data$pollutant <- factor(x = all_data$pollutant); all_data$sector_name <- factor(x = all_data$sector_name); all_data$main_activity_label <- factor(x = all_data$main_activity_label);
all_data$city <- factor(x = all_data$city); all_data$country <- factor(x = all_data$country);
all_data$max_wind_speed <- as.numeric(all_data$max_wind_speed); all_data$avg_wind_speed <- as.numeric(all_data$avg_wind_speed); all_data$min_wind_speed <- as.numeric(all_data$min_wind_speed) 
all_data$max_temp <- as.numeric(all_data$max_temp);all_data$avg_temp <- as.numeric(all_data$avg_temp);all_data$min_temp <- as.numeric(all_data$min_temp)
all_data$fog_days <- as.numeric(all_data$fog_days)


