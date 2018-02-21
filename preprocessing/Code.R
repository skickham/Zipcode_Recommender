#set directory
setwd("C:/Users/skick/Desktop/CKM/")

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(lubridate)

# #load 311 data
# #system.time(fread("data/311/med311_data.csv")) #took 16.5 minutes, need to find way to break this down!
# data.311 = fread("data/311/med311_data.csv")
# class(data.311)
# head(data.311, 5)
# 
# #summary for the data
# summary(data.311)
# nrow(data.311)          # 4.7 million (4765404)
# colnames(data.311)      
# 
# #delete duplicated rows
# nrow(distinct(data.311))      # 4755404 (10,000 duplicates)
# data.311[duplicated(data.311$Unique.Key) | duplicated(data.311$Unique.Key, fromLast = TRUE), ]  # all the same complaint at the same address
# data.311 = distinct(data.311)
# 
# #inspect certain columns
# data.311[,.N,by=Agency.Name]                        # delete
# data.311[,.N,by=Agency]
# data.311[,.N,by=City]                               # delete
# data.311[,.N,by=Descriptor][order(-N)][N>=5000]     # delete
# data.311[,.N,by=Location.Type][order(-N)][1:100]    # delete or update to residential/commercial/public
# data.311[,.N,by=Incident.Address]                   # delete
# data.311[,.N,by=Landmark][order(-N)]                # delete
# data.311[,.N,by=Facility.Type][order(-N)]           # delete
# data.311[,.N,by=Ferry.Direction]                    # delete
# data.311[,.N,by=Ferry.Terminal.Name]                # delete
# data.311[,.N,by=Status][order(-N)]                  # delete
# data.311[,.N,by=Due.Date][order(-N)]                # delete
# data.311[,.N,by=Resolution.Description][order(-N)]              # may need (violation given and nothing observed)
# data.311[,.N,by=Resolution.Action.Updated.Date][order(-N)]      # delete
# data.311[,.N,by=Community.Board]                    # delete
# data.311[,.N,by=Park.Facility.Name]                 # delete
# data.311[,.N,by=School.Number]                      # delete
# data.311[,.N,by=Complaint.Type]                     # delete
# 
# 
# #delete columns
# data.311[, c("Agency.Name", "Descriptor", 
#              "City", "Incident.Address", "Street.Name", 
#              "Cross.Street.1", "Cross.Street.2", 
#              "Intersection.Street.1", "Intersection.Street.2",
#              "Landmark", "Facility.Type", "Address.Type", "Status",
#              "Due.Date", "Resolution.Action.Updated.Date",
#              "Community.Board", "Borough",
#              "X.Coordinate..State.Plane.", "Y.Coordinate..State.Plane.",
#              "Park.Facility.Name", "Park.Borough",
#              "School.Name", "School.Number", "School.Region",
#              "School.Code", "School.Phone.Number",
#              "School.Address", "School.City", "School.State",
#              "School.Zip", "School.Not.Found", "School.or.Citywide.Complaint",
#              "Vehicle.Type", "Taxi.Company.Borough", "Taxi.Pick.Up.Location",
#              "Bridge.Highway.Name", "Bridge.Highway.Direction",
#              "Road.Ramp", "Bridge.Highway.Segment", "Garage.Lot.Name",
#              "Ferry.Direction", "Ferry.Terminal.Name")
#          := NULL]
# 
# #drop certain columns (may come back to)
# data.311[, c("Location.Type", "Resolution.Description") := NULL]
# 
# #save to csv
# fwrite(data.311, file = "data311.csv")


#open new & improved file
data.311 = fread("data311.csv")

#basic EDA & Missingness
head(data.311, 5)
lapply(data.311, class)     # unique key INT, lat/long NUM, else CHAR
#convertcoltypes
#checkoutvalues and changeblankstoNAs
aggr(data.311)



#zip code investigation
zips = data.311[,.N,by=Incident.Zip][order(-N)]
zips[Incident.Zip<10001][1:100]
zips[Incident.Zip>10286][400:500]

badzips = data.311[,c("Incident.Zip", "Latitude", "Longitude", "Location")][Incident.Zip < 10001 | Incident.Zip > 11697]
badzips[Incident.Zip == ""][, .N, by= Latitude][order(-N)]      # 387096 NAs
badzips[Incident.Zip == ""][, .N, by= Longitude][order(-N)]     # same number of NAs as Lat
data.311[, .N, by= Latitude][order(-N)]             # many more NAs in full dataset

#delete missing Zip/Lat/Long combos



cormat <- round(cor(data.311),2)
head(cormat)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#dealing with missingness




#complaint types
complaint.types = data.311[,.N,by=Complaint.Type][order(-N)]
summary(data.311)
data.311[,order(Incident.Zip)]


nrow(data.311[Incident.Zip<10001])        #376,997 missing zips
nrow(data.311[Incident.Zip>11697])        #1313 bad zips....keep for now, drop when combining unless time to impute


#Convert to Date & Pull Year and Month
data.311$Created.Date = mdy_hms(data.311$Created.Date)
data.311$Year = year(data.311$Created.Date)
data.311$Month = month(data.311$Created.Date)
data.311$Closed.Date = mdy_hms(data.311$Closed.Date)

data.311[,.N,.(Year, Month)]  #everything looks normal, drop 2017 september because far less dates
data.311 = data.311[!(Year == 2017 & Month == 9)]

head(data.311)
summary(data.311)
lapply(data.311, class)

#save to csv
fwrite(data.311, file = "data311.csv")

#read in subsetted data
data.311.1 = fread("data311.csv")


#set directory
setwd("C:/Users/skick/Desktop/CKM/")

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(lubridate)

#load data
nypd = fread("data/NYPD_ComplaintHistory/NYPD_Complaint_Data_Historic.csv")

#quick EDA
head(nypd)
names(nypd)
lapply(nypd, class)   # need to fix date types
summary(nypd)
# aggr(nypd)            #hard to tell with CHARs, but few missing elsewhere

#keep certain columns
nypd = nypd[, .(Key = CMPLNT_NUM,
         Zip = ZIPCODE,
         Date = CMPLNT_FR_DT,
         Time = CMPLNT_FR_TM,
         End.Date = CMPLNT_TO_DT,
         End.Time = CMPLNT_TO_TM,
         Type = OFNS_DESC,
         Boro = BORO_NM,
         Level = LAW_CAT_CD,
         Premises = PREM_TYP_DESC,
         Lat_Long = Lat_Lon)]

#look into some columns more deeply, possibly drop
nypd[, .N, by = Premises][order(-N)][N > 100]   #could condense, drop for now
nypd[, .N, by = Level][order(-N)]               #keep, change to factor
nypd[, .N, by = Boro][order(-N)]                #keep and replace with abbreviations, change to factor
nypd[, .N, by = Time][order(-N)][N>1000]        #combine with date, notice that hour increments most common
nypd[, .N, by = Date][order(-N)][N>1000]        #first of the month seems popular, but not far off
nypd[, .N, by = Type][order(-N)]                #could condense, but drop for now
nypd[, .N, by = Zip][order(-N)]                 #some zips with very few, ie. York College in Jamaica Queens has one report
nypd[, .N, by = Zip][order(-Zip)]               #zip code 83(?) and 146 NAs
nypd[Zip==83][,.N,by=Boro]                      #all missing in Manhattan, probably some pattern.... drop for now, impute with lat/lon later
nypd[Zip==83][,.N,by=Lat_Long][order(-N)]       #highest is the latlong for central park!! --> so are others
nypd[is.na(Zip)][,.N,by=Lat_Long][order(-N)]    #a mix with a lot fewer in manhattan and more in brooklyn, seems like on zip code borders
                                                #overall represents 0.1% of data, so just dropping

#subset further
nypd = nypd[, .(Key, Zip, Date, Time, End.Date, End.Time, Boro, Level, Lat_Long)]
nypd = nypd[Zip != 83 & !is.na(Zip)]

#combine date and times
nypd$Datetime = paste(nypd$Date, nypd$Time)
nypd$Datetime = mdy_hms(nypd$Datetime, tz = Sys.timezone())
# nypd$End.Date = mdy(nypd$End.Date, tz = Sys.timezone())
# nypd$End.Time = hms(nypd$End.Time)
summary(nypd)
nypd = nypd[!is.na(Datetime)]
nypd = nypd[, .(Key, Zip, Date = Datetime, Boro, Level, Lat_Long)]

#Pull important date info
nypd$Year = year(nypd$Date)
nypd$Month = month(nypd$Date)

#convert column types
nypd$Boro = replace(nypd$Boro, nypd$Boro=="BRONX", "BX")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="MANHATTAN", "MN")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="QUEENS", "QN")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="BROOKLYN", "BK")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="STATEN ISLAND", "SI")
nypd[, .N, by = Boro]

nypd[, c("Boro", "Level")] <- lapply(nypd[, c("Boro", "Level")], factor)
lapply(nypd,class)
summary(nypd)
head(nypd)

#save to csv
fwrite(nypd, "nypd.csv")

#read in updated datset
nypd1 = fread("nypd.csv")   #reading it in adds 4 hours (force_tzs)

#set directory
setwd("C:/Users/skick/Desktop/CKM/")

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(lubridate)

#load data
property = fread("data/rolling_property_sales.csv", 
                 stringsAsFactors = FALSE, 
                 integer64 ="character")              #so sale price can be read in correcty

#quick EDA
head(property)
names(property)
lapply(property, class)     #need to fix date types
summary(property)
# aggr(property)            #hard to tell with CHARs, but few missing elsewhere

#delete duplicates
nrow(property)
nrow(distinct(property))  
property = distinct(property)

#convert sale price to numeric
property$`SALE PRICE` = as.numeric(property$`SALE PRICE`)

#inspect some columns
#zips
property[,.N,by=`ZIP CODE`][order(-N)]   
property[,.N,by=`ZIP CODE`][order(-`ZIP CODE`)]                             #722 missing, 2 from Florida
property[`ZIP CODE` == 0, .N, by=NEIGHBORHOOD][order(-N)]
property[`ZIP CODE` == 0, .N, by=ADDRESS][order(-N)]
property[`ZIP CODE` == 0, .N, by=LOT][order(-N)]
property[`ZIP CODE` == 0, .N, by=BLOCK][order(-N)]
property[`ZIP CODE` == 0, .N, by=ADDRESS][order(-N)]                        #neighborhood might be best to impute, but just delete for now 
property = property[`ZIP CODE` != 0 & `ZIP CODE` != 33803]

#sq ft
property[,.N,by=.(`GROSS SQUARE FEET`, `LAND SQUARE FEET`)]                 #508789 missing from both

#price
property[,.N,by=`SALE PRICE`][order(-N)]                                                      #362,822 missing and 8,465 sold for $10
property[`SALE PRICE` ==10]                                                                   #10 million? seems extreme -- looking up addresses doesn't help
property$`SALE PRICE` = replace(property$`SALE PRICE`, property$`SALE PRICE` == 10, 0)
property[`SALE PRICE` < 10000,.N,by=`SALE PRICE`][order(-N)]                                  #still many sold for $1 or $100 or less than $10K
property[`SALE PRICE` < 10000 & `SALE PRICE` > 0,.N,by=`SALE PRICE`][order(-N)][,sum(N)]      #12,314 properties might have bad sale prices
    #Note: just keeping the sale prices for now....can try again later 
    #I changed my mind, I'm going to delete them
    #Deleting's not a good option because they may not be MAR, 
    #but it will move the project along and 12K aint too many out of the total
property$`SALE PRICE` = replace(property$`SALE PRICE`, property$`SALE PRICE` < 10000, 0)


#keep certain columns
property = property[, .(
                Zip = `ZIP CODE`,
                Date = `SALE DATE`,
                Neighborhood = `NEIGHBORHOOD`,
                Tax.Class = `TAX CLASS AT PRESENT`,
                Tax.Class.Before = `TAX CLASS AT TIME OF SALE`,
                Commercial = `COMMERCIAL UNITS`,
                Residential = `RESIDENTIAL UNITS`,
                Units = `TOTAL UNITS`,
                SqFt = `GROSS SQUARE FEET`,
                LandSqFt = `LAND SQUARE FEET`,
                Price = `SALE PRICE`)]

#commercial vs. residential
property[Commercial + Residential != Units]       #18K
property[Commercial + Residential == Units]       #1208K
property[Commercial > Residential]                #82K
property[Commercial < Residential]                #845K
property[Commercial == Residential]               #299K
ggplot(property, aes(Commercial)) +
  geom_density() +
  xlim(c(0.5,10))
property[Commercial > 100, .N, by=Commercial]
property$Commercial = replace(property$Commercial, property$Commercial > 100, NA)

#tax classes
property[, .N, by = Tax.Class][order(-N)]           #condense
property[, .N, by = Tax.Class.Before][order(-N)]
property$Tax.Class = replace(property$Tax.Class, property$Tax.Class == "1A", 1)
property$Tax.Class = replace(property$Tax.Class, property$Tax.Class == "1B", 1)
property$Tax.Class = replace(property$Tax.Class, property$Tax.Class == "1C", 1)
property$Tax.Class = replace(property$Tax.Class, property$Tax.Class == "1D", 1)
property$Tax.Class = replace(property$Tax.Class, property$Tax.Class == "2A", 2)
property$Tax.Class = replace(property$Tax.Class, property$Tax.Class == "2B", 2)
property$Tax.Class = replace(property$Tax.Class, property$Tax.Class == "2C", 2)
property[, .N, by=.(Tax.Class, Tax.Class.Before)]
property[Tax.Class != Tax.Class.Before, .N, by=.(Tax.Class, Tax.Class.Before)][,sum(N)]   #29,072 had tax classes change

#dates
property[, .N, by = Date][order(-N)][N>100]         #hmmm .... dont know what to make of this
property$Date = ymd_hms(property$Date)

property$Year = year(property$Date)
property$Month = month(property$Date)

#check again
head(property)
summary(property)
lapply(property, class)

#Note: condense neighborhoods to boroughs later if time

#save to csv
fwrite(property, "property.csv")

#read in updated datset
property1 = fread("property.csv")   #reading it in adds 4 hours (force_tzs)

#set directory
setwd("C:/Users/skick/Desktop/CKM/")

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(lubridate)

#load data
nypd = fread("data/NYPD_ComplaintHistory/NYPD_Complaint_Data_Historic.csv")

#quick EDA
head(nypd)
names(nypd)
lapply(nypd, class)   # need to fix date types
summary(nypd)
# aggr(nypd)            #hard to tell with CHARs, but few missing elsewhere

#keep certain columns
nypd = nypd[, .(Key = CMPLNT_NUM,
         Zip = ZIPCODE,
         Date = CMPLNT_FR_DT,
         Time = CMPLNT_FR_TM,
         End.Date = CMPLNT_TO_DT,
         End.Time = CMPLNT_TO_TM,
         Type = OFNS_DESC,
         Boro = BORO_NM,
         Level = LAW_CAT_CD,
         Premises = PREM_TYP_DESC,
         Lat_Long = Lat_Lon)]

#look into some columns more deeply, possibly drop
nypd[, .N, by = Premises][order(-N)][N > 100]   #could condense, drop for now
nypd[, .N, by = Level][order(-N)]               #keep, change to factor
nypd[, .N, by = Boro][order(-N)]                #keep and replace with abbreviations, change to factor
nypd[, .N, by = Time][order(-N)][N>1000]        #combine with date, notice that hour increments most common
nypd[, .N, by = Date][order(-N)][N>1000]        #first of the month seems popular, but not far off
nypd[, .N, by = Type][order(-N)]                #could condense, but drop for now
nypd[, .N, by = Zip][order(-N)]                 #some zips with very few, ie. York College in Jamaica Queens has one report
nypd[, .N, by = Zip][order(-Zip)]               #zip code 83(?) and 146 NAs
nypd[Zip==83][,.N,by=Boro]                      #all missing in Manhattan, probably some pattern.... drop for now, impute with lat/lon later
nypd[Zip==83][,.N,by=Lat_Long][order(-N)]       #highest is the latlong for central park!! --> so are others
nypd[is.na(Zip)][,.N,by=Lat_Long][order(-N)]    #a mix with a lot fewer in manhattan and more in brooklyn, seems like on zip code borders
                                                #overall represents 0.1% of data, so just dropping

#subset further
nypd = nypd[, .(Key, Zip, Date, Time, End.Date, End.Time, Boro, Level, Lat_Long)]
nypd = nypd[Zip != 83 & !is.na(Zip)]

#combine date and times
nypd$Datetime = paste(nypd$Date, nypd$Time)
nypd$Datetime = mdy_hms(nypd$Datetime, tz = Sys.timezone())
# nypd$End.Date = mdy(nypd$End.Date, tz = Sys.timezone())
# nypd$End.Time = hms(nypd$End.Time)
summary(nypd)
nypd = nypd[!is.na(Datetime)]
nypd = nypd[, .(Key, Zip, Date = Datetime, Boro, Level, Lat_Long)]

#Pull important date info
nypd$Year = year(nypd$Date)
nypd$Month = month(nypd$Date)

#convert column types
nypd$Boro = replace(nypd$Boro, nypd$Boro=="BRONX", "BX")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="MANHATTAN", "MN")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="QUEENS", "QN")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="BROOKLYN", "BK")
nypd$Boro = replace(nypd$Boro, nypd$Boro=="STATEN ISLAND", "SI")
nypd[, .N, by = Boro]

nypd[, c("Boro", "Level")] <- lapply(nypd[, c("Boro", "Level")], factor)
lapply(nypd,class)
summary(nypd)
head(nypd)

#save to csv
fwrite(nypd, "nypd.csv")

#read in updated datset
nypd1 = fread("nypd.csv")   #reading it in adds 4 hours (force_tzs)

#set directory
setwd("C:/Users/skick/Desktop/CKM/")

#load libraries
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(VIM)
library(lubridate)

# #load data
# fire = fread("data/FireIncidents/Incidents_Responded_to_by_Fire_Companies.csv")
# 
# #quick EDA
# head(fire)
# names(fire)
# lapply(fire, class)   # need to fix date types
# summary(fire)         # lots of NAs, many cols to drop
# aggr(fire)            # very long to run and not accurate because blanks aren't NAs yet
# 
# #keep certain columns
# fire = fire[, .(Key = IM_INCIDENT_KEY, 
#          Zip = ZIP_CODE, 
#          Date = INCIDENT_DATE_TIME, 
#          Type = INCIDENT_TYPE_DESC, 
#          Total.time = TOTAL_INCIDENT_DURATION,
#          Boro = BOROUGH_DESC,
#          Level = HIGHEST_LEVEL_DESC,
#          Property = PROPERTY_USE_DESC)]
# 
# #look into some columns more deeply, possibly drop
# fire[, .N, by = Property][order(-N)][N > 1000]  #drop
# fire[, .N, by = Level][order(-N)]   #keep, but replace values
# fire[, .N, by = Boro][order(-N)]    #keep and replace without numbers
# fire[, .N, by = Total.time][order(-Total.time)]  #measures seconds, drop, but maybe bring back to break into categories
# fire[Total.time == 71011364]      #level 1 but took long
# fire[, .N, by = Type][order(-N)]  #could condense, but drop for now
# fire[, .N, by = Zip][order(-N)]   #28,419 unknown zips (99999) .... cant infer, drop these rows 
# fire[, .N, by = Zip][order(-Zip)] #3 blank zips, drop rows... all other zips in correct range :)
# 
# #subset further
# fire = fire[, .(Key, Zip, Date, Boro, Level)]
# fire = fire[Zip != 99999 & Zip != ""]
# 
# #save to csv
# fwrite(fire, "fire.csv")

#load subsetted data
fire = fread("fire.csv")

#convert column types
lapply(fire, class)
fire$Date =  mdy_hms(fire$Date, tz = Sys.timezone())


#simplify values
fire[, .N, by = Level]
fire[, .N, by = Boro]                       #change to just Boro intial
fire[, .N, by = .(Boro, Level)][order(-N)]  #change Levels to 0 (intial), 1 (else), 7 (signal 7-5)

fire$Boro = replace(fire$Boro, fire$Boro=="2 - Bronx", "BX")
fire$Boro = replace(fire$Boro, fire$Boro=="1 - Manhattan", "MN")
fire$Boro = replace(fire$Boro, fire$Boro=="5 - Queens", "QN")
fire$Boro = replace(fire$Boro, fire$Boro=="4 - Brooklyn", "BK")
fire$Boro = replace(fire$Boro, fire$Boro=="3 - Staten Island", "SI")
fire[, .N, by = Boro]     

fire$Level = replace(fire$Level, fire$Level == "0 - Initial alarm", 0)
fire$Level = replace(fire$Level, fire$Level == "1 - More than initial alarm, less than Signal 7-5", 1)
fire$Level = replace(fire$Level, fire$Level == "2 - 2nd alarm", 1)
fire$Level = replace(fire$Level, fire$Level == "3 - 3rd alarm", 1)
fire$Level = replace(fire$Level, fire$Level == "4 - 4th alarm", 1)
fire$Level = replace(fire$Level, fire$Level == "5 - 5th alarm", 1)
fire$Level = replace(fire$Level, fire$Level == "7 - Signal 7-5", 7)
fire$Level = replace(fire$Level, fire$Level == "", NA)
fire[, .N, by = Level]   

fire[, c("Boro", "Level")] <- lapply(fire[, c("Boro", "Level")], factor)
lapply(fire,class)
summary(fire)

#Pull important date info
fire$Year = year(fire$Date)
fire$Month = month(fire$Date)

#save to csv
fwrite(fire, "fire.csv")

#read in subsetted data
fire1 = fread("fire.csv")

setwd("C:/Users/skick/Desktop/CKM")

#======================================================
#
# read in all datasets
#
#======================================================

census = fread("census.csv")
nypd = fread("nypd.csv")
fire = fread("fire.csv")
property = fread("property.csv")
# data311 = fread("data311.csv")




#======================================================
#
#subset/condense each data set by zip and year
#
#======================================================

#---------------- census ---------------------
census$Zip   #all good




#---------------- fire ---------------------
names(fire)
fire[, .N, .(Year, Month)]      # looks evenly proportioned, drop NAs
fire = fire[!is.na(Year)]

#subset it
fire_subset = fire[, .(Zip, Boro, Year, Month)]

#GROUP BY ZIP & YEAR
fire_byzipyear = fire_subset[,.(NFires = .N), by = .(Zip,Year,Month)][order(Zip,Year,Month)]
fire_byzipyear = fire_byzipyear[,.(Avg.Fires = mean(NFires, na.rm=TRUE),
                                   NFires = sum(NFires, na.rm=TRUE)), 
                                by = .(Zip,Year)][order(Zip,Year)]




#---------------- nypd ---------------------
names(nypd)
nypd$Zip
nypd[,.N, by=Zip][order(-Zip)]
head(nypd)
nypd[,.N,by=.(Year,Month)][order(Year,Month)]    #seams fine

#subset it
nypd_subset = nypd[, .(Zip, Boro, Year, Month)]

#GROUP BY ZIP & YEAR
nypd_byzipyear = nypd_subset[,.(NCrimes = .N), by = .(Zip,Year,Month)][order(Zip,Year,Month)]
nypd_byzipyear = nypd_byzipyear[,.(Avg.Crimes = mean(NCrimes, na.rm=TRUE),
                  NCrimes = sum(NCrimes, na.rm=TRUE)), 
                  by = .(Zip,Year)][order(Zip,Year)]





#---------------- property ---------------------
names(property)
property$Zip
property[,.N, by = Zip][order(-Zip)]
head(property)
prop_count_by_month = property[,.N,by=.(Year, Month)][order(Year,Month)]
        # a couple odd months (december 2012) but nothing too crazy
        # look at barchart of this

property$Price = replace(property$Price, property$Price == 0, NA)
property[is.na(Price), .N, by=Year][order(Year)]   #more missing values early on, but not too crazy
property[is.na(Price), .N, by=Month][order(Month)]

#compare missing to total
nas = property[is.na(Price), .N, by=Zip][order(N)][,.(Zip, na_count=N)]
totes = property[, .N, by=Zip][order(N)]
merged_df = merge(x = nas, y = totes, by = "Zip", all = TRUE)
merged_df = merged_df[, .(Zip, na_count, N, Prop = na_count/N)]
        # between 10-20% for most zip codes, but some like Astoria, 
        # Ditmars, Borough Hall have 50% missing!
        # Keep so can still count, but just keep in mind it may cause problems
summary(property)

property$SqFt = replace(property$SqFt, property$SqFt == 0, NA)
property$LandSqFt = replace(property$LandSqFt, property$LandSqFt == 0, NA)

nrow(property[is.na(Price) & is.na(LandSqFt) & is.na(SqFt)])   #97K missing all 3
nrow(property[is.na(Price) | is.na(LandSqFt) | is.na(SqFt)])    #806K missing at least one

#subset it
property_subset = property[, .(Zip, Year, Month, Price, SqFt, LandSqFt, Price.Per.SqFt = Price/SqFt)]


#GROUP BY ZIP & YEAR
prop_byzipyear = property_subset[,.(Avg.Price = mean(Price, na.rm=TRUE),
                   Avg.PricePerSqFt = mean(Price.Per.SqFt, na.rm=TRUE),
                   NSales = .N), by = .(Zip,Year,Month)][order(Zip,Year,Month)]
prop_byzipyear = prop_byzipyear[,.(Avg.Price = mean(Avg.Price, na.rm=TRUE),
                  Avg.PricePerSqFt = mean(Avg.PricePerSqFt, na.rm=TRUE),
                  Avg.NSales = mean(NSales, na.rm = TRUE),
                  NSales = sum(NSales)), 
                  by = .(Zip,Year)][order(Zip,Year)]




#---------------- 311 ---------------------
data311 = fread("data311.csv")
names(data311)
data311[Incident.Zip == "11228-2316"]

data311$Incident.Zip = replace(data311$Incident.Zip, data311$Incident.Zip == "11228-2316", "11228")
data311$Incident.Zip = replace(data311$Incident.Zip, data311$Incident.Zip == "", NA)
data311[nchar(Incident.Zip) > 5, Incident.Zip]    #4074 need adjustment
data311$Incident.Zip = substr(data311$Incident.Zip, start = 1, stop = 5)
data311$Incident.Zip = replace(data311$Incident.Zip, nchar(data311$Incident.Zip) < 5, NA)

data311[,.N,by=Incident.Zip][order(Incident.Zip)]$Incident.Zip
nrow(data311[Incident.Zip < 10001])
nrow(data311[Incident.Zip > 11697])
data311 = data311[Incident.Zip >= 10001 & Incident.Zip <= 11697]

data311$Incident.Zip = as.integer(data311$Incident.Zip)

#subset it
data311_subset = data311[, .(Zip=Incident.Zip, Year, Month, Complaint.Type)]

#GROUP BY ZIP & YEAR
data311_byzipyear = data311_subset[,.(NComplaints = .N), 
                                   by = .(Zip,Year,Month)][order(Zip,Year,Month)]
data311_byzipyear = data311_byzipyear[,.(Avg.Complaints = mean(NComplaints, na.rm=TRUE),
                                   NComplaints = sum(NComplaints, na.rm=TRUE)), 
                                by = .(Zip,Year)][order(Zip,Year)]





#---------------- save groud tables to csvs ---------------------
fwrite(prop_byzipyear, "prop_byzipyear.csv")
fwrite(data311_byzipyear, "data311_byzipyear.csv")
fwrite(fire_byzipyear, "fire_byzipyear.csv")
fwrite(nypd_byzipyear, "nypd_byzipyear.csv")



#---------------- read in groud tables and inspect ---------------------
prop = fread("prop_byzipyear.csv")
fire =  fread("fire_byzipyear.csv")
nypd = fread("nypd_byzipyear.csv")
census = fread("census.csv")
data311 = fread("data311_byzipyear.csv")



lapply(c(prop, fire, nypd, census, data311), names)
head(census)
head(data311)
head(prop)
head(fire)
head(nypd)

nypd[, .N, by=Year]
fire[, .N, by=Year]
data311[, .N, by=Year]
prop[, .N, by=Year]
# census[, .N, by=Year]




#======================================================
#
# join each dataset into one large table
#
#======================================================

# convert all Years and Zips to int to join
nypd$Zip = as.integer(nypd$Zip)
nypd$Year = as.integer(nypd$Year)

data311$Zip = as.integer(data311$Zip)
data311$Year = as.integer(data311$Year)

prop$Zip = as.integer(prop$Zip)
prop$Year = as.integer(prop$Year)

fire$Zip = as.integer(fire$Zip)
fire = fire[!is.na(Zip)]
fire$Year = as.integer(fire$Year)

census$Zip = as.integer(census$Zip)


# merge one by one
merged_df = merge(census, nypd, by=c("Zip"), all = TRUE)
merged_df = merge(merged_df, fire, by = c("Zip", "Year"), all = TRUE)
merged_df = merge(merged_df, prop, by = c("Zip", "Year"), all = TRUE)
merged_df = merge(merged_df, data311, by = c("Zip", "Year"), all = TRUE)

#======================================================
#
# save to csv and check
#
#======================================================

#save to csv
fwrite(merged_df, "merged.csv")

#read in subsetted data
merged = fread("merged.csv")
summary(merged)
names(merged)

#======================================================
#
# group and recommend
#
#======================================================

merged = merged[, Population := NULL]
merged = merge(merged, census, by=c("Zip"), all=TRUE)
merged = merged[!is.na(Zip)]

merged_group = merged[, .(
                          Avg.Crimes.by.Pop = (mean(Avg.Crimes, na.rm = TRUE)*100)/Population,
                          Avg.Fires.by.Pop = (mean(Avg.Fires, na.rm = TRUE)*100)/Population,
                          Avg.Complaints.by.Pop = (mean(Avg.Complaints, na.rm=TRUE)*100)/Population,
                          Avg.NSales = mean(Avg.NSales, na.rm = TRUE),
                          Avg.Price = mean(Avg.Price, na.rm = TRUE),
                          Avg.PricePerSqFt = mean(Avg.PricePerSqFt, na.rm = TRUE)),
                      by=.(Zip,Population)]
names(merged_group)

#drop zip codes without population (NAs & 0)
merged_group = merged_group[!is.na(Population) & Population != 0]

#scale everything
merged_group$Population = (merged_group$Population - min(merged_group$Population))/(max(merged_group$Population) - min(merged_group$Population))
merged_group$Avg.Crimes.by.Pop = (merged_group$Avg.Crimes.by.Pop - min(merged_group$Avg.Crimes.by.Pop, na.rm=TRUE))/(max(merged_group$Avg.Crimes.by.Pop, na.rm=TRUE) - min(merged_group$Avg.Crimes.by.Pop, na.rm=TRUE))
merged_group$Avg.Fires.by.Pop = (merged_group$Avg.Fires.by.Pop - min(merged_group$Avg.Fires.by.Pop, na.rm=TRUE))/(max(merged_group$Avg.Fires.by.Pop, na.rm=TRUE) - min(merged_group$Avg.Fires.by.Pop, na.rm=TRUE))
merged_group$Avg.Complaints.by.Pop = (merged_group$Avg.Complaints.by.Pop - min(merged_group$Avg.Complaints.by.Pop, na.rm=TRUE))/(max(merged_group$Avg.Complaints.by.Pop, na.rm=TRUE) - min(merged_group$Avg.Complaints.by.Pop, na.rm=TRUE))
merged_group$Avg.Price = (merged_group$Avg.Price - min(merged_group$Avg.Price, na.rm=TRUE))/(max(merged_group$Avg.Price, na.rm=TRUE) - min(merged_group$Avg.Price, na.rm=TRUE))
merged_group$Avg.PricePerSqFt = (merged_group$Avg.PricePerSqFt - min(merged_group$Avg.PricePerSqFt, na.rm=TRUE))/(max(merged_group$Avg.PricePerSqFt, na.rm=TRUE) - min(merged_group$Avg.PricePerSqFt, na.rm=TRUE))

#all in same direction
merged_group$Avg.Crimes.by.Pop = 1 - merged_group$Avg.Crimes.by.Pop
merged_group$Avg.Fires.by.Pop = 1 - merged_group$Avg.Fires.by.Pop
merged_group$Avg.Complaints.by.Pop = 1 - merged_group$Avg.Complaints.by.Pop
merged_group$Avg.Price = 1 - merged_group$Avg.Price
merged_group$Avg.PricePerSqFt = 1 - merged_group$Avg.PricePerSqFt

fwrite(merged_group, "finaldata.csv")

#score it
merged_group[is.na(merged_group)] <- 0

scores = merged_group[ , .(Zip, Score = 25*Population + 20*Avg.Crimes.by.Pop + 10*Avg.Fires.by.Pop + 
                    15*Avg.Complaints.by.Pop + 20*Avg.Price + 10*Avg.PricePerSqFt)]
final = merge(merged_group, scores, by = c("Zip"), all = TRUE)

scorebyzip = final[, .(Zip, Score)][order(-Score)]

fwrite(scorebyzip, "finalscores.csv")
x = fread("finalscores.csv")

nyczips = c(10453, 10457, 10460, 10458, 10467, 10468, 10451, 10452, 10456, 10454, 10455, 10459, 10474, 10463, 10471, 10466, 10469, 10470, 10475, 10461, 10462,10464, 10465, 10472, 10473, 11212, 11213, 11216, 11233, 11238, 11209, 11214, 11228, 11204, 11218, 11219, 11230, 11234, 11236, 11239, 11223, 11224, 11229, 11235, 11201, 11205, 11215, 11217, 11231, 11203, 11210, 11225, 11226, 11207, 11208, 11211, 11222, 11220, 11232, 11206, 11221, 11237, 10026, 10027, 10030, 10037, 10039, 10001, 10011, 10018, 10019, 10020, 10036, 10029, 10035, 10010, 10016, 10017, 10022, 10012, 10013, 10014, 10004, 10005, 10006, 10007, 10038, 10280, 10002, 10003, 10009, 10021, 10028, 10044, 10065, 10075, 10128, 10023, 10024, 10025, 10031, 10032, 10033, 10034, 10040, 11361, 11362, 11363, 11364, 11354, 11355, 11356, 11357, 11358, 11359, 11360, 11365, 11366, 11367, 11412, 11423, 11432, 11433, 11434, 11435, 11436, 11101, 11102, 11103, 11104, 11105, 11106, 11374, 11375, 11379, 11385, 11691, 11692, 11693, 11694, 11695, 11697, 11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429, 11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421, 11368, 11369, 11370, 11372, 11373, 11377, 11378, 10302, 10303, 10310, 10306, 10307, 10308, 10309, 10312, 10301, 10304, 10305, 10314)
class(nyczips)

x = x[Zip %in% nyczips]
best_zips = x[order(-Score)][1:5]
worst_zips = x[order(Score)][1:5]

