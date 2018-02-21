setwd("C:/Users/skick/Desktop/CKM")
library(data.table)
library(dplyr)
library(ggplot2)

#======================================================
#
# read in all datasets
#
#======================================================

census = fread("data/census.csv")
nypd = fread("data/nypd.csv")
fire = fread("data/fire.csv")
property = fread("data/property.csv")
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
prop = fread("data/prop_byzipyear.csv")
fire =  fread("data/fire_byzipyear.csv")
nypd = fread("data/nypd_byzipyear.csv")
census = fread("data/census.csv")
data311 = fread("data/data311_byzipyear.csv")



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

#only use NYC zips
nyczips = c(10453, 10457, 10460, 10458, 10467, 
            10468, 10451, 10452, 10456, 10454, 
            10455, 10459, 10474, 10463, 10471, 
            10466, 10469, 10470, 10475, 10461, 
            10462, 10464, 10465, 10472, 10473, 
            11212, 11213, 11216, 11233, 11238, 
            11209, 11214, 11228, 11204, 11218, 
            11219, 11230, 11234, 11236, 11239, 
            11223, 11224, 11229, 11235, 11201, 
            11205, 11215, 11217, 11231, 11203, 
            11210, 11225, 11226, 11207, 11208, 
            11211, 11222, 11220, 11232, 11206, 
            11221, 11237, 10026, 10027, 10030, 
            10037, 10039, 10001, 10011, 10018, 
            10019, 10020, 10036, 10029, 10035, 
            10010, 10016, 10017, 10022, 10012, 
            10013, 10014, 10004, 10005, 10006, 
            10007, 10038, 10280, 10002, 10003, 
            10009, 10021, 10028, 10044, 10065, 
            10075, 10128, 10023, 10024, 10025, 
            10031, 10032, 10033, 10034, 10040, 
            11361, 11362, 11363, 11364, 11354, 
            11355, 11356, 11357, 11358, 11359, 
            11360, 11365, 11366, 11367, 11412, 
            11423, 11432, 11433, 11434, 11435, 
            11436, 11101, 11102, 11103, 11104, 
            11105, 11106, 11374, 11375, 11379, 
            11385, 11691, 11692, 11693, 11694, 
            11695, 11697, 11004, 11005, 11411, 
            11413, 11422, 11426, 11427, 11428, 
            11429, 11414, 11415, 11416, 11417, 
            11418, 11419, 11420, 11421, 11368, 
            11369, 11370, 11372, 11373, 11377, 
            11378, 10302, 10303, 10310, 10306, 
            10307, 10308, 10309, 10312, 10301, 
            10304, 10305, 10314, 11249)

merged_df = merged_df[Zip %in% nyczips]

#======================================================
#
# save to csv and check
#
#======================================================

#save to csv
fwrite(merged_df, "merged2.csv")

#read in subsetted data
merged = fread("merged2.csv")
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
merged = merged[Zip %in% nyczips]


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

#distributions are very skewed, so need to normalize
merged_group$Population = log(merged_group$Population)
merged_group$Avg.Crimes.by.Pop = log(merged_group$Avg.Crimes.by.Pop)
merged_group$Avg.Fires.by.Pop = log(merged_group$Avg.Fires.by.Pop)
merged_group$Avg.Complaints.by.Pop = log(merged_group$Avg.Complaints.by.Pop)
merged_group$Avg.Price = log(merged_group$Avg.Price)
merged_group$Avg.PricePerSqFt = log(merged_group$Avg.PricePerSqFt)

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

#examine distributions
ggplot(merged_group, aes(x = Avg.Crimes.by.Pop)) + geom_histogram()
ggplot(merged_group, aes(x = Avg.Fires.by.Pop)) + geom_histogram()
ggplot(merged_group, aes(x = Avg.Complaints.by.Pop)) + geom_histogram()
ggplot(merged_group, aes(x = Avg.Price)) + geom_histogram()
ggplot(merged_group, aes(x = Avg.PricePerSqFt)) + geom_histogram() # + xlim(c(0,2000))


fwrite(merged_group, "finaldata2.csv")

#score it
merged_group[is.na(merged_group)] <- 0

scores = merged_group[ , .(Zip, Score = 25*Population + 20*Avg.Crimes.by.Pop + 10*Avg.Fires.by.Pop + 
                    15*Avg.Complaints.by.Pop + 20*Avg.Price + 10*Avg.PricePerSqFt)]
final = merge(merged_group, scores, by = c("Zip"), all = TRUE)

scorebyzip = final[, .(Zip, Score)][order(-Score)]

fwrite(scorebyzip, "finalscores2.csv")
x = fread("finalscores2.csv")

nyczips = c(10453, 10457, 10460, 10458, 10467, 10468, 10451, 10452, 10456, 10454, 10455, 10459, 10474, 10463, 10471, 10466, 10469, 10470, 10475, 10461, 10462,10464, 10465, 10472, 10473, 11212, 11213, 11216, 11233, 11238, 11209, 11214, 11228, 11204, 11218, 11219, 11230, 11234, 11236, 11239, 11223, 11224, 11229, 11235, 11201, 11205, 11215, 11217, 11231, 11203, 11210, 11225, 11226, 11207, 11208, 11211, 11222, 11220, 11232, 11206, 11221, 11237, 10026, 10027, 10030, 10037, 10039, 10001, 10011, 10018, 10019, 10020, 10036, 10029, 10035, 10010, 10016, 10017, 10022, 10012, 10013, 10014, 10004, 10005, 10006, 10007, 10038, 10280, 10002, 10003, 10009, 10021, 10028, 10044, 10065, 10075, 10128, 10023, 10024, 10025, 10031, 10032, 10033, 10034, 10040, 11361, 11362, 11363, 11364, 11354, 11355, 11356, 11357, 11358, 11359, 11360, 11365, 11366, 11367, 11412, 11423, 11432, 11433, 11434, 11435, 11436, 11101, 11102, 11103, 11104, 11105, 11106, 11374, 11375, 11379, 11385, 11691, 11692, 11693, 11694, 11695, 11697, 11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429, 11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421, 11368, 11369, 11370, 11372, 11373, 11377, 11378, 10302, 10303, 10310, 10306, 10307, 10308, 10309, 10312, 10301, 10304, 10305, 10314)
class(nyczips)

x = x[Zip %in% nyczips]
best_zips = x[order(-Score)][1:5]
worst_zips = x[order(Score)][1:5]
best_zips
worst_zips










