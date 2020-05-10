#loading data --------------------------------------------------------------------------------------------
#data use
#consumer price index

cpi <- read.csv("CPI_data_all.csv", header = TRUE)
#transpose data so that columns are rows & vice versa
cpi2 <- as.data.frame(t(cpi))

#use first row as column names
#unlist the col headers that R put in automatically
colnames(cpi2) <- as.character(unlist(cpi2[1,]))
#use first row as headers
cpi2 <- cpi2[-1,]


                                                   


#when transposing R took the first column and pplaced it at index 0
#need to add this to the datafre, using cbind
#Row.Names is the new name for the column that will be joined to the datframe
#rownames() returns all the values at index 0
cpi2 <- cbind(Row.Names = rownames(cpi2), cpi2)
#rname theis new column
names(cpi2)[1] <- "year_month"

#remove the X from the start of the rownames
cpi2$year_month <- substring(cpi2$year_month, 2)

#remove data pre 2000 
cpi2 <- cpi2[grepl("20" ,cpi2$year_month),]

#remove 2020 data
cpi2 <- cpi2[!grepl("2020" ,cpi2$year_month),]


#production output index
poi <- read.csv("POI_construction_all.csv")

#transpose data so that columns are rows & vice versa
poi2 <- as.data.frame(t(poi))

#use first row as column names
#unlist the col headers that R put in automatically
colnames(poi2) <- as.character(unlist(poi2[1,]))
#use first row as headers
poi2 <- poi2[-1,]

#when transposing R took the first column and pplaced it at index 0
#need to add this to the datafre, using cbind
#Row.Names is the new name for the column that will be joined to the datframe
#rownames() returns all the values f index 0
poi2 <- cbind(Row.Names = rownames(poi2), poi2)
#rname theis new column
names(poi2)[1] <- "Year"


#remove the X from the start of the rownames
poi2$Year <- substring(poi2$Year, 2)

poi2$Year <- NULL



cpi_year <- read.csv("CPI by Year2.csv")


#planning permission data
rm(planning_permission)
planning_permission <- read.csv("Planning_Application_Sites_2010_onwards.csv")

#check structure of planning permision
str(planning_permission)

# Cleaning Data------------------------------------------------------------------------------------------

na_count <-sapply(planning_permission, function(y) sum(length(which(is.na(y)))))
#convert to dataframe for easy interpratation
na_count <- data.frame(na_count)
#create a new column with percentages of na 




#some blank column values contain no values but have whitespace characters
#use regex to replce the whitespace with "" using a custom function
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
planning_permission[] <- lapply(planning_permission, trim)


#replace missing values with NA's
planning_permission[planning_permission == ""] <- NA




planning_permission 

#check missing value
library(mice)
md.pattern(planning_permission)

library(VIM)
missing_values <- aggr(planning_permission_clean, prop = FALSE, numbers = TRUE)
summary(missing_values)


na_count <-sapply(planning_permission, function(y) sum(length(which(is.na(y)))))
#convert to dataframe for easy interpratation
na_count <- data.frame(na_count)
#create a new column with percentages of na 
na_count$proportion <- round((na_count$na_count / nrow(planning_permission) *100),digits = 2)

na_count <- cbind(Row.Names = rownames(na_count), na_count)
names(na_count)[1] <- "Column_names"
na_count

na_count <- na_count[order(-na_count$proportion),]


granted_plan <- planning_permission[grepl("GRANT PERMISSION",planning_permission$Decision)
& planning_permission[is.na(planning_permission$AppealDecision),],]


granted_plan <- planning_permission[grepl("GRANT PERMISSION",planning_permission$Decision),]

granted_plan <- granted_plan[is.na(granted_plan$AppealDecision),]

granted_plan <- planning_permission[grepl("GRANT PERMISSION",planning_permission$Decision),]

granted_plan_na <- granted_plan[,is.na(granted_plan$DecisionDate)]

granted_plan_test <- granted_plan[,c("ApplicationNumber","Decision","AppealDecision")]


planning_permission$granted <- ifelse(grepl("GRANT PERMISSION",planning_permission$Decision) 
                                      & is.na(planning_permission$AppealDecision),1,0)





date_field <- as.character(planning_permission$DecisionDate)

#formatng date
new_date <- as.Date(date_field, "%Y-%d-%m")
#converting column in dataframe to date
planning_permission$DecisionDate <- new_date

planning_permission$Year <- format(planning_permission$DecisionDate, "%Y")


#create a table of year values to plot
planning_year_freq <- table(planning_permission$Year)

#plot frequncy of years
plot(  planning_year_freq ,  
       main = "Frequency of Granted planning permission by year", 
       ylab = "Frequency", 
       xlab = "Year",
       col = "blue")




summary(planning_permission$DecisionDate)

granted_plan_test <- granted_plan_test[order(-granted_plan_test$ApplicationNumber),]

granted_plan <- granted_plan[!grepl("REFUSE", granted_plan$Decision),]




unique(granted_plan$Decision)

nrow(grante)

unique(granted_plan_test$AppealDecision)

unique(planning_permission$Decision)
unique(planning_permission$AppealDecision)




test <- data.frame(table(planning_permission$ApplicationNumber))

test <- test[order(-test$Freq),]

test



na_count <- na_count[order(-na_count$proportion),]
na_count

na_count <- cbind(Row.Names = rownames(na_count), na_count)
names(na_count)[1] <- "Column_names"

library(na_count)

naplot <- ggplot(head(na_count,5), aes(Column_names,proportion))

naplot +geom_bar(stat = "identity")

plot(  na_count$Column_names,na_count$proportion,
          type = "h",
          main = "Percentage of NA's per column", 
          ylab = "Percent of NA's", 
          xlab = "Columns",
          col = "blue")




#check unique values in planning permission
unique(planning_permission_granted$Decision)

#over half the postcodes are missing
sum(is.na(planning_permission$DevelopmentPostcode))





#drop any column that has all na vlaues
planning_permission_clean <- planning_permission[,which(unlist(lapply(planning_permission, function(x) !all(is.na(x)))))]

planning_permission_clean < - planning_permission[ which(planning_permission$Year2 >1999 | planning_permission$Year2 < 2020) ,]

cols_to_drop <- c("ApplicationNumber","ETL_DATE", "SiteId", "DevelopmentAddress","DevelopmentDescription","DevelopmentPostcode",
                  "AppealRefNumber","ApplicationNumber", "LandUseCode")

planning_permission_clean <- planning_permission_clean[, !names(planning_permission_clean) %in% cols_to_drop]



#check the plannig authorities
authority_count <- table(planning_permission$PlanningAuthority)

#create a frequncy table
authority_count <- as.data.frame(authority_freq)

authority_freq

# create a datset based on deciosn and appeal decision. Sometimes permision was granted but then refused
#due to an appeal
planning_permission$granted <- ifelse(grepl("GRANT PERMISSION",planning_permission$Decision) 
                                      & !grepl("Refuse Permission",planning_permission$AppealDecision),1,0)









sum(planning_permission$granted)

#count rows in dataframe
nrow(planning_permission_granted)

#convert the decision date to a character to convert it to the proper time format
date_field <- as.character(planning_permission$DecisionDate)

#formatng date
new_date <- as.Date(date_field, "%Y-%d-%m")
#converting column in dataframe to date
planning_permission$DecisionDate <- new_date

#create a year column based on decisiondate
planning_permission$Year <- format(planning_permission$DecisionDate, "%Y")


date_field2 <- as.character(planning_permission$ReceivedDate)
#formatng date
new_date2 <- as.Date(date_field, "%Y-%d-%m")
#converting column in dataframe to date
planning_permission$ReceivedDate <- new_date2

#create a year column based on decisiondate
planning_permission$Year2 <- format(planning_permission$ReceivedDate, "%Y")



granted_perm <- planning_permission[which(planning_permission$granted == 1),]

granted_perm <- subset(planning_permission, granted == 1 )

#create a table of year values to plot
planning_year_freq <- table(planning_permission$Year2)

#plot frequncy of years
plot(  planning_year_freq ,  
        main = "Frequency of planning  permissions application per year by year", 
       ylab = "Frequency", 
       xlab = "Year",
       col = "blue")

#Pre 1999 has very litle data as does 2020. include data form 2000 - 2019
planning_permission_granted <- planning_permission_granted[which(planning_permission_granted$Year > 1999 
                                                          & planning_permission_granted$Year < 2020) , ]


library(mice)
md.pattern(planning_permission)



#check for na's in decision
sum(is.na(planning_permission_granted$DecisionDate))
  

#create a cloumn based on year and month from DecionDate colukmn to match with data in POI2 CPI2 
planning_permission$year_month <- format(planning_permission$DecisionDate, "%Y-%m")

#Replace - with M to match the "month and year" column in POI2 and CPI2
planning_permission$year_month <- gsub("-","M",planning_permission$year_month)
planning_permission$year_month


#use macth function to look up the cpi data using month and year columns
#cpi_check <-cpi2$`Consumer Price Index (Base Dec 2016=100)`[match(planning_permission_granted$month_year, cpi2$`Year and Month number`)]

#add new column to planning permission data set
#planning_permission_granted$CPI_2016 <- cpi_check



#cpi2$`Year and Month number`
 #merge cpi data
planning_permission_granted <- merge(planning_permission_granted,cpi2, by = "year_month", all =  TRUE)  

#merge poi data
planning_permission_granted <- merge(planning_permission_granted,poi2, by = "Year", all =  TRUE) 




#creat a table fo datae with deciosn date between 2000 and 2019
planning_year_freq <- table(planning_permission_granted$Year)

#plot the table fof data from 2000 - 2019
barplot(  planning_year_freq ,  
          main = "Frequency of Granted planning permission by year", 
          ylab = "Frequency", 
          xlab = "Year",
          col = "blue")

#check missing value
library(mice)
md.pattern(planning_permission_granted)

library(VIM)
missing_values <- aggr(planning_permission, prop = FALSE, numbers = TRUE)
summary(missing_values)

plan_auth_prop <- prop.table(table(planning_permission_granted$PlanningAuthority))



barplot(height = plan_auth_prop,
        main = "Planning application by authority", 
        ylab = "Frequency", 
        xlab = "Authority",
        col = "white")

#create a varaible where plannig permission was granted 
#create varaible where CPI is above 100 and below 100
#check proption of PP granted vs non granted
#delete columns
planning_permission<- planning_permission[-c(41:88)]

planning_permission_clean <-   planning_permission[which(planning_permission$Year > 1999 
                                                         & planning_permission$Year < 2020) , ]

cols_to_drop <- c("ï..OBJECTID","ITMEasting","ITMNorthing","ETL_DATE","LinkAppDetails","OneOffKPI","DevelopmentDescription",
                  "DevelopmentAddress","DevelopmentPostcode", "ApplicationStatus", "WithdrawnDate","Yearw","ApplicationType","LandUseCode",
                  "NumResidentialUnits","OneOffHouse","WithdrawnDate","DecisionDueDate","GrantDate","ExpiryDate","AppealRefNumber",
                  "AppealStatus","AppealDecision", "AppealSubmittedDate", "FIRequestDate","FIRecDate",
                  "Consumer Price Index (Base Dec 2016=100", "Consumer Price Index (Base Dec 2006=100)",
                  "Consumer Price Index (Base Dec 2001=100)","Consumer Price Index (Base Nov 1996=100)",
                  "Percentage Change over 1 month for Consumer Price Index (%)","Percentage Change over 12 months for Consumer Price Index (%)")


planning_permission_clean <- planning_permission_clean[,!names(planning_permission) %in% cols_to_drop]

planning_permission_clean <- planning_permission_clean[-c(17:37)]



planning_permission_clean$year_month <- format(planning_permission_clean$DecisionDate, "%Y-%m")









planning_permission$year_month <- format(planning_permission$DecisionDate, "%Y-%m")

#Replace - with M to match the "month and year" column in POI2 and CPI2
planning_permission$year_month <- gsub("-","M",planning_permission$year_month)
planning_permission$year_month


planning_permission <- merge (planning_permission,cpi2, by = "year_month", all =  TRUE)  

planning_permission <- merge (planning_permission,poi2, by = "Year", all =  TRUE)  

planning_permission <- merge (planning_permission,cpi_year, by = "Year", all =  TRUE) 

rm(planning_permission_clean)


str(cpi)
str(poi)
str(planning_permission)
nrow(planning_permission)
