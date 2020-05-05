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
#rownames() returns all the values f index 0
cpi2 <- cbind(Row.Names = rownames(cpi2), cpi2)
#rname theis new column
names(cpi2)[1] <- "Year and Month number"

#remove the X from the start of the rownames
cpi2$`Year and Month number` <- substring(cpi2$`Year and Month number`, 2)

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


#planning permission data

planning_permission <- read.csv("Planning_Application_Sites_2010_onwards.csv")

#check structure of planning permision
str(planning_permission)

# Cleaning Data------------------------------------------------------------------------------------------

#replace missing values with NA's
planning_permission[planning_permission == ""] <- NA

#check unique values in planning permission
unique(planning_permission_granted$Decision)


#check the plannig authorities
authority_freq <- table(planning_permission$PlanningAuthority)

#create a frequncy table
authority_freq <- as.data.frame(authority_freq)



# create a datset based on deciosn and appeal decision. Sometimes permision was granted but then refused
#due to an appeal
planning_permission_granted <- planning_permission[grepl("GRANT PERMISSION",planning_permission$Decision) 
                                                   & !grepl("Refuse Permission",planning_permission$AppealDecision) ,]

#count rows in dataframe
nrow(planning_permission_granted)

#convert the decision date to a character to convert it to the proper time format
date_field <- as.character(planning_permission_granted$DecisionDate)

#formatng date
new_date <- as.Date(date_field, "%Y-%d-%m")
#converting column in dataframe to date
planning_permission_granted$DecisionDate <- new_date

#create a year column based on decisiondate
planning_permission_granted$year <- format(planning_permission_granted$DecisionDate, "%Y")

#create a table of year values to plot
planning_year_freq <- table(planning_permission_granted$year)

#plot frequncy of years
barplot(  planning_year_freq ,  
        main = "Frequency of Granted planning permission by year", 
       ylab = "Frequency", 
       xlab = "Year",
       col = "blue")

#Pre 1999 has very litle data as does 2020. include data form 2000 - 2019
planning_permission_granted <- planning_permission_granted[which(planning_permission_granted$year > 1999 
                                                          & planning_permission_granted$year < 2020) , ]


#creat a table fo datae with deciosn date between 2000 and 2019
planning_year_freq <- table(planning_permission_granted$year)

#plot the table fof data from 2000 - 2019
barplot(  planning_year_freq ,  
          main = "Frequency of Granted planning permission by year", 
          ylab = "Frequency", 
          xlab = "Year",
          col = "blue")



#check for na's in decision
sum(is.na(planning_permission_granted$DecisionDate))
  

#create a cloumn based on year and month from DecionDate colukmn to match with data in POI2 CPI2 
planning_permission_granted$month_year <- format(planning_permission_granted$DecisionDate, "%Y-%m")

#Replace - with M to match the "month and year" column in POI2 and CPI2
planning_permission_granted$month_year <- gsub("-","M",planning_permission_granted$month_year)
planning_permission_granted$month_year


#use macth function to look up the cpi data using month and year columns
cpi_check <-cpi2$`Consumer Price Index (Base Dec 2016=100)`[match(planning_permission_granted$month_year, cpi2$`Year and Month number`)]

#add new column to planning permission data set
planning_permission_granted$CPI_2016 <- cpi_check


#using cpi data woth 2016 base predict whether it is above or below the 2016 base


cpi2$`Year and Month number`

str(cpi)
str(poi)
str(planning_permission)
nrow(planning_permission)
