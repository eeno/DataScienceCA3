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

#replace missing values with NA's
planning_permission[planning_permission == ""] <- NA

#check unique values in planning permission
unique(planning_permission_granted$Decision)

authority_freq <- table(planning_permission$PlanningAuthority)

authority_freq <- as.data.frame(authority_freq)



# create a datset based on deciosn and appeal decision. Sometimes permision was granted but then refused
#due to an appeal
planning_permission_granted <- planning_permission[grepl("GRANT PERMISSION",planning_permission$Decision) 
                                                   & !grepl("Refuse Permission",planning_permission$AppealDecision) ,]
str(planning_permission_granted)
#


nrow(planning_permission_granted)
  
date_field <- as.character(planning_permission_granted$DecisionDate)

new_date <- as.Date(date_field, "%Y-%d-%m")
planning_permission_granted$DecisionDate <- new_date

planning_permission_granted$year <- format(planning_permission_granted$DecisionDate, "%Y")

planning_year_freq <- table(planning_permission_granted$year)
#planning_year_freq <- as.data.frame(planning_year_freq)

planning_year_freq



barplot(  planning_year_freq ,  
        main = "Frequency of Granted planning permission by year", 
       ylab = "Frequency", 
       xlab = "Year",
       col = "blue")

planning_permission_granted <- planning_permission_granted[which(planning_permission_granted$year > 1999 
                                                          & planning_permission_granted$year < 2020) , ]


test <- planning_permission_granted[is.na(planning_permission_granted$DecisionDate),]

sum(!is.na(planning_permission_granted$DecisionDate))
  
unique(planning_permission_granted$AppealDecisionDate)

planning_permission_granted[planning_permission_granted$AppealDecisionDate == "",] <- NA




str(cpi)
str(poi)
str(planning_permission)
nrow(planning_permission)
