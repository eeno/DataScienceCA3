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

write.csv(cpi2,"CPI2.csv")
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



#check missing value
library(mice)
md.pattern(planning_permission)

library(VIM)
missing_values <- aggr(planning_permission, prop = FALSE, numbers = TRUE)
summary(missing_values)

#count na' in the data
na_count <-sapply(planning_permission, function(y) sum(length(which(is.na(y)))))
#convert to dataframe for easy interpratation
na_count <- data.frame(na_count)
#create a new column with percentages of na 
na_count$proportion <- round((na_count$na_count / nrow(planning_permission) *100),digits = 2)

#create a column of row names
na_count <- cbind(Row.Names = rownames(na_count), na_count)
names(na_count)[1] <- "Column_names"
na_count
#order by proprotion count
na_count <- na_count[order(-na_count$proportion),]




#creating a new colum called granted  based on if decion column contains "GRANT PERMISSION"
# and the appeal decision colmn contains NA
planning_permission$granted <- ifelse(grepl("GRANT PERMISSION",planning_permission$Decision) 
                                      & is.na(planning_permission$AppealDecision),1,0)




#converting the decions date field
date_field <- as.character(planning_permission$DecisionDate)

#formatng date
new_date <- as.Date(date_field, "%Y-%d-%m")
#converting column in dataframe to date
planning_permission$DecisionDate <- new_date

#creating a year colum fomr the decions date
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



#checking unique values of AppealDecision, Decision,AppealDecision


unique(planning_permission$Decision)
unique(planning_permission$AppealDecision)


#drop any column that has all na vlaues
planning_permission_clean <- planning_permission[,which(unlist(lapply(planning_permission, function(x) !all(is.na(x)))))]

planning_permission_clean < - planning_permission[ which(planning_permission$Year2 >1999 | planning_permission$Year2 < 2020) ,]

cols_to_drop <- c("ApplicationNumber","ETL_DATE", "SiteId", "DevelopmentAddress","DevelopmentDescription","DevelopmentPostcode",
                  "AppealRefNumber","ApplicationNumber", "LandUseCode")

planning_permission_clean <- planning_permission_clean[, !names(planning_permission_clean) %in% cols_to_drop]



#check the plannig authorities
authority_count <- table(planning_permission$PlanningAuthority)

#create a frequncy table
authority_count <- as.data.frame(authority_count)


# create a datset based on deciosn and appeal decision. Sometimes permision was granted but then refused
#due to an appeal
planning_permission$granted <- ifelse(grepl("GRANT PERMISSION",planning_permission$Decision) 
                                      & !grepl("Refuse Permission",planning_permission$AppealDecision),1,0)

str(planning_permission$granted)

unique(planning_permission$granted)
#count rows in dataframe
nrow(planning_permission_clean)

#convert the decision date to a character to convert it to the proper time format
date_field <- as.character(planning_permission$DecisionDate)

#formatng date
new_date <- as.Date(date_field, "%Y-%m-%d")
#converting column in dataframe to date
planning_permission$DecisionDate <- new_date

#create a year column based on decisiondate
planning_permission$Year <- format(planning_permission$DecisionDate, "%Y")


date_field2 <- as.character(planning_permission$ReceivedDate)
#formatng date
new_date2 <- as.Date(date_field2, "%Y-%m-%d")
#converting column in dataframe to date
planning_permission$ReceivedDate <- new_date2

#create a year column based on decisiondate
planning_permission$Year2 <- format(planning_permission$ReceivedDate, "%Y")


#create a table of year values to plot
planning_year_freq <- table(planning_permission$Year2)

#plot frequncy of years
plot(  planning_year_freq ,  
        main = "Frequency of planning  permissions application per year by year", 
       ylab = "Frequency", 
       xlab = "Year",
       col = "blue")

#Pre 1999 has very litle data as does 2020. include data form 2000 - 2019
planning_permission <- planning_permission[which(planning_permission$Year > 1999 
                                                          & planning_permission$Year < 2020) , ]




  

#create a cloumn based on year and month from DecionDate colukmn to match with data in POI2 CPI2 
planning_permission$year_month <- format(planning_permission$DecisionDate, "%Y-%m")

#Replace - with M to match the "month and year" column in POI2 and CPI2
planning_permission$year_month <- gsub("-","M",planning_permission$year_month)
planning_permission$year_month




#cpi2$`Year and Month number`
 #merge cpi data
planning_permission <- merge(planning_permission,cpi2, by = "year_month", all =  TRUE)  

#merge poi data
planning_permission <- merge(planning_permission,poi2, by = "Year", all =  TRUE) 




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




#create a clean planning permission datframe
planning_permission_clean <-   planning_permission[which(planning_permission$Year > 1999 
                                                         & planning_permission$Year < 2020) , ]

#list of columns ot drop
cols_to_drop <- c("ï..OBJECTID","ITMEasting","ITMNorthing","ETL_DATE","LinkAppDetails","OneOffKPI","DevelopmentDescription",
                  "DevelopmentAddress","DevelopmentPostcode", "ApplicationStatus", "WithdrawnDate","Yearw","ApplicationType","LandUseCode",
                  "NumResidentialUnits","OneOffHouse","WithdrawnDate","DecisionDueDate","GrantDate","ExpiryDate","AppealRefNumber",
                  "AppealStatus","AppealDecision", "AppealSubmittedDate", "FIRequestDate","FIRecDate",
                  "Consumer Price Index (Base Dec 2016=100", "Consumer Price Index (Base Dec 2006=100)",
                  "Consumer Price Index (Base Dec 2001=100)","Consumer Price Index (Base Nov 1996=100)",
                  "Percentage Change over 1 month for Consumer Price Index (%)","Percentage Change over 12 months for Consumer Price Index (%)")

#dro columns in list
planning_permission_clean <- planning_permission_clean[,!names(planning_permission) %in% cols_to_drop]
#drop extra cpi data and poi data
planning_permission_clean <- planning_permission_clean[-c(15,17:34,36)]

#rename the remaining CPI and POI data
new_name <- c("CPI_monthly", "Annual_POI")

names(planning_permission_clean)[15:16] <- new_name

#created a second granted column whicjh explicitly indicates if a plannig permision was granted
planning_permission_clean$granted2 <- ifelse(planning_permission_clean$granted == 1,"Granted","Not Granted")
#conver to ordered faactor
planning_permission_clean$granted2 <-  factor(planning_permission_clean$granted2, ordered = TRUE, labels = c("Granted", "Not Granted"))

#conver to numeric
planning_permission_clean$CPI_monthly <- as.numeric(planning_permission_clean$CPI_monthly)

#created an indicator colum to explitly label if a CPI value is above the benchmark of 100
planning_permission_clean$CPI_greater_base <- ifelse(planning_permission_clean$CPI_monthly >= 100 ,"Yes","No")
#convert to ordered faactor
planning_permission_clean$CPI_greater_base <- factor(planning_permission_clean$CPI_greater_base, ordered = TRUE,
                                                     c("Yes","No"))


#check structure
str(planning_permission_clean$CPI_greater_base)
#check structure
str(planning_permission_clean$granted2)




write.csv(planning_permission_clean,"Planning_permission_clean.csv", row.names = FALSE)



#pca analysis--------------------------------------------------------------------------------

#choose numeric data
pca_test <- planning_permission_clean[,c(1,12:18)]
#creat a datframe
pca_test <- as.data.frame(pca_test)
#convert to numeric typpe
pca_test <- sapply(pca_test,as.numeric)

#performing pca analysis
pca <- prcomp(pca_test,center = TRUE, scale. = TRUE)
pca
str(pca)
summary(pca)

pca$rotation[1:nrow(pca$rotation), 1:3]



library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values


fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))


pca_for_variables <- get_pca_var(pca)
pca_for_variables

install.packages("corrplot")
library("corrplot")
library("FactoMineR")
library("factoextra")

opar <- par(no.readonly = TRUE)
par(opar)

corrplot(pca_for_variables$cos2, is.corr = FALSE)

fviz_pca_var(pca, col.var = "black")

head(pca_for_variables$cos2, 10)

fviz_cos2(pca, choice = "var", axes = 1:2)

# Colour by cos2 values: quality on the factor map
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE) # Avoid text overlapping



head(pca_for_variables$contrib, 20)



fviz_pca_var(pca, 
             axes = c(1, 2),
             col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)

# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 8)

# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)


# Contribution to PC1 - PC5
fviz_contrib(pca, choice = "var", axes = 1:5, top = 20)



fviz_pca_ind(pca,
             axes = c(3,5), col.var = "contrib",
             geom.ind = "point", # show points only (but not "text values")
             col.ind = planning_permission_clean$granted2, # colour by groups
             palette = c("Red", "Green"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "granted"
)

fviz_pca_biplot(pca, 
                col.ind = planning_permission_clean$granted2, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Vote")



biplot <- fviz_pca_biplot(pca, 
                          col.ind = planning_permission_clean$granted2,
                          addEllipses = TRUE, label = "var",
                          col.var = "black", repel = TRUE
) 

ggpubr::ggpar(biplot,
              title = "Principal Component Analysis",
              subtitle = "Planning permission dataset",
              caption = "Source: Housing.gov",
              xlab = "PC 1", ylab = "PC 2",
              legend.title = "Granted", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")



# statistical test-----------------------------------------------------------------------------------------
histogram(~CPI_monthly | granted2, data = planning_permission_clean)

with(planning_permission_clean,
     qqplot(CPI_monthly[granted2 == "Granted"],
            CPI_monthly[granted2 == "Not Granted"], 
            main = "Comparing 2 samples", 
            xlab = "CPI = Granted",
            ylab =  "CPI = Not Granted"))


par(mfrow=c(1,0))

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution
with(planning_permission_clean, {
  qqnorm(CPI_monthly[granted2 == "Not Granted"], 
         main = "Not granted")
  qqline(CPI_monthly[granted2 == "Not Granted"])
})

with(planning_permission_clean, {
  qqnorm(CPI_monthly[granted2 == "Granted"], 
         main = "Granted")
  qqline(CPI_monthly[granted2 == "Granted"])
})
 


set.seed(100)
#sample 5000 random rows from the dataset with no NA's in the location 
#call ne dataframe random_crime_sample
random_planning_permission <- planning_permission_clean[sample(1:nrow(planning_permission_clean), 5000),]


#check normality
normaility_test <- shapiro.test(random_planning_permission$CPI_monthly)
normaility_test$p.value


install.packages("pwr")
library("pwr")
#run apower test to determin the smaple size needed
power_test <- pwr.chisq.test(w= 0.6,  df = 1, sig.level = 0.05, p = .9  )

power_test


#create table to perform chi squared test
test_table <- table( planning_permission_clean$granted2 ,planning_permission_clean$CPI_greater_base)

test_table


#run chi squared test to determin if null hypothesis is accepted
chisq.test(test_table)




