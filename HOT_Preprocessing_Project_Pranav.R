
#Set Directory
setwd("E:/PP/TCC")

#Read CSV
telCom_data = read.csv(file = "tel_customer_churn.csv", header = T, sep = ",")


#Read Data Type
nrow(telCom_data)
ncol(telCom_data)
str(telCom_data)
summary(telCom_data)

#Analysis: 

##DataTypes:

### Categorical Data

# 1. Gender (F: 3482 M: 3550 Blank: 11)
# 2. SeniorCitizen (0/1) --- Must be converted to Factor
# 3. Partrners (Yes/No)
# 4. Dependednts (Yes/No)
# 5. City (Hyd) --- Actually a Character Data type. If not used for analysis, can be unlisted
# 6. PhoneService (Yes/No)
# 7. MultipleLines (Yes/No/No Phone Service)
# 8. InternetService (DSL/Fiber Optic/No)
# 9. OnlineSecurity (Yes/No/No Internet Service)
# 10. OnlineBackup (Yes/No/No Internet Service)
# 11. DeviceProtection (Yes/No/No Internet Service)
# 12. TechSupport (Yes/No/No Internet Service)
# 13. StreamingTV (Yes/No/No Internet Service)
# 14. StreamingMovies (Yes/No/No Internet Service)
# 15. Contract (month-to-month, One Year, Two Year)
# 16. PaperlessBilling (Yes/No)
# 17. PaymentMethod (Bank transfer (automatic), Credit card (automatic), Electronic check, Mailed check)
# 18. Churn (Yes/No)

### Numeric Data

# 1. CustomerID --- Actually a Mixedtype Data
# 2. DateOfJoining (73) --- Can be converted as DATE type if needed
# 3. MonthlyCharges
# 4. TotalCharges


## Missing Data from summary observation

# 15 Blanks
# 7 ?
# 17 NA's

#Descriptive Stats from Summary

##Omitting NA's before performing Desc Stats
telCom_data[telCom_data == "?"] = NA
telCom_data[telCom_data == ""] = NA
telCom_data = na.omit(telCom_data)
sum(is.na(telCom_data))
summary(telCom_data)

#Customers Monthly Filling ranges from 18.25 to 118.75. With Mean and median being 64.74 & 70.30  respectively.
#Same for Total Chargers is 18.8, 8684.8, 2280.1 & 1396.2 respectively. With such a huge deviation between Mean 
#and Median, there is a possibility of an Outlier.


#Redundant Variables in Data

# 1. City
# 1. CustomerID

telCom_data_2 = telCom_data
telCom_data_2$City = NULL
telCom_data_2$customerID = NULL



#Converting Categorical Variables to Factors variables.

telCom_data_2$SeniorCitizen = as.factor(telCom_data_2$SeniorCitizen)
str(telCom_data_2)

#Categorical and Numeric separated 

telCom_data_2_Num = subset(telCom_data_2, select = c(DateOfJoining, MonthlyCharges, TotalCharges))
#Although DateOfJoining is not Num type, considered for timebeing as Date type converstion is not necessary yet.
#Alternatively, it can dropped after seperating Categorical variables.

telCom_data_2_Cat = subset(telCom_data_2, select = -c(DateOfJoining, MonthlyCharges,TotalCharges))



# Identifying customers with Total Charges less than Monthly Charges.

Tc_less_Mc = data.frame(which(telCom_data_2_Num$TotalCharges < telCom_data_2_Num$MonthlyCharges))

nrow(Tc_less_Mc)

#As observed, none of the customers have Total Charges less than Monthly Charges.

#---#
#If we were to remove customers whose Total Charges equal to Monthly Charges, code could have been;

#Tc_eql_Mc = telCom_data_Num[-(which(telCom_data_Num$TotalCharges == telCom_data_Num$MonthlyCharges)),]

#nrow(Tc_eql_Mc)
#---#


#Check Whether Numric Columns are Standardized 

summary(telCom_data_2_Num)

#Not Standardized



#Choosing on Numerical Column to Standardize using Z-Score 

#Choosing MonthlyCharges

library(vegan)

telCom_data_2_Num$MonthlyCharges = decostand(telCom_data_2_Num$MonthlyCharges, "standardize")

#The Standardized Values range from -1.54 to 1.795.





#Standardize Numeric Variable and Check for Outlier.

#Considering above standardized Numeric variable

boxplot(telCom_data_2_Num$MonthlyCharges)

# As the Boxplot suggests, there are no outliers in the Standardized Numeric Variable MonthlyCharges.



#Filling Null Values based on variable type chosen.

#Re-importing csv file and filtering it for Categorical variables to perform Central Imputation on 
#Categorical variables Gender and InternetService.


Sec_telco_d = read.csv(file = "tel_customer_churn.csv", header = T, sep = ",")
Sec_telco_d$SeniorCitizen = as.factor(Sec_telco_d$SeniorCitizen)
Sec_telco_d_Num = subset(Sec_telco_d, select = c(DateOfJoining, MonthlyCharges, TotalCharges))
Sec_telco_d_Cat = subset(Sec_telco_d, select = -c(DateOfJoining, MonthlyCharges,TotalCharges))

Sec_telco_d_Cat[Sec_telco_d_Cat == "?"] = NA
Sec_telco_d_Cat[Sec_telco_d_Cat == ""] = NA
summary(Sec_telco_d_Cat)
sum(is.na(Sec_telco_d_Cat))

#Reviewing high frequescy values in both gender & internetservice before Central Imputation (by Mode) 
#on Categorical variables for better understanding

table(Sec_telco_d_Cat$gender)
table(Sec_telco_d_Cat$InternetService)

library(DMwR)
Sec_telco_d_Cat = centralImputation(Sec_telco_d_Cat)
sum(is.na(Sec_telco_d_Cat))

#Notice the increase in count of Male & Fiber Optic as Central Imputation was applied.
table(Sec_telco_d_Cat$gender)
table(Sec_telco_d_Cat$InternetService)

#Since NA must be filled with Mode values for Catergorical Variables, The row values corresponding to
#Gender in row 36 was filled with Male after Cental Imputation. While, row 6, 36 & 265 were filled with
#Fiber Optic after Central Imputation.


# Considering numerical variable TotalCharges from telCom_data_Num dataframe agnist previously considered
# Variable to perform standardization using Min-Max method.

library(vegan)

telCom_data_2_Num$TotalCharges = decostand(telCom_data_2_Num$TotalCharges, "range")

#The Standardized Values fall between 0 to 1 alone. The mean and median are appx. 11% gap, leaving a chance for 
#possible outlier.


#Checking if dummification was perfromed on Cateogrical variables.

str(telCom_data_2_Cat)

#Not performed



#Dummifying Categorical Variables.

library(dummies)

gen_dum = dummy(telCom_data_2_Cat$gender)

telCom_data_2_Cat = data.frame(telCom_data_2_Cat, gen_dum)

#To observe the unique values created after dummification

head(gen_dum)

# To drop the dummy columns created so as to dummify all categorical variables at once

telCom_data_2_Cat$genderFemale = NULL
telCom_data_2_Cat$genderMale = NULL
rm(gen_dum)

# Dummification of all categorical varables.

#Since there are 6 variables with 2 levels, 8 variables with 3 levels and
#1 with 4 levels and 5 levels each, the total number of dummy colums that
#must be generated will be (2*6) + (8*3) + 4 + 5 = 45. 
#Storing these dummy columns in new variable telCom_data_2_Cat_Dum.

telCom_data_2_Cat_Dum = data.frame(apply(telCom_data_2_Cat, 2, dummy))



#Consider Monthly Charges variable to perform binning using inbuilt 'R' function.
#Equal width binning and Equal frequency

#Equal Width Binning:

bin_size = 4
minvalue = min(telCom_data_2_Num$MonthlyCharges)
maxvalue = max(telCom_data_2_Num$MonthlyCharges)
width = (maxvalue - minvalue)/bin_size

MonthlyCharges_Bin = cut(x = telCom_data_2_Num$MonthlyCharges, breaks = seq(minvalue,maxvalue,width))

table(MonthlyCharges_Bin)

#Equal Frequency Binning:

T_Rows = length(telCom_data_2_Num$MonthlyCharges)

bin_size = 4

Rem = (T_Rows)%%bin_size

#Since the reminder is 3, subtracting last 3 rows from the data.

telCom_data_2_Num_EF = data.frame(telCom_data_2_Num[(1:(T_Rows-Rem)),])

#Creating 4 frequencies for 4 bins:

freq_1 = length(telCom_data_2_Num_EF$MonthlyCharges)/(bin_size)

freq_2 = freq_1+1

freq_3 = freq_1*2

freq_4 = freq_1*3

bin_1 = telCom_data_2_Num_EF[1:freq_1,]
bin_2 = telCom_data_2_Num_EF[(freq_2:freq_3),]
bin_3 = telCom_data_2_Num_EF[((freq_3+1):freq_4),]
bin_4 = telCom_data_2_Num_EF[(((freq_4+1):nrow(telCom_data_2_Num_EF))),]

bin_col = rbind(bin_1,bin_2,bin_3,bin_4)
bin_col = telCom_data_2_Num_EF$MonthlyCharges
bin_col

#Sorting
bin_col_s = data.frame(sort(bin_col))


final_BinCol = bin_col_s

r1 = freq_3 - 1
r2 = freq_4 - 1

final_BinCol[1:freq_1,] = 1
final_BinCol[freq_2:r1,] = 2
final_BinCol[freq_3:r2,] = 3
final_BinCol[freq_4: nrow(final_BinCol),] = 4

table(final_BinCol)



#Considering Total Charges for binning manually using loops

bin_col2 = data.frame(telCom_data_2_Num$TotalCharges)

summary(bin_col2)

NewTC = 0

for (i in 1:nrow(bin_col2))
{
  if(bin_col2$telCom_data_2_Num.TotalCharges[i] < 0.15895)
  {
   NewTC[i] = 1 
  }
  else
  {
    NewTC[i] = 2
  }
}

table(NewTC)



# Extracting Month, Day, Year and Tenure (Customer tenure with company) from
# Date of Joining.


#Extracting Day, Month and Year

library(lubridate)

day(as.Date(Date_ext$telCom_data_2_Num.DateOfJoining, format = "%d/%m/%y"))
month(as.Date(Date_ext$telCom_data_2_Num.DateOfJoining, format = "%d/%m/%y"))
year(as.Date(Date_ext$telCom_data_2_Num.DateOfJoining, format = "%d/%m/%y"))

#Calculating Tenure Duration in days

Date_ext = data.frame(telCom_data_2_Num$DateOfJoining)

##Converting given date format to Standard format

Date_ext$telCom_data_2_Num.DateOfJoining = as.Date(Date_ext$telCom_data_2_Num.DateOfJoining, format = "%d/%m/%y")

today = Sys.Date()

cus_ten = data.frame(difftime(today, Date_ext$telCom_data_2_Num.DateOfJoining))










