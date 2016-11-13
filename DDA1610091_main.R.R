
#########################Part I - Data Preparation #######################

# Importing loan data set in to R using read.csv
loan<-read.csv("loan.csv")

# Remove unwanted rows
loan<-loan[-which(is.na(loan$member_id)),]


# Check for NA values in driver variables and impute them.
# Driver Variables:annual_inc,loan_amnt,funded_amnt,int_rate, grade, dti, emp_length, purpose, home_ownership, loan_status

# Looking for NA values in the annual income
sum(is.na(loan$annual_inc))

# We have four NA values impute them with the median
median_annualincome<-median(loan$annual_inc,na.rm = T)

loan$annual_inc[is.na(loan$annual_inc)]<-median_annualincome

# Looking for NA values in the loan amount
sum(is.na(loan$loan_amnt))

#There are no NA's in the loan amount

# Looking for NA values in the funded amount
sum(is.na(loan$funded_amnt))

#There are no NA's in the funded amount

# Looking for NA values in the interest rate
sum(is.na(loan$int_rate))

#There are no NA's in the Interest rate

# Looking for NA values in the Grade
sum(is.na(loan$grade))

#There are no NA's in the grade


# Looking for NA values in the Debt to income Ratio
sum(is.na(loan$dti))

#There are no NA's in the Debt to income Ratio

# Looking for NA values in the Employee Length
sum(is.na(loan$emp_length))

loan<-loan[-which(loan$emp_length=='n/a'),]
#There are no NA's in the Employee Length

# Looking for NA values in the Purpose
sum(is.na(loan$purpose))

#There are no NA's in the Purpose


# Looking for NA values in the Home Owner Ship
sum(is.na(loan$home_ownership))

#There are no NA's in the Home Owner Ship


# Looking for NA values in the loan status
sum(is.na(loan$loan_status))

#There are no NA's in the loan status

# Removing rows the loan status is fully paid
loan_pending<-loan[!loan$loan_status %in% c("Fully Paid","Does not meet the credit policy. Status:Fully Paid"),]

# Create a new column named loan_status_1  with three levels current_new, default_new and late such that
# 1.) rows with loan_status as "current" or "in grace period" are tagged as  "current_new"
# 2.) rows with loan_status as " default" or "charged off" are tagged as "default_new"
# 3.) rows with loan_status as " late 16- 30 days" "late 31-120 days" are tagged as "late"  
loan_pending$loan_status1[loan_pending$loan_status %in% (c('Current','In Grace Period'))]<-'Current_new'
loan_pending$loan_status1[loan_pending$loan_status %in% (c('Default','Charged Off','Does not meet the credit policy. Status:Charged Off'))]<-'default_new'
loan_pending$loan_status1[loan_pending$loan_status %in% (c('Late (16-30 days)','Late (31-120 days)'))]<-'late'

# Convert loan_status_1 to factor type
loan_pending$loan_status1<-as.factor(loan_pending$loan_status1)

# Convert int_rate column to numeric type
loan_pending$int_rate=gsub("%","", loan_pending$int_rate)
loan_pending$int_rate=as.numeric(loan_pending$int_rate)

# Create new bin variables for int_rate and emp_length respectively as follows
# 1.) int_rate < 10 is tagged "Low"; int_rate (10-18) is tagged "Medium"; int_rate (>18) is tagged "High"  
loan_pending$int_rate_grp=ifelse(loan_pending$int_rate<10,"low",ifelse(loan_pending$int_rate<18,"medium","high"))
#keeping the int_rate_grp column next to int_rate for more readability
loan_pending=loan_pending[c(1:7,113,8:112)]
# Convert int_rate_grp column to factor type
loan_pending$int_rate_grp=as.factor(loan_pending$int_rate_grp)

# 2.) Create emp_len_grp such that emp_length (0-4) is tagged as "Junior"; emp_length (5-8) is tagged as "Mid-level"; emp_length (>8) is tagged as "Senior"  

loan_pending$Emp_length_grp[loan_pending$emp_length %in% (c('< 1 year','1 year','2 years','3 years','4 years'))]<-'Junior'
loan_pending$Emp_length_grp[loan_pending$emp_length %in% (c('5 years','6 years','7 years','8 years'))]<-'Mid-level'
loan_pending$Emp_length_grp[loan_pending$emp_length %in% (c('9 years','10+ years'))]<-'Senior'
#keeping the emp_len_grp column next to emp_len for more readability
loan_pending=loan_pending[c(1:13,114,14:113)]
# Convert emp_len_grp column to factor type
loan_pending$Emp_length_grp=as.factor(loan_pending$Emp_length_grp)

#######################Part II - Exploratory Data Analysis#####################
# Univariate Analysis

# Summary Statistics
annual_inc.summary=summary(loan_pending$annual_inc)
loan_amnt.summary=summary(loan_pending$loan_amnt)
funded_amnt.summary=summary(loan_pending$funded_amnt)
int_rate.summary=summary(loan_pending$int_rate)
int_rate_grp.summary=summary(loan_pending$int_rate_grp)
grade.summary=summary(loan_pending$grade)
dti.summary=summary(loan_pending$dti)
emp_len_grp.summary=summary(loan_pending$Emp_length_grp)
purpose.summary=summary(loan_pending$purpose)
home_own.summary=summary(loan_pending$home_ownership)
loan_status.summary=summary(loan_pending$loan_status)
loan_status_1.summary=summary(loan_pending$loan_status_1)

# Skewness 
library(moments)
annual_inc.skew=skewness(loan_pending$annual_inc)
loan_amnt.skew=skewness(loan_pending$loan_amnt)
funded_amnt.skew=skewness(loan_pending$funded_amnt)
dti.skew=skewness(loan_pending$dti)

# Kurtosis
annual_inc.kurt=kurtosis(loan_pending$annual_inc)
loan_amnt.kurt=kurtosis(loan_pending$loan_amnt)
funded_amnt.kurt=kurtosis(loan_pending$funded_amnt)
dti.kurt=kurtosis(loan_pending$dti)

# Distribution Plots
library(ggplot2)

# Annual Income Histogram
ggplot(loan_pending,aes(annual_inc))+geom_histogram(binwidth = 10000)

#Loan Amount Histogram
ggplot(loan_pending,aes(loan_amnt))+geom_histogram(binwidth = 1000)

# Funded Amount Histogram
ggplot(loan_pending,aes(funded_amnt))+geom_histogram(binwidth = 1000)

# Debt to Income ratio Histogram
ggplot(loan_pending,aes(dti))+geom_histogram(binwidth =1)


# Annual Income Boxplot
annual_inc.boxplot=boxplot(loan_pending$annual_inc)
# Loan Amount Boxplot
loan_amnt.boxplot=boxplot(loan_pending$loan_amnt)
# Funded Amount Income Boxplot
funded_amnt.boxplot=boxplot(loan_pending$funded_amnt)
# Debt to Income Ratio Boxplot
dti.boxplot=boxplot(loan_pending$dti)


## Interest Rate Group
ggplot(loan_pending,aes(int_rate_grp,fill=int_rate_grp))+geom_bar()+
stat_count(aes(label=..count..),geom = "text",vjust=-0.3)


## Grade
ggplot(loan_pending,aes(grade,fill=grade))+geom_bar()+
  stat_count(aes(label=..count..),geom = "text",vjust=-0.3)



# Emp Length

ggplot(loan_pending,aes(Emp_length_grp,fill=Emp_length_grp))+geom_bar()+
  stat_count(aes(label=..count..),geom = "text",vjust=-0.3)


# Purpose
ggplot(loan_pending,aes(purpose,fill=purpose))+geom_bar()+
  stat_count(aes(label=..count..),geom = "text",vjust=-0.3)

# Home ownership
ggplot(loan_pending,aes(home_ownership,fill=home_ownership))+geom_bar()+
  stat_count(aes(label=..count..),geom = "text",vjust=-0.3)

# Loan Status
ggplot(loan_pending,aes(loan_status1,fill=loan_status1))+geom_bar()+
  stat_count(aes(label=..count..),geom = "text",vjust=-0.3)

# Outlier Detection
annual_inc.outliers=boxplot.stats(loan_pending$annual_inc)$out

loan_amt.outliers=boxplot.stats(loan_pending$loan_amnt)$out

funded_amt.outliers=boxplot.stats(loan_pending$funded_amnt)$out

dti.outliers=boxplot.stats(loan_pending$dti)$out

# The outliers in annual_inc, compared to the other outliers, affect the spread of data points across other variables.
# So we remove these outliers from the loan dataset  
loan_pending=loan_pending[!(loan_pending$annual_inc) %in% c(annual_inc.outliers,loan_amt.outliers,funded_amt.outliers,dti.outliers),]


#Multivariate Analysis

loan_sub<-subset(loan_pending,select = c(annual_inc,loan_amnt,funded_amnt,dti))

# Correlation Matrix
cor(loan_sub)

library(GGally)
# Correlation Pairs
ggpairs(loan_sub)

# Loan Status1 against annual income,loan amount,funded amount and DTI
ggplot(loan_pending,aes(x=annual_inc,fill=loan_status1))+
  geom_histogram(binwidth = 10000)+facet_wrap(~loan_status1)+
  geom_freqpoly(binwidth=10000)+ggtitle("Annual Income for various levels of Loan status")

ggplot(loan_pending,aes(x=loan_amnt,fill=loan_status1))+
  geom_histogram(binwidth = 5000)+facet_wrap(~loan_status1)+
  geom_freqpoly(binwidth=5000)+ggtitle("Loan Amount for various levels of Loan status")

ggplot(loan_pending,aes(x=funded_amnt,fill=loan_status1))+
  geom_histogram(binwidth = 5000)+facet_wrap(~loan_status1)+
  geom_freqpoly(binwidth=5000)+ggtitle("Funded Amount for various levels of Loan status")

ggplot(loan_pending,aes(x=dti,fill=loan_status1))+
  geom_histogram(binwidth = 2)+facet_wrap(~loan_status1)+
  geom_freqpoly(binwidth=2)+ggtitle("Debt to Income Ratio for various levels of Loan status")
 

# Interest Rate Group against annual income,loan amount,funded amount and DTIs
ggplot(loan_pending,aes(x=annual_inc,fill=int_rate_grp))+
  geom_histogram(binwidth = 10000)+facet_wrap(~int_rate_grp)+
  geom_freqpoly(binwidth=10000)+ggtitle("Annual Income for various levels of Interest Groups")

ggplot(loan_pending,aes(x=loan_amnt,fill=int_rate_grp))+
  geom_histogram(binwidth = 5000)+facet_wrap(~int_rate_grp)+
  geom_freqpoly(binwidth=5000)+ggtitle("Loan Amount for various levels of Interest Groups")


ggplot(loan_pending,aes(x=funded_amnt,fill=int_rate_grp))+
  geom_histogram(binwidth = 5000)+facet_wrap(~int_rate_grp)+
  geom_freqpoly(binwidth=5000)+ggtitle("Funded Amount for various levels of Interest Groups")

ggplot(loan_pending,aes(x=dti,fill=int_rate_grp))+
  geom_histogram(binwidth = 2)+facet_wrap(~int_rate_grp)+
  geom_freqpoly(binwidth=2)+ggtitle("Debt to Income Ratio for various levels of Interest Groups")

# Hypothesis Testing--Loan Status 1

 default<-subset(loan_pending,loan_pending$loan_status1=="default_new")
 current<-subset(loan_pending,loan_pending$loan_status1=="Current_new")
 
 t.test(default$annual_inc,current$annual_inc)
 t.test(default$loan_amnt,current$loan_amnt)
 t.test(default$funded_amnt,current$funded_amnt)
 t.test(default$dti,current$dti)
 
 # Hypothesis Testing--Int Rate Group
 
 high<-subset(loan_pending,loan_pending$int_rate_grp=="high")
 low<-subset(loan_pending,loan_pending$int_rate_grp=="low")
 
 t.test(high$annual_inc,low$annual_inc)
 t.test(high$loan_amnt,low$loan_amnt)
 t.test(high$funded_amnt,low$funded_amnt)
 t.test(high$dti,low$dti)
 
 # Writing csv file from R to the system
  
 driver_variables<-c("annual_inc","loan_amnt","funded_amnt",
                     "int_rate_grp","grade","dti","Emp_length_grp",
                     "purpose","home_ownership","loan_status1")
 loan_driver_variables<-loan_pending[driver_variables]
 
 
 write.csv(loan_driver_variables,"loan_new.csv",row.names = F)
 