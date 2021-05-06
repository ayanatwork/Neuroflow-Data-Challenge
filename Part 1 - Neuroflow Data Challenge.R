########### Developed by - Ayan Das ##############################
########### Developed on - May 06, 2021 ##########################
########### Developed for - Neuroflow Data Challenge ##############

#------------------------------------------
## Preliminary ##
#------------------------------------------
## Clearing workspace
rm(list=ls())

## Set working directory
setwd("C:/Users/Ayan/Desktop/neuroflow_data_challenge")

## Load Libraries
library(dplyr) # data manipulation
library(ggplot2) # plots and visualization

#------------------------------------------

## Import the phq_all_final.csv data as a dataframe
ds <- read.csv(file = "phq_all_final.csv", 
               stringsAsFactors = FALSE)

# Obtain structure
str(ds)

# Rows
nrow(ds) ## 53698 rows in the dataset 

# Columns
ncol(ds) ## 5 columns in the dataset


# Obtain basic descriptive statistics
summary(ds)

# Obtain no. of patients
length(unique(ds$patient_id)) ## 15502 unique no. of patients

#------------------------------------------

# Split date and time and store it in different columns
## date column
ds$meas_date <- substr(ds$date,0,10)
ds$meas_time <- substr(ds$date,12,19)

## patient_date_created column
ds$created_date <- substr(ds$patient_date_created,0,10)
ds$created_time <- substr(ds$patient_date_created,12,19)

# Remove redundant columns
ds <- ds[-c(1,3,4)]

#------------------------------------------
## Descriptive Statistics & EDA ##
#------------------------------------------
# EDA on our target variable i.e. score
## Calculate the frequency distribution of our target variable
score_freq <- ds %>% group_by(score) %>% summarise(frequency=n())

## Visualize the frequency distribution as a barplot 
score_freq_ch <- ggplot(score_freq, aes(x = score, y = frequency))+geom_col(aes(fill = score), width = 0.4)
score_freq_ch + labs(x="GAD-7 Score", y="Frequency", title = "Frequency distribution of GAD-7 scores")

#------------------------------------------

# Create pie chart of Percentage distribution of our target variable
## Calculate percentage distribution of our target variable
score_freq$percent <- round(score_freq$frequency*100/sum(score_freq$frequency),2)

## Transform our target variable as characters to use it in legends
score_freq$score=as.character(score_freq$score)

## Pie chart of Percentage distribution
pie(score_freq$percent, labels=score_freq$percent, main="Percentage distribution of GAD-7 scores", radius=1,
    col = rainbow(length(score_freq$percent)))

legend("topright", score_freq$score,cex=0.8, fill=rainbow(length(score_freq$percent)))

#------------------------------------------

# EDA on patient_id variable
## Calculate frequency distribution of patients wrt no. of assessments
patient_freq <- ds %>% group_by(patient_id) %>% summarise(frequency=n())

## Visualize patients wrt no. of assessments
patient_freq_plot <- ggplot(patient_freq, aes(x = patient_id, y = frequency))+geom_col(aes(fill = patient_id), width = 10)

patient_freq_plot + ylim(0,100) + scale_x_continuous(n.breaks=20) + scale_y_continuous(n.breaks=20)

## Boxplot on patient_id vs GAD-7 score
boxplot(patient_id ~ score,data=ds, main="Patients vs GAD-7 score",
        xlab="GAD-7 score", ylab="Patient_ID")

#------------------------------------------

# Analysis on monthly_created and monthly_assessment
## Monthly creation frequency
monthly_created <- ds %>% group_by (created_date=substr(created_date,0,7)) %>% summarise(patient_count=n())

## Monthly assessment frequency
monthly_assess <- ds %>% group_by (meas_date=substr(meas_date,0,7)) %>% summarise(patient_count=n())

#------------------------------------------

# Get the first (oldest) data point wrt to assessment_date of each patient
## Ranking data-points wrt ascending meas_date partitioned by patient_id
ds2 <- ds %>% group_by (patient_id) %>% arrange(meas_date) %>% mutate(rank=row_number()) 

#------------------------------------------

# Fetching the data-point with rank=1. This gives the oldest assessment details of each patient
oldest_assess <- ds2 %>% filter(rank==1) %>% rename(oldest_score=score, oldest_meas_date=meas_date)

#------------------------------------------

# Get the most recent (newest) data-point wrt assessment_date of each patient
## Ranking data-points wrt descending meas_date partitioned by patient_id
ds3 <- ds %>% group_by (patient_id) %>% arrange(desc(meas_date)) %>% mutate(rank=row_number())


## Fetching the data-point with rank=1. This gives the newset assessment details of each patient
newest_assess <- ds3 %>% filter(rank==1) %>% rename(newest_score=score, newest_meas_date=meas_date)

#------------------------------------------

# Visualize latest status of patients
## Latest status of patients
newest_assess <- newest_assess %>% mutate(Severity_Label = case_when(newest_score>=0 & newest_score<=5 ~ 'Low to Minimal',
                                                                     newest_score>=6 & newest_score<=10 ~ 'Mild',
                                                                     newest_score>=11 & newest_score<=15 ~ 'Moderate',
                                                                     newest_score>=16 & newest_score<=21 ~ 'Severe'))


# Visualize severity label of patients
Sev_lb_count <- newest_assess %>% group_by(Severity_Label) %>% summarise(count=n())

patient_sev_lb <- ggplot(Sev_lb_count, aes(x = Severity_Label, y = count))+geom_col(aes(fill = count), width = 0.4)

patient_sev_lb + labs(x="Count", y="Severity_Label", title = "Latest severity label summary of patients") + coord_flip()

#------------------------------------------

# Visualize progress of patients
## Combining oldest and newest assessment datasets together
ds4 <- inner_join(oldest_assess, newest_assess, by = c("patient_id"="patient_id"))  %>% 
       select(patient_id, oldest_meas_date, oldest_score, newest_meas_date, newest_score)

## Removing patients with 0 scores as it is redundant and will not be useful in futher analysis
ds5 <- ds4 %>% filter(oldest_score!=0 & newest_score!=0) ##4595 rows removed

ds5 <- ds5 %>% mutate(progress = case_when(oldest_score==newest_score ~ 'No Change', 
                                             oldest_score<newest_score ~ 'Worsened',
                                             oldest_score>newest_score ~ 'Improved'))                                                                         
## Visualize progress of patients as barplot
progress_count <- ds5 %>% group_by(progress) %>% summarise(count=n())

patient_prog <- ggplot(progress_count, aes(x = progress, y = count))+geom_col(aes(fill = count), width = 0.4)

patient_prog + labs(x="Count", y="Progress", title = "Progress Summary") + coord_flip()

