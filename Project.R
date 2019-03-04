#Import Data
myd=read.csv("Medical Appointment.csv", header=T)

#First impression of data
head(myd)
summary(myd)
dim(myd)

#Explore the dataset
library(VIM)
library(mice)
aggr(myd, prop=FALSE, numbers=TRUE)
boxplot(myd$Age)

#Cut out PatientId, AppointmentID
myd_n=myd[,3:14]
summary(myd_n)

#Create new variable
ScheduledDay=as.Date(myd$ScheduledDay)
typeof(ScheduledDay)
AppointmentDay=as.Date(myd$AppointmentDay)
typeof(AppointmentDay)
DateDifference=AppointmentDay-ScheduledDay
typeof(DateDifference)
head(DateDifference,100)
myd_n$DateDifference=DateDifference
head(myd_n)

#Create Dummy Variable
myd_n$Gender=ifelse(myd$Gender=="F",1,0)
myd_n$Gender=ifelse(myd$Gender=="M",1,0)
head(myd_n)

#Cut off Variable Neighborhood
mydn=myd_n[,-5]
head(mydn)
dim(mydn)
mydn$AppointmentDay=as.Date(mydn$AppointmentDay)
typeof(mydn$AppointmentDay)
mydn$ScheduledDay=as.Date(mydn$ScheduledDay)
typeof(mydn$ScheduledDay)

#Convert Date to Weekdays
mydn$Appointment_Weekday=weekdays(mydn$AppointmentDay)
head(mydn)


#Add labels to categorical variables
typeof(mydn$Appointment_Weekday)
mydn$Appointment_Weekday=as.character(mydn$Appointment_Weekday)
mydn$Appointment_Weekday=factor(mydn$Appointment_Weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), labels = c("1","2","3","4","5","6","7"))

head(mydn)
mydn=mydn[,-2:-3]
head(mydn)
mydn$No.show=as.character(mydn$No.show)
typeof(mydn$Scholarship)
mydn$Age=as.double(mydn$Age)
mydn$Scholarship=as.double(mydn$Scholarship)
mydn$Hipertension=as.double(mydn$Hipertension)
mydn$Diabetes=as.double(mydn$Diabetes)
mydn$Alcoholism=as.double(mydn$Alcoholism)
mydn$Handcap=as.double(mydn$Handcap)
mydn$SMS_received=as.double(mydn$SMS_received)
mydn$DateDifference=as.double(mydn$DateDifference)
mydn$Appointment_Weekday=as.numeric(mydn$Appointment_Weekday)

library(MASS)
AppointmentLDA=lda(mydn$No.show~mydn$Gender+mydn$Age+mydn$Scholarship+mydn$Hipertension+mydn$Diabetes+mydn$Alcoholism+mydn$Handcap+mydn$SMS_received+mydn$DateDifference+mydn$Appointment_Weekday)
AppointmentLDA
plot(AppointmentLDA)


#Age, Scholarship, Diabetes, Aloco, SM, Date Differece, Appnt Weekday
AppointmentLDA2=lda(mydn$No.show~mydn$Age+mydn$Scholarship+mydn$Diabetes+mydn$Alcoholism+mydn$SMS_received+mydn$DateDifference+mydn$Appointment_Weekday)
plot(AppointmentLDA2)


