# Auto-Theft
    # INITIAL CODE AND RESULT
    # OUTLIER DETECTION
    # AUTHOR:  HOAN BAO NGOC NGO
    # STUDENT ID: 501009323_642


library(dplyr)
library(ggplot2)
library(corrplot)
library(stats)
library(cluster)
library(readr)

# STEP 1: LOADING DATA, PLOT, CONVERT DATA TO NUMERIC 
# Load the original dataset
FullSet =  read.csv("G:/sonar/Auto_Theft_2014_to_2019.csv", header = T)
#having a glance of the dataset
head(FullSet)

unique(FullSet$occurrenceyear)

#Cleaning up data, start removing columns that we don't need to use.
#Reason: either represent same value (such as MCI, offense) or not relevant to this project (event id, for officier use only)
AutoTheft  = select(FullSet, -c( Lat, Long ,MCI,ï..X, Y, event_unique_id, offence))
head(AutoTheft)

#let see if we need to remove a few more column if they contain the exact same value
unique(AutoTheft$ucr_code)
unique(AutoTheft$ucr_ext)

#Both column provide the same value, no need to use it here. Then I will also remove it.
AutoTheft = select(AutoTheft, -c(ucr_code,ucr_ext ))

#Having a statistic glance of the dataset
summary (AutoTheft)

#since our dataset contains large attributes, I will show unique vales of the dataset as a glance.
Permise = unique(AutoTheft$premisetype)
Permise

#converting data
#converting OCCURENCE Month
AutoTheft$occurrencemonth = match(AutoTheft$occurrencemonth, month.name)
print(head(AutoTheft$occurrencemonth))

#CONVERT REPORT MONTH
AutoTheft$reportedmonth = match(AutoTheft$reportedmonth, month.name)
print(unique(AutoTheft$reportedmonth))

#CONVERT OCCURENCE DATE
AutoTheft$occurrencedate = as.Date(AutoTheft$occurrencedate)
head(AutoTheft$occurrencedate)

#CONVERT REPORTED DATE
AutoTheft$reporteddate = as.Date(AutoTheft$reporteddate)
head(AutoTheft$reporteddate)

#CONVERT WEEK DAY
AutoTheft$reporteddayofweek = weekdays(AutoTheft$reporteddayofweek)

#Removing missing value
AutoTheft = na.omit(AutoTheft)

#PLOT

par(mfrow = c(3,1))

# As seen in plot, number of report cases surged from 2018 to 2019
ggplot(AutoTheft, aes(reportedyear))+geom_bar()
hist(AutoTheft$occurrenceyear, main = "Occurance year of incidient", xlab = "Year", xlim = c(2013,2019),ylim = c(0,6000),label = T, col = 'pink') 

#I mean to put ggplot above hist, so when the code run, we can see 2 graphs into each others
#Box plot to show the relation of Occurrence year and month reported
boxplot(AutoTheft$reportedmonth ~ AutoTheft$occurrenceyear, notch = T , las = 1) 


#Box plot to show the relation of Hood ID and when the incident was report (month)
boxplot(AutoTheft$reportedmonth  ~ AutoTheft$Hood_ID, xlab = 'Hood Id', ylab = 'Reported Month', las  = 1)

#Showing which Hood ID has the highest cases of incidents. 
hist(AutoTheft$Hood_ID, label = T, ylim = c(0,5000))

#the plot here show us even more information, the year of incident, the hours and the neighbour hood (as hood_id)
ggplot(data = AutoTheft) + geom_point(mapping = aes(x = occurrenceyear, y = occurrencehour, color = Hood_ID))


#table of sum
#sum of count of premise type
table(AutoTheft$premisetype)

#sum of month of incidents
Count_of_month = table(AutoTheft$occurrencemonth)

plot(Count_of_month, main = "Count of month")

Count_of_day = table(AutoTheft$occurrenceday)
plot(Count_of_day)
#What i found here is that October and November have a highest rate of incident.
#Does it relate to the weather, when it is not full of snow like December and Jan or Feb, which make the crime easy to commit?
#October and Fall are also in fall, which offer the most beautiful view, and outdoor activities such as hiking, biking or taking a walk.
#Should citizen aware of this information, so that they can be careful, and data will be collected to see if the rate actually drops?  

#showing the relation of 2 categorical data and numeric.
ggplot(data = AutoTheft) + geom_point(mapping = aes(x= premisetype, y = occurrenceyear, color= Hood_ID), size = 4)

#showing when incidient happens, weekday or weekend, at home or outside, in which neighbourhood. 
ggplot(data = AutoTheft) + geom_point(mapping = aes(x= premisetype, y = occurrencedayofweek, color= Hood_ID))

#STEP 2: SPLIT DATA AND CLUSTERING DATA

#1. CLUSTERING DATA. 
#IN THIS STEP, DATA ARE CONVERT TO NUMERIC IN ORDER TO PERFORM CLUSTER BY K-MEANS 
Set1= select(AutoTheft,-c(2,3,4,9,15,17,19))
head(Set1)

#DUE TO THE NATURE OF THE DATA, IM NOT SURE HERE IF ITS A GOOD IDEA TO SCALE OR NORMALIZE DATA
scale(Set1)

#I WAS THINKING TO CHOOSE GOWER METHOD, BUT FOUND MYSELF NOT CONFIDENT YET. 
#THIS IS JUST A NOTE FOR MYSELF.
gower_dist = daisy(Set1, metric = 'gower')
gower_mat  = as.matrix(gower_dist)

#USING K-CLUSTER
Clus = kmeans(Set1, centers = 5)
str(Clus)
Clus

#HAVING A PROBLEM HERE TO VISULIZE THE CLUSTER (STILL WORKING ON IT)

  
 

#SPLITTING DATA
sampleset = sample(1:nrow(AutoTheft), 0.7 * nrow(AutoTheft))
train.set = AutoTheft[sampleset,]
test.set = AutoTheft[-sampleset,]
print(train.set)
print(test.set)


#I will remove OBJECT iD in both set 
train.set = train.set[-23]
test.set = test.set[-23]


#NOTE: STILL A FEW STEP TO BE CLEAR ON, AND I AM KINDA NOT SURE WHERE TO GO, WILL CONTACT FOR HELP FOR FURTHER QUESTION. 
