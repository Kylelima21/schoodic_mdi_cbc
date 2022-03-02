#CBC_MDI_analysis

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/Nick/Documents/Projects/Birds/CBC_ANP/Rwork")

#read in the file without the 'X' at the beginning of column names (check.names = F)
T1<-read.csv('CBC_MDI_All.csv', header=TRUE, check.names = FALSE)
Y1<-read.csv('Years_MDI.csv', header=TRUE) #begins with 1971
#O1<-read.csv('Observers.csv', header=TRUE)
str(T1)
names(T1)
head(T1)

#Just keep the 50 years with Bill Townsend
T1<-T1[T1$Year >= 1971, ]
#remove species with zero birds
T1<-T1[!(T1$CommonName %in% c("American Three-toed Woodpecker",
                              "Blue Grosbeak", 
                              "Common Scoter",
                              "Little Gull",
                              "Northern Hawk Owl",
                              "Osprey",
                              "Red Phalarope",
                              "Red-shouldered Hawk")), ]

#######################################################################
#no. of species per year
#remove zeros
T1.1<-T1[T1$Count > 0, ]
#no. of species, no. of birds, birds sum by party hours,  
#party hours, observers

T2<-T1.1 %>%
  group_by(Year) %>%
  summarise(NoSpecies=length(CommonName),
            Birds=sum(Count),
            BirdsPartyHour=sum(CountPartyHour),
            PartyHours=mean(PartyHours))

#head(T2)
write.csv(T2, "Year_summary_OUT_MDI.csv", row.names=FALSE)

#statistical tests for number of bird species
cor(T2$Year, T2$NoSpecies, method="spearman") #-0.66
cor.test(T2$Year, T2$NoSpecies, method="spearman")
#S = 32460, p-value = 3.092e-07
#rho = -0.6561013

m.NoSpecies<-lm(NoSpecies~Year, data=T2)
summary(m.NoSpecies) #R^2: 0.43, F(1,47): 35.72, p-value: <0.0001
summary(T2$NoSpecies)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#50      56      62      62      67      82 
sd(T2$NoSpecies) #7.144


##################################################
#statistical tests for bird per year
cor(T2$Year, T2$Birds, method="spearman") #-0.70153
cor.test(T2$Year, T2$Birds, method="spearman")
#S = 33350, p-value = 8.523e-08
#rho = -0.7015306
m.Birds<-lm(Birds~Year, data=T2)
summary(m.Birds) #R^2=0.46, F(1,47)=40.4, p-value: <0.0001
summary(T2$Birds) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2122    7186    8591    9399   12002   16995 
sd(T2$Birds) #3275.232

###################################################################################
#math determining percent decline from linear model
60.1*52
311529.33+1971*-151.41 #13100.22
311529.33+2020*-151.41 #5681.13
1-(5681.13/13100.22) #= 56.63332% decline

#math determining percent decline from 10 year averages
100-(sum(T2$Birds[T2$Year >2010])/sum(T2$Birds[T2$Year <1981]))*100
#54.4% decline


###################################################################################
###################################################################################

##################################################
#statistical tests for bird per year
cor(T2$Year, T2$BirdsPartyHour, method="spearman") #-0.06040816
cor.test(T2$Year, T2$BirdsPartyHour, method="spearman")
#S = 20784, p-value = 0.6793
#rho = -0.06040816

m.BirdsPartyHour<-lm(BirdsPartyHour~Year, data=T2)
summary(m.BirdsPartyHour) #R^2=0, F(1,47)=0.0001, p-value = 0.99
summary(T2$BirdsPartyHour)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#66.77  116.38  152.50  163.97  197.50  346.87 
sd(T2$BirdsPartyHour) #67.00175

###################################################################################
#math determining percent decline from linear model
#9985.572+1969*-4.897 #
#9985.572+2020*-4.897 #
#1-(93.632/343.379) #%

#math determining percent decline from 10 year averages
100-(sum(T2$BirdsPartyHour[T2$Year >2010])/sum(T2$BirdsPartyHour[T2$Year <1979]))*100
#3.29% decline


###################################################################################
###################################################################################
###################################################################################

##################################################
#total no of species observed
#min year per species
T3<- T1.1 %>%
  group_by(CommonName) %>%
  summarise(Year=min(Year))

#max year per species
T3.max<- T1.1 %>%
  group_by(CommonName) %>%
  summarise(Year=max(Year))

head(T3)

T3.1<- T3 %>% 
  group_by(Year) %>%
  summarise(NewSpecies=length(Year))
head(T3.1)

T3.2<-merge(T3.1, Y1, by="Year",
            all.y=TRUE) #include all rows from second data frame
head(T3.2)
T3.2[is.na(T3.2)] <- 0
str(T3.2)
T3.2$cumsum<-cumsum(T3.2$NewSpecies)

sum(T3.2$NewSpecies[T3.2$Year > 2010]) #6
sum(T3.2$NewSpecies[T3.2$Year > 2000]) #11
sum(T3.2$NewSpecies[T3.2$Year > 1990]) #16
##################################################

