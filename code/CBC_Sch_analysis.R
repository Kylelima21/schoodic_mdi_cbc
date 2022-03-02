#CBC_Sch_analysis

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("/Users/Nick/Documents/Projects/Birds/CBC_ANP/Rwork")

#read in the file without the 'X' at the beginning of column names (check.names = F)
T1<-read.csv('CBC_Sch_52Years.csv', header=TRUE, check.names = FALSE)
Y1<-read.csv('Years_Sch.csv', header=TRUE) #begins with 1971
#O1<-read.csv('Observers.csv', header=TRUE)
str(T1)
names(T1)
head(T1)

#Just keep the 50 years with Bill Townsend
T1<-T1[T1$Year >= 1971, ]
#Check to see if there are any species to remove due to zero birds
T.x<-T1 %>%
  group_by(CommonName) %>%
  summarise(Birds=sum(Count))

#remove species with zero birds
#T1<-T1[!(T1$CommonName %in% c("American Three-toed Woodpecker", "")), ]
rm(T.x)
#
####################################################


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
write.csv(T2, "Year_summary_OUT_Sch.csv", row.names=FALSE)

#statistical tests for number of bird species
cor(T2$Year, T2$NoSpecies, method="spearman") #0.177
cor.test(T2$Year, T2$NoSpecies, method="spearman")
#S = 17158, p-value = 0.2212
#rho = 0.1760903

m.NoSpecies<-lm(NoSpecies~Year, data=T2)
summary(m.NoSpecies) #R^2: 0.04, F(1,48): 2.115, p-value: 0.15
summary(T2$NoSpecies)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#44.00   50.00   54.00   53.92   57.75   67.00 
sd(T2$NoSpecies) #5.649


##################################################
#statistical tests for bird per year
cor(T2$Year, T2$Birds, method="spearman") #-0.49
cor.test(T2$Year, T2$Birds, method="spearman")
#S = 31046, p-value = 0.0003512
#rho = -0.4908043

m.Birds<-lm(Birds~Year, data=T2)
summary(m.Birds) #R^2=0.0848, F(1,48)=4.448, p-value: 0.04019
summary(T2$Birds) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2210    3471    4916    5554    6657   21153  
sd(T2$Birds) #3097.031

###################################################################################
#math determining percent decline from linear model
129012.39+1971*-61.87 #7066.62
129012.39+2020*-61.87 #4034.99
1-(4034.99/7066.62) #= 42.9% decline

#math determining percent decline from 10 year averages
100-(sum(T2$Birds[T2$Year >2010])/sum(T2$Birds[T2$Year <1981]))*100
#45.239% decline


###################################################################################
###################################################################################

##################################################
#statistical tests for birds per party hour per year
cor(T2$Year, T2$BirdsPartyHour, method="spearman") #-0.6268908
cor.test(T2$Year, T2$BirdsPartyHour, method="spearman")
#S = 33880, p-value = 2.022e-06
#-0.6268908

m.BirdsPartyHour<-lm(BirdsPartyHour~Year, data=T2)
summary(m.BirdsPartyHour) #R^2=0.1962, F(1,48)=11.72, p-value = 0.001274
summary(T2$BirdsPartyHour)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#58.53  105.91  194.17  208.35  254.90  813.58 
sd(T2$BirdsPartyHour) #139.54

###################################################################################
#math determining percent decline from linear model
8670.473+1969*-4.241 #319.944
8670.473+2020*-4.241 #103.653
1-(103.653/319.944) #67.6% decline

#math determining percent decline from 10 year averages
100-(sum(T2$BirdsPartyHour[T2$Year >2010])/sum(T2$BirdsPartyHour[T2$Year <1979]))*100
#57.2% decline


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

sum(T3.2$NewSpecies[T3.2$Year > 2010]) #7
sum(T3.2$NewSpecies[T3.2$Year > 2000]) #13
sum(T3.2$NewSpecies[T3.2$Year > 1990]) #20
##################################################

