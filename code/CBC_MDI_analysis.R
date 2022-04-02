#CBC_MDI_analysis - Schoodic Institute at Acadia National Park
#First written by Nick Fisichelli 2021, adapted and updated by Kyle Lima 2022

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(dplyr)
library(tidyr)
library(utils)
library(ggplot2)

select <- dplyr::select



#------------------------------------------------#
####          Read in Required Data           ####
#------------------------------------------------#

#read in the file without the 'X' at the beginning of column names (check.names = F)
T1 <- read.csv('data/CBC_MDI_All.csv', header=TRUE, check.names = FALSE)
Y1 <- read.csv('data/Years_MDI.csv', header=TRUE) #begins with 1971
tnew <- read.csv('data/cbcmdi_rawdata_2021.csv', header=TRUE)



#------------------------------------------------#
####          Manipulation of Data            ####
#------------------------------------------------#

###Edit the 2021 data to get into the correct format for joining with the full dataset
##Add sci names to data
#Make columns, rename, and clean
tnew <- tnew %>% 
  rename(CommonName=Species, Count=Number, CountPartyHour=Number_Per_Party_Hrs) %>% 
  mutate(Year = 2021) %>% 
  select(CommonName, Year, everything()) %>% 
  select(-Editorial_Comments) %>% 
  filter(!row_number() %in% c(5, 6))

#Fix common names
tnew['CommonName'][tnew['CommonName'] == "Green-winged Teal (American)"] <- "Green-winged Teal"
tnew['CommonName'][tnew['CommonName'] == "Common Eider (Dresser's)"] <- "Common Eider"
tnew['CommonName'][tnew['CommonName'] == "Rock Pigeon (Feral Pigeon)"] <- "Rock Pigeon"
tnew['CommonName'][tnew['CommonName'] == "Dark-eyed Junco (Slate-colored)"] <- "Dark-eyed Junco"

#Create a list of common and sci names to join
scinames <- T1 %>% 
  select(CommonName, SciName2)
scinames <- unique(scinames)

#Join the sci names to our 2021 data and reorder columns to fit full dataset format
tnew <- left_join(tnew, scinames, by = "CommonName", keep = FALSE)
tnew <- tnew %>% 
  select(CommonName, SciName2, everything())



##Add in PartyHour column
tnew <- tnew %>% 
  mutate(PartyHours = Count/CountPartyHour) %>% 
  mutate(PartyHours = 71.9)



##Combine with full dataset
T1 <- rbind(T1, tnew) %>% 
  arrange(CommonName)



###Full dataset manipulation
#Just keep the 50 years with Bill Townsend
T1 <- T1 %>% 
  filter(Year >= 1971)

#Remove species with zero birds
T1 <- T1[!(T1$CommonName %in% c("American Three-toed Woodpecker",
                              "Blue Grosbeak", 
                              "Common Scoter",
                              "Little Gull",
                              "Northern Hawk Owl",
                              "Osprey",
                              "Red Phalarope",
                              "Red-shouldered Hawk")), ]


write.csv(T1, "outputs/mdi/cbcmdi_fulldata_20220402.csv", row.names=FALSE)



#------------------------------------------------#
####              Running stats               ####
#------------------------------------------------#

##Produce a summary table and export
#no. of species per year
#remove zeros
T1.1 <- T1 %>% 
  filter(T1$Count > 0)

#no. of species, no. of birds, birds sum by party hours,  
#party hours, observers
T2 <- T1.1 %>%
  group_by(Year) %>%
  summarise(NoSpecies=length(CommonName),
            Birds=sum(Count),
            BirdsPartyHour=sum(CountPartyHour),
            PartyHours=mean(PartyHours))


write.csv(T2, "outputs/mdi/cbcmdi_summarybyyear_20220402.csv", row.names=FALSE)



#Frequency of years species is present
F1<-T1.1 %>% 
  group_by(CommonName) %>% 
  summarise(Freq=length(Year),
            TotalBirds=sum(Count),
            TotBirdsPH=sum(CountPartyHour),
            MinYear=min(Year),
            MaxYear=max(Year))

write.csv(F1, "outputs/mdi/cbcmdi_freqtotals_20220402.csv", row.names=FALSE)




####Number of species by Year

#statistical tests for number of bird species
cor(T2$Year, T2$NoSpecies, method="spearman") #-0.66   #WITH 2021 DATA = -0.6345473
cor.test(T2$Year, T2$NoSpecies, method="spearman")
#ORIGINAL
#S = 32460, p-value = 3.092e-07
#rho = -0.6561013

#WITH 2021 DATA
#S = 34039, p-value = 7.482e-07
#rho = -0.6345473 


m.NoSpecies<-lm(NoSpecies~Year, data=T2)
summary(m.NoSpecies) #R^2: 0.43, F(1,47): 35.72, p-value: <0.0001     #WITH 2021 DATA = R^2: 0.3942, F(1,48): 32.89, p-value: <0.0001
summary(T2$NoSpecies)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    #THIS IS THE SAME WITH 2021 DATA
#50      56      62      62      67      82 
sd(T2$NoSpecies) #7.144   #WITH 2021 DATA = 7.071





####Number of birds by Year

#statistical tests for bird per year
cor(T2$Year, T2$Birds, method="spearman") #-0.70153  # WITH 2021 DATA = -0.7139976
cor.test(T2$Year, T2$Birds, method="spearman")
#ORIGINAL
#S = 33350, p-value = 8.523e-08
#rho = -0.7015306

#WITH 2021 DATA
#S = 35694, p-value = 3.074e-08
#rho = -0.7139976 


m.Birds<-lm(Birds~Year, data=T2)
summary(m.Birds) #R^2=0.46, F(1,47)=40.4, p-value: <0.0001    #WITH 2021 DATA = R^2=0.4613, F(1,48)=42.96, p-value: <0.0001
summary(T2$Birds) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2122    7186    8591    9399   12002   16995 

#WITH 2021 DATA
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2122    7184    8506    9333   11965   16995 

sd(T2$Birds) #3275.232    #WITH 2021 DATA = 3274.39



#math determining percent decline from linear model
60.1*52
311529.33+1971*-151.41 #13100.22
311529.33+2020*-151.41 #5681.13
1-(5681.13/13100.22) #= 56.63332% decline

#math determining percent decline from 10 year averages
100-(sum(T2$Birds[T2$Year >2010])/sum(T2$Birds[T2$Year <1981]))*100
#54.4% decline
#WITH 2021 DATA = 49.53%





####Number of birds per party hour by Year

#statistical tests for bird per year
cor(T2$Year, T2$BirdsPartyHour, method="spearman") #-0.06040816    #WITH 2021 DATA = -0.1112605
cor.test(T2$Year, T2$BirdsPartyHour, method="spearman")
#S = 20784, p-value = 0.6793
#rho = -0.06040816

#WITH 2021 Data
#S = 23142, p-value = 0.4406
#rho = -0.1112605 

m.BirdsPartyHour<-lm(BirdsPartyHour~Year, data=T2)
summary(m.BirdsPartyHour) #R^2=0, F(1,47)=0.0001, p-value = 0.99   #WITH 2021 DATA = R^2= -0.019, F(1,48)=0.0704, p-value: 0.7918
summary(T2$BirdsPartyHour)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#66.77  116.38  152.50  163.97  197.50  346.87 

#WITH 2021 DATA
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#66.77  110.82  148.04  162.40  193.14  346.87 

sd(T2$BirdsPartyHour) #67.00175   #WITH 2021 DATA = 67.23997

#math determining percent decline from linear model
#9985.572+1969*-4.897 #
#9985.572+2020*-4.897 #
#1-(93.632/343.379) #%

#math determining percent decline from 10 year averages
100-(sum(T2$BirdsPartyHour[T2$Year >2010])/sum(T2$BirdsPartyHour[T2$Year <1979]))*100
#3.29% decline
#WITH 2021 DATA = -3.92





####Number of new species added each year

#THIS IS ALL THE SAME WITH 2021 DATA

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


