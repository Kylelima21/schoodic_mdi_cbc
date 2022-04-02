#CBC_SCH_analysis - Schoodic Institute at Acadia National Park
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
T1 <- read.csv('data/CBC_Sch_52Years.csv', header=TRUE, check.names = FALSE)
Y1 <- read.csv('data/Years_Sch.csv', header=TRUE) #begins with 1971
tnew <- read.csv('data/cbcsch_rawdata_2021.csv', header=TRUE)



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
tnew['CommonName'][tnew['CommonName'] == "Rock Pigeon (Feral Pigeon)"] <- "Rock Pigeon"
tnew['CommonName'][tnew['CommonName'] == "Northern Flicker (Yellow-shafted)"] <- "Northern Flicker"

#Create a list of common and sci names to join
scinames <- T1 %>% 
  select(CommonName, SciName)
scinames <- unique(scinames)

#Join the sci names to our 2021 data and reorder columns to fit full dataset format
tnew <- left_join(tnew, scinames, by = "CommonName", keep = FALSE)
tnew <- tnew %>% 
  select(CommonName, SciName, everything())



##Add in PartyHour column
tnew <- tnew %>% 
  mutate(PartyHours = Count/CountPartyHour) %>% 
  mutate(PartyHours = 40.50)



##Combine with full dataset
T1 <- rbind(T1, tnew) %>% 
  arrange(CommonName)



###Full dataset manipulation
#Just keep the 50 years with Bill Townsend
T1 <- T1 %>% 
  filter(Year >= 1971)


write.csv(T1, "outputs/sch/cbcsch_fulldata_20220402.csv", row.names=FALSE)



#------------------------------------------------#
####              Running stats               ####
#------------------------------------------------#

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


write.csv(T2, "outputs/sch/cbcsch_summarybyyear_20220402.csv", row.names=FALSE)



#Frequency of years species is present
F1 <- T1.1 %>% 
  group_by(CommonName) %>% 
  summarise(Freq=length(Year),
            TotalBirds=sum(Count),
            TotBirdsPH=sum(CountPartyHour),
            MinYear=min(Year),
            MaxYear=max(Year))

write.csv(F1, "outputs/sch/cbcsch_freqtotals_20220402.csv", row.names=FALSE)




####Number of species by Year

#statistical tests for number of bird species
cor(T2$Year, T2$NoSpecies, method="spearman") #0.177      #WITH 2021 DATA = 0.1885102
cor.test(T2$Year, T2$NoSpecies, method="spearman")
#S = 17158, p-value = 0.2212
#rho = 0.1760903

#WITH 2021 DATA = 
#S = 17934, p-value = 0.1852
#rho = 0.1885102 

m.NoSpecies<-lm(NoSpecies~Year, data=T2)
summary(m.NoSpecies) #R^2: 0.04, F(1,48): 2.115, p-value: 0.15      #WITH 2021 DATA = R^2: 0.0253, F(1,49): 2.298, p-value: 0.136  
summary(T2$NoSpecies)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#44.00   50.00   54.00   53.92   57.75   67.00 

#WITH 2021 DATA
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#44.00   50.00   54.00   53.96   57.50   67.00 


sd(T2$NoSpecies) #5.649    #WITH 2021 DATA = 5.59986




####Number of birds by Year

#statistical tests for bird per year
cor(T2$Year, T2$Birds, method="spearman") #-0.49   #WITH 2021 DATA = -0.5173756
cor.test(T2$Year, T2$Birds, method="spearman")
#S = 31046, p-value = 0.0003512
#rho = -0.4908043

#WITH 2021 DATA
#S = 33534, p-value = 0.0001267
#rho = -0.5173756 


m.Birds<-lm(Birds~Year, data=T2)
summary(m.Birds) #R^2=0.0848, F(1,48)=4.448, p-value: 0.04019       #WITH 2021 DATA = R^2=0.07732, F(1,49)=5.19, p-value: 0.02712 
summary(T2$Birds) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2210    3471    4916    5554    6657   21153  

#WITH 2021 DATA
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2210    3426    4911    5503    6634   21153 


sd(T2$Birds) #3097.031   #WITH 2021 DATA = 3088.236


#math determining percent decline from linear model
129012.39+1971*-61.87 #7066.62
129012.39+2020*-61.87 #4034.99
1-(4034.99/7066.62) #= 42.9% decline

#math determining percent decline from 10 year averages
100-(sum(T2$Birds[T2$Year >2010])/sum(T2$Birds[T2$Year <1981]))*100
#45.239% decline
#WITH 2021 DATA = 40.46418%





####Number of birds per party hour by Year

#statistical tests for birds per party hour per year
cor(T2$Year, T2$BirdsPartyHour, method="spearman") #-0.6268908     #WITH 2021 DATA = -0.6464253
cor.test(T2$Year, T2$BirdsPartyHour, method="spearman")
#S = 33880, p-value = 2.022e-06
#-0.6268908

#WITH 2021 DATA
#S = 36386, p-value = 6.459e-07
#rho = -0.6464253 


m.BirdsPartyHour<-lm(BirdsPartyHour~Year, data=T2)
summary(m.BirdsPartyHour) #R^2=0.1962, F(1,48)=11.72, p-value = 0.001274      #WITH 2021 DATA = R^2=0.1945, F(1,49)=13.07, p-value = 0.0007066 
summary(T2$BirdsPartyHour)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#58.53  105.91  194.17  208.35  254.90  813.58 

#WITH 2021 DATA 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#58.53  103.58  192.95  205.67  253.05  813.58 


sd(T2$BirdsPartyHour) #139.54     #WITH 2021 DATA =  139.4584


#math determining percent decline from linear model
8670.473+1969*-4.241 #319.944
8670.473+2020*-4.241 #103.653
1-(103.653/319.944) #67.6% decline

#math determining percent decline from 10 year averages
100-(sum(T2$BirdsPartyHour[T2$Year >2010])/sum(T2$BirdsPartyHour[T2$Year <1979]))*100
#57.2% decline
#WITH 2021 DATA = 54.38421%





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

sum(T3.2$NewSpecies[T3.2$Year > 2010]) #7
sum(T3.2$NewSpecies[T3.2$Year > 2000]) #13
sum(T3.2$NewSpecies[T3.2$Year > 1990]) #20


