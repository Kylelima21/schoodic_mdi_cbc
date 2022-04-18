#Regional CBC analysis for ANP and surrounding areas - Schoodic Institute at Acadia National Park
#By Nick Fisichelli 2021, and Kyle Lima 2022

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(ggplot2)
library(dplyr)
library(utils)
library(tidyr)
library(sp)
library(ggmap)
library(rgdal)
library(reshape2)
library(purrr)
library(sf)

select <- dplyr::select



#------------------------------------------------#
####          Read in Required Data           ####
#------------------------------------------------#

#read in the files
mdi <- read.csv("outputs/mdi/cbcmdi_fulldata_20220402.csv", header = TRUE)
sch <- read.csv("outputs/sch/cbcsch_fulldata_20220402.csv", header = TRUE)
Y1 <- data.frame(Year = 2021:1971)



#------------------------------------------------#
####          Combine mdi and sch             ####
#------------------------------------------------#

#Get separate data ready for join
mdi <- mdi %>% 
  mutate(circle = "mdi") %>% 
  rename(SciName = SciName2)

sch <- sch %>% 
  mutate(circle = "schoodic")

#Combine for regional analysis
cbc <- bind_rows(mdi, sch)



##Do calculations
#Get mean count and party hour
metr <- cbc %>%
  group_by(CommonName, Year) %>% 
  summarise(Count=mean(Count), PartyH=mean(PartyHours)) 

#Party hour is not correct for species only detected in one circle that year, fix
yh <- metr %>% 
  group_by(Year) %>% 
  summarise(PartyHours = median(PartyH))

#Combine new party hours that are right and remove incorrect column
newjoin <- left_join(metr, yh) %>% 
  select(-PartyH)

#Calculate countpartyhour statistic from these updated mean values
T2.0 <- newjoin %>% 
  mutate(CountPartyHour = Count/PartyHours)
 

 
##Fill in missing years with zero
#Create a list of all the species to run through the purrr loop
sp.list <- unique(cbc$CommonName)

#Summarize for totals by year to merge for each species in the loop
all.bird.data <- T2.0 %>% 
  group_by(Year) %>% 
  summarise(PartyHours = median(PartyHours))

#Function to add in years where species were not seen
add.zero <- function(spname) {
  
  sp <- T2.0 %>% 
    filter(CommonName == paste(spname))
  
  withzed <- full_join(sp, all.bird.data, by = "Year") %>% 
    select(-PartyHours.x) %>% 
    rename(PartyHours=PartyHours.y)
  
  withzed$CommonName <- paste(spname)
  
  output <- withzed %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  
  return(output)
  
}


#Run this function through purrr loop to add data for each year for each species
#162 species over 51 year --- 162*51 = 8262 rows
fulldat <- map(sp.list, ~add.zero(.))

#turn into usable r object
sp.full <- as.data.frame(do.call(rbind, fulldat))

#Write out for use in rmarkdown report
#write.csv(sp.full, "outputs/cbc_alldata_20220415.csv", row.names = F)


##Create summary dfs to use in analyses
#Remove zeros to use later
T1.1 <- T2.0 %>%
  filter(Count > 0)

#Year summaries: # of species, total count, count/partyhour, party hours
T2 <- T2.0 %>% 
  group_by(Year) %>%
  summarise(NoSpecies=length(which(Count>0)),
            Birds=sum(Count),
            BirdsPartyHour=sum(CountPartyHour),
            PartyHours=mean(PartyHours))

#Species summaries: # of years species was present, total count, count/partyhour, year of min and max count
F1 <- T1.1 %>% 
  group_by(CommonName) %>% 
  summarise(Freq=length(Year),
            TotalBirds=sum(Count),
            TotBirdsPH=sum(CountPartyHour),
            MinYear=min(Year),
            MaxYear=max(Year))



#------------------------------------------------#
####           Summary Statistics             ####
#------------------------------------------------#

#Mean species across all years
mean.num <- T2 %>% 
  summarise(mean = mean(NoSpecies), sd = sd(NoSpecies))


#Mean count across all years
mean.count <- T2 %>% 
  summarise(mean = mean(Birds), sd = sd(Birds))


#Mean count/party hour across all years
mean.cph <- T2 %>% 
  summarise(mean = mean(BirdsPartyHour), sd = sd(BirdsPartyHour))


#Number of spp in the last decade
yep <- T1.1 %>%
  filter(Year > 2011)
check <- as.data.frame(unique(yep$CommonName)) #120



#------------------------------------------------#
####          Statistical Analyses            ####
#------------------------------------------------#

####Number of species by Year

#statistical tests for number of bird species
cor(T2$Year, T2$NoSpecies, method="spearman")
cor.test(T2$Year, T2$NoSpecies, method="spearman")
#S = 31892, p-value = 0.001129**
#rho = -0.44309

m.NoSpecies <- lm(NoSpecies~Year, data=T2)
summary(m.NoSpecies) #Adjusted R-squared:  0.1543, F-statistic: 10.12 on 1 and 49 DF,  p-value: 0.002546**
summary(T2$NoSpecies)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#52.00   68.00   70.00   70.94   75.00   84.00 

sd(T2$NoSpecies) #6.080828

T2 %>% 
  ggplot(aes(Year, NoSpecies)) +
  geom_point(shape = 21, size = 1.9, color = "black") +
  geom_smooth(method = "lm", color = "navyblue") +
  theme_bw() +
  labs(title="Number of Species by Year", y="Number of species", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0), limits = c(50,90)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))


#------------------------------------------------#


####Number of birds by Year

#Spearman rank correlation test
cor(T2$Year, T2$Birds, method="spearman")
test <- cor.test(T2$Year, T2$Birds, method="spearman")
#S = 38636, p-value < 2.2e-16***
#rho = -0.7482353 


#Linear model
m.Birds <- lm(Birds~Year, data=T2)
summary(m.Birds) #Adjusted R-squared:  0.3565, F-statistic:  28.7 on 1 and 49 DF,  p-value: 2.25e-06***
summary(T2$Birds) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2756    5672    7422    7452    8775   16730 

sd(T2$Birds) #2596.276


##Test for difference between first and last decades
#Math determining percent decline from 10 year averages
100-(sum(T2$Birds[T2$Year>2011])/sum(T2$Birds[T2$Year<1981]))*100
#53.03% decline

#Run test of normality
birddif <- with(T2, Birds[Year<1981] - Birds[Year>2011])
shapiro.test(birddif) #very much not normal

#Create dataframe of the first and last decades and assign group
count.t <- T2 %>% 
  filter(Year>2011 | Year<1981) %>% 
  mutate(group = ifelse(Year>2011, "last", "first"))

#Compute paired t-test
t.test(Birds ~ group, data = count.t, paired = TRUE)


T2 %>% 
  ggplot(aes(Year, Birds)) +
  geom_point(shape = 21, size = 1.9, color = "black") +
  geom_smooth(method = "lm", color = "navyblue", na.rm = TRUE) +
  theme_bw() +
  labs(title="Number of Birds by Year", y="Number of birds", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0), limits = c(5000,35000)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))


#------------------------------------------------#


####Number of birds per party hour by Year

cor(T2$Year, T2$BirdsPartyHour, method="spearman")
cor.test(T2$Year, T2$BirdsPartyHour, method="spearman")
#S = 30334, p-value = 0.007416
#rho = -0.3725792 

m.BirdsPartyHour <- lm(BirdsPartyHour~Year, data=T2)
summary(m.BirdsPartyHour) #Adjusted R-squared:  0.02728, F-statistic: 2.402 on 1 and 49 DF,  p-value: 0.1276

summary(T2$BirdsPartyHour)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#61.99  122.69  148.70  171.54  194.75  514.75 

sd(T2$BirdsPartyHour) #82.00265



#Test for difference between the two decades
#Math determining percent decline from 10 year averages
100-(sum(T2$BirdsPartyHour[T2$Year>2011])/sum(T2$BirdsPartyHour[T2$Year<1981]))*100
#43.01% decline

#Run test of normality
phdif <- with(T2, BirdsPartyHour[Year<1981] - BirdsPartyHour[Year>2011])
shapiro.test(phdif) #very much not normal

#Compute paired t-test
t.test(BirdsPartyHour ~ group, data = count.t, paired = TRUE)


T2 %>% 
  ggplot(aes(Year, BirdsPartyHour)) +
  geom_point(shape = 21, size = 1.9, color = "black") +
  geom_smooth(method = "lm", color = "navyblue", na.rm = TRUE) +
  theme_bw() +
  labs(title="Number of Birds by Year", y="Number of birds", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0), limits = c(50,550)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black"),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))


#------------------------------------------------#


####Number of new species added each year

#total no of species observed
#min year per species
T3 <- T1.1 %>%
  group_by(CommonName) %>%
  summarise(Year=min(Year))

#max year per species
T3.max <- T1.1 %>%
  group_by(CommonName) %>%
  summarise(Year=max(Year))

head(T3)

T3.1 <- T3 %>% 
  group_by(Year) %>%
  summarise(NewSpecies=length(Year))
head(T3.1)

T3.2 <- merge(T3.1, Y1, by="Year",
            all.y=TRUE) #include all rows from second data frame
head(T3.2)
T3.2[is.na(T3.2)] <- 0
str(T3.2)
T3.2$cumsum <- cumsum(T3.2$NewSpecies)

sum(T3.2$NewSpecies[T3.2$Year >= 2011 & T3.2$Year <= 2020]) #10
sum(T3.2$NewSpecies[T3.2$Year >= 2001 & T3.2$Year <= 2010]) #6
sum(T3.2$NewSpecies[T3.2$Year >= 1991 & T3.2$Year <= 2000]) #6
#sum(T3.2$NewSpecies[T3.2$Year > 1980 & T3.2$Year < 1991]) #15
#sum(T3.2$NewSpecies[T3.2$Year > 1970 & T3.2$Year < 1981]) #124


#------------------------------------------------#


##Statistical tests for each species
fsub <- F1 %>% 
  filter(Freq > 2)

#Create input for purrr loop
species <- unique(fsub$CommonName) 

#Create function that runs spearman tests on every species
sprmn <- function(spname) {
  sp <- T2.0 %>% 
    filter(CommonName == paste(spname))
  
  c <- cor.test(sp$Year, sp$Count, method = "spearman")
  
  cph <- cor.test(sp$Year, sp$CountPartyHour, method = "spearman")
  
  newrow <- c(paste(spname), paste(c$estimate), paste(c$p.value), paste(cph$estimate), paste(cph$p.value))
  
  return(newrow)
  
}


#Run purrr loop
output <- map(species, ~sprmn(.))

#Convert to a data frame
sp.stats <- as.data.frame(do.call(rbind, output))

#Rename columns
colnames(sp.stats) <- c("species", "count.est", "count.p", "cph.est", "cph.p")

#Round the numbers to look simpler/cleaner
sp.stats$count.est <- round(as.numeric(sp.stats$count.est), digits = 3)
sp.stats$count.p <- round(as.numeric(sp.stats$count.p), digits = 3)
sp.stats$cph.est <- round(as.numeric(sp.stats$cph.est), digits = 3)
sp.stats$cph.p <- round(as.numeric(sp.stats$cph.p), digits = 3)

#Determine the significant relationships
sig <- sp.stats %>% 
  filter(cph.p < 0.05)

sig$change <- ifelse(sig$cph.est > 0, "increase", "decrease")
sig.ch <- sig %>% select(species, change)

sp.stats2 <- left_join(sp.stats, sig.ch, by = "species")

sp.stats2$change[is.na(sp.stats2$change)] <- "no change"
sp.stats2$change[is.na(sp.stats2$count.p)] <- "not enough data"


fmis <- F1 %>% 
  filter(Freq <= 2) %>% 
  mutate(count.est = NA,
         count.p = NA,
         cph.est = NA,
         cph.p = NA,
         change = "not enough data") %>% 
  select(CommonName, count.est, count.p, cph.est, cph.p, change) %>% 
  rename(species=CommonName)

sp.stats3 <- bind_rows(sp.stats2, fmis)

sp.stats3 <- sp.stats3 %>% arrange(change, species)

#write.csv(sp.stats3, "outputs/regional/speciesstats_table_20220415.csv", row.names = F)

