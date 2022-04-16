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
Y1 <- read.csv('data/Years_MDI.csv', header=TRUE) #this starts with 1971



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
#Get mean count and party h
metr <- cbc %>%
  group_by(CommonName, Year) %>% 
  summarise(Count=mean(Count), PartyH=mean(PartyHours)) 

#Party h is not correct for species only detected in one circle that year, fix
yh <- metr %>% 
  group_by(Year) %>% 
  summarise(PartyHours = median(PartyH))

#Combine new party hours that are right
newjoin <- left_join(metr, yh) %>% 
  select(-PartyH)

#Calculate countpartyhour
T2.0 <- newjoin %>% 
  mutate(CountPartyHour = Count/PartyHours)
  
#Fill in missing years with zero
sp.list <- unique(cbc$CommonName)


all.bird.data <- T2.0 %>% 
  group_by(Year) %>% 
  summarise(PartyHours = median(PartyHours))


add.zero <- function(spname) {
  
  sp <- T2.0 %>% 
    filter(CommonName == paste(spname))
  
  #sp$Year <- as.character(sp$Year)
  
  withzed <- full_join(sp, all.bird.data, by = "Year") %>% 
    select(-PartyHours.x) %>% 
    rename(PartyHours=PartyHours.y)
  
  withzed$CommonName <- paste(spname)
  
  output <- withzed %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  
  return(output)
  
}


fulldat <- map(sp.list, ~add.zero(.))

sp.full <- as.data.frame(do.call(rbind, fulldat))

#write.csv(sp.full, "outputs/cbc_alldata_20220415.csv", row.names = F)

#no. of species, no. of birds, birds sum by party hours,  
#party hours, observers
#remove zeros
T1.1 <- T2.0 %>%
  filter(Count > 0)

#For species number based calcs
T2 <- T2.0 %>% 
  group_by(Year) %>%
  summarise(NoSpecies=length(which(Count>0)),
            Birds=sum(Count),
            BirdsPartyHour=sum(CountPartyHour),
            PartyHours=mean(PartyHours))

#Frequency of years species is present
#remove zeros
F1.0 <- T2.0 %>%
  filter(Count > 0)

F1 <- F1.0 %>% 
  group_by(CommonName) %>% 
  summarise(Freq=length(Year),
            TotalBirds=sum(Count),
            TotBirdsPH=sum(CountPartyHour),
            MinYear=min(Year),
            MaxYear=max(Year))



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

cor(T2$Year, T2$Birds, method="spearman")
cor.test(T2$Year, T2$Birds, method="spearman")
#S = 38636, p-value < 2.2e-16***
#rho = -0.7482353 

m.Birds <- lm(Birds~Year, data=T2)
summary(m.Birds) #Adjusted R-squared:  0.3565, F-statistic:  28.7 on 1 and 49 DF,  p-value: 2.25e-06***
summary(T2$Birds) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2756    5672    7422    7452    8775   16730 

sd(T2$Birds) #2596.276

# #math determining percent decline from linear model
# 129012.39+1971*-61.87 #7066.62
# 129012.39+2020*-61.87 #4034.99
# 1-(4034.99/7066.62) #= 42.9% decline

#math determining percent decline from 10 year averages
100-(sum(T2$Birds[T2$Year>2011])/sum(T2$Birds[T2$Year<1981]))*100
#53.03% decline


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

# #math determining percent decline from linear model
# 8670.473+1969*-4.241 #319.944
# 8670.473+2020*-4.241 #103.653
# 1-(103.653/319.944) #67.6% decline

#math determining percent decline from 10 year averages
100-(sum(T2$BirdsPartyHour[T2$Year>2011])/sum(T2$BirdsPartyHour[T2$Year<1981]))*100
#43.01% decline

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


#statistical tests for each species

# sp.stat <- data.frame(species  = NA,
#                       count.est = NA,
#                       count.p = NA,
#                       cph.est = NA,
#                       cph.p = NA)

fsub <- F1 %>% 
  filter(Freq > 2)

species <- unique(fsub$CommonName) 
#species <- unique(T2.0$CommonName) 

sprmn <- function(spname) {
  sp <- T2.0 %>% 
    filter(CommonName == paste(spname))
  
  c <- cor.test(sp$Year, sp$Count, method = "spearman")
  
  cph <- cor.test(sp$Year, sp$CountPartyHour, method = "spearman")
  
  newrow <- c(paste(spname), paste(c$estimate), paste(c$p.value), paste(cph$estimate), paste(cph$p.value))
  
  return(newrow)
  
}


output <- map(species, ~sprmn(.))

sp.stats <- as.data.frame(do.call(rbind, output))

colnames(sp.stats) <- c("species", "count.est", "count.p", "cph.est", "cph.p")


sp.stats$count.est <- round(as.numeric(sp.stats$count.est), digits = 3)
sp.stats$count.p <- round(as.numeric(sp.stats$count.p), digits = 3)
sp.stats$cph.est <- round(as.numeric(sp.stats$cph.est), digits = 3)
sp.stats$cph.p <- round(as.numeric(sp.stats$cph.p), digits = 3)


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




#------------------------------------------------#
####               Create Viz                 ####
#------------------------------------------------#

##Map
#Create dataframe with circle centers
circpoint <- data.frame(circle  = c("Mount Desert Island center", "Schoodic center"),
                        latitude = c("44.34", "44.43"),
                        longitude = c("-68.31", "-68.11"))

#Fix non-numeric
circpoint$latitude <- as.numeric(circpoint$latitude)
circpoint$longitude <- as.numeric(circpoint$longitude)

#Read in CBC Circles
buff1 <- readOGR("outputs/mdi_circle.kml")
buff2 <- readOGR("outputs/sch_circle.kml")

#Fortify for ggplot
buff1f <- fortify(buff1)
buff2f <- fortify(buff2)

#This section removes the sch data that overlapped with the MDI circle
sf_buff1 <- st_as_sf(buff1)
sf_buff1_polygons <- st_polygonize(sf_buff1)
shp_buff1 <- as(sf_buff1_polygons, "Spatial")

buff2f$longitude <- buff2f$"long"
buff2f$latitude <- buff2f$"lat"

coordinates(buff2f) <- c("long", "lat")

slot(buff2f, "proj4string") <- slot(shp_buff1, "proj4string")

output <- over(shp_buff1, buff2f, returnList = TRUE) 

output.df <- as.data.frame(output$`0`) #need to remove rows 14:29

buff2.0 <- fortify(buff2)

buff2.0[c(14:29),] <- NA


#Get base map
base.map <- get_stamenmap(
  bbox = c(left = -68.61, bottom = 44.15, right = -67.83, top = 44.6),
  maptype = 'toner-lite',
  zoom = 11)

#Plot
ggmap(base.map) +
  geom_point(data = circpoint, aes(longitude, latitude, fill = circle), 
             shape = 21,
             size = 2.5,
             color = 'black') +
  geom_path(data = buff1f, aes(x=long, y=lat), color = "navyblue") +
  geom_path(data = buff2.0, aes(x=long, y=lat), color = "forestgreen") +
  theme_classic(base_size = 14) +
  ggtitle("Christmas Bird Count Circles") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.79, 0.13),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.border = element_rect(color = 'black', size = 1.5, fill = NA)) +
  scale_fill_manual("CBC circle", values = c("navyblue", "forestgreen"))


#------------------------------------------------#


##Number of Years Species Observed
png(filename = "outputs/regional/aa_YearsSpeciesObserved.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

hist(F1$Freq, xlab="Years observed", ylab="No. of Species",
     main="Number of Years Species Observed")

dev.off()

#------------------------------------------------#

##Bird Species Per Year
png(filename = "outputs/regional/aa_NoSpecies.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$NoSpecies,
     pch=16, cex=0.5,
     xlab="Year", ylab="Species",
     main="Total species per year")
lines(T2$Year, T2$NoSpecies)

dev.off()

#------------------------------------------------#

##Birds Per Year
png(filename = "outputs/regional/aa_NoBirds.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$Birds,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds",
     main="Birds per year")
lines(T2$Year, T2$Birds)

dev.off()

#------------------------------------------------#

##Birds Per Party Hour
png(filename = "outputs/regional/aa_NoBirdsPartyHour.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$BirdsPartyHour,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds/PartyHour",
     main="Total birds per party hour")
lines(T2$Year, T2$BirdsPartyHour)

dev.off()

#------------------------------------------------#

##Party Hour
png(filename = "outputs/regional/aa_PartyHours.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$PartyHours,
     pch=16, cex=0.5,
     xlab="Year", ylab="PartyHours",
     main="Party Hours per Year")
lines(T2$Year, T2$PartyHours)

dev.off()




#------------------------------------------------#
#------------------------------------------------#



##Observers per year and party hours

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

plot(T3.1$Year, T3.1$NewSpecies)

#need to add in all of the zeros and then need to add in new
#species from each year

T3.2 <- merge(T3.1, Y1, by="Year",
            all.y=TRUE) #include all rows from second data frame
head(T3.2)
T3.2[is.na(T3.2)] <- 0
T3.2$cumsum <- cumsum(T3.2$NewSpecies)

#Plot
png(filename = "outputs/regional/aa_CumulativeBirds.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T3.2$Year, T3.2$cumsum,
     pch=16, cex=0.5,
     xlab="Year", ylab="Cumulative species",
     main="Cumulative Bird Species")
lines(T3.2$Year, T3.2$cumsum)

dev.off()



#------------------------------------------------#
#------------------------------------------------#



##Plot of Count and Count Party Hour
#FOR EVERY INDIVIDUAL SPECIES

T1 <- T2.0

#create plot for each species
up <- as.vector(unique(T1$CommonName))


#begin loop
for (i in 1:length(up)) {
  T1.1<-subset(T1,CommonName==up[i])
  
  png(filename =paste("outputs/regional/spfigs/Figure_",up[i],".png",sep=""),
      width=5.0, height=3.0, units="in", res = 150)
  
  par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
  
  plot(T1.1$Year, T1.1$CountPartyHour,
       pch=16, cex=0.5,
       xlab="Year", ylab="Birds/Party Hour",
       main=T1.1$CommonName[T1.1$Year==1971])
  lines(T1.1$Year, T1.1$CountPartyHour, lwd=1.2)
  
  par(new = T)
  plot(T1.1$Year, T1.1$Count,
       axes=F, xlab=NA, ylab=NA, 
       pch=16, cex=0.2, col="red")
  lines(T1.1$Year, T1.1$Count, col="red", lwd=0.8)
  axis(side = 4, col="red", col.axis="red", col.lab="red")
  mtext(side = 4, line = 1.5, 'Number of birds', col="red")
  
  
  dev.off()
  
  
  rm(T1.1) # remove the T1 table after each loop to ensure no carry over
} # close i
#end of loop


ATSP <- T1 %>% 
  filter(CommonName=="American Tree Sparrow") %>% 
  mutate(t.count = Count/60)



ggplot(ATSP, aes(x=Year)) +
  geom_line(aes(y=CountPartyHour, color="Count/party hour"), size = 1.3) +
  geom_line(aes(y=t.count, color="Count"), size = 1.3) +
  scale_y_continuous(name="Count/party hour", sec.axis = sec_axis(~.*60, name="Count")) +
  theme_bw() +
  labs(title="American Tree Sparrow Population Changes Through Time", x="Year") +
  theme(plot.title = element_text(hjust = 0.5, size = "20"),
        axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(size = "16"),
        axis.line.y.right = element_line(color = "#2166AC"), 
        axis.ticks.y.right = element_line(color = "#2166AC"),
        axis.text.y.right = element_text(color = "#2166AC"), 
        axis.title.y.right = element_text(color = "#2166AC"),
        axis.line.y.left = element_line(color = "#D6604D"), 
        axis.ticks.y.left = element_line(color = "#D6604D"),
        axis.text.y.left = element_text(color = "#D6604D"), 
        axis.title.y.left = element_text(color = "#D6604D"),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "13"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(color = 'black', fill = NA),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16)) +
  scale_color_manual(name="Statistic",
                     breaks=c("Count/party hour", "Count"),
                     values=c("Count/party hour"="#D6604D", "Count"="#2166AC"))
