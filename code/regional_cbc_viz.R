#Regional CBC analysis visualizations for ANP and surrounding areas 
#Schoodic Institute at Acadia National Park
#By Nick Fisichelli 2021, and Kyle Lima 2022

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(ggplot2)
library(dplyr)
library(ggstatsplot)
library(utils)
library(tidyr)
library(sp)
library(ggmap)
library(rgdal)
library(cowplot)
library(sf)


#define function
select <- dplyr::select



#------------------------------------------------#
####          Read in Required Data           ####
#------------------------------------------------#

#read in the files
cbc <- read.csv("outputs/cbc_alldata_20220415.csv")
buff1 <- readOGR("outputs/mdi_circle.kml")
buff2 <- readOGR("outputs/sch_circle.kml")
sp.tab <- read.csv("outputs/regional/speciesstats_table_20220415.csv")
effort <- read.csv("data/schmdi_cbceffort_20220418.csv")
tax <- read.csv("data/eBird_Taxonomy_v2021.csv")



#------------------------------------------------#
####       Summarize Data for Plotting        ####
#------------------------------------------------#

##First fix incorrect bird names
sp.tab['species'][sp.tab['species'] == "Gray Jay"] <- 'Canada Jay'
sp.tab['species'][sp.tab['species'] == "Ring-necked duck"] <- 'Ring-necked Duck'
cbc['CommonName'][cbc['CommonName'] == "Gray Jay"] <- 'Canada Jay'
cbc['CommonName'][cbc['CommonName'] == "Ring-necked duck"] <- 'Ring-necked Duck'


#Create list of years
Y1 <- data.frame(Year = 2021:1971)

#Species by year
T2.0 <- cbc %>%
  group_by(CommonName, Year) %>% 
  summarise(Count=mean(Count), PartyHours=mean(PartyHours)) %>% 
  mutate(CountPartyHour = Count/PartyHours)

#Remove zeros to use later
T1.1 <- T2.0 %>%
  filter(Count > 0)

#Year summaries
T2 <- T2.0 %>% 
  group_by(Year) %>%
  summarise(NoSpecies=length(which(Count>0)),
            Birds=sum(Count),
            BirdsPartyHour=sum(CountPartyHour),
            PartyHours=mean(PartyHours))

#Species based summaries
F1 <- T2.0 %>% 
  group_by(CommonName) %>% 
  filter(Count > 0) %>% 
  summarise(Freq=length(Year),
            TotalBirds=sum(Count),
            TotBirdsPH=sum(CountPartyHour),
            MinYear=min(Year),
            MaxYear=max(Year))
F1$TotBirdsPH <- round(F1$TotBirdsPH, digits = 3)



#------------------------------------------------#
####             Study Area Map               ####
#------------------------------------------------#

#Create dataframe with circle centers
circpoint <- data.frame(circle  = c("Mount Desert Island", "Schoodic"),
                        latitude = c("44.34", "44.43"),
                        longitude = c("-68.31", "-68.11"))

#Fix non-numeric
circpoint$latitude <- as.numeric(circpoint$latitude)
circpoint$longitude <- as.numeric(circpoint$longitude)

#Fortify for ggplot
buff1f <- fortify(buff1)
buff2f <- fortify(buff2)

#Turn into spatial object to determine what values we need to remove to fix sch circle
sf_buff1 <- st_as_sf(buff1)
sf_buff1_polygons <- st_polygonize(sf_buff1)
shp_buff1 <- as(sf_buff1_polygons, "Spatial")
buff2f$longitude <- buff2f$"long"
buff2f$latitude <- buff2f$"lat"
coordinates(buff2f) <- c("long", "lat")
slot(buff2f, "proj4string") <- slot(shp_buff1, "proj4string")
output <- over(shp_buff1, buff2f, returnList = TRUE) 
output.df <- as.data.frame(output$`0`) 
#need to remove rows 14:29

#Refortify
buff2.0 <- fortify(buff2)

#Subset
buff2.0[c(14:29),] <- NA

##Map the data
#Get base map
base.map <- get_stamenmap(
  bbox = c(left = -68.61, bottom = 44.15, right = -67.83, top = 44.6),
  maptype = 'toner-lite',
  zoom = 11)

#Plot
png(filename = "outputs/regional/forpub/cbc_study_area.png",
    width=6, height=6, units="in", res = 600)

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
  scale_fill_manual("CBC circle", values = c("navyblue", "forestgreen")) +
  ggsn::scalebar(base.map, dist = 100, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')

dev.off()



#------------------------------------------------#
####        Summary Plots - Singles           ####
#------------------------------------------------#

##Number of Years Species Observed
png(filename = "outputs/regional/forpub/YearsSpeciesObserved.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

hist(F1$Freq, xlab="Years observed", ylab="No. of species",
     main="Frequency of Observation by Species")

dev.off()




##Species Per Year
png(filename = "outputs/regional/forpub/NoSpecies.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$NoSpecies,
     pch=16, cex=0.5,
     xlab="Year", ylab="Species",
     main="Species totals by year")
lines(T2$Year, T2$NoSpecies)

dev.off()




##Birds Per Year
png(filename = "outputs/regional/forpub/NoBirds.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$Birds,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds",
     main="Total count of birds by year")
lines(T2$Year, T2$Birds)

dev.off()




##Birds Per Party Hour
png(filename = "outputs/regional/forpub/NoBirdsPartyHour.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$BirdsPartyHour,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds/PartyHour",
     main="Total count of birds per party hour")
lines(T2$Year, T2$BirdsPartyHour)

dev.off()




##Party Hour
png(filename = "outputs/regional/forpub/PartyHours.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$PartyHours,
     pch=16, cex=0.5,
     xlab="Year", ylab="Party hours",
     main="Average number of party hours per year")
lines(T2$Year, T2$PartyHours)

dev.off()




##Observers per year
png(filename = "outputs/regional/forpub/Participants.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(effort$year, effort$mean.participants,
     pch=16, cex=0.5,
     xlab="Year", ylab="No. of participants",
     main="Average number of participants per year")
lines(effort$year, effort$mean.participants)

dev.off()




##Total number of species observed
#min year per species
T3 <- T1.1 %>%
  group_by(CommonName) %>%
  summarise(Year=min(Year))

#Summarize by year
T3.1 <- T3 %>% 
  group_by(Year) %>%
  summarise(NewSpecies=length(Year))

#Merge the two
T3.2 <- merge(T3.1, Y1, by="Year", all.y=TRUE)

#Fill 0s
T3.2[is.na(T3.2)] <- 0
T3.2$cumsum <- cumsum(T3.2$NewSpecies)

#Plot
png(filename = "outputs/regional/forpub/CumulativeBirds.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T3.2$Year, T3.2$cumsum,
     pch=16, cex=0.5,
     xlab="Year", ylab="Cumulative species",
     main="Cumulative bird species")
lines(T3.2$Year, T3.2$cumsum)

dev.off()



#------------------------------------------------#
####         Summary Plots - Quads            ####
#------------------------------------------------#


##Summary figure 1
png(filename = "outputs/regional/forpub/summary1.png",
    width=11, height=6, units="in", res = 500)

par(mfrow=c(2,2), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(effort$year, effort$mean.participants,
     pch=16, cex=0.5,
     xlab="Year", ylab="No. of participants",
     main="Average number of participants per year")
lines(effort$year, effort$mean.participants)

plot(T2$Year, T2$PartyHours,
     pch=16, cex=0.5,
     xlab="Year", ylab="Party hours",
     main="Average number of party hours per year")
lines(T2$Year, T2$PartyHours)

plot(T2$Year, T2$Birds,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds",
     main="Total count of birds by year")
lines(T2$Year, T2$Birds)

plot(T2$Year, T2$BirdsPartyHour,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds/party hour",
     main="Total count of birds per party hour")
lines(T2$Year, T2$BirdsPartyHour)

dev.off()




#Summary figure 2
png(filename = "outputs/regional/forpub/Summary2.png",
    width=11, height=6.0, units="in", res = 500)

par(mfrow=c(2,2), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$NoSpecies,
     pch=16, cex=0.5,
     xlab="Year", ylab="Species",
     main="Species totals by year")
lines(T2$Year, T2$NoSpecies)

plot(T3.2$Year, T3.2$cumsum,
     pch=16, cex=0.5,
     xlab="Year", ylab="Cumulative species",
     main="Cumulative bird species")
lines(T3.2$Year, T3.2$cumsum)

hist(F1$Freq, xlab="Years observed", ylab="No. of species",
     main="Frequency of Observation by Species")

dev.off()



#------------------------------------------------#
####             SRC Test Plots               ####
#------------------------------------------------#

rm.out <- T2 %>% 
  filter(Year!=2001)

##Number of species by Year
png(filename = "outputs/regional/forpub/spbyyear.png",
    width=8.5, height=6, units="in", res = 600)

ggscatterstats(data = rm.out, x = Year, y = NoSpecies, 
               type = "spearman",
               title = "Number of species by year",
               ylab = "No. of species",
               ggtheme = theme_classic())

dev.off()



##Number of birds by Year
png(filename = "outputs/regional/forpub/birdsbyyear.png",
    width=8.5, height=6, units="in", res = 600)

ggscatterstats(data = T2, x = Year, y = Birds, 
               type = "spearman",
               title = "Total count of birds by year",
               ylab = "Total count of birds",
               ggtheme = theme_classic())

dev.off()



##Number of birds/partyhour by Year
png(filename = "outputs/regional/forpub/birdspartyhour_byyear.png",
    width=8.5, height=6, units="in", res = 600)

ggscatterstats(data = T2, x = Year, y = BirdsPartyHour, 
               type = "spearman",
               title = "Total count of birds/party hour by year",
               ylab = "Total count/party hour",
               ggtheme = theme_classic())

dev.off()




#------------------------------------------------#
####          Species-specific Plots          ####
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




##Example plot for pub?
#American Tree Sparrow
ATSP <- T1 %>% 
  filter(CommonName=="American Tree Sparrow") %>% 
  mutate(t.count = Count/60)

#Plot
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



#------------------------------------------------#
####                  Tables                  ####
#------------------------------------------------#


##Table of the species seen all 51 years
#Get only those species seen all 51 years
sub.frq <- F1 %>% 
  arrange(desc(Freq)) %>% 
  filter(Freq == 51) %>% 
  select(CommonName, Freq) %>% 
  rename(species=CommonName)

#Pull the species trend data from the table
sp.tr <- sp.tab %>% 
  select(species, change)

#Join this so that we have the trend for the species seen all 51 years
frgq <- left_join(sub.frq, sp.tr, by = "species")

#Format
frgq <- frgq %>% 
  arrange(change, species) %>% 
  rename('Common name' = species, Frequency = Freq, 'Species trend' = change)

#Write
#write.csv(frgq, "outputs/regional/forpub/allyears_speciestable.csv")



##Table of the most abundant species (count/party hour)
#Gather the top 28 (same as the number of spp seen all 51 years)
sub.abund <- F1 %>% 
  arrange(desc(TotBirdsPH)) %>% 
  filter(TotBirdsPH>33) %>% 
  select(CommonName, TotBirdsPH) %>% 
  rename(species=CommonName)

#Join species trend data so that we have the trend for the species seen all 51 years
frabun <- left_join(sub.abund, sp.tr, by = "species")

#Format
frabun <- frabun %>% 
  rename('Common name' = species, 'Total count/party hour' = TotBirdsPH, 'Species trend' = change)

#Write
#write.csv(frabun, "outputs/regional/forpub/species_abundance_table.csv")



##Table of frequency, totals, min/max years for all species
#Sorted by frequency
tot.freq <- F1 %>% 
  arrange(desc(Freq)) %>% 
  rename('Common name'=CommonName, Frequency=Freq, 'Total count'=TotalBirds,
         'Total count/party hour'=TotBirdsPH, 'Min year'=MinYear, 'Max year'=MaxYear)

#Sorted by abundance
tot.abund <- F1 %>% 
  arrange(desc(TotalBirds)) %>% 
  rename('Common name'=CommonName, Frequency=Freq, 'Total count'=TotalBirds,
         'Total count/party hour'=TotBirdsPH, 'Min year'=MinYear, 'Max year'=MaxYear)

#Write
#write.csv(tot.freq, "outputs/regional/forpub/allspecies_freqtable.csv")
#write.csv(tot.abund, "outputs/regional/forpub/allspecies_abundancetable.csv")



##Table of spearman test results
#Edits
sp.table <- sp.tab %>% 
  rename('Common name'=species, 'Count estimate'=count.est, 'Count p-value'=count.p,
         'Count/party hour estimate'=cph.est, 'Count/party hour p-value'=cph.p, 
         'Species trend'=change)
  
#Write
#write.csv(sp.table, "outputs/regional/forpub/allspecies_trendtable.csv")



##Table of species in study with taxonomy
#rename columns for merging
tax2 <- tax %>% 
  select(-c(TAXON_ORDER,CATEGORY,SPECIES_CODE,SPECIES_GROUP,REPORT_AS)) %>% 
  rename('Common name'=PRIMARY_COM_NAME, 'Scientific name'=SCI_NAME, Order=ORDER1,
         Family=FAMILY)

#Get list of species
sp <- sp.table %>% 
  select('Common name')

#Join for output
tax.table <- left_join(sp, tax2, by = 'Common name')

#Write
#write.csv(tax.table, "outputs/regional/forpub/allspecies_taxonomytable.csv")


