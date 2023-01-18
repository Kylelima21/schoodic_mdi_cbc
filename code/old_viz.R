#Regional CBC analysis visualizations for ANP and surrounding areas 
#Schoodic Institute at Acadia National Park 2021, 2022

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(ggplot2)
library(ggstatsplot)
library(sp)
library(ggmap)
library(rgdal)
library(cowplot)
library(sf)
library(magick)
library(RColorBrewer)
library(png)




#------------------------------------------------#
####          Read in Required Data           ####
#------------------------------------------------#

#Read in the files
cbc <- read.csv("outputs/cbc_alldata_20220415.csv")
buff1 <- readOGR("outputs/mdi_circle.kml")
buff2 <- readOGR("outputs/sch_circle.kml")
sp.tab <- read.csv("outputs/regional/speciesstats_table_20220415.csv")
effort <- read.csv("data/schmdi_cbceffort_20220418.csv")
tax <- read.csv("data/eBird_Taxonomy_v2021.csv")
nps.bounds <- readOGR("data/nps_boundary/nps_boundary.shp")
cons.land <- readOGR("data/maine_conserved_lands/Maine_Conserved_Lands.shp")
acad <- readOGR("data/ACAD_ParkLands_202004/ACAD_ParkBoundary_PY_202004.shp")




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

#Create data frame with circle centers
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

#Filter NPS boundaries for just Acadia
select.bounds <- nps.bounds[nps.bounds@data$UNIT_NAME=="Acadia National Park", ]
cons.land@data$PROJECT[is.na(cons.land@data$PROJECT)] = "Unknown"
cons.land@data$PARCEL_NAM[is.na(cons.land@data$PARCEL_NAM)] = "Unknown"
select.cons <- cons.land[cons.land@data$PROJECT != "Acadia National Park", ]
select.cons <- test[test@data$PARCEL_NAM != "Original Reservation", ]

acad.bounds <- spTransform(acad, CRS("+proj=longlat +datum=NAD83 +no_defs"))

##Map the data
#Get base map
base.map <- get_stamenmap(
  bbox = c(left = -68.61, bottom = 44.15, right = -67.83, top = 44.6),
  maptype = 'toner-lite',
  zoom = 11)

#Plot
ggmap(base.map) +
  geom_polygon(data = select.cons, aes(x = long, y = lat, group = group, fill = "#CC9933"),
               color = "black", alpha = 0.5, size = 0.22) +
  geom_polygon(data = acad.bounds, aes(x = long, y = lat, group = group, fill = "#009900"),
               color = "black", alpha = 0.5, size = 0.22) +
  geom_point(data = circpoint, aes(longitude, latitude, fill = circle), 
             shape = 21, size = 2.5, color = 'black') +
  geom_path(data = buff1f, aes(x=long, y=lat), color = "#3A82CA") +
  geom_path(data = buff2.0, aes(x=long, y=lat), color = "darkorange") +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.795, 0.117),
        legend.spacing.y = unit(0, "cm"),
        legend.key.size = unit(.4, "cm"),
        legend.key.width = unit(.9,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Helvetica", size = 9),
        legend.background = element_rect(fill = "white", color = "black"),
        panel.border = element_rect(color = 'black', size = 1.5, fill = NA)) +
  scale_fill_manual("", values = c("#009900", "#CC9933", "#3A82CA", "darkorange"),
                    label = c("Acadia National Park","Other conserved lands", "MDI CBC circle", "Schoodic CBC circle")) #+
  #ggsn::scalebar(base.map, dist = 100, st.size=3, height=0.01, dd2km = TRUE, model = 'WGS84')

ggsave("outputs/regional/forpub/cbc_study_area.png", height = 5.28, width = 5.28, dpi = 500)




#------------------------------------------------#
####        Summary Plots - Singles           ####
#------------------------------------------------#

# ##Number of Years Species Observed
# png(filename = "outputs/regional/forpub/YearsSpeciesObserved.png",
#     width=5.5, height=3.0, units="in", res = 150)
# 
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# hist(F1$Freq, xlab="Years observed", ylab="No. of species",
#      main="Frequency of Observation by Species")
# 
# dev.off()
# 
# 
# 
# 
# ##Species Per Year
# png(filename = "outputs/regional/forpub/NoSpecies.png",
#     width=5.5, height=3.0, units="in", res = 150)
# 
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(T2$Year, T2$NoSpecies,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Species",
#      main="Species totals by year")
# lines(T2$Year, T2$NoSpecies)
# 
# dev.off()
# 
# 
# 
# 
# ##Birds Per Year
# png(filename = "outputs/regional/forpub/NoBirds.png",
#     width=5.5, height=3.0, units="in", res = 150)
# 
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(T2$Year, T2$Birds,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Birds",
#      main="Total count of birds by year")
# lines(T2$Year, T2$Birds)
# 
# dev.off()
# 
# 
# 
# 
# ##Birds Per Party Hour
# png(filename = "outputs/regional/forpub/NoBirdsPartyHour.png",
#     width=5.5, height=3.0, units="in", res = 150)
# 
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(T2$Year, T2$BirdsPartyHour,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Birds/PartyHour",
#      main="Total count of birds per party hour")
# lines(T2$Year, T2$BirdsPartyHour)
# 
# dev.off()



##Party Hours

# png(filename = "outputs/regional/forpub/PartyHours.png",
#     width=5.5, height=3.0, units="in", res = 150)
# 
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(T2$Year, T2$PartyHours,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Party hours",
#      main="Average number of party hours per year")
# lines(T2$Year, T2$PartyHours)
# 
# dev.off()

#Alter effort for plotting
eff <- effort %>% 
  as_tibble() %>% 
  select(year, mdi.participants, sch.participants, mdi.hours, sch.hours) %>% 
  mutate(mdi.participants = replace_na(mdi.participants, 0),
         mdi.hours = replace_na(mdi.hours, 0),
         participants = mdi.participants + sch.participants, 
         partyhours = mdi.hours + sch.hours) %>% 
  select(year, participants, partyhours)

#Participant plot
part <- eff %>% 
  ggplot(aes(year, participants)) +
  geom_point(shape = 21, size = 1, color = "black") +
  #geom_line(color = "black", size = .5) +
  geom_smooth(method = "loess", color = "black", span = 1.5, size = 0.5) +
  theme_bw() +
  labs(y="Participants (n)", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), expand = c(0,2), limits = c(10,80)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black", size = 8, family = "Helvetica"),
        axis.title = element_text(color = "black", size = 8, family = "Helvetica"),
        axis.ticks = element_line(color = "black", size = 0.2),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 0.5))

#Party hour plot
hour <- eff %>% 
  ggplot(aes(year, partyhours)) +
  geom_point(shape = 21, size = 1, color = "black") +
  #geom_line(color = "black", size = .5) +
  geom_smooth(method = "loess", color = "black", span = 1.5, size = 0.5) +
  theme_bw() +
  labs(y="Party hours", x="Year") +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 6), expand = c(0,2), limits = c(10,80)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(color = "black", size = 8),
        axis.ticks = element_line(color = "black", size = 0.2),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 0.5))

#Put the number of species with the cumulative species plot and save
plot_grid(part, hour, nrow=1, labels=c('a', 'b'), align = "h", label_size = 10)

ggsave("outputs/regional/forpub/effort_biplot_20220709.jpg", height = 2.4, width = 5.28, dpi = 500)



# ##Observers per year
# png(filename = "outputs/regional/forpub/Participants.png",
#     width=5.5, height=3.0, units="in", res = 150)
# 
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(effort$year, effort$mean.participants,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="No. of participants",
#      main="Average number of participants per year")
# lines(effort$year, effort$mean.participants)
# 
# dev.off()




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
# png(filename = "outputs/regional/forpub/CumulativeBirds.png",
#     width=5.5, height=3.0, units="in", res = 150)
# 
# par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(T3.2$Year, T3.2$cumsum,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Cumulative species",
#      main="Cumulative bird species")
# lines(T3.2$Year, T3.2$cumsum)
# 
# dev.off()

cumul.sp <- T3.2 %>% 
  ggplot(aes(Year, cumsum)) + 
  geom_point(size = 0.5) +
  geom_line(size = 0.3) +
  theme_bw() +
  labs(y="Cumulative number of species", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), expand = c(0,0), limits = c(75,165)) +
  theme(axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(color = "black", size = 8),
        axis.ticks = element_line(color = "black", size = 0.2),
        #strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        #strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 0.5))




#------------------------------------------------#
####         Summary Plots - Quads            ####
#------------------------------------------------#


# ##Summary figure 1
# png(filename = "outputs/regional/forpub/summary1.png",
#     width=11, height=6, units="in", res = 500)
# 
# par(mfrow=c(2,2), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(effort$year, effort$mean.participants,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="No. of participants",
#      main="Average number of participants per year")
# lines(effort$year, effort$mean.participants)
# 
# plot(T2$Year, T2$PartyHours,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Party hours",
#      main="Average number of party hours per year")
# lines(T2$Year, T2$PartyHours)
# 
# plot(T2$Year, T2$BirdsPartyHour,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Total count/party hour",
#      main="Total count of birds per party hour")
# lines(T2$Year, T2$BirdsPartyHour)
# 
# plot(T2$Year, T2$Birds,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Birds",
#      main="Total count of birds by year")
# lines(T2$Year, T2$Birds)
# 
# dev.off()




# #Summary figure 2
# png(filename = "outputs/regional/forpub/Summary2.png",
#     width=11, height=6.0, units="in", res = 500)
# 
# par(mfrow=c(2,2), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
# 
# plot(T2$Year, T2$NoSpecies,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Species",
#      main="Species totals by year")
# lines(T2$Year, T2$NoSpecies)
# 
# plot(T3.2$Year, T3.2$cumsum,
#      pch=16, cex=0.5,
#      xlab="Year", ylab="Cumulative species",
#      main="Cumulative bird species")
# lines(T3.2$Year, T3.2$cumsum)
# 
# hist(F1$Freq, xlab="Years observed", ylab="No. of species",
#      main="Frequency of Observation by Species")
# 
# dev.off()



#------------------------------------------------#
####       SRC/linear regression Plots        ####
#------------------------------------------------#

rm.out <- T2 %>% 
  filter(Year!=2001)

##Number of species by Year
# ggscatterstats(data = rm.out, x = Year, y = NoSpecies, 
#                type = "spearman",
#                title = "Number of species by year",
#                ylab = "No. of species",
#                ggtheme = theme_classic())
speciesdat <- T2 %>% 
  mutate(spPartyHour = NoSpecies/PartyHours)

avg.sp <- speciesdat %>% 
  ggplot(aes(Year, spPartyHour)) +
  geom_point(shape = 21, size = 1, color = "black") +
  geom_smooth(method = "loess", color = "black", span = 1.5, size = 0.5) +
  theme_bw() +
  labs(y="Species/party hour", x="Year") +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0), limits = c(50,90)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(color = "black", size = 8),
        axis.ticks = element_line(color = "black", size = 0.2),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 0.5))

#Put the number of species with the cumulative species plot and save
plot_grid(avg.sp, cumul.sp, nrow=1, labels=c('a', 'b'), align = "h", label_size = 10)

ggsave("outputs/regional/forpub/speciesnum_biplot_20220709.jpg", height = 2.4, width = 5.28, dpi = 500)




##Number of birds by Year
# ggscatterstats(data = T2, x = Year, y = Birds, 
#                type = "spearman",
#                title = "Total count of birds by year",
#                ylab = "Total count of birds",
#                ggtheme = theme_classic())

reg.1 <- T2 %>% 
  ggplot(aes(Year, Birds)) +
  geom_point(shape = 21, size = 1, color = "black") +
  geom_smooth(method = "loess", color = "black", span = 1.5, size = 0.5) +
  theme_bw() +
  labs(y="Total individual birds", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), expand = c(0,0), limits = c(1200,17000)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(color = "black", size = 8),
        axis.ticks = element_line(color = "black", size = 0.2),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 0.5))

#ggsave("outputs/regional/forpub/totalcount_year_regression.png", height = 5, width = 7)




##Number of birds/partyhour by Year
# ggscatterstats(data = T2, x = Year, y = BirdsPartyHour, 
#                type = "spearman",
#                title = "Total count of birds/party hour by year",
#                ylab = "Total count/party hour",
#                ggtheme = theme_classic())

reg.2 <- T2 %>% 
  ggplot(aes(Year, BirdsPartyHour)) +
  geom_point(shape = 21, size = 1, color = "black") +
  geom_smooth(method = "loess", color = "black", span = 1.5, size = 0.5) +
  #geom_smooth(span = .75, color = "black") +
  theme_bw() +
  labs(y="Birds/party hour", x="Year") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0,0), limits = c(-10,530)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank(),
        axis.text = element_text(color = "black", size = 8),
        axis.title = element_text(color = "black", size = 8),
        axis.ticks = element_line(color = "black", size = 0.2),
        strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = "12"), 
        strip.background = element_rect(colour="black", fill="gray"),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 0.5))

ggsave("outputs/regional/forpub/regression_birdphour_20221114.jpg", height = 4.2, width = 5.28, dpi = 500)


plot_grid(reg.1, reg.2, nrow=1, labels=c('a', 'b'), align = "h", label_size = 10)

ggsave("outputs/regional/forpub/regression_biplot_20220709.png", height = 2.4, width = 5.28, dpi = 300)




#------------------------------------------------#
####          Species-specific Plots          ####
#------------------------------------------------#

##Plot of Count and Count Party Hour
#FOR EVERY INDIVIDUAL SPECIES

T1 <- T2.0

#create plot for each species
up <- as.vector(unique(T1$CommonName))


# #begin loop
# for (i in 1:length(up)) {
#   T1.1<-subset(T1,CommonName==up[i])
#   
#   png(filename =paste("outputs/regional/spfigs/Figure_",up[i],".png",sep=""),
#       width=5.0, height=3.0, units="in", res = 150)
#   
#   par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))
#   
#   plot(T1.1$Year, T1.1$CountPartyHour,
#        pch=16, cex=0.5,
#        xlab="Year", ylab="Birds/Party Hour",
#        main=T1.1$CommonName[T1.1$Year==1971])
#   lines(T1.1$Year, T1.1$CountPartyHour, lwd=1.2)
#   
#   par(new = T)
#   plot(T1.1$Year, T1.1$Count,
#        axes=F, xlab=NA, ylab=NA, 
#        pch=16, cex=0.2, col="red")
#   lines(T1.1$Year, T1.1$Count, col="red", lwd=0.8)
#   axis(side = 4, col="red", col.axis="red", col.lab="red")
#   mtext(side = 4, line = 1.5, 'Number of birds', col="red")
#   
#   
#   dev.off()
#   
#   
#   rm(T1.1) # remove the T1 table after each loop to ensure no carry over
# } # close i
# #end of loop


# library(grid)
# inc <- readPNG("outputs/regional/increase.png")



##12 species plot
#Create list of desired species
select.sp <- c("American Crow", "Harlequin Duck", "Northern Cardinal", "Wild Turkey",
               "American Tree Sparrow", "Blue Jay", "Boreal Chickadee", "Common Eider",
               "American Black Duck", "Dark-eyed Junco", "Herring Gull", "Red-breasted Nuthatch")
  

  
T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species %in% select.sp) %>% 
  mutate(species = factor(species, 
                          levels = c("American Crow", "American Black Duck", "American Tree Sparrow",
                                     "Harlequin Duck", "Dark-eyed Junco", "Blue Jay",
                                     "Northern Cardinal", "Herring Gull", "Boreal Chickadee",
                                     "Wild Turkey", "Red-breasted Nuthatch", "Common Eider"))) %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "black") +
  #geom_smooth(se=F, color = "black", method = "lm") +
  facet_wrap(vars(species), labeller = label_wrap_gen(), scales = "free_y", ncol = 3) +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 9),
        axis.text = element_text(color = "black", size = 6.5),
        panel.grid.minor = element_blank())


ggsave("outputs/regional/twelve_species_plot.png", height = 7.28, width = 5.28)


T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species == "Common Eider") %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "gray60", size = 0.9) +
  geom_smooth(se=F, color = "black", span = 1.5) +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank())

ggsave("outputs/coei_plot.png", height = 5, width = 4.5)

T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species == "White-winged Scoter") %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "gray60", size = 0.9) +
  geom_smooth(se = F, color = "black", span = 1.5) +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank())

ggsave("outputs/wwsc_plot.png", height = 5, width = 4.5)

T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species == "American Black Duck") %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "gray60", size = 0.9) +
  geom_smooth(se = F, color = "black", span = 1.5) +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank())

ggsave("outputs/abdu_plot.png", height = 5, width = 4.5)

T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species == "Long-tailed Duck") %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "gray60", size = 0.9) +
  geom_smooth(se = F, color = "black", span = 1.5) +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank())

ggsave("outputs/ltdu_plot.png", height = 5, width = 4.5)

T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species == "Peregrine Falcon") %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "gray60", size = 0.9) +
  geom_smooth(se = F, color = "black", method = "lm") +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank())

ggsave("outputs/pefa_plot.png", height = 5, width = 4.5)

T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species == "Bald Eagle") %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "gray60", size = 0.9) +
  geom_smooth(se = F, color = "black", method = "lm") +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank())

ggsave("outputs/baea_plot.png", height = 5, width = 4.5)

T1 %>% 
  select(species = CommonName, year = Year, countph = CountPartyHour) %>% 
  filter(species == "Wild Turkey") %>% 
  ggplot(aes(year, countph)) +
  geom_line(color = "gray60", size = 0.9) +
  geom_smooth(se = F, color = "black", span = 0.5) +
  labs(x = "Year", y = "Count/party hour") +
  theme_bw() +
  theme(strip.text.x = element_text(margin = margin(.2,0,.2,0, "cm"), color = "black", size = 9, family = "Helvetica"),
        strip.background = element_rect(color = "black", size = 1),
        panel.border = element_rect(color = "black", size = 1),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank())

ggsave("outputs/witu_plot.png", height = 5, width = 4.5)





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
  select('Common name') %>% 
  mutate(`Common name` = case_when(`Common name` == "Ring-necked duck" ~ "Ring-necked Duck",
                                   `Common name` == "Gray Jay" ~ "Canada Jay",
                                   TRUE ~ `Common name`))
                                   

#Join for output
tax.table <- left_join(sp, tax2, by = 'Common name') %>% 
  as_tibble() %>% 
  arrange(`Common name`)

#Write
#write.csv(tax.table, "outputs/regional/forpub/allspecies_taxonomytable.csv")




#------------------------------------------------#
####          Resident/Nonresident            ####
#------------------------------------------------#

##Resident Species Change
#List resident species
res.sp <- c("Black-capped Chickadee", "Blue Jay", "Common Eider", "Common Grackle",
            "Common Raven", "Downy Woodpecker", "European Starling", "Great Black-backed Gull", 
            "Hairy Woodpecker", "House Sparrow", "Mourning Dove", "Northern Mockingbird", 
            "Red-winged Blackbird", "Ruffed Grouse", "American Crow", "American Goldfinch",
            "Bald Eagle", "Barred Owl", "Black Guillemot", "Cedar Waxwing", "Common Loon",
            "Cooper's Hawk", "Golden-crowned Kinglet", "Hooded Merganser", 
            "House Finch", "Mallard", "Northern Cardinal", "Northern Flicker",
            "Northern Harrier", "Peregrine Falcon", "Pileated Woodpecker",
            "Purple Finch", "Rock Pigeon", "Tufted Titmouse", "Wild Turkey", "American Black Duck",
            "American Robin", "Belted Kingfisher", "Brown Creeper", "Canada Goose",
            "Chipping Sparrow", "Common Merganser", "Dark-eyed Junco", "Great Horned Owl",
            "Herring Gull", "Northern Goshawk", "Pine Siskin", "Red Crossbill",
            "Red-breasted Nuthatch", "Red-tailed Hawk", "Sharp-shinned Hawk", "Song Sparrow",
            "Spruce Grouse", "White-breasted Nuthatch", "White-throated Sparrow",
            "White-winged Crossbill", "Yellow-rumped Warbler")

#Determine summary stats
resident <- sp.tab %>% 
  as_tibble() %>% 
  filter(species %in% res.sp) %>% 
  select(species, change) %>% 
  group_by(change) %>% 
  summarise(n = length(species)) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  add_row(change = "not analyzed", n = 0, percent = 0.0) %>% 
  mutate(cat = "resident")

#Write
#write.csv(resident, "outputs/regional/forpub/resident_species.csv", row.names = F)



##Nonresident Species Change
#Determine summary stats
nonresident <- sp.tab %>% 
  as_tibble() %>% 
  filter(!species %in% res.sp) %>% 
  select(species, change) %>% 
  group_by(change) %>% 
  summarise(n = length(species)) %>% 
  mutate(percent = n/sum(n)*100,
         change = str_replace(change, "not enough data", "not analyzed"),
         cat = "nonresident") 
  

#Plot Percent
bind_rows(resident, nonresident) %>% 
  ggplot(aes(y = percent, x = change, fill = cat)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  # geom_text(aes(label = round(percent, 1)), position = position_dodge(width = 0.9), 
  #           vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Paired", labels = c("Non-resident", "Resident")) +
  labs(y = "Percentage (%)") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 11),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 9),
        legend.position = c(0.16, 0.9)) 

ggsave("outputs/regional/forpub/res_nonres_20221111.jpg", height = 4, width = 5.28, units = "in", dpi = 500)


#Plot raw numbers
bind_rows(resident, nonresident) %>% 
  ggplot(aes(y = n, x = change, fill = cat)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = n), position = position_dodge(width=0.9), vjust = -0.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "Total species") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 11),
        axis.title.y = element_text(color = "black", size = 13),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 11),
        legend.position = c(0.16, 0.92))

ggsave("outputs/regional/forpub/res_count_20220614.png", height = 4.5, width = 6)



##Nonresident Species Change
non.n <- c("European Starling", "House Finch", "House Sparrow", "Rock Pigeon")

#Determine summary stats
sp.tab %>% 
  as_tibble() %>% 
  filter(species %in% non.n) %>% 
  select(species, change) %>% 
  group_by(change) %>% 
  summarise(n = length(species)) %>% 
  mutate(percent = n/sum(n)*100)



##New taxonomy/species list table with resident status
new.tax <- tax.table %>% 
  as_tibble() %>% 
  mutate('Resident status' = ifelse(`Common name` %in% res.sp, "Resident", "Non-resident"))

#Write
#write.csv(new.tax, "outputs/regional/forpub/allspecies_table_20220709.csv", row.names = F)
