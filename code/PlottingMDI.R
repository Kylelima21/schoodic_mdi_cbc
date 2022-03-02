#Initial data exploration for CBC circle on MDI 

library(ggplot2)
library(dplyr)

setwd("/Users/Nick/Documents/Projects/Birds/CBC_ANP/Rwork")

#read in the file without the 'X' at the beginning of column names (check.names = F)
T1<-read.csv('CBC_MDI_All.csv', header=TRUE, check.names = FALSE)
Y1<-read.csv('Years_MDI.csv', header=TRUE) #this starts with 1971
#O1<-read.csv('Observers.csv', header=TRUE)
str(T1)
names(T1)
head(T1)

#Just keep the 50 years with Bill Townsend
T1<-T1[T1$Year >= 1971, ]
#Y1<-Y1[Y1$Year >= 1971, ] #this doesn't work, no longer a table?
#remove species with zero birds
T1<-T1[!(T1$CommonName %in% c("American Three-toed Woodpecker",
                              "Blue Grosbeak", 
                              "Common Scoter",
                              "Little Gull",
                              "Northern Hawk Owl",
                              "Osprey",
                              "Red Phalarope",
                              "Red-shouldered Hawk")), ]

#
####################################################
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

head(T2)


############
#Frequency of years species is present
F1<-T1.1 %>% 
  group_by(CommonName) %>% 
  summarise(Freq=length(Year),
            TotalBirds=sum(Count),
            TotBirdsPH=sum(CountPartyHour),
            MinYear=min(Year),
            MaxYear=max(Year))

write.csv(F1, "BirdFreqTotals_MDI.csv", row.names=FALSE)


###############################################
setwd("/Users/Nick/Documents/Projects/Birds/CBC_ANP/Rwork/Plots_MDI")


##################################################
########Years each species observed############
##################################################
png(filename = "aa_YearsSpeciesObserved.png",
    width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

hist(F1$Freq, xlab="Years observed", ylab="No. of Species",
     main="Number of Years Species Observed")

dev.off()
##################################################


##################################################
########Bird Species Per Year############
##################################################
png(filename = "aa_NoSpecies.png",
     width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$NoSpecies,
     pch=16, cex=0.5,
     xlab="Year", ylab="Species",
     main="Total species per year")
lines(T2$Year, T2$NoSpecies)

dev.off()
##################################################

###################################################################################
###################################################################################
###################################################################################


###################################################################################
###################################################################################
###################################################################################
##################################################
########Birds Per Year############
##################################################
png(filename = "aa_NoBirds.png",
     width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$Birds,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds",
     main="Birds per year")
lines(T2$Year, T2$Birds)

dev.off()
##################################################


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
##################################################
########Birds Per Party Hour############
##################################################
png(filename = "aa_NoBirdsPartyHour.png",
     width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$BirdsPartyHour,
     pch=16, cex=0.5,
     xlab="Year", ylab="Birds/PartyHour",
     main="Total birds per party hour")
lines(T2$Year, T2$BirdsPartyHour)

dev.off()
##################################################

###################################################################################
###################################################################################
###################################################################################
###################################################################################
##################################################
########Party Hour############
##################################################
png(filename = "aa_PartyHours.png",
     width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T2$Year, T2$PartyHours,
     pch=16, cex=0.5,
     xlab="Year", ylab="PartyHours",
     main="Party hours per year")
lines(T2$Year, T2$PartyHours)

dev.off()
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################






###################################################################################
###################################################################################
###################################################################################
#observers per year and party hours
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

plot(T3.1$Year, T3.1$NewSpecies)

#need to add in all of the zeros and then need to add in new
#species from each year

T3.2<-merge(T3.1, Y1, by="Year",
            all.y=TRUE) #include all rows from second data frame
head(T3.2)
T3.2[is.na(T3.2)] <- 0
T3.2$cumsum<-cumsum(T3.2$NewSpecies)

##################################################
png(filename = "aa_CumulativeBirds.png",
     width=5.5, height=3.0, units="in", res = 150)

par(mfrow=c(1,1), mgp=c(1.5,0.5,0), mar=c(2.5,2.5,2,2.5), oma=c(0.1,0.1,0.1,0.1))

plot(T3.2$Year, T3.2$cumsum,
     pch=16, cex=0.5,
     xlab="Year", ylab="Cumulative species",
     main="Cumulative bird species")
lines(T3.2$Year, T3.2$cumsum)

dev.off()
##################################################




########################################################################
########################################################################
######################## Plot of Count and Count Party Hour ##########################
########################################################################
########################################################################
#T1.1<-subset(T1, CommonName == "American Black Duck")
############################################################
#create plot for each species
up<-as.vector(unique(T1$CommonName))

#set destination for plots
setwd("/Users/Nick/Documents/Projects/Birds/CBC_ANP/Rwork//Plots_MDI")

#begin loop
for (i in 1:length(up)) {
  T1.1<-subset(T1,CommonName==up[i]) #need to double check that this is correct (T1.1)
  
  ##################################################
  
  ##################################################
  png(filename =paste("Figure_",up[i],".png",sep=""),
      width=5.0, height=3.0, units="in", res = 150)
  ##################################################
  
  ##########
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
  lines(T1.1$Year, T1.1$Count, col="red", lwd=0.7)
  axis(side = 4, col="red", col.axis="red", col.lab="red")
  mtext(side = 4, line = 1.5, 'Number of birds', col="red")
  
  
  dev.off()
  
  
  rm(T1.1) # remove the T1 table after each loop to ensure no carry over
} # close i
#end of loop




#####################################
########################################################################

####
#1930s birds for Catherine
T1930<-T1[T1$Year < 1940, ]
T1930a<-T1930 %>%
  group_by(CommonName) %>%
  summarise(Count=sum(Count))
T1930b<-T1930a[T1930a$Count > 0, ] 

