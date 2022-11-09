library(tidyverse)
library(ggplot2)
library(RColorBrewer)

mdi <- read.csv("outputs/mdi/cbcmdi_fulldata_20220402.csv", header = TRUE)
sch <- read.csv("outputs/sch/cbcsch_fulldata_20220402.csv", header = TRUE)
Y1 <- data.frame(Year = 2021:1971)
effort <- read.csv("data/schmdi_cbceffort_20220418.csv")
tax <- read.csv("data/eBird_Taxonomy_v2021.csv")

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
  summarise(Count=mean(Count), 
            PartyH=mean(PartyHours))

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
            PartyHours=mean(PartyHours)) %>% 
  select(year = Year, NoSpecies)

data <- tibble(effort) %>% 
  select(year, sum.participants) %>% 
  arrange(year) %>% 
  left_join(., T2, by = "year") %>% 
  pivot_longer(., c(NoSpecies, sum.participants)) %>% 
  mutate(name = factor(name, levels = c("sum.participants", "NoSpecies")))

data %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(labels = c("Participants", "Species"), values = c("#1F78B4", "gray60")) +
  labs(x = "Year", y = "Participants / Species") +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.position = "bottom",
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.text = element_text(size = 16)
  )

ggsave("outputs/species_people_figure.png", height = 8, width = 13)










