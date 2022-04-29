#Environmental variables for CBC analysis - ANP and surrounding areas 
#Schoodic Institute at Acadia National Park, 2022

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(tidyverse)
require(tidyr)
require(dplyr)
require(googledrive)
require(weathermetrics)
require(udunits2)

#define function
select <- dplyr::select



#------------------------------------------------#
####       Download data from G Drive         ####
#------------------------------------------------#

####IMPORTANT
##You will have to enter 0 in the console to obtain your own token linked to your account
##Commented out as to not rerun

# ##Download data
# drive_download('christmas_bird_count/data/land_cover/dec_min_change.csv', path = 'data/dec_min_change.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/precip_change.csv', path = 'data/precip_change.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/sea_surface_temp.csv', path = 'data/sea_surface_temp.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/temp_change.csv', path = 'data/temp_change.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/tidal_area.csv', path = 'data/tidal_area.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/urban_area.csv', path = 'data/urban_area.csv', overwrite = TRUE)



#------------------------------------------------#
####            Read in the data              ####
#------------------------------------------------#

#Read in the files
dec_min_change <- read.csv("data/dec_min_change.csv")
precip_change <- read.csv("data/precip_change.csv")
sea_surface_temp <- read.csv("data/sea_surface_temp.csv")
temp_change <- read.csv("data/temp_change.csv")
tidal_area <- read.csv("data/tidal_area.csv")
urban_area <- read.csv("data/urban_area.csv")



#------------------------------------------------#
####            Data manipulation             ####
#------------------------------------------------#

##December min temperature
#Pivot to long format
dec_min <- dec_min_change %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X19790102_mean_2m_air_temperature:X20200709_mean_2m_air_temperature)

#Get year and month out of the name column
dec_min[c('dat', 'trash')] <- str_split_fixed(dec_min$name, '_', 2)
dec_min[c('trash2', 'year_month')] <- str_split_fixed(dec_min$dat, 'X', 2)
dec_min[c('trash3', 'month_day')] <- str_split_fixed(dec_min$year_month, "^\\d\\d\\d.", 2)
dec_min[c('year', 'trash4')] <- str_split_fixed(dec_min$year_month, "\\d\\d\\d\\d$", 2)
dec_min[c('month', 'trash4')] <- str_split_fixed(dec_min$month_day, "\\d\\d$", 2)

#Clean and simplify to get temp values
dec_mins <- dec_min %>% 
  group_by(year, month) %>% 
  summarise(temp.k = min(value, na.rm = T)) %>% 
  select(year, month, temp.k) %>% 
  filter(year != 2020 & month == 12)

#Convert temperature
dec_mins$temp.f <- kelvin.to.fahrenheit(dec_mins$temp.k)



##Annual precipitation
#Pivot to long format
precip_change <- precip_change %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X197101_pr:X202112_pr)

#Get year and month out of the name column
precip_change[c('dat', 'trash')] <- str_split_fixed(precip_change$name, '_', 2)
precip_change[c('trash2', 'year_month')] <- str_split_fixed(precip_change$dat, 'X', 2)
precip_change[c('trash3', 'month')] <- str_split_fixed(precip_change$year_month, "^\\d\\d\\d.", 2)
precip_change[c('year', 'trash4')] <- str_split_fixed(precip_change$year_month, "\\d\\d$", 2)

#Clean and simplify to get precip values
ann_precip <- precip_change %>% 
  select(year, month, value) %>% 
  rename(precip.mm = value)

#Convert temperature -- Something doesn't seem right with these values??
#ann_precip$precip.in <- udunits2::ud.convert(ann_precip$precip.mm, "mm", "in")



##Sea surface temperature
#Pivot to long format
sea_surf_temp <- sea_surface_temp %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X19810825023019_sea_surface_temperature:X20211231103139_sea_surface_temperature)

#Get year and month out of the name column
sea_surf_temp[c('dat', 'trash')] <- str_split_fixed(sea_surf_temp$name, '_', 2)
sea_surf_temp[c('trash2', 'year_month_time')] <- str_split_fixed(sea_surf_temp$dat, 'X', 2)
sea_surf_temp[c('trash3', 'month_time')] <- str_split_fixed(sea_surf_temp$year_month_time, "^\\d\\d\\d.", 2)
sea_surf_temp[c('year', 'trash4')] <- str_split_fixed(sea_surf_temp$year_month_time, "\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d$", 2)
sea_surf_temp[c('month', 'trash5')] <- str_split_fixed(sea_surf_temp$month_time, "\\d\\d\\d\\d\\d\\d\\d\\d$", 2)

#Clean and simplify to get sea temp values
ocean_temp <- sea_surf_temp %>% 
  filter(year!=1981) %>% 
  group_by(year, month) %>% 
  summarise(temp.k = mean(value, na.rm = T)) %>% 
  select(year, month, temp.k)

#Convert temperature -- Something doesn't seem right with these temps??
#ocean_temp$temp.f <- kelvin.to.fahrenheit(ocean_temp$temp.k)



##Annual temperature changes
#Pivot to long format
temp_changes <- temp_change %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X19790102_mean_2m_air_temperature:X20200709_mean_2m_air_temperature)

#Get year and month out of the name column
temp_changes[c('dat', 'trash')] <- str_split_fixed(temp_changes$name, '_', 2)
temp_changes[c('trash2', 'year_month')] <- str_split_fixed(temp_changes$dat, 'X', 2)
temp_changes[c('trash3', 'month_day')] <- str_split_fixed(temp_changes$year_month, "^\\d\\d\\d.", 2)
temp_changes[c('year', 'trash4')] <- str_split_fixed(temp_changes$year_month, "\\d\\d\\d\\d$", 2)
temp_changes[c('month', 'trash4')] <- str_split_fixed(temp_changes$month_day, "\\d\\d$", 2)

#Clean and simplify to get temp values
ann_temp <- temp_changes %>% 
  group_by(year, month) %>% 
  summarise(temp.k = mean(value, na.rm = T)) %>% 
  select(year, month, temp.k) %>% 
  filter(year!=2020)

#Convert temperature
ann_temp$temp.f <- kelvin.to.fahrenheit(ann_temp$temp.k)



##December temperature changes
#Use the temp_changes df
#Clean and simplify to get december only data by year
dec_temp <- temp_changes %>% 
  group_by(year, month) %>% 
  summarise(temp.k = mean(value, na.rm = T)) %>% 
  select(year, month, temp.k) %>% 
  filter(year!=2020 & month == 12)

#Convert temperature
dec_temp$temp.f <- kelvin.to.fahrenheit(dec_temp$temp.k)



##Tidal area change over time
#Pivot to long format
tide_zone <- tidal_area %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X1984.1986_classification:X2014.2016_classification)

#Get year and month out of the name column
tide_zone[c('dat', 'trash')] <- str_split_fixed(tide_zone$name, '_', 2)
tide_zone[c('trash2', 'year')] <- str_split_fixed(tide_zone$dat, 'X', 2)

#Clean and simplify to get december only data by year
mudflat_area <- tide_zone %>% 
  select(name, year, value) %>% 
  rename(area = value)



##Urban area change over time
#Pivot to long format
urban_zone <- urban_area %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X2001_impervious:X2019_landcover)

#Get year and month out of the name column
urban_zone[c('dat', 'type')] <- str_split_fixed(urban_zone$name, '_', 2)
urban_zone[c('trash2', 'year')] <- str_split_fixed(urban_zone$dat, 'X', 2)

#Clean and simplify to get december only data by year
urbanized_area <- urban_zone %>% 
  select(type, year, value) %>% 
  rename(area = value)



#------------------------------------------------#
####            Statistical tests             ####
#------------------------------------------------#

##December min temperature
#Fix numeric issue for cor.test
dec_mins$year <- as.numeric(dec_mins$year)

#Run spearman correlation -- rho 0.391, p = 0.012* 
cor.test(dec_mins$year, dec_mins$temp.f, method="spearman")

#Calculate percentage change -- 57.18% increase
100-(mean(dec_mins$temp.f[dec_mins$year<1984])/mean(dec_mins$temp.f[dec_mins$year>2014]))*100



##Annual precipitation
#Calculate year means
ann_rainfall <- ann_precip %>% 
  group_by(year) %>% 
  summarise(rainfall = sum(precip.mm))

#Fix numeric issue for cor.test
ann_rainfall$year <- as.numeric(ann_rainfall$year)

#Run spearman correlation -- rho 0.197, p = 0.166
cor.test(ann_rainfall$year, ann_rainfall$rainfall, method="spearman")

#Calculate percentage change -- 5.36% increase
100-(mean(ann_rainfall$rainfall[ann_rainfall$year<1987])/mean(ann_rainfall$rainfall[ann_rainfall$year>2016]))*100



##Sea surface temperature
#Calculate year means
ocean_temp <- ocean_temp %>% 
  group_by(year) %>% 
  summarise(temp = mean(temp.k))

#Fix numeric issue for cor.test
ocean_temp$year <- as.numeric(ocean_temp$year)

#Run spearman correlation -- rho 0.559, p < 0.001**
cor.test(ocean_temp$year, ocean_temp$temp, method="spearman")

#Calculate percentage change -- 13.48% increase
100-(mean(ocean_temp$temp[ocean_temp$year<1987])/mean(ocean_temp$temp[ocean_temp$year>2016]))*100



##Annual temperature changes
#Fix numeric issue for cor.test
ann_temp$year <- as.numeric(ann_temp$year)

#Run spearman correlation -- rho 0.038, p = 0.39 
cor.test(ann_temp$year, ann_temp$temp.f, method="spearman")

#Calculate percentage change -- 2.07% increase
100-(mean(ann_temp$temp.f[ann_temp$year<1984])/mean(ann_temp$temp.f[ann_temp$year>2014]))*100



##December temperature changes
#Fix numeric issue for cor.test
dec_temp$year <- as.numeric(dec_temp$year)

#Run spearman correlation -- rho 0.125, p = 0.44 
cor.test(dec_temp$year, dec_temp$temp.f, method="spearman")

#Calculate percentage change -- 3.78% increase
100-(mean(dec_temp$temp.f[dec_temp$year<1984])/mean(dec_temp$temp.f[dec_temp$year>2014]))*100



##Tidal area change over time
#Fix numeric issue for cor.test
mudflat_area$year <- as.numeric(mudflat_area$year)

#Run spearman correlation -- rho -0.75, p = 0.01*
cor.test(mudflat_area$year, mudflat_area$area, method="spearman")

#Calculate percentage change -- -39.64% decrease
100-(mean(mudflat_area$area[mudflat_area$year<1988])/mean(mudflat_area$area[mudflat_area$year>2010]))*100



##Urban area change over time
#Get only the impervious landcover metric
urban <- urbanized_area %>% filter(type == "impervious")

#Fix numeric issue for cor.test
urban$year <- as.numeric(urban$year)

#Run spearman correlation -- rho 1, p < 0.0001***
cor.test(urban$year, urban$area, method="spearman")

#Calculate percentage change -- 6.6% increase
100-(mean(urban$area[urban$year<2006])/mean(urban$area[urban$year>2014]))*100









