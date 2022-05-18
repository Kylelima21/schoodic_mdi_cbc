#Environmental variables for CBC analysis - ANP and surrounding areas 
#Schoodic Institute at Acadia National Park
#By Kyle Lima, Peter Nelson, and Nick Fisichelli, 2022

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

require(tidyverse)
require(googledrive)
require(weathermetrics)
require(udunits2)
require(cowplot)




#------------------------------------------------#
####       Download data from G Drive         ####
#------------------------------------------------#

####IMPORTANT
##You will have to enter 0 to obtain your own token linked to your account
##Commented out as to not rerun

# ##Download data
# drive_download('christmas_bird_count/data/land_cover/dec_min_temp.csv', path = 'data/dec_min_temp.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/precip_change.csv', path = 'data/precip_change.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/sea_surface_temp.csv', path = 'data/sea_surface_temp.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/temp_change.csv', path = 'data/temp_change.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/tidal_area.csv', path = 'data/tidal_area.csv', overwrite = TRUE)
# drive_download('christmas_bird_count/data/land_cover/urban_area.csv', path = 'data/urban_area.csv', overwrite = TRUE)




#------------------------------------------------#
####            Read in the data              ####
#------------------------------------------------#

#Read in the files
dec.min.temp <- read.csv("data/dec_min_temp.csv") %>% as_tibble()
precip.change <- read.csv("data/precip_change.csv") %>% as_tibble()
sea.surface.temp <- read.csv("data/sea_surface_temp.csv") %>% as_tibble()
temp.change <- read.csv("data/temp_change.csv") %>% as_tibble()
tidal.area <- read.csv("data/tidal_area.csv") %>% as_tibble()
urban.area <- read.csv("data/urban_area.csv") %>% as_tibble()
sea.level <- read_delim("data/tide_rawdata.txt", delim = ";", col_names = 
             c("yearday", "mean.sl", "xa",  "xb")) #full date range




#------------------------------------------------#
####            Data manipulation             ####
#------------------------------------------------#

##December minimum temperature - - ERROR IN THE GOOGLE EARTH ENGINE CODE OR SOMETHING
#Pivot data into long format
dec.min.temp <- dec.min.temp %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X197101_tmmn:X202112_tmmn)

#Gather date from the names column
dec.min.temp[c('dat', 'trash')] <- str_split_fixed(dec.min.temp$name, '_', 2)
dec.min.temp[c('trash2', 'year_month')] <- str_split_fixed(dec.min.temp$dat, 'X', 2)
dec.min.temp[c('trash3', 'month')] <- str_split_fixed(dec.min.temp$year_month, "^\\d\\d\\d.", 2)
dec.min.temp[c('year', 'trash4')] <- str_split_fixed(dec.min.temp$year_month, "\\d\\d$", 2)

#Clean to get December mins for each year
dec.minT <- dec.min.temp %>% 
  filter(month == 12) %>% 
  select(year, value) %>% 
  rename(temp = value)

#Convert to Fahrenheit
dec.minT$temp <- celsius.to.fahrenheit(dec.minT$temp, round = 2)




##Annual precipitation - - ERROR IN THE GOOGLE EARTH ENGINE CODE OR SOMETHING
#Pivot data into long format
precip.changes <- precip.change %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X197101_pr:X202112_pr)

#Gather date from the names column
precip.changes[c('dat', 'trash')] <- str_split_fixed(precip.changes$name, '_', 2)
precip.changes[c('trash2', 'year_month')] <- str_split_fixed(precip.changes$dat, 'X', 2)
precip.changes[c('trash3', 'month')] <- str_split_fixed(precip.changes$year_month, "^\\d\\d\\d.", 2)
precip.changes[c('year', 'trash4')] <- str_split_fixed(precip.changes$year_month, "\\d\\d$", 2)

#Clean to get accumulated precip for each month
ann.precip <- precip.changes %>% 
  select(year, month, year_month, value) %>% 
  rename(precip=value)

#Convert units to inches
ann.precip$precip <- udunits2::ud.convert(ann.precip$precip, "mm", "in")




##Sea surface temperature - - ERROR IN THE GOOGLE EARTH ENGINE CODE OR SOMETHING
#Pivot data into long format
sea.surf.temp <- sea.surface.temp %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X19810825023019_sea_surface_temperature:X20211231103139_sea_surface_temperature)

#Gather date from the names column
sea.surf.temp[c('dat', 'trash')] <- str_split_fixed(sea.surf.temp$name, '_', 2)
sea.surf.temp[c('trash2', 'year_month_time')] <- str_split_fixed(sea.surf.temp$dat, 'X', 2)
sea.surf.temp[c('trash3', 'monthtime')] <- str_split_fixed(sea.surf.temp$year_month_time, "^\\d\\d\\d.", 2)
sea.surf.temp[c('month', 'trash4')] <- str_split_fixed(sea.surf.temp$monthtime, "\\d\\d\\d\\d\\d\\d\\d.$", 2)
sea.surf.temp[c('year', 'trash5')] <- str_split_fixed(sea.surf.temp$year_month_time, "\\d\\d\\d\\d\\d\\d\\d\\d\\d.$", 2)

#Clean to get temp for each month
ocean.temp <- sea.surf.temp %>% 
  select(name, year, month, value) %>% 
  rename(temp=value) %>% 
  group_by(year, month) %>% 
  summarise(temp.k = mean(temp, na.rm=T)) %>% 
  filter(year!=1981)

#Convert units to Fahrenheit
ocean_temp$temp.f <- kelvin.to.fahrenheit(ocean.temp$temp.k, round = 2)




##Annual temperature
#Pivot data into long format
temp.changes <- temp.change %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X19790102_mean_2m_air_temperature:X20200709_mean_2m_air_temperature)

#Gather date from the names column
temp.changes[c('dat', 'trash')] <- str_split_fixed(temp.changes$name, '_', 2)
temp.changes[c('trash2', 'year_month_day')] <- str_split_fixed(temp.changes$dat, 'X', 2)
temp.changes[c('trash3', 'monthday')] <- str_split_fixed(temp.changes$year_month_day, "^\\d\\d\\d.", 2)
temp.changes[c('month', 'motrash')] <- str_split_fixed(temp.changes$monthday, "\\d\\d$", 2)
temp.changes[c('year', 'trash4')] <- str_split_fixed(temp.changes$year_month_day, "\\d\\d\\d\\d$", 2)

#Clean to get temps for each month for all years
ann.temps <- temp.changes %>% 
  select(name, year, month, value) %>% 
  rename(temp=value) %>% 
  group_by(year, month) %>% 
  summarise(temp.k = mean(temp, na.rm=T)) %>% 
  mutate(year = as.numeric(year))

#Convert units to fahrenheit
ann.temps$temp.f <- kelvin.to.fahrenheit(ann.temps$temp.k, round = 2)

#Take annual means
ann.tempF <- ann.temps %>% 
  group_by(year) %>% 
  summarise(mean.temp = mean(temp.f))

ann.tempF %>% 
  ggplot(aes(x=year, y=mean.temp)) +
  geom_point() +
  geom_smooth(method = "lm")




##December temperature changes
#Use the temp.changes df
#Clean and simplify to get December only data by year
dec.temp <- temp.changes %>% 
  group_by(year, month) %>% 
  summarise(temp.k = mean(value, na.rm = T)) %>% 
  select(year, month, temp.k) %>% 
  filter(year!=2020 & month == 12) %>% 
  mutate(year = as.numeric(year))

#Convert temperature -- Something doesn't seem right with these temps??
dec.temp$temp.f <- kelvin.to.fahrenheit(dec.temp$temp.k)

dec.temp %>% 
  ggplot(aes(x=year, y=temp.f)) +
  geom_point() +
  geom_smooth()




##Tidal area change over time
#Pivot to long format
tide.zone <- tidal.area %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X1984.1986_classification:X2014.2016_classification)

#Get year and month out of the name column
tide.zone[c('dat', 'trash')] <- str_split_fixed(tide.zone$name, '_', 2)
tide.zone[c('trash2', 'year')] <- str_split_fixed(tide.zone$dat, 'X', 2)

#Clean and simplify to get December only data by year
mudflat.area <- tide.zone %>% 
  select(name, year, value) %>% 
  rename(area = value) %>% 
  mutate(year = str_replace(year, "\\.\\d*$", ""),
         year = as.numeric(year)) %>% 
  select(-name)

mudflat.area %>% 
  ggplot(aes(x=year, y=area)) +
  geom_point() +
  geom_smooth()




##Urban area change over time
#Pivot to long format
urban.zone <- urban.area %>%
  select(-c(system.index, `.geo`)) %>% 
  pivot_longer(X2001_impervious:X2019_landcover)

#Get year and month out of the name column
urban.zone[c('dat', 'type')] <- str_split_fixed(urban.zone$name, '_', 2)
urban.zone[c('trash2', 'year')] <- str_split_fixed(urban.zone$dat, 'X', 2)

#Clean and simplify to get December only data by year
urbanized.area <- urban.zone %>% 
  select(type, year, value) %>% 
  rename(area = value) %>% 
  filter(type == "impervious") %>% 
  select(-type) %>% 
  mutate(year = as.numeric(year))

urbanized.area %>% 
  ggplot(aes(x=year, y=area)) +
  geom_point() +
  geom_smooth()



##Sea level change
sea.rise <- sea.level %>% 
  select(-c(xa, xb)) %>% 
  mutate(yearday = trimws(yearday),
         mean.sl = trimws(mean.sl),
         mean.sl = as.numeric(mean.sl),
         year = as.integer(yearday)) %>% 
  filter(year > 1970 & mean.sl > 0)

sea.rise %>% 
  ggplot(aes(x=year, y=mean.sl)) +
  geom_point() +
  geom_smooth()




#------------------------------------------------#
####            Statistical tests             ####
#------------------------------------------------#

##Annual mean temp area change over time
summary(lm(mean.temp~year, data = ann.tempF))

#R2 = 0.124, F(1, 40) = 5.665, p = 0.022



##December mean temp area change over time
summary(lm(temp.f~year, data = dec.temp))

#R2 = 0.041, F(1, 39) = 1.652, p = 0.206



##Mudflat area change over time
summary(lm(area~year, data = mudflat.area))

#R2 = 0.496, F(1, 9) = 8.869, p = 0.016



##Urban area change over time
summary(lm(area~year, data = urbanized.area))

#R2 = 0.976, F(1, 6) = 248.5, p < 0.001



##Sea level change
summary(lm(mean.sl ~ year, data = sea.rise))

#R2 = 0.377, F(1, 521) = 314.7, p < 2.2e-16




#------------------------------------------------#
####             Make a cowplot               ####
#------------------------------------------------#

ann.temp.plot <- ann.tempF %>% 
  ggplot(aes(x=year, y=mean.temp)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Mean annual\ntemperature (F)") +
  theme(axis.title.x = element_blank())

dec.temp.plot <- dec.temp %>% 
  ggplot(aes(x=year, y=temp.f)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "black") +
  theme_classic() +
  labs(y = "Mean December\ntemperature (F)") +
  theme(axis.title.x = element_blank())

mud.plot <- mudflat.area %>% 
  ggplot(aes(x=year, y=area)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Percent of study area\ncovered by mudflat") +
  theme(axis.title.x = element_blank())

urb.plot <- urbanized.area %>%
  mutate(area = area/10000) %>% 
  ggplot(aes(x=year, y=area)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Urbanized area\n(hectares)") +
  theme(axis.title.x = element_blank())

sea.lev.plot <- sea.rise %>% 
  ggplot(aes(x=year, y=mean.sl)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(x = "Year", y = "Sea level\n(m)")



# plot_grid(ann.temp.plot, dec.temp.plot, mud.plot, urb.plot, sea.lev.plot, 
#           nrow=3, labels=c('a', 'b', 'c', 'd', 'e'), align = "none")

first <- plot_grid(ann.temp.plot, dec.temp.plot, nrow = 1, labels = c('a', 'b'))
second <- plot_grid(mud.plot, urb.plot, nrow = 1, labels=c('c', 'd'))
third <- plot_grid(sea.lev.plot, nrow = 1, labels = 'e')

plot_grid(first, second, third, nrow = 3, rel_heights = c(1,1,1.5))


ggsave("outputs/regional/forpub/envir_var_table.png", height = 8.5, width = 11)






