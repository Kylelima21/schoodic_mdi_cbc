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
require(broom)




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
#tidal.area <- read.csv("data/tidal_area.csv") %>% as_tibble()
urban.area <- read.csv("data/urban_area.csv") %>% as_tibble()
sea.level <- read_delim("data/tide_rawdata.txt", delim = ";", col_names = 
             c("yearday", "mean.sl", "xa",  "xb")) #Can probably use this instead of mudflat area




#------------------------------------------------#
####            Data manipulation             ####
#------------------------------------------------#

##Annual precipitation -- unit = cm
#Pivot data into long format
precip.changes <- precip.change %>%
  mutate(year = as.numeric(str_replace(date, "\\d\\d$", "")),
         month = str_replace(date, "^\\d\\d\\d\\d", "")) %>% 
  select(-date) %>% 
  select(year, month, precip) %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  summarise(mean.precip = sum(precip)) %>% 
  mutate(mean.precip = inches_to_metric(mean.precip, round = 2, unit = "cm"))

precip.changes %>% 
  ggplot(aes(x=year, y=mean.precip)) +
  geom_point() +
  geom_smooth(method = "lm")




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
  filter(year!=1981) %>% 
  mutate(temp = ifelse(temp.k >= 0, temp.k / 273.15, temp.k / 273.15),
         temp.f = kelvin.to.fahrenheit(temp, round = 2))

#Convert units to Fahrenheit
ocean.temp$temp <- kelvin.to.fahrenheit(ocean.temp$temp.k, round = 2)




##Annual temperature -- unit = C
#Pivot data into long format
temp.changes <- temp.change %>%
  mutate(year = as.numeric(str_replace(date, "\\d\\d$", "")),
         month = str_replace(date, "^\\d\\d\\d\\d", "")) %>% 
  select(-date) %>% 
  select(year, month, temp) %>% 
  filter(year < 2022) %>% 
  mutate(temp = fahrenheit.to.celsius(temp, round = 2))

#Take annual means
ann.temp <- temp.changes %>% 
  group_by(year) %>% 
  summarise(mean.temp = mean(temp))

ann.temp %>% 
  ggplot(aes(x=year, y=mean.temp)) +
  geom_point() +
  geom_smooth(method = "lm")




##December temperature changes -- unit = C
#Use the temp.changes df
#Clean and simplify to get December only data by year
dec.temp <- temp.changes %>% 
  filter(month == 12)

dec.temp %>% 
  ggplot(aes(x=year, y=temp)) +
  geom_point() +
  geom_smooth(method = "lm")




##December minimum temperature -- units = C
#Clean to get December mins for each year
dec.mins <- dec.min.temp %>%
  mutate(year = as.numeric(str_replace(date, "\\d\\d$", "")),
         month = str_replace(date, "^\\d\\d\\d\\d", ""),
         temp = fahrenheit.to.celsius(temp, round = 2)) %>% 
  select(-date) %>% 
  select(year, month, temp) %>% 
  filter(year < 2022)

dec.mins %>% 
  ggplot(aes(x=year, y=temp)) +
  geom_point() +
  geom_smooth(method = "lm")




# ##Tidal area change over time
# #Pivot to long format
# tide.zone <- tidal.area %>%
#   select(-c(system.index, `.geo`)) %>% 
#   pivot_longer(X1984.1986_classification:X2014.2016_classification)
# 
# #Get year and month out of the name column
# tide.zone[c('dat', 'trash')] <- str_split_fixed(tide.zone$name, '_', 2)
# tide.zone[c('trash2', 'year')] <- str_split_fixed(tide.zone$dat, 'X', 2)
# 
# #Clean and simplify to get December only data by year
# mudflat.area <- tide.zone %>% 
#   select(name, year, value) %>% 
#   rename(area = value) %>% 
#   mutate(year = str_replace(year, "\\.\\d*$", ""),
#          year = as.numeric(year)) %>% 
#   select(-name)
# 
# mudflat.area %>% 
#   ggplot(aes(x=year, y=area)) +
#   geom_point() +
#   geom_smooth()




##Urban area change over time -- units = square kilometers
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
  mutate(year = as.numeric(year),
         area = area/100)

urbanized.area %>% 
  ggplot(aes(x=year, y=area)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black")




##Sea level change -- units = mm
#Clean
sea.rise <- sea.level %>% 
  select(-c(xa, xb)) %>% 
  mutate(yearday = trimws(yearday),
         mean.sl = trimws(mean.sl),
         mean.sl = as.numeric(mean.sl),
         year = as.integer(yearday)) %>% 
         #mean.sl = mean.sl/1000) %>% 
  filter(year > 1970 & mean.sl > 0) %>% 
  group_by(year) %>% 
  summarise(mean.sl = mean(mean.sl))

sea.rise %>% 
  ggplot(aes(x=year, y=mean.sl)) +
  geom_point() +
  geom_smooth(method = "lm")




#------------------------------------------------#
####            Statistical tests             ####
#------------------------------------------------#

##Annual mean precipitation change over time
precip.r <- precip.changes %>% 
  summarise(reg.results = lm(mean.precip ~ year, data = .) %>% tidy()) %>% 
  filter(reg.results$term == "year") %>% 
  mutate(env.var = "Annual precipitation",
         est = reg.results$estimate,
         p.value = reg.results$p.value) %>% 
  select(env.var, est, p.value)

#R2 = 0.002, F(1, 49) = 0.075, p = 0.785



##Annual mean temp change over time
ann.temp.r <- ann.temp %>% 
  summarise(reg.results = lm(mean.temp ~ year, data = .) %>% tidy()) %>% 
  filter(reg.results$term == "year") %>% 
  mutate(env.var = "Mean annual temp.",
         est = reg.results$estimate,
         p.value = reg.results$p.value) %>% 
  select(env.var, est, p.value)

#R2 = 0.303, F(1, 49) = 21.29, p < 0.001*



##December mean temp change over time
dec.temp.r <- dec.temp %>% 
  summarise(reg.results = lm(temp ~ year, data = .) %>% tidy()) %>% 
  filter(reg.results$term == "year") %>% 
  mutate(env.var = "Mean December temp.",
         est = reg.results$estimate,
         p.value = reg.results$p.value) %>% 
  select(env.var, est, p.value)

#R2 = 0.095, F(1, 49) = 5.134, p = 0.028*



##December min temp change over time
dec.min.r <- dec.mins %>% 
  summarise(reg.results = lm(temp ~ year, data = .) %>% tidy()) %>% 
  filter(reg.results$term == "year") %>% 
  mutate(env.var = "Minimum December temp.",
         est = reg.results$estimate,
         p.value = reg.results$p.value) %>% 
  select(env.var, est, p.value)

#R2 = 0.113, F(1, 49) = 6.211, p = 0.016*



# ##Mudflat area change over time
# summary(lm(area~year, data = mudflat.area))
# 
# #R2 = 0.496, F(1, 9) = 8.869, p = 0.016*



##Urban area change over time
urban.r <- urbanized.area %>% 
  summarise(reg.results = lm(area ~ year, data = .) %>% tidy()) %>% 
  filter(reg.results$term == "year") %>% 
  mutate(env.var = "Impervious land cover",
         est = reg.results$estimate,
         p.value = reg.results$p.value) %>% 
  select(env.var, est, p.value)

#R2 = 0.976, F(1, 6) = 248.5, p < 0.001*



##Sea level change over time
sea.rise.r <- sea.rise %>% 
  summarise(reg.results = lm(mean.sl ~ year, data = .) %>% tidy()) %>% 
  filter(reg.results$term == "year") %>% 
  mutate(env.var = "Sea level",
         est = reg.results$estimate,
         p.value = reg.results$p.value) %>% 
  select(env.var, est, p.value)

#R2 = 0.653, F(1, 46) = 86.65, p < 0.001*




#------------------------------------------------#
####            Env. Var. Table               ####
#------------------------------------------------#

#Compile results into one table and clean
env.tab <- precip.r %>% 
  bind_rows(ann.temp.r, dec.temp.r, dec.min.r, urban.r, sea.rise.r) %>% 
  mutate(supported = ifelse(p.value > 0.05, "no", "yes"),
         est = round(est, digits = 2))

#write.csv(env.tab, "outputs/regional/forpub/envir_var_table.csv", row.names = F)




#------------------------------------------------#
####             Make a cowplot               ####
#------------------------------------------------#

ann.precip.plot <- precip.changes %>% 
  ggplot(aes(x=year, y=mean.precip)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "black") +
  theme_classic() +
  labs(y = "Mean annual\nprecipiation (in)") +
  theme(axis.title.x = element_blank())

ann.temp.plot <- ann.temp %>% 
  ggplot(aes(x=year, y=mean.temp)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Mean annual\ntemperature (F)") +
  theme(axis.title.x = element_blank())

dec.temp.plot <- dec.temp %>% 
  ggplot(aes(x=year, y=temp)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Mean December\ntemperature (F)") +
  theme(axis.title.x = element_blank())

dec.min.plot <- dec.mins %>% 
  ggplot(aes(x=year, y=temp)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Min December\ntemperature (F)") +
  theme(axis.title.x = element_blank())

mud.plot <- mudflat.area %>% 
  ggplot(aes(x=year, y=area)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Percent cover\nof mudflat") +
  theme(axis.title.x = element_blank())

urb.plot <- urbanized.area %>%
  mutate(area = area) %>% 
  ggplot(aes(x=year, y=area)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(y = "Urbanized area\n(???)") +
  theme(axis.title.x = element_blank())

sea.lev.plot <- sea.rise %>% 
  ggplot(aes(x=year, y=mean.sl)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "white") +
  geom_smooth(method = "lm", color = "red") +
  theme_classic() +
  labs(x = "Year", y = "Sea level\n(m)")


# plot_grid(ann.temp.plot, dec.temp.plot, mud.plot, urb.plot, sea.lev.plot, 
#           nrow=3, labels=c('a', 'b', 'c', 'd', 'e'), align = "none")

first <- plot_grid(ann.precip.plot, ann.temp.plot, nrow = 1, labels = c('a', 'b'))
second <- plot_grid(dec.temp.plot, dec.min.plot, nrow = 1, labels = c('c', 'd'))
third <- plot_grid(mud.plot, urb.plot, nrow = 1, labels=c('e', 'f'))
fourth <- plot_grid(sea.lev.plot, nrow = 1, labels='g')


plot_grid(first, second, third, fourth, nrow = 4, rel_heights = c(1,1,1,1.5), align = "v")


ggsave("outputs/regional/forpub/envir_var_table.png", height = 8.5, width = 11)






