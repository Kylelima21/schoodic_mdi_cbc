#Regional CBC analysis species trend visualization (GIF)
#Schoodic Institute at Acadia National Park 2023

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse) 
library(ggplot2)
library(gganimate)



#------------------------------------------------#
####          Read in Required Data           ####
#------------------------------------------------#

fulldat <- tibble(read.csv("outputs/cbc_alldata_20220415.csv"))



#------------------------------------------------#
####            Write the Function            ####
#------------------------------------------------#

species_trend_gif <- function(species, trend) {
  
  ## Check for erroneous inputs for trend
  # if(trend != "increase" | trend != "decrease" | trend != "stable") {
  #   stop("Incorrect input for trend. Needs to be 1 of 3: 'increase', 'decrease', 'stable'.")
  # }
  
  
  ## Select color based on trend from CBC paper
  if(trend == "increase") {
    line.color <- "#7EAB55"
  }
  
  if(trend == "stable") {
    line.color <- "#4F71BE"
  }
  
  if(trend == "decrease") {
    line.color <- "#DE8344"
  }
  
  
  ## Check for correct species inputs
  if(species %in% fulldat$CommonName == "FALSE") {
    stop("Invalid species input. Please check your spelling, capitalization, or this species isn't inlcuded in the study.")
  }
  
    
  ## Create the plot
  p <- fulldat %>% 
    filter(CommonName == paste(species)) %>% 
    select(CommonName, Year, CountPartyHour) %>% 
    ggplot(aes(x=Year)) +
    scale_y_continuous(name = "Count/party hour") +
    labs(title = paste(species), subtitle = "Mount Desert Island and Schoodic Point CBC data (1971 - 2021)") +
    theme_bw() +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(color = "gray40", size = 11),
          axis.text = element_text(color = "black", size = 11),
          axis.title = element_text(size = 11),
          axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
          axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
          panel.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          legend.position = "none")
  
  
  ## Define the animation
  a <- p + 
    geom_line(aes(y=CountPartyHour), color = paste(line.color), size = 0.8) +
    # geom_label(aes(x = 2014, y = 2, label = round(fulldat$Year, digits = 0)), fontface = "bold",  label.size = 0,
    #            vjust = 2) +
    transition_reveal(Year)
  
  
  ## Animate and save the gif
  animate(a, width = 5.28, height = 3.75, unit = "in", res = 350, end_pause = 30) 
  anim_save(paste0("outputs/trend_animations/", str_replace(tolower(species), "\\s", "_"), "_trend.gif"))

}
  


#------------------------------------------------#
####          Produce species GIFs            ####
#------------------------------------------------#

## Run GIFs for the species selected for 2023 bird stories
species_trend_gif("Common Eider", "decrease")
species_trend_gif("American Tree Sparrow", "decrease")
species_trend_gif("Wild Turkey", "increase")
species_trend_gif("Boreal Chickadee", "decrease")
species_trend_gif("American Black Duck", "stable")
species_trend_gif("Herring Gull", "stable")
species_trend_gif("Blue Jay", "decrease")
species_trend_gif("American Crow", "increase")
species_trend_gif("Dark-eyed Junco", "stable")
species_trend_gif("Harlequin Duck", "increase")
species_trend_gif("Red-breasted Nuthatch", "stable")










# p <- fulldat %>% 
#   filter(CommonName == "American Tree Sparrow") %>% 
#   select(CommonName, Year, CountPartyHour) %>% 
#   ggplot(aes(x=Year)) +
#   scale_y_continuous(name="Count/party hour") +
#   labs(title = "American Tree Sparrow", subtitle = "Mount Desert Island and Schoodic Point CBC data (1971 - 2021)") +
#   theme_bw() +
#   theme(plot.title.position = "plot",
#         plot.title = element_text(face = "bold", size = 16),
#         plot.subtitle = element_text(color = "gray40", size = 11),
#         axis.text = element_text(color = "black", size = 11),
#         axis.title = element_text(size = 11),
#         axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
#         axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
#         panel.background = element_blank(), 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         legend.position = "none")
# 
# a <- p + 
#   geom_line(aes(y=CountPartyHour), color = "#9d4025", size = 0.8) +
#   # geom_label(aes(x = 2014, y = 2, label = round(fulldat$Year, digits = 0)), fontface = "bold",  label.size = 0,
#   #            vjust = 2) +
#   transition_reveal(Year)
# 
# animate(a, width = 5.28, height = 3.75, unit = "in", res = 350)
# anim_save("outputs/trend_animations/atsp_trend.gif")



