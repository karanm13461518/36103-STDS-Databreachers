library(tidyverse)
library(usmap)
library(openintro)
library(viridis)

df_Merged_final <- read_csv("CSV_EDA/20190919MergedData__mdStock.csv")
df_temp <- read_csv("data/MergedData_InitialOutput.csv")

####Themes
#Special theme for the map - no extra features shown on plot other than legend
theme_Custom_USMap <- theme_void()
#General graphs - can be modified by adding additional components to the theme either here or using the "+" method (e.g. new_Theme <- theme_Custom_USMap + theme_Custom_GeneralGraphs)
theme_Custom_GeneralGraphs <- theme(panel.background = element_blank(),
                                    axis.line = element_line(colour = "black"),
                                    axis.text = element_text(colour = "black"),
                                    plot.title = element_text(size = rel(2), hjust = 0.5))  #hjust is relative to the graph only

#Map plot generation
#Separate dataframe for the map plot only
df_Merged_final$StateAbbriv <- df_Merged_final$State
df_Merged_final$StateAbbriv[which(!is.na(df_Merged_final$StateAbbriv))] <- state2abbr(df_Merged_final$StateAbbriv[which(!is.na(df_Merged_final$StateAbbriv))])
#Morphing data
df_Map_Plot <- df_Merged_final[, c("StateAbbriv", "BreachType")]
df_Map_Plot <- df_Map_Plot %>% 
  filter(!is.na(StateAbbriv)) %>%
  group_by(StateAbbriv) %>%
  summarise(BreachesByState = n())
df_Map_Plot$fips <- fips(df_Map_Plot$StateAbbriv)

#The actual map plotting function
Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap) + scale_fill_viridis_c() + labs(fill = "Breaches")
Map_Ouput

#Plotter saving
ggsave("USMapPlot.jpg", plot = Map_Ouput, device = "jpeg") #use a combination of variables width, height, and unit to define size of image as width and height in specified unit (in, cm, or mm)
