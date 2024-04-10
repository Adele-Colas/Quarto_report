install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("plotly")
library(tidyverse)
library(RColorBrewer)
library(plotly)

# import dataset

unicef_metadata <- read_delim("unicef_metadata.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
unicef_indicator_1 <- read_delim("unicef_indicator_1.csv", 
                                 delim = ";", escape_double = FALSE, col_types = cols(year = col_double()), trim_ws = TRUE)
localisation <- read_delim("localisation.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

# join data

data_join <- unicef_metadata %>%
  left_join(unicef_indicator_1) %>%
  left_join(localisation)

# World map of 1960's life expectancy 

indicator_data_1960 <- unicef_metadata %>%
  filter(year==1960)

map_world <- map_data("world")
map_data_join <- full_join(indicator_data_1960,map_world, by=c("country"="region"))

ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill=lifeExp) +
  geom_polygon() +
  labs(title = "Life Expectancy (1960)", fill = "Life Expectancy") +
  scale_fill_distiller(palette = "Spectral",direction = 1)
options(scipen=999)

# World map of 2020's life expectancy 

indicator_data_2020 <- unicef_metadata %>%
  filter(year==2020)

map_world <- map_data("world")
map_data_join_2 <- full_join(indicator_data_2020,map_world, by=c("country"="region"))

ggplot(map_data_join_2) +
  aes(x = long, y = lat, group = group, fill=lifeExp) +
  geom_polygon() +
  labs(title = "Life Expectancy (2020)", fill = "Life Expectancy") +
  scale_fill_distiller(palette = "Spectral",direction = 1)
options(scipen=999)

# Evolution of life expectancy by continent (timeseries)

timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(year, lifeExp, group=country, color=Region) +
  geom_line()

ggplotly(timeseries_plot_1)

# Evolution of the relation between Life expectancy and Limited access to handwashing facility (scatterplot)

data_filtered <- subset(data_join, year %in% c(2000, 2004, 2008, 2012, 2016, 2020))

ggplot(data_filtered) +
  aes(prop_of_pop_with_limited_handwashing_facility, lifeExp, color=Region,size=prop_of_pop_with_limited_handwashing_facility) +
  geom_point(alpha=0.5) +
  facet_wrap(~year) +
  scale_x_continuous(limits= c(0, 100), breaks = c(20, 40, 60,80), labels = c("20%", "40%", "60%","80%")) +
  labs(x = "Pop Proportion", y = "Life Expectancy", 
       fill = "Region", 
       size = "Proportion", 
       title= "Evolution of the relation between Life expectancy and Limited access to handwashing facility") +
  theme_classic() +
  theme(text= element_text(family= "serif"))

# Average life expectancy per region

data_join %>%
  group_by(Region, year) %>%
  summarize(m_lifeExp = (mean(lifeExp, na.rm = TRUE))) %>%
  ggplot() +
  aes(Region, m_lifeExp, fill= Region) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title= "Average life expectancy per region")
  

# Average proportion of population with limited access to hand washing facilities per region
data_join %>%
  group_by(Region, year) %>%
  summarize(m_Pop_Proportion = (mean(prop_of_pop_with_limited_handwashing_facility, na.rm = TRUE))) %>%
  ggplot() +
  aes(Region, m_Pop_Proportion, fill= Region) +
  geom_col() +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()

  