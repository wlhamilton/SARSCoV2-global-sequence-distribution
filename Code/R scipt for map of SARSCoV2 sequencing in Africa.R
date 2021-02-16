############ R script for SARSCoV2 sequences plotted on maps

# Based on: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# City latitude/ longitude data from https://simplemaps.com/data/world-cities

# Set wd
setwd("~/Documents/science/covid19/analysis/global_gisaid_seq")

# Load packages
library("tidyverse")

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
theme_set(theme_bw())

# Import data
seq_demographic_data <- read.csv("sequence_counts_with_population_GDP_income_continets.csv")
world_citi_data <- read.csv("worldcities.csv")

# Manipulations
colnames(world_citi_data)
head(world_citi_data)

world_citi_sub <- world_citi_data %>%
  filter(capital == "primary") %>%
  select(country, city, lat, lng) %>%
  rename(capital_city = city,
         latitude = lat,
         longitude = lng)

dim(world_citi_data)
dim(world_citi_sub)

# Changes so country name formats match the seq data

world_citi_ed <- world_citi_sub %>%
  mutate(country = str_replace(country, "Côte D’Ivoire", "Cote d'Ivoire")) %>%
  mutate(country = str_replace(country, "Gambia, The", "Gambia")) %>%
  mutate(country = str_replace(country, "Saint Kitts And Nevis", "Saint Kitts and Nevis")) %>%
  mutate(country = str_replace(country, "Congo [(]Brazzaville[)]", "Republic of the Congo")) %>%
  mutate(country = str_replace(country, "Saint Vincent And The Grenadines", "Saint Vincent and the Grenadines")) %>%
  mutate(country = str_replace(country, "Congo [(]Kinshasa[)]", "Democratic Republic of the Congo")) %>%
  mutate(country = str_replace(country, "Bahamas, The", "Bahamas")) %>%
  mutate(country = str_replace(country, "Burma", "Myanmar")) %>%
  mutate(country = str_replace(country, "Trinidad And Tobago", "Trinidad and Tobago")) %>%
  mutate(country = str_replace(country, "Czechia", "Czech Republic")) %>%
  mutate(country = str_replace(country, "Cabo Verde", "Cape Verde")) %>%
  mutate(country = str_replace(country, "Korea, North", "North Korea")) %>%
  mutate(country = str_replace(country, "Sao Tome And Principe", "Sao Tome and Principe")) %>%
  mutate(country = str_replace(country, "Antigua And Barbuda", "Antigua and Barbuda")) %>%
  mutate(country = str_replace(country, "Bosnia And Herzegovina", "Bosnia and Herzegovina")) %>%
  mutate(country = str_replace(country, "Korea, South", "South Korea")) %>%
  mutate(country = str_replace(country, "Macedonia", "North Macedonia")) %>%
  mutate(country = str_replace(country, "United States", "USA")) %>%
  mutate(country = str_replace(country, "Timor-Leste", "Timor")) %>%
  mutate(country = str_replace(country, "West Bank", "Palestine")) %>%
  mutate(country = str_replace(country, "Swaziland", "Eswatini")) %>%
  filter(country != "Kosovo") %>%
  filter(country != "Micronesia, Federated States Of") %>%
  filter(capital_city != "Cotonou") %>%
  filter(capital_city != "La Paz") %>%
  filter(capital_city != "Bujumbura") %>%
  filter(capital_city != "Abidjan") %>%
  filter(capital_city != "Lobamba") %>%
  filter(capital_city != "Nay Pyi Taw") %>%
  filter(capital_city != "The Hague") %>%
  filter(capital_city != "Cape Town") %>%
  filter(capital_city != "Bloemfontein") %>%
  filter(capital_city != "Sri Jayewardenepura Kotte") %>%
  filter(capital_city != "Dodoma")
  
## Merge
seq_geo_data <- merge(seq_demographic_data,
                      world_citi_ed,
                      by="country",
                      all.x=T,all.y=F)
#write.csv(seq_geo_data, "sequence_counts_with_population_GDP_income_continets_geospatial.csv", row.names=F)

dim(seq_demographic_data)
dim(seq_geo_data)



##### Plotting data with maps

######### Africa
# Prepare data for plotting
data_plot <- seq_geo_data %>%
  filter(continent == "Africa") %>%
  rename(Continent = continent,
         Income.group = income_group)

data_plot$latitude <- as.numeric(data_plot$latitude)
data_plot$longitude <- as.numeric(data_plot$longitude)

# Move Kinshasa and Brazzaville apart a bit or they are hard to distinguish
data_plot_ed <- data_plot %>%
  mutate(longitude = ifelse(data_plot$country=="Democratic Republic of the Congo",
                            longitude+1,
                            longitude)) %>%
  mutate(longitude = ifelse(data_plot$country=="Republic of the Congo",
                            longitude-1,
                            longitude))

# Test data - to sense check latitude/ longitude 
country <- c("DRC", "Kenya", "Gambia", "South Africa")
longitude <- c(25, 38, -15, 25)
latitude <- c(-13, 0, 13, -30)
sequence_count <- c(371, 679, 430, 3418)
Income.group <- c("Low income", "Lower-middle income", "Low income", "Upper-middle income")
data_plot_test <- data.frame(country,latitude,longitude,sequence_count,Income.group)


### Prepare the map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf()

### Try out with test data
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-20, 53), ylim = c(-36, 38.5), expand = FALSE) +
  geom_point(data=data_plot_test, aes(x=latitude, y=longitude, size=sequence_count, color=Income.group)) +
  scale_size(range = c(1, 20), breaks = c(0,3000), name="Sequences") +
  scale_color_brewer(palette="Set2")
  
### Using real data
pmap <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-20, 53), ylim = c(-36, 38.5), expand = FALSE) +
  geom_point(data=data_plot_ed, aes(x=longitude, y=latitude, size=sequence_count, color=Income.group), alpha=0.85) +
  scale_size(range = c(1, 28), breaks = c(1,300,3000), name="Sequences") +
  scale_color_brewer(palette="Set2")
pmap
#ggsave(plot=pmap, file="Africa_SARSCoV2_sequence_map.png", height=7, width=8, units="in", dpi=300)
#ggsave(plot=pmap, file="Africa_SARSCoV2_sequence_map.pdf", height=7, width=8, units="in", dpi=300)


######### Global dataset
pmap2 <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-160, 179.5), ylim = c(-55, 80), expand = FALSE) +
  geom_point(data=seq_geo_data, aes(x=longitude, y=latitude, size=sequence_count, color=income_group), alpha=0.75) +
  scale_size(range = c(1, 18), breaks = c(1,1000,100000), name="Sequences") +
  scale_color_brewer(palette="Set2")
pmap2
#ggsave(plot=pmap2, file="global_SARSCoV2_sequence_map.png", height=6, width=12, units="in", dpi=300)
#ggsave(plot=pmap2, file="global_SARSCoV2_sequence_map.pdf", height=6, width=12, units="in", dpi=300)

