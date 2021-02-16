############## Plotting sequences by Nextstrain region

# See Readme for info on data source

# Set wd
setwd("~/Documents/science/covid19/analysis/global_gisaid_seq")

# Load packages
library("tidyverse")

# Import data
data <- read.csv("gisaid_metadata_20210212.csv")

# Check the data out
colnames(data)
dim(data)

head(data)

# Manipulations
data_v1 <- data %>%
  select(gisaid_epi_isl, date_submitted, region, country, age, sex, Nextstrain_clade, pangolin_lineage, GISAID_clade)

data_v1$epi_week <- as.numeric (strftime(data_v1$date, format = "%V") )
data_v1$year <- as.Date(data_v1$date, format = "%d/%m/%Y")
data_v1$year <- as.numeric(format(data_v1$year,'%Y'))

unique(data_v1$year)

data_v1 <- data_v1 %>%
  arrange(region, epi_week)

head(data_v1)
  
unique(data_v1$region)
unique(data_v1$country)

data_v1$continent_UK <- data_v1$region
data_v2 <- data_v1 %>%
  mutate(continent_UK = ifelse (data_v1$country == "United Kingdom",
                                "United Kingdom",
                                data_v1$continent_UK))

unique(data_v2$continent_UK)


######## Basic counts

### UK samples in GISAID

# Basics - how many sequences, how many UK
data_UK <- data_v2 %>% filter(country == "United Kingdom")

all_sample_n <- as.numeric(nrow(data_v2))
UK_sample_n <- as.numeric(nrow(data_UK))
UK_sample_prop <- UK_sample_n / all_sample_n
UK_sample_pcnt <- signif(UK_sample_prop * 100, 3)

print(paste("Total samples in GISAID database: ", all_sample_n, sep=""))
print(paste("UK samples in GISAID database: ", UK_sample_n, sep=""))
print(paste("% of GISAID samples from UK: ", UK_sample_pcnt, "%", sep=""))

data_v2 %>% group_by(continent_UK) %>% tally()

### Biggest countries per continent
unique(data_v2$region)
data_africa <- data_v2 %>% filter(region == "Africa")
data_asia <- data_v2 %>% filter(region == "Asia")
data_europe <- data_v2 %>% filter(region == "Europe")
data_namerica <- data_v2 %>% filter(region == "North America")
data_samerica <- data_v2 %>% filter(region == "South America")
data_oceania <- data_v2 %>% filter(region == "Oceania")

# Proportion of sequences from Africa
Africa_sample_n <- nrow(data_africa)
Africa_pcnt <- signif((Africa_sample_n / all_sample_n * 100), 3)
Africa_sample_n
total_sample_n
Africa_pcnt

## country counts per continent
# Africa
country_count_africa <- data.frame (data_africa %>% group_by(country) %>% tally()) %>%
  rename(count = n) %>%
  arrange(desc(count))
country_count_africa
top_country_africa <- country_count_africa[1,1]

# Asia
country_count_asia <- data.frame (data_asia %>% group_by(country) %>% tally()) %>%
  rename(count = n) %>%
  arrange(desc(count))
country_count_asia
top_country_asia <- country_count_asia[1,1]

# Europe
country_count_europe <- data.frame (data_europe %>% group_by(country) %>% tally()) %>%
  rename(count = n) %>%
  arrange(desc(count))
country_count_europe
top_country_europe <- country_count_europe[1,1]

# N America
country_count_namerica <- data.frame (data_namerica %>% group_by(country) %>% tally()) %>%
  rename(count = n) %>%
  arrange(desc(count))
country_count_namerica
top_country_namerica <- country_count_namerica[1,1]

# S America
country_count_samerica <- data.frame (data_samerica %>% group_by(country) %>% tally()) %>%
  rename(count = n) %>%
  arrange(desc(count))
country_count_samerica
top_country_samerica <- country_count_samerica[1,1]

# Oceania
country_count_oceania <- data.frame (data_oceania %>% group_by(country) %>% tally()) %>%
  rename(count = n) %>%
  arrange(desc(count))
country_count_oceania
top_country_oceania <- country_count_oceania[1,1]

## Create column highlighting the top countries in each continent
# Africa
data_africa$continent_hilight <- data_africa$region
data_africa <- data_africa %>%
  mutate(continent_hilight = ifelse (data_africa$country == top_country_africa,
                                     top_country_africa,
                                     "Other Africa"))

# Asia
data_asia$continent_hilight <- data_asia$region
data_asia <- data_asia %>%
  mutate(continent_hilight = ifelse (data_asia$country == top_country_asia,
                                     top_country_asia,
                                     "Other Asia"))
# Europe
data_europe$continent_hilight <- data_europe$region
data_europe <- data_europe %>%
  mutate(continent_hilight = ifelse (data_europe$country == top_country_europe,
                                     top_country_europe,
                                     "Other Europe"))
# N America
data_namerica$continent_hilight <- data_namerica$region
data_namerica <- data_namerica %>%
  mutate(continent_hilight = ifelse (data_namerica$country == top_country_namerica,
                                     top_country_namerica,
                                     "Other N America"))
# S America
data_samerica$continent_hilight <- data_samerica$region
data_samerica <- data_samerica %>%
  mutate(continent_hilight = ifelse (data_samerica$country == top_country_samerica,
                                     top_country_samerica,
                                     "Other S America"))
# Oceania
data_oceania$continent_hilight <- data_oceania$region
data_oceania <- data_oceania %>%
  mutate(continent_hilight = ifelse (data_oceania$country == top_country_oceania,
                                     top_country_oceania,
                                     "Other Oceania"))

## Merge back together
data_all <- rbind(data_africa,
                  data_asia,
                  data_europe,
                  data_namerica,
                  data_samerica,
                  data_oceania)
nrow(data_all)


###### Plots
colnames(data_all)
region_order <- c("Africa", "Asia", "Europe", "N America", "S America", "Oceania")
mycol_2 <- c("#4682b4", "#dae6f0")

data_plot_p1 <- data_all %>%
  group_by(region) %>%
  count(continent_hilight) %>%
  rename(count = n) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "South Africa", "Majority country")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Japan", "Majority country")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "United Kingdom", "Majority country")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "USA", "Majority country")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Brazil", "Majority country")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Australia", "Majority country")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Other Africa", "Other")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Other Asia", "Other")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Other Europe", "Other")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Other N America", "Other")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Other S America", "Other")) %>%
  mutate(continent_hilight = str_replace(continent_hilight, "Other Oceania", "Other")) %>%
  mutate(region = str_replace(region, "North America", "N America")) %>%
  mutate(region = str_replace(region, "South America", "S America")) %>%
  mutate(count = count/1000)
  
data_plot_p1$region <- factor(data_plot_p1$region, levels = region_order)

p1 <- ggplot(data=data_plot_p1, aes(x=region, y=count, fill=continent_hilight)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  scale_fill_manual(values=mycol_2) +
  xlab("Nextstrain region") +
  ylab("Sequence count (thousands)") + 
  theme(axis.text=element_text(size=11),
        axis.text.x=element_text(angle=30),
        axis.title=element_text(size=12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10)) +
  guides(fill=guide_legend(title="Breakdown"))
p1
#ggsave(plot=p1, file="GISAID_sequence_counts_byRegion_major_country_20210215.png", width=7, height=4, units="in", dpi=300)
#ggsave(plot=p1, file="GISAID_sequence_counts_byRegion_major_country_20210215.pdf", width=7, height=4, units="in", dpi=300)

# Majority countries =
# Africa = South Africa
# Asia = Japan
# Europe = United Kingdom
# N America = USA
# S America = Brazil
# Oceania = Australia




##### By time
colnames(data_all)

# Just looking at the year 2020
data_plot_p2 <- data_all %>%
  filter(year == 2020) %>%
  group_by(epi_week) %>%
  count(region) %>%
  rename(count = n)

p2 <- ggplot(data=data_plot_p2, aes(x=epi_week, y=count, fill=region)) +
  geom_bar(stat="identity")
p2


