############ Merging GISAID sequence metadata with geographic data from Our World In Data

# See readme for info on source of the datasets used here

# Set wd
setwd("~/Documents/science/covid19/analysis/global_gisaid_seq")

# Load packages
library("tidyverse")
library("plotly")
library("hrbrthemes")

# Import data
seq_data <- read.csv("gisaid_metadata_20210212.csv")
pop_data <- read.csv("owid_world_population.csv")
gdp_data <- read.csv("owid_real_gdp_per_capita.csv")
income_data <- read.csv("owid_world_bank_income_groups.csv")
region_names <- read.csv("owid_world_region_names.csv")

### Prepare sequence count data per country
seq_data_v1 <- seq_data %>%
  select(gisaid_epi_isl, date_submitted, region, country) %>%
  rename(nextstrain_region = region)

seq_data_v1$epi_week <- as.numeric (strftime(seq_data_v1$date, format = "%V") )
seq_data_v1$year <- as.Date(seq_data_v1$date, format = "%d/%m/%Y")
seq_data_v1$year <- as.numeric(format(seq_data_v1$year,'%Y'))

unique(seq_data_v1$year)

colnames(seq_data_v1)
seq_data_v2 <- seq_data_v1 %>%
  group_by(country) %>%
  tally() %>%
  rename(sequence_count = n)
seq_data_v2

## Alterations to make consistent with OWID data and remove special characters:
#print(seq_data_v2, n=250)
seq_data_v3 <- seq_data_v2 %>%
  mutate(country = str_replace(country, "Saint Barthélemy", "Saint Barthlemy")) %>%
  mutate(country = str_replace(country, "Côte d'Ivoire", "Cote d'Ivoire")) %>%
  filter(country != "Kosovo") %>%
  filter(country != "Saint Martin") %>%
  filter(country != "Timor-Leste")
seq_data_v3



### Prepare population counts - using year 2016

# Format year as numeric
pop_data$Year <- as.numeric(pop_data$Year)
gdp_data$Year <- as.numeric(gdp_data$Year)
income_data$Year <- as.numeric(income_data$Year)

# Select year to use for data
chosen_year = 2016

## Select chosen year for demographic variables

# Population
colnames(pop_data)
pop_data_yr <- pop_data %>%
  filter(Year == chosen_year) %>%
  select(Entity, Total.population..Gapminder..HYDE...UN.) %>%
  rename(country = Entity, # Note - not all OWID Entities are countries
         population = Total.population..Gapminder..HYDE...UN.) %>%
  mutate(country = str_replace(country, "United States", "USA")) %>%
  mutate(country = str_replace(country, "Czechia", "Czech Republic")) %>%
  mutate(country = str_replace(country, "Congo", "Republic of the Congo")) %>%
  mutate(country = str_replace(country, "Democratic Republic of Republic of the Congo", "Democratic Republic of the Congo")) %>%
  filter(country != "Africa") %>%
  filter(country != "Asia") %>%
  filter(country != "Europe") %>%
  filter(country != "Oceania") %>%
  filter(country != "World") %>%
  filter(country != "Latin America") %>%
  filter(country != "North America")
nrow(pop_data_yr)
head(pop_data_yr)

# GDP per capita
colnames(gdp_data)
gdp_data_yr <- gdp_data %>%
  filter(Year == chosen_year) %>%
  select(Entity, Real.GDP.per.capita.in.2011US...multiple.benchmarks..Maddison.Project.Database..2018..) %>%
  rename(country = Entity, # Note - not all OWID Entities are countries
         gdp_per_capita = Real.GDP.per.capita.in.2011US...multiple.benchmarks..Maddison.Project.Database..2018..) %>%
  mutate(country = str_replace(country, "United States", "USA")) %>%
  mutate(country = str_replace(country, "Czechia", "Czech Republic")) %>%
  mutate(country = str_replace(country, "Congo", "Republic of the Congo")) %>%
  mutate(country = str_replace(country, "Democratic Republic of Republic of the Congo", "Democratic Republic of the Congo")) %>%
  filter(country != "Africa") %>%
  filter(country != "Asia") %>%
  filter(country != "Europe") %>%
  filter(country != "Oceania") %>%
  filter(country != "World") %>%
  filter(country != "Latin America") %>%
  filter(country != "North America") %>%
  filter(country != "East Asia") %>%
  filter(country != "Former Yugoslavia") %>%
  filter(country != "Former USSR") %>%
  filter(country != "Eastern Europe") %>%
  filter(country != "Western Asia") %>%
  filter(country != "Czechoslovakia") %>%
  filter(country != "Western Europe") %>%
  filter(country != "Western Offshoots")
nrow(gdp_data_yr)
head(gdp_data_yr)

# Income groups
colnames(income_data)
income_data_yr <- income_data %>%
  filter(Year == chosen_year) %>%
  select(Entity, Income.classifications..World.Bank..2017..) %>%
  rename(country = Entity, # Note - not all OWID Entities are countries
         income_group = Income.classifications..World.Bank..2017..) %>%
  mutate(country = str_replace(country, "United States", "USA")) %>%
  mutate(country = str_replace(country, "Czechia", "Czech Republic")) %>%
  mutate(country = str_replace(country, "Congo", "Republic of the Congo")) %>%
  mutate(country = str_replace(country, "Democratic Republic of Republic of the Congo", "Democratic Republic of the Congo")) %>%
  filter(country != "Africa") %>%
  filter(country != "Asia") %>%
  filter(country != "Europe") %>%
  filter(country != "Oceania") %>%
  filter(country != "World") %>%
  filter(country != "Latin America") %>%
  filter(country != "North America") %>%
  filter(country != "Kosovo") %>%
  filter(country != "Micronesia") %>%
  filter(country != "Czechoslovakia") %>%
  filter(country != "Netherlands Antilles")
nrow(income_data_yr)
head(income_data_yr)

####### Merging
data_merg_1 <- merge(seq_data_v3,
                     pop_data_yr,
                     by="country",
                     all.x=T, all.y=T)

# If there are no sequences then change NA to zero
data_merg_1$sequence_count <- as.numeric(data_merg_1$sequence_count)

data_merg_2 <- data_merg_1 %>%
  mutate(sequence_count = sequence_count %>% replace_na(0))

# Sequences per capita
data_merg_2$sequences_per_capita <- data_merg_2$sequence_count / data_merg_2$population
#write.csv(data_merg_2, "sequence_counts_with_population.csv", row.names=F)


#### Add GDP per capita
data_merg_3 <- merge(data_merg_2,
                     gdp_data_yr,
                     by="country",
                     all.x=T, all.y=F)

#### Add income
data_merg_4 <- merge(data_merg_3,
                     income_data_yr,
                     by="country",
                     all.x=T, all.y=F)

# Remove income group NA or Not categorized
data_merg_5 <- data_merg_4 %>% drop_na(income_group)
dim(data_merg_4)
dim(data_merg_5)

### Add continet names
colnames(region_names)
colnames(data_merg_5)

region_names_v1 <- region_names %>%
  rename(country = Entity,
         continent = Continent) %>%
  select(-Code) %>%
  mutate(country = str_replace(country, "United States", "USA")) %>%
  mutate(country = str_replace(country, "Czechia", "Czech Republic")) %>%
  mutate(country = str_replace(country, "Congo", "Republic of the Congo")) %>%
  mutate(country = str_replace(country, "Democratic Republic of Republic of the Congo", "Democratic Republic of the Congo"))

data_merg_6 <- merge(data_merg_5,
                     region_names_v1,
                     by="country",
                     all.x=T,all.y=F) %>%
  select(country,continent,sequence_count,population,sequences_per_capita,gdp_per_capita,income_group)

dim(data_merg_5)
dim(data_merg_6)
unique(data_merg_6$continent)

#write.csv(data_merg_6, "sequence_counts_with_population_GDP_income_continets.csv", row.names=F)





########### Sequencing in low and middle income countries (LMIC) - counts

## Population 
unique(data_merg_6$income_group)
LMIC_data <- data_merg_6 %>%
  filter(income_group == "Low income" |
           income_group == "Lower-middle income" |
           income_group == "Upper-middle income") %>%
  arrange(desc(sequence_count))

HIC_data <- data_merg_6 %>%
  filter(income_group == "High income") %>%
  arrange(desc(sequence_count))

# Population counts
LMIC_pop_M <- (sum(LMIC_data$population))/1000000
HIC_pop_M <- (sum(HIC_data$population))/1000000

pop_total_M <- LMIC_pop_M + HIC_pop_M

LMIC_pop_pcnt <- signif(LMIC_pop_M / pop_total_M * 100, 3)
HIC_pop_pcnt <- signif(HIC_pop_M / pop_total_M * 100, 3)

# Sequence counts
LMIC_seq_T <- (sum(LMIC_data$sequence_count))/1000
HIC_seq_T <- (sum(HIC_data$sequence_count))/1000

seq_total_T <- LMIC_seq_T + HIC_seq_T

LMIC_seq_pcnt <- signif(LMIC_seq_T / seq_total_T * 100, 3)
HIC_seq_pcnt <- signif(HIC_seq_T / seq_total_T * 100, 3)





########## Which LMIC have produced the most sequences?

LIC_data <- data_merg_6 %>% filter(income_group == "Low income") %>% arrange(desc(sequence_count))
LoMIC_data <- data_merg_6 %>% filter(income_group == "Lower-middle income") %>% arrange(desc(sequence_count))
UpMIC_data <- data_merg_6 %>% filter(income_group == "Upper-middle income") %>% arrange(desc(sequence_count))
Africa_data <- data_merg_6 %>% filter(continent=="Africa") %>% arrange(desc(sequence_count))

# Top 5 countries for each economic band
LIC_top_seq_5 <- LIC_data[1:5,]
LoMIC_top_seq_5 <- LoMIC_data[1:5,]
UpMIC_top_seq_5 <-UpMIC_data[1:5,]
HIC_top_seq_5 <-HIC_data[1:5,]

Africa_top_seq_10 <-Africa_data[1:10,]
Africa_top_seq_10$country

# Largest SARSCoV2 genome contributors among low income countries are The Gambia (430), DRC (371), Uganda (208), Rwanda (182), Senegal (136)
# Largest SARSCoV2 genome contributors among lower-middle income countries are India (5130), Bangladesh (823), Kenya (679), Jordan (590), Egypt (409)
# Largest SARSCoV2 genome contributors among upper-middle income countries are South Africa (3418), Brazil (2649), Russia (1820), China (1014), Mexico (868)
# Largest SARSCoV2 genome contributors among high income income countries are UK (~221K), USA (~103K), Denmark (~42K), Australia (~17K), Japan (~17K)

# Largest SARSCoV2 genome contributors among African countries are South Africa (3418), Kenya (679), The Gambia (430), Egypt (409), DRC (371), Nigeria (339), Zambia (221), Uganda (208), Ghana (204), Rwanda (182)







########### Plots

## Static bubble plot in ggplot2
colnames(data_merg_6)
head(data_merg_6)
unique(data_merg_6$continent)

data_plot <- data_merg_6 %>%
  filter(country != "Qatar") %>%
  rename(Continent = continent,
         Income.group = income_group)

## Still plot
p1 <- data_plot %>%
  ggplot(aes(x=gdp_per_capita, y=sequences_per_capita, size=sequence_count, color=Continent)) +
  geom_point(alpha=0.8) +
  scale_size(range = c(1, 20), breaks = c(0,200000), name="Sequences") +
  theme_minimal() +
  scale_color_brewer(palette="Set2") +
  xlab("GDP per capita") +
  ylab("Sequences per capita (Log 10)") + 
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10)) +
  #  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10')
p1
#ggsave(plot=p1, file="sequencePerCapitaLog10_vs_GDPperCapita_covar_SeqSize_Continent.png", width=8, height=4.5,units="in",dpi=300)



## Interactive plot with plotly
# Based on https://www.r-graph-gallery.com/bubble_chart_interactive_ggplotly.html
colnames(data_merg_6)
data_plot_2 <- data_merg_6 %>%
  filter(country != "Qatar") %>%
  rename(Continent = continent,
         Income.group = income_group) %>%
  mutate(population = signif(population/1000000, 3))

# Interactive version
p2 <- data_plot_2 %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", population, "\nSequences: ", sequence_count, "\nGDP per capita: ", gdp_per_capita, "\nIncome group: ", Income.group, sep="")) %>%
  
  # ggplot
  ggplot(aes(x=gdp_per_capita, y=sequences_per_capita, size=sequence_count, color=Continent, text=text)) +
  geom_point(alpha=0.8) +
  scale_size(range = c(1, 25), breaks = c(0,200000), name="Sequences") +
  scale_y_continuous(trans = 'log10') +
  xlab("GDP per capita") +
  ylab("Sequences per capita (Log 10)") + 
  scale_color_brewer(palette="Set2") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12)) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp2 <- ggplotly(p2, tooltip="text")
pp2

# save the widget
#library(htmlwidgets)
#saveWidget(pp2, file="SARSCoV2_sequences_by_GDP.html", selfcontained = TRUE)

