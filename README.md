# SARSCoV2-global-sequence-distribution
Global distribution of SARS-CoV-2 sequences on the GISAID database

## Folders in repo
Input = all input data required for the analysis
Outputs = Output files: bubble plot of sequence per capita vs GDP per capita (also as interactive html) & csv file of processed data
Code = R script

## Source of the data
SARS-CoV-2 sequence metadata downloaded from GISAID, nextmeta file, accessed 15/02/2021
https://www.gisaid.org/

Information on nextstrain column headings here:
https://nextstrain.github.io/ncov/data-prep.html

Location where samples were collected was used (ie columns 'country' and 'region'), and date_submitted (date submitted to public database/ GISAID) as collection date data incomplete

Geographic/ global demographic datasets are all from Our World In Data:
https://ourworldindata.org/

Country Populations:
https://ourworldindata.org/grapher/world-population-by-world-regions-post-1820

GDP per capita:
https://ourworldindata.org/grapher/average-real-gdp-per-capita-across-countries-and-regions

World Bank Income Groups:
https://ourworldindata.org/grapher/world-banks-income-groups?time=2000

World region names:
https://ourworldindata.org/world-region-map-definitions

Human Development Index:
https://ourworldindata.org/grapher/human-development-index?time=2017

Some country/ region names were changed or dropped to consistify across datasets as per the code
