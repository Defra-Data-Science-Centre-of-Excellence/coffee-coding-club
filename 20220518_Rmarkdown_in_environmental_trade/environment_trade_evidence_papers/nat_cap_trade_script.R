library(tidyverse)
'%not_in%' <- Negate('%in%')

# define country groups
cptpp_countries <- c("AUS", "CAN", "CHL", "JPN", 
                     "MEX", "NZL", "BRN", "MYS", "PER", "SGP", "VNM")

gcc_countries <- c("BHR", "KWT", "OMN", "QAT", "SAU", "ARE")

agri_exporters_to_gcc <- c("IND", "BRA", "USA", "AUS", "NLD", "FRA", "EGY", "ARG", "NZL", "TUR")

# the percentage of GCC agri-food imports that come from respective source countries in agri_exporters_to_gcc (from stats team)
weights_exp_to_gcc <- c(11,9,8,4,4,3,3,3,3,3)

# World Bank data

#define column types
wb_col_types <- c(rep("text", 4), rep("numeric", 60))

## import data
# GDP growth
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG?view=chart
gdp_growth <- readxl::read_xls("API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_2252047.xls", sheet = "Data", range = "a4:bl268", col_types = wb_col_types)

# GDP
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?view=chart
gdp <- readxl::read_xls("API_NY.GDP.MKTP.CD_DS2_en_excel_v2_2252098.xls", sheet = "Data", range = "a4:bl268", col_types = wb_col_types)

# trade as a % of GDP
# https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS
trade_intensity <- readxl::read_xls("API_NE.TRD.GNFS.ZS_DS2_en_excel_v2_2254016.xls", sheet = "Data", range = "a4:bl268", col_types = wb_col_types)

# % percentage protected terrestrial and marine area
# https://data.worldbank.org/indicator/ER.PTD.TOTL.ZS?view=chart
protected_terr_and_mar <- readxl::read_xls("API_ER.PTD.TOTL.ZS_DS2_en_excel_v2_2256187.xls", sheet = "Data", range = "a4:bl268", col_types = wb_col_types)

# Annual freshwater withdrawals, total (% of internal resources)
# https://data.worldbank.org/indicator/ER.H2O.FWTL.ZS?view=chart
freshwater_withdrawals <- readxl::read_xls("API_ER.H2O.FWTL.ZS_DS2_en_excel_v2_2260585.xls", sheet = "Data", range = "a4:bl268", col_types = wb_col_types)

# forest area as a % of land area
# https://data.worldbank.org/indicator/AG.LND.FRST.ZS
forest_area_percent <- readxl::read_xls("API_AG.LND.FRST.ZS_DS2_en_excel_v2_2170903.xls", sheet = "Data", range = "a4:bl268", col_types = wb_col_types)

# Adjusted savings: natural resource depletion (% of GNI)
# https://data.worldbank.org/indicator/NY.ADJ.DRES.GN.ZS
nat_resource_depletion <- readxl::read_xlsx("API_NY.ADJ.DRES.GN.ZS_DS2_en_csv_v2_2177203.xlsx", sheet = "API_NY.ADJ.DRES.GN.ZS_DS2_en_cs", range = "a4:bl268", col_types = wb_col_types)

# pivot them longer
gdp_growth <- gdp_growth %>%
  select(- "Indicator Code")%>%
  pivot_longer(c(4:63), names_to = "year", values_to = "value")
  
gdp <- gdp %>%
  select(- "Indicator Code")%>%
  pivot_longer(c(4:63), names_to = "year", values_to = "value")

trade_intensity <- trade_intensity %>%
  select(- "Indicator Code")%>%
  pivot_longer(c(4:63), names_to = "year", values_to = "value")

protected_terr_and_mar <- protected_terr_and_mar %>%
  select(- "Indicator Code")%>%
  pivot_longer(c(4:63), names_to = "year", values_to = "value")

freshwater_withdrawals <- freshwater_withdrawals %>%
  select(- "Indicator Code")%>%
  pivot_longer(c(4:63), names_to = "year", values_to = "value")

forest_area_percent <- forest_area_percent %>%
  select(- "Indicator Code")%>%
  pivot_longer(c(4:63), names_to = "year", values_to = "value")

nat_resource_depletion <- nat_resource_depletion %>%
  select(- "Indicator Code")%>%
  pivot_longer(c(4:63), names_to = "year", values_to = "value")

# bind them
world_bank_data <- rbind(gdp_growth, gdp, trade_intensity, protected_terr_and_mar, freshwater_withdrawals, forest_area_percent, nat_resource_depletion) %>%
  rename(variable = "Indicator Name", country = "Country Name", country_code = "Country Code")

#recode the variable names

unique(world_bank_data$variable)

world_bank_data <- world_bank_data %>%
  mutate(variable = case_when(
    variable == "GDP growth (annual %)" ~ "gdp_growth",
    variable == "GDP (current US$)" ~ "gdp_usd",
    variable == "Trade (% of GDP)" ~ "trade_intensity",
    variable == "Terrestrial and marine protected areas (% of total territorial area)" ~ "terr_mar_protected",
    variable == "Annual freshwater withdrawals, total (% of internal resources)" ~ "freshwater_use",
    variable == "Forest area (% of land area)" ~ "forest_area",
    variable == "Adjusted savings: natural resources depletion (% of GNI)" ~ "nat_res_dep",
    TRUE ~ variable
  ))


#create a list of the country aggregations in the data (observations which are not nation states)
country_aggregations <- c("Arab World",
                          "Central Europe and the Baltics",
                          "Channel Islands, Caribbean small states",
                          "East Asia & Pacific (excluding high income)", "Early-demographic dividend",
                          "East Asia & Pacific",
                          "Europe & Central Asia (excluding high income)",
                          "Europe & Central Asia",
                          "Euro area",
                          "European Union",
                          "Fragile and conflict affected situations",
                          "High income",
                          "Heavily indebted poor countries (HIPC)",
                          "IBRD only",
                          "IDA & IBRD total",
                          "IDA total",
                          "IDA blend",
                          "IDA only",
                          "Not classified",
                          "Latin America & Caribbean (excluding high income)",
                          "Latin America & Caribbean",
                          "Least developed countries: UN classification",
                          "Low income",
                          "Lower middle income",
                          "Low & middle income",
                          "Late-demographic dividend",
                          "Middle East & North Africa",
                          "Middle income",
                          "Middle East & North Africa (excluding high income)",
                          "North America",
                          "OECD members",
                          "Other small states",
                          "Pre-demographic dividend",
                          "Post-demographic dividend",
                          "South Asia",
                          "Sub-Saharan Africa (excluding high income)",
                          "Sub-Saharan Africa",
                          "Small states",
                          "East Asia & Pacific (IDA & IBRD countries)",
                          "Europe & Central Asia (IDA & IBRD countries)",
                          "Latin America & the Caribbean (IDA & IBRD countries)",
                          "Middle East & North Africa (IDA & IBRD countries)",
                          "South Asia (IDA & IBRD)",
                          "Sub-Saharan Africa (IDA & IBRD countries)",
                          "Upper middle income",
                          "Western Sahara"
)

# filter for CPTPP countries
cptpp_world_bank <- world_bank_data %>%
  filter (country_code %in% cptpp_countries)

# create a cptpp 'country'
cptpp_averages <- cptpp_world_bank %>%
  group_by(variable, year)%>%
  summarise(value = mean(value, na.rm = TRUE))%>%
  mutate(country = "CPTPP", country_code = "CPTPP")

# repeat this for the GCC countries
gcc_world_bank <- world_bank_data %>%
  filter (country_code %in% gcc_countries)

# create a gcc 'country'
gcc_averages <- gcc_world_bank %>%
  group_by(variable, year)%>%
  summarise(value = mean(value, na.rm = TRUE))%>%
  mutate(country = "GCC", country_code = "GCC")

# repeat this for agri_exporters_to_gcc
agri_exporters_to_gcc_world_bank <- world_bank_data %>%
  filter (country_code %in% agri_exporters_to_gcc)

# create an agri_exporters_to_gcc 'country' using the mean weighted by agri-food export value of exports to the GCC countries.
agri_exporters_to_gcc_averages <- agri_exporters_to_gcc_world_bank %>%
  group_by(variable, year)%>%
  summarise(value = weighted.mean(value, w = weights_exp_to_gcc,  na.rm = TRUE))%>%
  mutate(country = "agri_exporters_to_GCC", country_code = "AETGCC")

# bind the rows together
world_bank_data <- rbind(world_bank_data, cptpp_averages, gcc_averages, agri_exporters_to_gcc_averages)

# Pivot wider
world_bank_data <- world_bank_data %>%
  pivot_wider(names_from = "variable", values_from = "value")


# Governance index
# http://info.worldbank.org/governance/wgi/
# I need to manually edit the row of column titles in excel, and keep only a fraction of the columns (the indicator estimate itself), before importing. 
# I also deleted the first 13 rows of each sheet, and row 15 (the column titles).

# define variable types
governance_col_types <- c("text", "text", rep("numeric", 21))

#import
voice_accountability <- readxl::read_xlsx("wgidataset.xlsx", sheet = "VoiceandAccountability", range = "a1:w215", col_types = governance_col_types)
political_stability <- readxl::read_xlsx("wgidataset.xlsx", sheet = "Political StabilityNoViolence", range = "a1:w215", col_types = governance_col_types)
government_effectiveness <- readxl::read_xlsx("wgidataset.xlsx", sheet = "GovernmentEffectiveness", range = "a1:w215", col_types = governance_col_types)
regulatory_quality <- readxl::read_xlsx("wgidataset.xlsx", sheet = "RegulatoryQuality", range = "a1:w215", col_types = governance_col_types)
rule_of_law <- readxl::read_xlsx("wgidataset.xlsx", sheet = "RuleofLaw", range = "a1:w215", col_types = governance_col_types)
control_of_corruption <- readxl::read_xlsx("wgidataset.xlsx", sheet = "ControlofCorruption", range = "a1:w215", col_types = governance_col_types)

#pivot them all longer to make them tidy

control_of_corruption <- control_of_corruption %>%
  pivot_longer(c(3:23), names_to = "year", values_to = "value")%>%
  mutate(variable = "control_of_corruption")
 
government_effectiveness <- government_effectiveness %>%
  pivot_longer(c(3:23), names_to = "year", values_to = "value")%>%
  mutate(variable = "government_effectiveness")

political_stability <- political_stability %>%
  pivot_longer(c(3:23), names_to = "year", values_to = "value")%>%
  mutate(variable = "political_stability")

regulatory_quality <- regulatory_quality %>%
   pivot_longer(c(3:23), names_to = "year", values_to = "value")%>%
  mutate(variable = "regulatory_quality")
 
rule_of_law <- rule_of_law %>%
  pivot_longer(c(3:23), names_to = "year", values_to = "value")%>%
  mutate(variable = "rule_of_law")

voice_accountability <- voice_accountability %>%
  pivot_longer(c(3:23), names_to = "year", values_to = "value")%>%
  mutate(variable = "voice_accountability")

# combine and average to a single governance index (per country per year)

gov_indicators <- rbind(control_of_corruption, government_effectiveness, political_stability, regulatory_quality, rule_of_law, voice_accountability)

governance_index <- gov_indicators %>%
  group_by(`Country/Territory`, Code, year)%>%
  summarise(value = mean(value, na.rm = TRUE))%>%
  mutate(variable = "mean_governance_index")

# Combine the individual indicators with the mean of them all
# Rename the country and code variables
# index the score to 0 - 100. currently it's at -2.5 - + 2.5
governance_index <- rbind(gov_indicators, governance_index)%>%
  rename(country = "Country/Territory", country_code = "Code")%>%
  mutate(value = 20*(value + 2.5), year = as.numeric(year))

# create world averages for governance
world_average_governance <- governance_index %>%
  group_by(year, variable)%>%
  summarise(value = mean(value, na.rm = TRUE))%>%
  mutate(country = "World", country_code = "WLD")

# filter for CPTPP countries
cptpp_countries_governance <- governance_index %>%
  filter (country_code %in% cptpp_countries)

# create a cptpp 'country'
cptpp_average_governance <- cptpp_countries_governance %>%
  group_by(variable, year)%>%
  summarise(value = mean(value, na.rm = TRUE))%>%
  mutate(country = "CPTPP", country_code = "CPTPP")

# repeat this for the GCC countries
gcc_governance <- governance_index %>%
  filter (country_code %in% gcc_countries)

# create a gcc 'country'
gcc_average_governance <- gcc_governance %>%
  group_by(variable, year)%>%
  summarise(value = mean(value, na.rm = TRUE))%>%
  mutate(country = "GCC", country_code = "GCC")

# repeat this for agri_exporters_to_gcc
agri_exporters_to_gcc_governance <- governance_index %>%
  filter (country_code %in% agri_exporters_to_gcc)

# create an agri_exporters_to_gcc 'country' using the mean weighted by agri-food export value of exports to the GCC countries.
agri_exporters_to_gcc_average_governance <- agri_exporters_to_gcc_governance %>%
  group_by(variable, year)%>%
  summarise(value = weighted.mean(value, w = weights_exp_to_gcc,  na.rm = TRUE))%>%
  mutate(country = "agri_exporters_to_GCC", country_code = "AETGCC")

#bind these together
governance_index <- rbind(governance_index, gcc_average_governance, agri_exporters_to_gcc_average_governance, world_average_governance, cptpp_average_governance)

# Pivot wider
governance_index <- governance_index %>%
  pivot_wider(names_from = "variable", values_from = "value")



##Yale University Environmental Performance Index
# https://epi.yale.edu/downloads

# list of names of all the measures included. Use this list to choose some useful indicators to look at.
index_names_table <- readr::read_csv("epi2020indicatortla20200604.csv")

# create vector of column names (years)
yale_col_names = c("code", "country_code", "country", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005",
                   "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

#tidy the data to get the new value and the change
yale_epi_2020 <- readr::read_csv("epi2020results20200604.csv") %>%  select(c(2:49))%>%
  pivot_longer(c(3:48), names_to = "variable", values_to = "value_new")%>%
  mutate(variable = substr(variable, 1,3))
           
yale_epi_change <- readr::read_csv("epi2020results20200604.csv") %>%  select(2, 3, c(50:95))%>%
  pivot_longer(c(3:48), names_to = "variable", values_to = "change")%>%
  mutate(variable = substr(variable, 1,3))

yale_epi <- inner_join(yale_epi_change, yale_epi_2020, by = c("iso", "country", "variable"))

# create the old value
yale_epi <- yale_epi %>%
  mutate(value_old = value_new - change)%>%
  select(- change)

# join the indicator descriptions
yale_epi <- left_join(yale_epi, index_names_table, by = c("variable" = "Abbreviation"))

# drop and rearrange columns
yale_epi <- yale_epi[ ,c(1:3, 7, 6, 4, 5)]%>%
  rename(var_name = "Variable", country_code = "iso", type = "Type")
           
# create world averages for the Yale indicators
yale_epi_world_averages <- yale_epi %>%
  group_by(variable, var_name, type)%>%
  summarise(value_new = mean(value_new, na.rm = TRUE), value_old = mean(value_old, na.rm = TRUE))%>%
  mutate(country = "World", country_code = "WLD")

# create CPTPP and GCC average indicators

# filter for CPTPP countries
cptpp_yale_epi <- yale_epi %>%
  filter (country_code %in% cptpp_countries)%>%
  group_by(variable, var_name, type)%>%
  summarise(value_new = mean(value_new, na.rm = TRUE), value_old = mean(value_old, na.rm = TRUE))%>%
  mutate(country = "CPTPP", country_code = "CPTPP")

# repeat this for the GCC countries
gcc_yale_epi <- yale_epi %>%
  filter (country_code %in% gcc_countries)%>%
  group_by(variable, var_name, type)%>%
  summarise(value_new = mean(value_new, na.rm = TRUE), value_old = mean(value_old, na.rm = TRUE))%>%
  mutate(country = "GCC", country_code = "GCC")

# repeat this for agri_exporters_to_gcc 'country' using the mean weighted by agri-food export value of exports to the GCC countries.
agri_exporters_to_gcc_average_yale_epi <- yale_epi %>%
  filter (country_code %in% agri_exporters_to_gcc)%>%
  group_by(variable, var_name, type)%>%
  summarise(value_new = weighted.mean(value_new, w = weights_exp_to_gcc, na.rm = TRUE), value_old = mean(value_old, w = weights_exp_to_gcc, na.rm = TRUE))%>%
  mutate(country = "AETGCC", country_code = "AETGCC")


# bind these together
yale_epi <- rbind (yale_epi, yale_epi_world_averages, cptpp_yale_epi, gcc_yale_epi, agri_exporters_to_gcc_average_yale_epi)

# pivot longer to get the year of observation in a column as a variable.
yale_epi <- yale_epi %>%
  pivot_longer(c(6,7), names_to = "year", values_to = "value")%>%
  mutate(year = case_when(year == "value_old" ~ 2010,
                          year == "value_new" ~ 2020)
         )

#rearrange the columns
yale_epi <- yale_epi[ , c(2,1,6,3,4,5,7)]






# WWF Living Planet Index (LPI)
wwf_lpi <- readr::read_csv("living_planet_index.csv")%>%
  pivot_longer(c(2:7), names_to = "continent", values_to = "value")

#rearrange the columns
wwf_lpi <- wwf_lpi[ , c(2,1,3)]%>%
  rename(year = "Year")



# write this to an excel file
writexl::write_xlsx(list("world_bank_data" = world_bank_data, "governance_index" = governance_index, 
                         "yale_epi" = yale_epi,"wwf_lpi" = wwf_lpi),
                    "natural_capital_trade_data.xlsx")

