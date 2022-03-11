
setwd("C:\\Users\\linst\\Github\\adb-winter-2022\\data\\final")

## Loading Libraries
library(tidyverse)
library(vtable)
library(Hmisc)
library(corrplot)
library(naniar)
library(stargazer)
library(htmltools)
library(readr)
library(AER)
library(plm)
library(imputeTS)
library(Rcpp)

## Loading Data 
df <- read_csv("merged_data.csv") 
output.fig.dir <- "../../output/figures" 
output.tab.dir <- "../../output/tables" 
SAVE.RESULTS = TRUE

df <- df %>% 
  rename(migrant_stock=ims_both_sex, 
         refugee_stock=estimated_refugee_stock_incl_asylum_seekers_both_sexes,
         disaster_displacement=disaster_stock_displacementr_raw,
         conflict_displacement=conflict_stock_displacement_raw, 
         climate_change=CCH, 
         air_quality=AIR, 
         rule_of_law=`value.Rule of Law: Estimate`, 
         gov_effectiveness=`value.Government Effectiveness: Estimate`, 
         corruption_control=`value.Control of Corruption: Estimate`,
         state_legit=`P1: State Legitimacy`, 
         cpa_d_12=D12, 
         cpa_d_avg=D_avg, 
         gdp=`GDP per capita (constant 2015 US$)`, 
         gini=`value.Gini index (World Bank estimate)`) %>% 
  mutate(conflict_displacement=conflict_displacement/10000, 
         disaster_displacement=disaster_displacement/10000,
         migrant_stock=migrant_stock/10000,
         refugee_stock=refugee_stock/10000,
         gdp=gdp/1000,
         state_legit=100-state_legit)
col_names <- c('migrant_stock'='Migrant Stock (10,000s)', 
               'refugee_stock'='Refugee Stock (10,000s)',
               'disaster_displacement'='Internal Displacement Due to Disasters (10,000s)',
               'conflict_displacement'='Internal Displacement Due to Conflict (10,000s)', 
               'climate_change'='Climate Change', 
               'air_quality'='Air Quality', 
               'rule_of_law'='Rule of Law', 
               'gov_effectiveness'='Government Effectiveness', 
               'corruption_control'='Control of Corruption',
               'state_legit'='State Legitimacy', 
               'cpa_d_12'='CPA: D-12', 
               'cpa_d_avg'='CPA: Cluster D Average', 
               'gdp'='GDP Per Capita (1,000s)', 
               'hdi_value'="HDI",
               'gini'="Gini Index") 
names(df)

df_clean<- df %>%
          select(iso, year, migrant_stock, refugee_stock, disaster_displacement, conflict_displacement, climate_change, 
                 air_quality, rule_of_law, gov_effectiveness, corruption_control, state_legit, cpa_d_12, 
                 cpa_d_avg, gdp, hdi_value)%>%
          filter(year>=2010)
df_clean ['hdi_value'] <- as.numeric(unlist(df_clean['hdi_value']))

df_imputed<- df_clean %>%
              na_interpolation(df_clean[,3:ncol(df_clean)], option='spline', maxgap=Inf)

# plm(state_legit ~ air_quality + climate_change + hdi_value + gdp, data = df, index = c("iso", "year"), model = "within")
# FSI as fragility Indicator.
fsi_climate.model <- plm(state_legit ~ air_quality + climate_change + hdi_value + gdp,df_imputed, 
                         index = c("iso","year"), na.model = "within", effect= "twoways")
fsi_migration.model <- plm(state_legit ~ migrant_stock + refugee_stock + conflict_displacement + disaster_displacement + gdp, 
                           data = df_imputed, index = c("iso", "year"), model = "within", effect = "twoways")
fsi_governance.model <- plm(state_legit ~ rule_of_law + gov_effectiveness + corruption_control + gdp,
                            data = df_imputed, index = c("iso", "year"), model = "within", effect = "twoways")
# fsi_full.model <- plm(state_legit ~ rule_of_law + gov_effectiveness + corruption_control + migrant_stock + refugee_stock + conflict_displacement + disaster_displacement + climate_change + air_quality + hdi_value + gdp, data = df, index = c("iso", "year"), model = "within", effect = "twoways")
stargazer(fsi_climate.model, fsi_migration.model, fsi_governance.model, type='text')

if (SAVE.RESULTS) {
  stargazer(fsi_climate.model, fsi_migration.model, fsi_governance.model, type='html', out=paste(output.tab.dir, '/state_legit_reg_fe.html', sep=''))
}

# CPA as fragility indicator with a focuse on Fragility of Public Sector Management and Institutions.
cpa_climate.model <- plm(cpa_d_avg ~ air_quality + climate_change + hdi_value + gdp, 
                         data = df_imputed, index = c("iso", "year"), model = "within", effect = "twoways")
cpa_migration.model <- plm(cpa_d_avg ~ migrant_stock + refugee_stock + conflict_displacement + disaster_displacement + gdp, 
                           data = df_imputed, index = c("iso", "year"), model = "within", effect = "twoways")
cpa_governance.model <- plm(cpa_d_avg ~ rule_of_law + gov_effectiveness + corruption_control + gdp,
                            data = df_imputed, index = c("iso", "year"), model = "within", effect = "twoways")
# cpa_full.model <- plm(cpa_d_avg ~ rule_of_law + gov_effectiveness + corruption_control + migrant_stock + refugee_stock + conflict_displacement + disaster_displacement + climate_change + air_quality + hdi_value + gdp, data = df, index = c("iso", "year"), model = "within", effect = "twoways")

stargazer(cpa_climate.model, cpa_migration.model, cpa_governance.model, type='text')

if (SAVE.RESULTS) {
  stargazer(cpa_climate.model, cpa_migration.model, cpa_governance.model, type='html', 
            out=paste(output.tab.dir, '/state_legit_reg_fe.html', sep=''))
}

# summary(model)