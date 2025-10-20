# Install and load necessary packages (if you haven't already)
# install.packages(c("sf", "dplyr", "ggplot2", "viridis","usmap"))
library(sf)
library(dplyr)
library(ggplot2)
library(viridis) # For color palettes
library(usmap)
library(readr)
library(stringr)

# 1. Prepare Data for Mapping

data <- read_csv("data.csv")

data_levels=data %>% select(COUNTY_FIPS:nads) %>% mutate(aiInt=ai/nads) %>% 
  group_by(COUNTY_FIPS) %>% summarise(aiInt=mean(aiInt))
data_ch=data %>% filter(Year>2017)%>% select(COUNTY_FIPS:nads) %>% mutate(aiInt=ai/nads) %>% 
  group_by(COUNTY_FIPS) %>% summarise(aiIntch=last(aiInt)-first(aiInt))
# Download counties shapefile in r
# Let's assume your shapefile is named "us_counties.shp" and is in your working directory.
counties <- tigris::counties(year = "2020")

counties=counties %>% mutate(COUNTY_FIPS=as.numeric(GEOID))

merged_data_lev <- data_levels %>% mutate(fips=COUNTY_FIPS %>% str_pad(5,"0",side="left"))
merged_data_ch <- data_ch%>% mutate(fips=COUNTY_FIPS %>% str_pad(5,"0",side="left"))

merged_data_lev <- merged_data_lev %>%
  mutate(
    ai_share = aiInt * 100 )%>% 
  mutate(caseai=case_when(
    ai_share >= 0 & ai_share < 0.06~ 1,
    ai_share > 0.06 & ai_share < 0.14~ 2,
    ai_share > 0.14& ai_share < 0.23~ 3,
    ai_share > 0.23 & ai_share < 0.37~ 4,
    ai_share > 0.37 & ai_share < 0.7~ 5,
    ai_share > 0.7~ 6,
    is.na(ai_share) ~ NA  ))


merged_data_ch=merged_data_ch %>%
  mutate(
    change_in_share = aiIntch * 100 )
merged_data_ch <- merged_data_ch%>% 
  mutate(caseaich=case_when(
    change_in_share < -0.12~ 1,
    change_in_share > -0.12 & change_in_share <= 0~ 2,
    change_in_share > 0& change_in_share < 0.09~ 3,
    change_in_share > 0.09 & change_in_share < 0.24~ 4,
    change_in_share > 0.24 & change_in_share < 0.57~ 5,
    change_in_share > 0.57~ 6,
    is.na(change_in_share) ~ NA  )) 



# 3. Visualize Percent Share (Similar to Panel A)

plot_share <- plot_usmap(
  color = "white",
  linewidth = 0.1,
  regions = "counties",
  data = merged_data_lev,
  values = "caseai") + # Use geom_sf for spatial data
  scale_fill_viridis_c(direction = 1,
                       name = "Percent Share of AI Jobs") +
  labs(title = paste("Percent Share of AI Jobs")) +
  theme_void()

# 4. Visualize Percentage Point Change (Similar to Panel B)

plot_change <-  plot_usmap(
  color = "white",
  linewidth = 0.1,
  regions = "counties",
  data = merged_data_ch,
  values = "caseaich") +
  scale_fill_gradient2(name = "Percentage Point Change") +
  labs(title = "Percentage Point Change in AI Share 2018-2023") +
  theme_void()

plot_change

