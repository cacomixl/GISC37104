library(tidycensus)
library(tidyverse)
library(dplyr)
library(sf)
library(raster)

# Load in County-Level HPI Data
hpi <- read.csv("~/GISC 37104/Final_Project/Data/HPI_AT_BDL_county (1).csv")

# Make HPI numeric rather than string
hpi$HPI <- hpi$HPI %>%
  as.numeric()

# Format table nicely
hpi <- hpi %>%
  dplyr::select(State, County, FIPS.code, Year, HPI)  %>% 
  rename(GEOID = FIPS.code) %>%
  spread(Year,HPI) 

View(hpi)
View(read.csv("~/GISC 37104/Final_Project/Data/HPI_AT_BDL_county (1).csv"))

#  Create 1990-2020 comparison
hpi1620 <- hpi %>%
  dplyr::select(GEOID, "2016", "2020")
hpi1620$incr <- hpi1620$"2020"/hpi1620$"2016"-1

counties <- st_read("~/GISC 37104/Final_Project/Intermediates/us_counties_contiguous.shp")
counties$GEOID <- counties$GEOID %>%
    as.integer()

hpi1620_counties <- left_join(counties, hpi1620, by="GEOID")

st_write(hpi1620_counties, "hpi1620.shp")

full_incomers <- as.data.frame(read_csv("~/GISC 37104/Final_Project/Data/full_incomers.csv"))

full_incomers$GEOID <- full_incomers$GEOID %>%
  as.numeric()
full_incomers <- full_incomers %>%
  dplyr::select(GEOID,Incomers)

# LA Stuff

la_incomers <- as.data.frame(read_csv("~/GISC 37104/Final_Project/Data/LA_incomers.csv"))

joined_la <- inner_join(full_incomers,la_incomers,by="GEOID")
joined_la$lapct <- joined$LA_Incomers/joined_la$Incomers
joined2_la <- inner_join(joined_la,counties,by="GEOID")

st_write(joined2_la, "full_lapct.shp")

hpi_lamig <- inner_join(joined_la,hpi1620,by="GEOID")


# OC Stuff

oc_incomers <- as.data.frame(read_csv("~/GISC 37104/Final_Project/Data/OC_incomers.csv"))

joined_oc <- inner_join(full_incomers,oc_incomers,by="GEOID")
joined_oc$ocpct <- joined_oc$OC_Incomers/joined_oc$Incomers
joined2_oc <- inner_join(joined_oc,counties,by="GEOID")
hpi_ocmig <- inner_join(joined_oc,hpi1620,by="GEOID")

hpi_ocmig2 <- inner_join(ocpct_o1000,counties,by="GEOID")

st_write(hpi_ocmig2, "~/GISC 37104/Final_Project/Outputs/hpi_ocpct.shp")


## LA Regressions

# Modeling scatterplot between LA migration pct and housing price increase

model <- lm(lapct ~ incr, data = hpi_lamig)
summary(model)

plot(hpi_lamig$lapct, hpi_lamig$incr, main = "LA In-Migration Percent vs HPI Increase")
abline(model, col = "red")  # Add regression line to the plot

# Test with some outliers removed

lapct_o1000 <- hpi_lamig %>%
  subset(Incomers > 1000)

model2 <- lm(lapct ~ incr, data = lapct_o1000)
summary(model2)

plot(lapct_o1000$lapct, lapct_o1000$incr, main = "Large LA In-Migration Percent vs HPI Increase")
abline(model2, col = "red")  # Add regression line to the plot

# Modeling scatterplot between total incomers and housing price increase

model3 <- lm(Incomers ~ incr, data = hpi_lamig)
summary(model3)

plot(hpi_lamig$Incomers, hpi_lamig$incr, main = "Scatter plot with regression line")
abline(model3, col = "red")  # Add regression line to the plot

# Adding natural log

model4 <- lm(log(Incomers) ~ incr, data = hpi_lamig)
summary(model4)

plot(log(hpi_lamig$Incomers), hpi_lamig$incr, main = "ln(Number of Incomers) vs. Increase in HPI")
abline(model4, col = "red")  # Add regression line to the plot

# Adding natural log

model5 <- lm(log(LA_Incomers) ~ incr, data = hpi_lamig)
summary(model5)

plot(log(hpi_lamig$LA_Incomers), hpi_lamig$incr, main = "ln(Number of LA Incomers) vs. Increase in HPI")
abline(model5, col = "red")  # Add regression line to the plot

## OC Regressions

ocpct_o1000 <- hpi_ocmig %>%
  subset(Incomers > 1000)

model2 <- lm(ocpct ~ incr, data = ocpct_o1000)
summary(model2)

plot(ocpct_o1000$ocpct, ocpct_o1000$incr, main = "Large LA In-Migration Percent vs HPI Increase")
abline(model2, col = "red")  # Add regression line to the plot

# Adding natural log

model4 <- lm(log(Incomers) ~ incr, data = ocpct_o1000)
summary(model4)

plot(log(ocpct_o1000$Incomers), ocpct_o1000$incr, main = "ln(Number of Incomers) vs. Increase in HPI")
abline(model4, col = "red")  # Add regression line to the plot

# Adding natural log

model5 <- lm(log(OC_Incomers) ~ incr, data = ocpct_o1000)
summary(model5)

plot(log(ocpct_o1000$OC_Incomers), ocpct_o1000$incr, main = "ln(Number of LA Incomers) vs. Increase in HPI")
abline(model5, col = "red")  # Add regression line to the plot

