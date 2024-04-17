library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(dplyr)
library(sf)
library(stats)
library(ggplot2)
library(reshape2)

# Replace this with your Excel file path
out_file <- "~/GISC 37104/Final_Project/Data/analysis2_county-to-county-2016-2020-previous-residence-sort.xlsx"  # Replace with your Excel file path

# Get all sheet names in the Excel file
out_sheets <- excel_sheets(out_file)

# Read all sheets and combine them into one dataframe
out_dfs <- lapply(out_sheets, function(sheet) {
  read_excel(out_file, sheet = sheet)
})
combined_out_df <- bind_rows(out_dfs)
combined_out_df_small <- combined_out_df[, c(1:34, 37:38)]

out_df_nona <- subset(combined_out_df_small, complete.cases(combined_out_df_small))

write_xlsx(out_df_nona,"~/GISC 37104/Final_Project/Intermediates/out_df_nona.xlsx")

# Clean file in Excel, put it back in
out_df <- read_excel("~/GISC 37104/Final_Project/Intermediates/out_df_nona.xlsx")

# Fix column formats
out_df_num <- out_df
out_df_num[, c(1:2,5:18, 21:34)] <- lapply(out_df_num[, c(1:2,5:18, 21:34)], as.numeric)

out_df_num$ctypct <- out_df_num$flow / out_df_num$move_diffst_1

nrow(out_df_num %>%
  subset(flowm>flow))
nrow(out_df_num %>%
       subset(flow>flowm))
nrow(out_df_num %>%
       subset(flow==flowm))
nrow(out_df_num)
nrow(out_df_num[out_df_num$flowm>out_df_num$flow])

############

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

#  Create 1990-2020 comparison
hpi1620 <- hpi %>%
  dplyr::select(GEOID, "2016", "2020")
hpi1620$incr <- hpi1620$"2020"/hpi1620$"2016"-1

# Add county dataset
counties <- st_read("~/GISC 37104/Final_Project/Data/nhgis0001_shapefile_tl2021_us_county_2021/US_county_2021.shp")
counties$GEOID <- counties$GEOID %>%
  as.numeric()
counties_no_geom <- st_drop_geometry(counties)
write_csv(counties_no_geom,"~/GISC 37104/Final_Project/Data/counties_no_geom.csv")


# Load in County-Level HPI Data
hpi2 <- read.csv("~/GISC 37104/Final_Project/Data/HPI_AT_BDL_county (1).csv")

# Make HPI numeric rather than string
hpi2$HPI <- hpi2$HPI.with.1990.base %>%
  as.numeric()

# Format table nicely
hpi2 <- hpi2 %>%
  dplyr::select(State, County, FIPS.code, Year, HPI.with.1990.base)  %>% 
  rename(GEOID = FIPS.code) %>%
  spread(Year,HPI.with.1990.base) 

### Anchorage only test

# Convert the HPI columns to numeric
hpi2[, 4:51] <- lapply(hpi2[, 4:51], as.numeric)

# Extract data for Anchorage County
anchorage_data <- subset(hpi2, County == "Anchorage" & State == "AK")

# Convert data to long format for ggplot
anchorage_data_long <- tidyr::gather(anchorage_data, key = "Year", value = "HPI", -State, -County, -GEOID)
anchorage_data_long[4] <- lapply(anchorage_data_long[4], as.numeric)

# Plot using ggplot2
ggplot(anchorage_data_long, aes(x = Year, y = HPI)) +
  geom_line() +
  labs(title = "HPI Trend for Anchorage County, AK", x = "Year", y = "HPI Value")

### All lines test

library(ggplot2)

# Convert the HPI columns to numeric (if not already)
hpi2[, 4:51] <- lapply(hpi2[, 4:51], as.numeric)

# Melt the data to long format for plotting
melted_data <- melt(hpi2, id.vars = c("State", "County", "GEOID"), variable.name = "Year", value.name = "HPI")

fl_melted_data <- melted_data %>%
  subset(GEOID > 12000 & GEOID < 12999)

melted_data$Year <- as.numeric(melted_data$Year)
melted_data$Year <- melted_data$Year + 1974
str(melted_data)

head(fl_melted_data)

# Plot all counties
ggplot() +
  geom_line(data=melted_data, aes(x = as.integer(Year), y = HPI,col=County)) +
  labs(title = "HPI Trend for Counties", x = "Year", y = "HPI Value") +
  theme(legend.position = "none")


############################

### All outs

# HPI Formatting
hpi1620_1 <- hpi1620 %>%
  rename(GEOID_1 = GEOID)

# Create a point dataset from centroids
counties_pts <- st_as_sf(counties_no_geom, coords = c("INTPTLON", "INTPTLAT"))  # Specify coordinates

counties_pts <- counties_pts %>%
  dplyr::select(GEOID)

str(out_df_num)
hpi_mig <- inner_join(out_df_num, hpi1620_1, "GEOID_1")
model_1 <- lm(incr ~ move_diffst_1, data = hpi_mig)
summary(model_1)

## LA
# Create LA Origin Subset
la_out <- out_df_num %>%
  subset(GEOID_0==6037) %>%
  subset(!(6000<GEOID_1&GEOID_1<6999))

total_mig <- out_df_num %>%
  select(GEOID_0,GEOID_1,flow) %>%
  subset(GEOID_0==6037|GEOID_0==6059|GEOID_0==6065|GEOID_0==6071|GEOID_0==6073|GEOID_0==6075|GEOID_0==6085|GEOID_0==6013|GEOID_0==6001|GEOID_0==6067) %>%
  subset(!(6000<GEOID_1&GEOID_1<6999)) %>%
  spread(GEOID_0,flow)
total_mig[is.na(total_mig)] <- 0
total_mig$sum <- rowSums(total_mig[2:ncol(total_mig)])
strip_counties <- counties %>%
  select(GEOID) %>%
  rename("GEOID_1" = GEOID)
total_mig <- inner_join(strip_counties,total_mig,by="GEOID_1")
st_write(total_mig, "~/GISC 37104/Final_Project/Outputs/total_mig.shp")

head(total_mig)

full_outer_join(la_out_thin, "GEOID_1")

# Join with HPI 16-20
la_out_hpi <- inner_join(x=la_out,y=hpi1620_1,by="GEOID_1")

# Subset for flows where est>moe
la_large_flows <- la_out_hpi %>%
  subset(flow>flowm)

## Distance calculations

# Create a data frame with the coordinates
la_point_data <- data.frame(lon = -118.2375, lat = 34.0460)
# Create an sf object for the point and set the CRS
la_point <- st_as_sf(la_point_data, coords = c("lon", "lat"), crs = st_crs(counties_pts))

la_distances <- as.data.frame(st_distance(counties_pts,la_point))
la_distances$GEOID_1 <- counties_pts$GEOID
names(la_distances)[1] <- "dist"

# Join distances back in large flows
la_large_flows <- inner_join(la_large_flows,la_distances,by="GEOID_1")

## Modeling
str(la_large_flows)

model_la <- lm(incr ~ ctypct, data = la_large_flows)
summary(model_la)

model_la_2 <- lm(incr ~ dist, data = la_large_flows)
summary(model_la_2)

model_la_3 <- lm(incr ~ dist + ctypct, data = la_large_flows)
summary(model_la_3)


## Condensed version
la_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6037) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6037)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

oc_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6059) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                              as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6059)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

riv_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6065) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6065)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

sb_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6071) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6071)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

sd_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6073) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6073)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

sf_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6075) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6075)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

sc_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6085) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6085)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

cc_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6013) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6013)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

ala_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                          subset(GEOID_0==6001) %>%
                                          subset(!(6000<GEOID_1&GEOID_1<6999))
                                        ,y=hpi1620_1,by="GEOID_1") %>%
                               subset(flow>flowm),
                             as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6001)))) %>%
                               mutate(GEOID_1 = counties_pts$GEOID) %>%
                               rename_with(~ "dist", 1),by="GEOID_1")

sac_large_flows <- inner_join(inner_join(x=out_df_num %>%
                                           subset(GEOID_0==6067) %>%
                                           subset(!(6000<GEOID_1&GEOID_1<6999))
                                         ,y=hpi1620_1,by="GEOID_1") %>%
                                subset(flow>flowm),
                              as.data.frame(st_distance(counties_pts,(counties_pts %>% subset(GEOID==6067)))) %>%
                                mutate(GEOID_1 = counties_pts$GEOID) %>%
                                rename_with(~ "dist", 1),by="GEOID_1")

la_dist_shp <- inner_join(la_distances %>% rename(GEOID=GEOID_1), counties_pts, "GEOID")
str(la_dist_shp)
plot(st_as_sf(la_dist_shp))

## PCA

# Example dataset (replace with your actual dataset)

str(la_large_flows_num)

la_large_flows_num <- na.omit(la_large_flows[,c(15,35,38,39)])

# Perform PCA
la_pca <- prcomp(la_large_flows_num, scale. = TRUE)  # Perform PCA with scaling

# Summary of PCA results
la_pca
summary(la_pca)

## Modeling

# Subset the data for a specific level of X2
la_large_flows_far <- subset(la_large_flows, dist > 20) 
# Kept 215/290

# Fit a regression model on the subset of data controlling for X2
model_1 <- lm(incr ~ ctypct, data = la_large_flows_far)
summary(model_1)
# sooooo insignificant

# Subset the data for a specific level of X2
sd_large_flows_far <- subset(sd_large_flows, dist > 20) 
# Kept 215/290

# Fit a regression model on the subset of data controlling for X2
model_1 <- lm(incr ~ ctypct, data = sd_large_flows_far)
summary(model_1)
# sooooo insignificant

# Subset the data for a specific level of X2
oc_large_flows_far <- subset(oc_large_flows, dist > 20) 
# Kept 215/290

View(oc_large_flows_far)

# Fit a regression model on the subset of data controlling for X2
model_1 <- lm(incr ~ ctypct, data = oc_large_flows_far)
summary(model_1)
# sooooo insignificant

# LA
summary(lm(incr ~ ctypct, data = la_large_flows))

summary(lm(incr ~ dist, data = la_large_flows))

summary(lm(incr ~ dist + ctypct, data = la_large_flows))

summary(lm(ctypct ~ dist, data = la_large_flows))

# OC
summary(lm(incr ~ ctypct, data = oc_large_flows))

summary(lm(incr ~ dist, data = oc_large_flows))

summary(lm(incr ~ dist + ctypct, data = oc_large_flows))

summary(lm(ctypct ~ dist, data = oc_large_flows))

# Riv
summary(lm(incr ~ ctypct, data = riv_large_flows))

summary(lm(incr ~ dist, data = riv_large_flows))

summary(lm(incr ~ dist + ctypct, data = riv_large_flows))

summary(lm(ctypct ~ dist, data = riv_large_flows))

# SD
summary(lm(incr ~ ctypct, data = sd_large_flows))

summary(lm(incr ~ dist, data = sd_large_flows))

summary(lm(incr ~ dist + ctypct, data = sd_large_flows))

summary(lm(ctypct ~ dist, data = sd_large_flows))

# SB
summary(lm(incr ~ ctypct, data = sb_large_flows))

summary(lm(incr ~ dist, data = sb_large_flows))

summary(lm(incr ~ dist + ctypct, data = sb_large_flows))

summary(lm(ctypct ~ dist, data = sb_large_flows))

# SF
summary(lm(incr ~ ctypct, data = sf_large_flows))

summary(lm(incr ~ dist, data = sf_large_flows))

summary(lm(incr ~ dist + ctypct, data = sf_large_flows))

summary(lm(ctypct ~ dist, data = sf_large_flows))

# SC
summary(lm(incr ~ ctypct, data = sc_large_flows))

summary(lm(incr ~ dist, data = sc_large_flows))

summary(lm(incr ~ dist + ctypct, data = sc_large_flows))

summary(lm(ctypct ~ dist, data = sc_large_flows))

# CC
summary(lm(incr ~ ctypct, data = cc_large_flows))

summary(lm(incr ~ dist, data = cc_large_flows))

summary(lm(incr ~ dist + ctypct, data = cc_large_flows))

summary(lm(ctypct ~ dist, data = cc_large_flows))

# Ala
summary(lm(incr ~ ctypct, data = ala_large_flows))

summary(lm(incr ~ dist, data = ala_large_flows))

summary(lm(incr ~ dist + ctypct, data = ala_large_flows))

summary(lm(ctypct ~ dist, data = ala_large_flows))

# Sac
summary(lm(incr ~ ctypct, data = sac_large_flows))

summary(lm(incr ~ dist, data = sac_large_flows))

summary(lm(incr ~ dist + ctypct, data = sac_large_flows))

summary(lm(ctypct ~ dist, data = sac_large_flows))


## Similarity scores of migration patterns

# Create LA Origin Subset
ca_out <- out_df_num %>%
  subset(6000<GEOID_0&GEOID_0<6999) %>%
  subset(!(6000<GEOID_1&GEOID_1<6999))

ca_out_spread <- ca_out %>%
  dplyr::select(cty_0, GEOID_1,flow) %>%
  spread(GEOID_1,flow)

ca_out_spread[is.na(ca_out_spread)] <- 0

write_csv(ca_out_spread,"~/GISC 37104/Final_Project/Outputs/ca_out_spread.csv")

## K-means (raw)

# Perform K-means clustering (example with 3 clusters)
set.seed(1338)
num_clusters <- 5

# Scaling the data (if needed)
scaled_data <- scale(ca_out_spread[2:ncol(ca_out_spread)])

# Perform K-means clustering
kmeans_result <- kmeans(scaled_data, centers = num_clusters)

# Get cluster assignments for each row
cluster_assignments <- kmeans_result$cluster

kmeans_df <- as.data.frame(kmeans_result$cluster)
kmeans_df$cty <- ca_out_spread$cty_0

View(kmeans_df)

ca_out_pct <- ca_out_spread[2:ncol(ca_out_spread)] / rowSums(ca_out_spread[2:ncol(ca_out_spread)])

## K-means (pct of total domestic interstate emigration)

# Perform K-means clustering (example with 3 clusters)
set.seed(1338)
num_clusters <- 5

# Scaling the data (if needed)
scaled_data <- scale(ca_out_pct)

# Perform K-means clustering
kmeans_result <- kmeans(scaled_data, centers = num_clusters)

# Get cluster assignments for each row
cluster_assignments <- kmeans_result$cluster

kmeans_df_2 <- as.data.frame(kmeans_result$cluster)
kmeans_df_2$cty <- ca_out_spread$cty_0

View(kmeans_df_2)

# Join with HPI 16-20
la_out_hpi <- inner_join(x=la_out,y=hpi1620_1,by="GEOID_1")

# Subset for flows where est>moe
la_large_flows <- la_out_hpi %>%
  subset(flow>flowm)

###########################

# Perform PCA
transposed_matrix <- t(as.matrix(ca_out_spread[,2:ncol(ca_out_spread)]))
transposed_df <- as.data.frame(transposed_matrix)

str(transposed_df)

us_pca <- prcomp(transposed_df, scale. = TRUE)  # Perform PCA with scaling

# Summary of PCA results
us_pca
summary(us_pca)

top5_us <- as.data.frame(us_pca$x)[1:5]
View(top5_us)

top5_us <- top5_us %>%
  tibble::rownames_to_column(var = "GEOID")

top5_us$GEOID <- as.numeric(top5_us$GEOID)

pca_us_join <- inner_join(counties,top5_us,by="GEOID") %>%
  select("PC1","PC2","PC3","PC4","PC5")

st_write(pca_us_join,"~/GISC 37104/Final_Project/Outputs/pca_join.shp")

## More PCA: transposing it back

ca_pca <- prcomp(ca_out_pct, scale. = TRUE)  # Perform PCA with scaling

str(ca_out_spread)
head(ca_out_spread[,2:ncol(ca_out_spread)])
# Summary of PCA results

top5_ca <- as.data.frame(ca_pca$x)[1:5]

ca_cty <- out_df_num %>%
  dplyr::select(GEOID_0,cty_0) %>%
  subset(6000<GEOID_0&GEOID_0<6999)

# Identify rows with unique values in the 'ID' column
unique_rows <- !duplicated(ca_cty$GEOID_0)
# Select only rows with unique values in the 'ID' column
ca_cty <- ca_cty[unique_rows, ]

top5_ca$GEOID <- ca_cty$GEOID_0
View(top5_ca)

pca_ca_join <- inner_join(counties,top5_ca,by="GEOID") %>%
  select("PC1","PC2","PC3","PC4","PC5")

st_write(pca_ca_join,"~/GISC 37104/Final_Project/Outputs/pca_ca_pct_join.shp")


# Plotting

# plot(log(la_large_flows$ctypct), la_large_flows$incr, main = "ln(Percent of Incomers from LA) vs. Increase in HPI")
# abline(model_la, col = "red")  # Add regression line to the plot
# 
# counties_1 <- counties %>%
#   rename(GEOID_1 = GEOID)
# la_out_hpi_geo <- inner_join(la_out_hpi, counties_1, "GEOID_1")
# st_write(la_out_hpi_geo, "~/GISC 37104/Final_Project/Outputs/la_out_hpi.shp")

#######
# Unnecessary In File thing

############

# Replace this with your Excel file path
in_file <- "~/GISC 37104/Final_Project/Data/current_residence_sort.xlsx"  # Replace with your Excel file path

# Get all sheet names in the Excel file
in_sheets <- excel_sheets(in_file)

# Read all sheets and combine them into one dataframe
in_dfs <- lapply(in_sheets, function(sheet) {
  read_excel(in_file, sheet = sheet)
})
combined_in_df <- bind_rows(in_dfs)
combined_in_df_small <- combined_in_df[, c(1:34, 37:38)]

in_df_nona <- subset(combined_in_df_small, complete.cases(combined_in_df_small))

write_xlsx(in_df_nona,"~/GISC 37104/Final_Project/Intermediates/in_df_nona.xlsx")

# Clean file in Excel, put it back in
in_df <- read_excel("~/GISC 37104/Final_Project/Intermediates/in_df_nona.xlsx")

# Fix column formats
in_df_num <- in_df
in_df_num[, c(1:2,5:18, 21:34)] <- lapply(in_df_num[, c(1:2,5:18, 21:34)], as.numeric)
str(in_df_num)
