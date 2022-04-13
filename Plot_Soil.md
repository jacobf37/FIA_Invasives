---
title: "FIA_SSURGO_Intersect"
output: 
  html_document: 
    keep_md: true
date: '2022-04-12'
---
Load packages.

```r
library(tidyverse)
library(rFIA)
library(soilDB)
library(sp)
```
Set some global variables.

```r
db_dir <- 'D:/FIA/rFIA'

db_states <- c('CT','DE','IL','IN','IA','KS','ME','MD',
                     'MA','MI','MN','MO','NE','NH','NJ','NY',
                     'ND','OH','PA','RI','SD','VT','WV','WI',
                     'AL','AR','FL','GA','KY','LA','MS','NC',
                     'OK','SC','TN','VA', 'TX')
```
## SSURGO Database
Read in plot coordinates and make spatial.


```r
plot_coord <- read_csv('./Data/plot_coordinates.csv', show_col_types = F)

p <- SpatialPointsDataFrame(plot_coord[, c('LON', 'LAT')], data = plot_coord[, 'pltID'], proj4string = CRS('+proj=longlat +datum=WGS84'))
```
Get unique FIA plot IDs.

```r
plt_id <- plot_coord %>% 
  mutate(pltID = pltID %>% str_remove('[1-9]_')) %>% 
  distinct(pltID) %>% pull()
```

Run spatial query at each point for MUKEY. Takes a while.

```r
res <- SDA_spatialQuery(p, what = 'mukey', db = 'SSURGO', byFeature = T)
```

Write out results.

```r
write_csv(p@data %>% bind_cols(res %>% select(mukey, muname)), './Data/SSURGO_intersect.csv')
```

## FIA Soil Data

Read in FIA soils data.


Check for plots with soil information.

```r
inv_soils_lab <- sub_db$SOILS_LAB %>% 
  mutate(pltID = stringr::str_c(STATECD, COUNTYCD, PLOT, sep = '_')) %>% 
           filter(pltID %in% plt_id) 
inv_soils_loc <- sub_db$SOILS_SAMPLE_LOC %>% 
  mutate(pltID = stringr::str_c(STATECD, COUNTYCD, PLOT, sep = '_')) %>% 
           filter(pltID %in% plt_id) 
print(paste('Lab plots -', nrow(inv_soils_lab), 'Local plots -', nrow(inv_soils_loc)))
```

```
## [1] "Lab plots - 240 Local plots - 267"
```

```r
rm(sub_db)
```

