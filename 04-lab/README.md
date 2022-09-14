04-lab-Data-Visualization
================
Flemming Wu
2022-09-14

### 1. Read in the data

``` r
if(!file.exists("met_all.gz")){
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method = "libcurl",
    timeout = 60
  )
}

met <- data.table::fread("met_all.gz")
```

``` r
library(tidyverse)
library(data.table)
```

### 2. Preparing the data

``` r
dim(met)
```

    ## [1] 2377343      30

``` r
summary(met$elev)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -13.0   101.0   252.0   415.8   400.0  9999.0

``` r
summary(met$temp)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  -40.00   19.60   23.50   23.59   27.80   56.00   60089

``` r
met <- met[met$temp > -17][elev == 9999.0, elev := NA]
```

``` r
met$date = paste(met$year, met$month, met$day, sep = "-")
met <- mutate(met, date = as.Date(date, "%Y-%m-%d"))
```

``` r
met$week <- data.table::week(met$date)
```

``` r
met <- met[met$week == min(met$week)]
```

``` r
met_avg <- met[,.(
  temp     = mean(temp,na.rm=TRUE),
  rh       = mean(rh,na.rm=TRUE),
  wind.sp  = mean(wind.sp,na.rm=TRUE),
  vis.dist = mean(vis.dist,na.rm=TRUE),
  lat      = mean(lat),
  lon      = mean(lon), 
  elev     = mean(elev,na.rm=TRUE)
), by="USAFID"]
```

``` r
met_avg[, lat_region := fifelse(lon > -98.00, "E", "W")]
met_avg[, lon_region := fifelse(lat > 39.71, "N", "S")]
met_avg$region <- paste(met_avg$lon_region, met_avg$lat_region)
```

``` r
met_avg[, elev_cat := fifelse(elev > 252, "high", "low")]
```

### 3. Use geom_violin() to examine the wind speed and dew point temperature by region

``` r
#ggplot()
```
