Assignment 01: Exploratory Data Analysis
================
Flemming Wu
2022-09-09

#### We will work with air pollution data from the U.S. Environmental Protection Agency (EPA). The primary question you will answer is whether daily concentrations of PM 2.5 (particulate matter air pollution with aerodynamic diameter less than 2.5 m) have decreased in California over the last 15 years (from 2004 to 2019).

``` r
library(tidyverse)
library(leaflet)
library(webshot)
```

##### 1. Read in the data using data.table(). For each of the two datasets, check the dimensions, headers, footers, variable names, and variable types. Check for any data issues, particularly in the key variable you are analyzing. Make sure you write up a summary of all of your findings.

``` r
if(!file.exists("data_2004.csv")){
  download.file("https://raw.githubusercontent.com/flemm0/PM_566/main/01-assignment/data_2004.csv", destfile = "data_2004.csv", method="libcurl", timeout = 60)
}

if(!file.exists("data_2019.csv")){
  download.file("https://raw.githubusercontent.com/flemm0/PM_566/main/01-assignment/data_2019.csv", destfile = "data_2019.csv", method="libcurl", timeout=60)
}

data_2004 <- data.table::fread("data_2004.csv")
data_2019 <- data.table::fread("data_2019.csv")
```

``` r
noquote(c("2004 dimensions: ", dim(data_2004)))
```

    ## [1] 2004 dimensions:  19233             20

``` r
noquote(c("2019 dimensions: ", dim(data_2019)))
```

    ## [1] 2019 dimensions:  53156             20

``` r
head(data_2004)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 01/01/2004    AQS 60010007   1                            8.9 ug/m3 LC
    ## 2: 01/02/2004    AQS 60010007   1                           12.2 ug/m3 LC
    ## 3: 01/03/2004    AQS 60010007   1                           16.5 ug/m3 LC
    ## 4: 01/04/2004    AQS 60010007   1                           18.1 ug/m3 LC
    ## 5: 01/05/2004    AQS 60010007   1                           11.5 ug/m3 LC
    ## 6: 01/06/2004    AQS 60010007   1                           32.5 ug/m3 LC
    ##    DAILY_AQI_VALUE Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              37 Livermore               1              100
    ## 2:              51 Livermore               1              100
    ## 3:              60 Livermore               1              100
    ## 4:              64 Livermore               1              100
    ## 5:              48 Livermore               1              100
    ## 6:              94 Livermore               1              100
    ##    AQS_PARAMETER_CODE                     AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88101               PM2.5 - Local Conditions     41860
    ## 2:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ## 3:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ## 4:              88101               PM2.5 - Local Conditions     41860
    ## 5:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ## 6:              88502 Acceptable PM2.5 AQI & Speciation Mass     41860
    ##                            CBSA_NAME STATE_CODE      STATE COUNTY_CODE  COUNTY
    ## 1: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 2: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 3: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 4: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 5: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 6: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ##    SITE_LATITUDE SITE_LONGITUDE
    ## 1:      37.68753      -121.7842
    ## 2:      37.68753      -121.7842
    ## 3:      37.68753      -121.7842
    ## 4:      37.68753      -121.7842
    ## 5:      37.68753      -121.7842
    ## 6:      37.68753      -121.7842

``` r
tail(data_2004)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 12/14/2004    AQS 61131003   1                             11 ug/m3 LC
    ## 2: 12/17/2004    AQS 61131003   1                             16 ug/m3 LC
    ## 3: 12/20/2004    AQS 61131003   1                             17 ug/m3 LC
    ## 4: 12/23/2004    AQS 61131003   1                              9 ug/m3 LC
    ## 5: 12/26/2004    AQS 61131003   1                             24 ug/m3 LC
    ## 6: 12/29/2004    AQS 61131003   1                              9 ug/m3 LC
    ##    DAILY_AQI_VALUE            Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              46 Woodland-Gibson Road               1              100
    ## 2:              59 Woodland-Gibson Road               1              100
    ## 3:              61 Woodland-Gibson Road               1              100
    ## 4:              38 Woodland-Gibson Road               1              100
    ## 5:              76 Woodland-Gibson Road               1              100
    ## 6:              38 Woodland-Gibson Road               1              100
    ##    AQS_PARAMETER_CODE       AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88101 PM2.5 - Local Conditions     40900
    ## 2:              88101 PM2.5 - Local Conditions     40900
    ## 3:              88101 PM2.5 - Local Conditions     40900
    ## 4:              88101 PM2.5 - Local Conditions     40900
    ## 5:              88101 PM2.5 - Local Conditions     40900
    ## 6:              88101 PM2.5 - Local Conditions     40900
    ##                                  CBSA_NAME STATE_CODE      STATE COUNTY_CODE
    ## 1: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 2: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 3: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 4: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 5: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 6: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ##    COUNTY SITE_LATITUDE SITE_LONGITUDE
    ## 1:   Yolo      38.66121      -121.7327
    ## 2:   Yolo      38.66121      -121.7327
    ## 3:   Yolo      38.66121      -121.7327
    ## 4:   Yolo      38.66121      -121.7327
    ## 5:   Yolo      38.66121      -121.7327
    ## 6:   Yolo      38.66121      -121.7327

``` r
head(data_2019)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 01/01/2019    AQS 60010007   3                            5.7 ug/m3 LC
    ## 2: 01/02/2019    AQS 60010007   3                           11.9 ug/m3 LC
    ## 3: 01/03/2019    AQS 60010007   3                           20.1 ug/m3 LC
    ## 4: 01/04/2019    AQS 60010007   3                           28.8 ug/m3 LC
    ## 5: 01/05/2019    AQS 60010007   3                           11.2 ug/m3 LC
    ## 6: 01/06/2019    AQS 60010007   3                            2.7 ug/m3 LC
    ##    DAILY_AQI_VALUE Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              24 Livermore               1              100
    ## 2:              50 Livermore               1              100
    ## 3:              68 Livermore               1              100
    ## 4:              86 Livermore               1              100
    ## 5:              47 Livermore               1              100
    ## 6:              11 Livermore               1              100
    ##    AQS_PARAMETER_CODE       AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88101 PM2.5 - Local Conditions     41860
    ## 2:              88101 PM2.5 - Local Conditions     41860
    ## 3:              88101 PM2.5 - Local Conditions     41860
    ## 4:              88101 PM2.5 - Local Conditions     41860
    ## 5:              88101 PM2.5 - Local Conditions     41860
    ## 6:              88101 PM2.5 - Local Conditions     41860
    ##                            CBSA_NAME STATE_CODE      STATE COUNTY_CODE  COUNTY
    ## 1: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 2: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 3: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 4: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 5: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ## 6: San Francisco-Oakland-Hayward, CA          6 California           1 Alameda
    ##    SITE_LATITUDE SITE_LONGITUDE
    ## 1:      37.68753      -121.7842
    ## 2:      37.68753      -121.7842
    ## 3:      37.68753      -121.7842
    ## 4:      37.68753      -121.7842
    ## 5:      37.68753      -121.7842
    ## 6:      37.68753      -121.7842

``` r
tail(data_2019)
```

    ##          Date Source  Site ID POC Daily Mean PM2.5 Concentration    UNITS
    ## 1: 11/11/2019    AQS 61131003   1                           13.5 ug/m3 LC
    ## 2: 11/17/2019    AQS 61131003   1                           18.1 ug/m3 LC
    ## 3: 11/29/2019    AQS 61131003   1                           12.5 ug/m3 LC
    ## 4: 12/17/2019    AQS 61131003   1                           23.8 ug/m3 LC
    ## 5: 12/23/2019    AQS 61131003   1                            1.0 ug/m3 LC
    ## 6: 12/29/2019    AQS 61131003   1                            9.1 ug/m3 LC
    ##    DAILY_AQI_VALUE            Site Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1:              54 Woodland-Gibson Road               1              100
    ## 2:              64 Woodland-Gibson Road               1              100
    ## 3:              52 Woodland-Gibson Road               1              100
    ## 4:              76 Woodland-Gibson Road               1              100
    ## 5:               4 Woodland-Gibson Road               1              100
    ## 6:              38 Woodland-Gibson Road               1              100
    ##    AQS_PARAMETER_CODE       AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88101 PM2.5 - Local Conditions     40900
    ## 2:              88101 PM2.5 - Local Conditions     40900
    ## 3:              88101 PM2.5 - Local Conditions     40900
    ## 4:              88101 PM2.5 - Local Conditions     40900
    ## 5:              88101 PM2.5 - Local Conditions     40900
    ## 6:              88101 PM2.5 - Local Conditions     40900
    ##                                  CBSA_NAME STATE_CODE      STATE COUNTY_CODE
    ## 1: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 2: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 3: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 4: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 5: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ## 6: Sacramento--Roseville--Arden-Arcade, CA          6 California         113
    ##    COUNTY SITE_LATITUDE SITE_LONGITUDE
    ## 1:   Yolo      38.66121      -121.7327
    ## 2:   Yolo      38.66121      -121.7327
    ## 3:   Yolo      38.66121      -121.7327
    ## 4:   Yolo      38.66121      -121.7327
    ## 5:   Yolo      38.66121      -121.7327
    ## 6:   Yolo      38.66121      -121.7327

``` r
str(data_2004)
```

    ## Classes 'data.table' and 'data.frame':   19233 obs. of  20 variables:
    ##  $ Date                          : chr  "01/01/2004" "01/02/2004" "01/03/2004" "01/04/2004" ...
    ##  $ Source                        : chr  "AQS" "AQS" "AQS" "AQS" ...
    ##  $ Site ID                       : int  60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 ...
    ##  $ POC                           : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Daily Mean PM2.5 Concentration: num  8.9 12.2 16.5 18.1 11.5 32.5 14 29.9 21 15.7 ...
    ##  $ UNITS                         : chr  "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" ...
    ##  $ DAILY_AQI_VALUE               : int  37 51 60 64 48 94 55 88 70 59 ...
    ##  $ Site Name                     : chr  "Livermore" "Livermore" "Livermore" "Livermore" ...
    ##  $ DAILY_OBS_COUNT               : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PERCENT_COMPLETE              : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ AQS_PARAMETER_CODE            : int  88101 88502 88502 88101 88502 88502 88101 88502 88502 88101 ...
    ##  $ AQS_PARAMETER_DESC            : chr  "PM2.5 - Local Conditions" "Acceptable PM2.5 AQI & Speciation Mass" "Acceptable PM2.5 AQI & Speciation Mass" "PM2.5 - Local Conditions" ...
    ##  $ CBSA_CODE                     : int  41860 41860 41860 41860 41860 41860 41860 41860 41860 41860 ...
    ##  $ CBSA_NAME                     : chr  "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" ...
    ##  $ STATE_CODE                    : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ STATE                         : chr  "California" "California" "California" "California" ...
    ##  $ COUNTY_CODE                   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY                        : chr  "Alameda" "Alameda" "Alameda" "Alameda" ...
    ##  $ SITE_LATITUDE                 : num  37.7 37.7 37.7 37.7 37.7 ...
    ##  $ SITE_LONGITUDE                : num  -122 -122 -122 -122 -122 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
str(data_2019)
```

    ## Classes 'data.table' and 'data.frame':   53156 obs. of  20 variables:
    ##  $ Date                          : chr  "01/01/2019" "01/02/2019" "01/03/2019" "01/04/2019" ...
    ##  $ Source                        : chr  "AQS" "AQS" "AQS" "AQS" ...
    ##  $ Site ID                       : int  60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 60010007 ...
    ##  $ POC                           : int  3 3 3 3 3 3 3 3 3 3 ...
    ##  $ Daily Mean PM2.5 Concentration: num  5.7 11.9 20.1 28.8 11.2 2.7 2.8 7 3.1 7.1 ...
    ##  $ UNITS                         : chr  "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" "ug/m3 LC" ...
    ##  $ DAILY_AQI_VALUE               : int  24 50 68 86 47 11 12 29 13 30 ...
    ##  $ Site Name                     : chr  "Livermore" "Livermore" "Livermore" "Livermore" ...
    ##  $ DAILY_OBS_COUNT               : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ PERCENT_COMPLETE              : num  100 100 100 100 100 100 100 100 100 100 ...
    ##  $ AQS_PARAMETER_CODE            : int  88101 88101 88101 88101 88101 88101 88101 88101 88101 88101 ...
    ##  $ AQS_PARAMETER_DESC            : chr  "PM2.5 - Local Conditions" "PM2.5 - Local Conditions" "PM2.5 - Local Conditions" "PM2.5 - Local Conditions" ...
    ##  $ CBSA_CODE                     : int  41860 41860 41860 41860 41860 41860 41860 41860 41860 41860 ...
    ##  $ CBSA_NAME                     : chr  "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" "San Francisco-Oakland-Hayward, CA" ...
    ##  $ STATE_CODE                    : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  $ STATE                         : chr  "California" "California" "California" "California" ...
    ##  $ COUNTY_CODE                   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY                        : chr  "Alameda" "Alameda" "Alameda" "Alameda" ...
    ##  $ SITE_LATITUDE                 : num  37.7 37.7 37.7 37.7 37.7 ...
    ##  $ SITE_LONGITUDE                : num  -122 -122 -122 -122 -122 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

It looks like some of the column names are separated by space, so will
update them to be syntactically valid.

``` r
names(data_2004) <- make.names(names(data_2004))
names(data_2019) <- make.names(names(data_2019))
```

Checking Key Variable: PM 2.5 Concentration

``` r
summary(data_2004$Daily.Mean.PM2.5.Concentration)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -0.10    6.00   10.10   13.13   16.30  251.00

``` r
table(data_2004$Daily.Mean.PM2.5.Concentration) %>% head()
```

    ## 
    ## -0.1    0  0.1  0.2  0.3  0.4 
    ##    1   11   15   20   27   32

``` r
table(data_2004$Daily.Mean.PM2.5.Concentration) %>% tail()
```

    ## 
    ## 102.1 110.4 122.5 148.4 170.4   251 
    ##     1     1     1     1     1     1

``` r
summary(data_2019$Daily.Mean.PM2.5.Concentration)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -2.20    4.00    6.50    7.74    9.90  120.90

``` r
table(data_2019$Daily.Mean.PM2.5.Concentration) %>% head()
```

    ## 
    ## -2.2   -2 -1.9 -1.8 -1.7 -1.6 
    ##    1   12   16   11   12   12

``` r
table(data_2019$Daily.Mean.PM2.5.Concentration) %>% tail()
```

    ## 
    ##  91.1  97.3  98.9 103.5 104.5 120.9 
    ##     1     1     1     1     1     1

The EPA standard for PM 2.5 concentration post-1997 is 65 ug/m3 and was
lowered to 35 ug/m3 since 2006. The negative concentrations of PM 2.5 as
well as the very high maximums will be investigated.

``` r
data_2004 %>%
  filter(Daily.Mean.PM2.5.Concentration < 0) %>%
  select(Daily.Mean.PM2.5.Concentration, Site.Name, CBSA_NAME)
```

    ##    Daily.Mean.PM2.5.Concentration         Site.Name  CBSA_NAME
    ## 1:                           -0.1 Kaiser Wilderness Fresno, CA

``` r
filter(data_2004, Site.Name == "Kaiser Wilderness") %>%
  select(Daily.Mean.PM2.5.Concentration) %>%
  table()
```

    ## Daily.Mean.PM2.5.Concentration
    ## -0.1  0.1  0.2  0.3  0.4  0.5  0.6  0.8  0.9    1  1.1  1.3  1.4  1.5  1.6  1.7 
    ##    1    3    3    3    4    4    4    3    3    4    4    2    2    1    1    1 
    ##    2  2.1  2.2  2.4  2.5  2.6  2.7  2.8  2.9    3  3.1  3.2  3.3  3.5  3.6  3.7 
    ##    1    1    2    1    1    2    3    3    1    1    1    1    1    3    3    1 
    ##  3.8    4  4.1  4.2  4.3  4.4  4.6  4.7  4.8  4.9  5.1  5.3  5.4  5.5  5.7  5.8 
    ##    2    3    1    2    1    1    2    1    1    1    3    1    1    1    2    2 
    ##  5.9  6.3  6.4  6.5  6.8  6.9    7  7.9  8.3  8.4  8.5  9.4 
    ##    1    4    2    1    1    1    1    1    1    1    1    1

``` r
data_2019 %>%
  filter(Daily.Mean.PM2.5.Concentration < 0) %>%
  select(Site.Name) %>%
  unique()
```

    ##                                Site.Name
    ##  1:                         Oakland West
    ##  2:                        Laney College
    ##  3:                    Chico-East Avenue
    ##  4:                   Paradise - Theater
    ##  5:                              Concord
    ##  6:   Table Mountain Air Monitoring Site
    ##  7:                                Huron
    ##  8:                         Tranquillity
    ##  9:                      Ridgecrest-Ward
    ## 10:                                Lebec
    ## 11:            Lancaster-Division Street
    ## 12:                        Ukiah-Library
    ## 13:   Willits-125 East Commercial Street
    ## 14:                        Carmel Valley
    ## 15:                            Salinas 3
    ## 16:                        Auburn-Atwood
    ## 17:                     Colfax-City Hall
    ## 18:             Tahoe City-Fairway Drive
    ## 19:                             Pechanga
    ## 20:       Morongo Air Monitoring Station
    ## 21:                             Rubidoux
    ## 22:                        Lake Elsinore
    ## 23:            Sacramento-Del Paso Manor
    ## 24:                    Folsom-Natoma St.
    ## 25:                          Sloughhouse
    ## 26:                               Upland
    ## 27:                       Camp Pendleton
    ## 28:                              Manteca
    ## 29:                    Arroyo Grande CDF
    ## 30:                           Atascadero
    ## 31:                         Redwood City
    ## 32:                          Santa Maria
    ## 33:                      Lompoc H Street
    ## 34:                               Goleta
    ## 35: Red Bluff-Walnut St. District Office
    ## 36:               Weaverville-Courthouse
    ## 37:                        Thousand Oaks
    ## 38:                       Piru - Pacific
    ## 39:           Simi Valley-Cochran Street
    ## 40:            El Rio-Rio Mesa School #2
    ##                                Site.Name

``` r
quantile(data_2019$Daily.Mean.PM2.5.Concentration, seq(0, 1, 0.1))
```

    ##    0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
    ##  -2.2   2.3   3.5   4.5   5.5   6.5   7.7   9.1  10.9  14.2 120.9

##### 2. Combine the two years of data into one data frame. Use the Date variable to create a new column for year, which will serve as an identifier. Change the names of the key variables so that they are easier to refer to in your code.

``` r
data_2004 <- mutate(data_2004, Date = as.Date(Date, "%m/%d/%Y"))
data_2004$year <- format(as.Date(data_2004$Date, format="%m/%d/%Y"),"%Y")


data_2019 <- mutate(data_2019, Date = as.Date(Date, "%m/%d/%Y"))
data_2019$year <- format(as.Date(data_2019$Date, format="%m/%d/%Y"),"%Y")
```

``` r
df_all <- rbind(data_2004, data_2019)

names(df_all) <- make.names(names(df_all))
names(df_all)[names(df_all) == "Daily.Mean.PM2.5.Concentration"] <- "PM2.5Concentration"

df_all <- df_all[order(df_all$PM2.5Concentration)] #Ascending order by PM 2.5
```

``` r
df_all
```

    ##              Date Source  Site.ID POC PM2.5Concentration    UNITS
    ##     1: 2019-03-16    AQS 60130002   3               -2.2 ug/m3 LC
    ##     2: 2019-03-02    AQS 60611004   3               -2.0 ug/m3 LC
    ##     3: 2019-03-05    AQS 60611004   3               -2.0 ug/m3 LC
    ##     4: 2019-03-06    AQS 60611004   3               -2.0 ug/m3 LC
    ##     5: 2019-03-07    AQS 60611004   3               -2.0 ug/m3 LC
    ##    ---                                                           
    ## 72385: 2019-10-11    AQS 60371201   3              120.9 ug/m3 LC
    ## 72386: 2004-07-21    AQS 60431001   3              122.5 ug/m3 LC
    ## 72387: 2004-07-20    AQS 60431001   3              148.4 ug/m3 LC
    ## 72388: 2004-07-19    AQS 60431001   3              170.4 ug/m3 LC
    ## 72389: 2004-07-18    AQS 60431001   3              251.0 ug/m3 LC
    ##        DAILY_AQI_VALUE                                  Site.Name
    ##     1:               0                                    Concord
    ##     2:               0                   Tahoe City-Fairway Drive
    ##     3:               0                   Tahoe City-Fairway Drive
    ##     4:               0                   Tahoe City-Fairway Drive
    ##     5:               0                   Tahoe City-Fairway Drive
    ##    ---                                                           
    ## 72385:             185                                     Reseda
    ## 72386:             186 Yosemite NP-Yosemite Village Vistor Center
    ## 72387:             199 Yosemite NP-Yosemite Village Vistor Center
    ## 72388:             221 Yosemite NP-Yosemite Village Vistor Center
    ## 72389:             301 Yosemite NP-Yosemite Village Vistor Center
    ##        DAILY_OBS_COUNT PERCENT_COMPLETE AQS_PARAMETER_CODE
    ##     1:               1              100              88101
    ##     2:               1              100              88502
    ##     3:               1              100              88502
    ##     4:               1              100              88502
    ##     5:               1              100              88502
    ##    ---                                                    
    ## 72385:               1              100              88502
    ## 72386:               1              100              88502
    ## 72387:               1              100              88502
    ## 72388:               1              100              88502
    ## 72389:               1              100              88502
    ##                            AQS_PARAMETER_DESC CBSA_CODE
    ##     1:               PM2.5 - Local Conditions     41860
    ##     2: Acceptable PM2.5 AQI & Speciation Mass     40900
    ##     3: Acceptable PM2.5 AQI & Speciation Mass     40900
    ##     4: Acceptable PM2.5 AQI & Speciation Mass     40900
    ##     5: Acceptable PM2.5 AQI & Speciation Mass     40900
    ##    ---                                                 
    ## 72385: Acceptable PM2.5 AQI & Speciation Mass     31080
    ## 72386: Acceptable PM2.5 AQI & Speciation Mass        NA
    ## 72387: Acceptable PM2.5 AQI & Speciation Mass        NA
    ## 72388: Acceptable PM2.5 AQI & Speciation Mass        NA
    ## 72389: Acceptable PM2.5 AQI & Speciation Mass        NA
    ##                                      CBSA_NAME STATE_CODE      STATE
    ##     1:       San Francisco-Oakland-Hayward, CA          6 California
    ##     2: Sacramento--Roseville--Arden-Arcade, CA          6 California
    ##     3: Sacramento--Roseville--Arden-Arcade, CA          6 California
    ##     4: Sacramento--Roseville--Arden-Arcade, CA          6 California
    ##     5: Sacramento--Roseville--Arden-Arcade, CA          6 California
    ##    ---                                                              
    ## 72385:      Los Angeles-Long Beach-Anaheim, CA          6 California
    ## 72386:                                                  6 California
    ## 72387:                                                  6 California
    ## 72388:                                                  6 California
    ## 72389:                                                  6 California
    ##        COUNTY_CODE       COUNTY SITE_LATITUDE SITE_LONGITUDE year
    ##     1:          13 Contra Costa      37.93601      -122.0262 2019
    ##     2:          61       Placer      39.16602      -120.1488 2019
    ##     3:          61       Placer      39.16602      -120.1488 2019
    ##     4:          61       Placer      39.16602      -120.1488 2019
    ##     5:          61       Placer      39.16602      -120.1488 2019
    ##    ---                                                           
    ## 72385:          37  Los Angeles      34.19925      -118.5328 2019
    ## 72386:          43     Mariposa      37.74871      -119.5871 2004
    ## 72387:          43     Mariposa      37.74871      -119.5871 2004
    ## 72388:          43     Mariposa      37.74871      -119.5871 2004
    ## 72389:          43     Mariposa      37.74871      -119.5871 2004

##### 3. Create a basic map in leaflet() that shows the locations of the sites (make sure to use different colors for each year). Summarize the spatial distribution of the monitoring sites.

``` r
pal <- colorFactor(
  palette = c('red', 'blue'),
  domain = df_all$year
)

leaflet(df_all) %>%
  addProviderTiles('OpenStreetMap') %>%
  addCircles(lat=~SITE_LATITUDE,lng=~SITE_LONGITUDE, color = ~pal(year))
```

![](01-assignment-EDA_files/figure-gfm/Plotting%20leaflet-1.png)<!-- -->

##### 4. Check for any missing or implausible values of PM 2.5 in the combined dataset. Explore the proportions of each and provide a summary of any temporal patterns you see in these observations.

``` r
sum(as.numeric(is.na(df_all$PM2.5Concentration)))
```

    ## [1] 0

No missing values

``` r
summary(df_all$PM2.5Concentration)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -2.200   4.400   7.200   9.171  11.300 251.000

In the case of concentration of particulate matter in the air, it seems
implausible to have a negative concentration. Remove these observations
from the data.

``` r
df_all %>%
  filter(PM2.5Concentration < 0) %>%
  group_by(Site.Name, year) %>%
  summarise(negative_readings = n()) %>%
  arrange(desc(negative_readings))
```

    ## `summarise()` has grouped output by 'Site.Name'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 41 × 3
    ## # Groups:   Site.Name [41]
    ##    Site.Name                            year  negative_readings
    ##    <chr>                                <chr>             <int>
    ##  1 Tahoe City-Fairway Drive             2019                153
    ##  2 Lake Elsinore                        2019                 30
    ##  3 Lompoc H Street                      2019                 12
    ##  4 Arroyo Grande CDF                    2019                  9
    ##  5 Piru - Pacific                       2019                  5
    ##  6 Ridgecrest-Ward                      2019                  5
    ##  7 Salinas 3                            2019                  5
    ##  8 Lebec                                2019                  4
    ##  9 Red Bluff-Walnut St. District Office 2019                  4
    ## 10 Table Mountain Air Monitoring Site   2019                  4
    ## # … with 31 more rows

``` r
df_all %>%
  filter(PM2.5Concentration == max(PM2.5Concentration))
```

    ##          Date Source  Site.ID POC PM2.5Concentration    UNITS DAILY_AQI_VALUE
    ## 1: 2004-07-18    AQS 60431001   3                251 ug/m3 LC             301
    ##                                     Site.Name DAILY_OBS_COUNT PERCENT_COMPLETE
    ## 1: Yosemite NP-Yosemite Village Vistor Center               1              100
    ##    AQS_PARAMETER_CODE                     AQS_PARAMETER_DESC CBSA_CODE
    ## 1:              88502 Acceptable PM2.5 AQI & Speciation Mass        NA
    ##    CBSA_NAME STATE_CODE      STATE COUNTY_CODE   COUNTY SITE_LATITUDE
    ## 1:                    6 California          43 Mariposa      37.74871
    ##    SITE_LONGITUDE year
    ## 1:      -119.5871 2004

##### 5. Explore the main question of interest at three different spatial levels. Create exploratory plots (e.g. boxplots, histograms, line plots) and summary statistics that best suit each level of data. Be sure to write up explanations of what you observe in these data.

``` r
boxplot(PM2.5Concentration ~ year, data = df_all, col = df_all$year)
```

![](01-assignment-EDA_files/figure-gfm/Boxplot%20of%20concentrations-1.png)<!-- -->

``` r
#Probably not the best option, as the data are highly skewed
```

``` r
par(mfrow = c(2, 1), mar = c(2, 4, 2, 1))
hist(subset(df_all, year == "2004")$PM2.5Concentration, col = "red", breaks = 200, main = "2004 PM 2.5 Concentration", xlab = "PM 2.5")
abline(v = 65, lwd = 2)
abline(v = median(subset(df_all, year == "2004")$PM2.5Concentration), lwd = 2, col = "purple")
hist(subset(df_all, year == "2019")$PM2.5Concentration, col = "blue", breaks = 200, main = "2019 PM 2.5 Concentration", xlab = "PM 2.5")
abline(v = 35, lwd = 2)
abline(v = median(subset(df_all, year == "2019")$PM2.5Concentration), lwd = 2, col = "purple")
```

![](01-assignment-EDA_files/figure-gfm/Histograms%20of%20concentrations-1.png)<!-- -->

``` r
#Need to adjust the x-axis to be the same scale
```

\#At .05 significance level, we conclude that the PM2.5 concentrations
in 2004 and 2019 from the data set are nonidentical populations.
