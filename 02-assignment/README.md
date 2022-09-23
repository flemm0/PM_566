Assignment 02: Data Viz and Wrangling
================
Flemming Wu
2022-09-22

For this assignment, we will be analyzing data from USC’s Children’s
Health Study. The learning objectives are to conduct data wrangling and
visualize the data with key questions in mind.

### Data Wrangling

You will need to download two datasets from
<https://github.com/USCbiostats/data-science-data>. The individual and
regional CHS datasets in 01_chs. The individual data includes personal
and health characteristics of children in 12 communities across Southern
California. The regional data include air quality measurements at the
community level. Once downloaded, you can merge these datasets using the
location variable. Once combined, you will need to do the following:  

1.  After merging the data, make sure you don’t have any duplicates by
    counting the number of rows. Make sure it matches.  

In the case of missing values, impute data using the average within the
variables “male” and “hispanic.” If you are interested (and feel
adventurous) in the theme of Data Imputation, take a look at this paper
on “Multiple Imputation” using the Amelia R package here.  

2.  Create a new categorical variable named “obesity_level” using the
    BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight
    BMI 22-24; obese BMI\>24). To make sure the variable is rightly
    coded, create a summary table that contains the minimum BMI, maximum
    BMI, and the total number of observations per category.  

3.  Create another categorical variable named “smoke_gas_exposure” that
    summarizes “Second Hand Smoke” and “Gas Stove.” The variable should
    have four categories in total.  

4.  Create four summary tables showing the average (or proportion, if
    binary) and sd of “Forced expiratory volume in 1 second (ml)” and
    asthma indicator by town, sex, obesity level, and
    “smoke_gas_exposure.”  

Load libraries

``` r
library(data.table)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(dtplyr)
```

Download data

``` r
if (!file.exists("individual.csv")) {
    download.file(url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv",
        destfile = "individual.csv", method = "libcurl", timeout = 60)
}

if (!file.exists("regional.csv")) {
    download.file(url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv",
        destfile = "regional.csv", method = "libcurl", timeout = 60)
}

# Read data tables
individual <- fread("individual.csv")
regional <- fread("regional.csv")
```

Merge data tables and ensure dimensions are correct.

``` r
# Check rows
dim(individual)  #merged data table should contain 1200 rows
```

    ## [1] 1200   23

``` r
dim(regional)  #merged data table should have 23 + 26 columns
```

    ## [1] 12 27

``` r
dat <- merge(x = individual, y = regional, by.x = "townname",
    by.y = "townname")

# Check dimensions
dim(dat)  #1200 rows and 49 columns as expected
```

    ## [1] 1200   49

Impute missing values with averages within variables “male” and
“hispanic”

``` r
# get columns that have na count greater than 0, and pass
# them into a vector
cols <- names(which(colSums(is.na(dat)) > 0))


# subset data table for hispanic males and compute averages
# on the columns of interest
dat[male == 1 & hispanic == 1, lapply(.SD, mean, na.rm = TRUE),
    .SDcols = cols]
```

    ##      agepft   height   weight      bmi    asthma father_asthma mother_asthma
    ## 1: 9.966942 138.5984 82.76707 19.41148 0.1601562    0.08403361     0.1067194
    ##       wheeze  hayfever   allergy educ_parent     smoke  gasstove      fev
    ## 1: 0.3534137 0.1744681 0.2540323    2.423868 0.1501976 0.8156863 2120.266
    ##         fvc     mmef  no_24hr pm2_5_fr
    ## 1: 2443.876 2447.494 15.86585 19.94291

``` r
dat2 <- copy(dat)

dat2[, m_bmi := mean(bmi, na.rm = TRUE), by = .(male, hispanic)]

in_names  <- cols
out_names <- paste0(in_names, "_avg")
(dat3 <- merge(
  
  x = dat[,
  setNames(lapply(.SD, mean, na.rm = TRUE), out_names),
  .SDcols = in_names, 
  by  = .(hispanic, male)
  ],
  
  y = dat2,
  by.x = c("hispanic", "male"),
  by.y = c("hispanic", "male")
))
```

    ##       hispanic male agepft_avg height_avg weight_avg  bmi_avg asthma_avg
    ##    1:        0    0   9.849789   138.9720   77.39564 18.05281  0.1239193
    ##    2:        0    0   9.849789   138.9720   77.39564 18.05281  0.1239193
    ##    3:        0    0   9.849789   138.9720   77.39564 18.05281  0.1239193
    ##    4:        0    0   9.849789   138.9720   77.39564 18.05281  0.1239193
    ##    5:        0    0   9.849789   138.9720   77.39564 18.05281  0.1239193
    ##   ---                                                                   
    ## 1196:        1    1   9.966942   138.5984   82.76707 19.41148  0.1601562
    ## 1197:        1    1   9.966942   138.5984   82.76707 19.41148  0.1601562
    ## 1198:        1    1   9.966942   138.5984   82.76707 19.41148  0.1601562
    ## 1199:        1    1   9.966942   138.5984   82.76707 19.41148  0.1601562
    ## 1200:        1    1   9.966942   138.5984   82.76707 19.41148  0.1601562
    ##       father_asthma_avg mother_asthma_avg wheeze_avg hayfever_avg allergy_avg
    ##    1:        0.08750000         0.1190476  0.3153153    0.1920732   0.2991202
    ##    2:        0.08750000         0.1190476  0.3153153    0.1920732   0.2991202
    ##    3:        0.08750000         0.1190476  0.3153153    0.1920732   0.2991202
    ##    4:        0.08750000         0.1190476  0.3153153    0.1920732   0.2991202
    ##    5:        0.08750000         0.1190476  0.3153153    0.1920732   0.2991202
    ##   ---                                                                        
    ## 1196:        0.08403361         0.1067194  0.3534137    0.1744681   0.2540323
    ## 1197:        0.08403361         0.1067194  0.3534137    0.1744681   0.2540323
    ## 1198:        0.08403361         0.1067194  0.3534137    0.1744681   0.2540323
    ## 1199:        0.08403361         0.1067194  0.3534137    0.1744681   0.2540323
    ## 1200:        0.08403361         0.1067194  0.3534137    0.1744681   0.2540323
    ##       educ_parent_avg smoke_avg gasstove_avg  fev_avg  fvc_avg mmef_avg
    ##    1:        3.045977 0.1522989    0.7291066 1945.743 2198.915 2365.589
    ##    2:        3.045977 0.1522989    0.7291066 1945.743 2198.915 2365.589
    ##    3:        3.045977 0.1522989    0.7291066 1945.743 2198.915 2365.589
    ##    4:        3.045977 0.1522989    0.7291066 1945.743 2198.915 2365.589
    ##    5:        3.045977 0.1522989    0.7291066 1945.743 2198.915 2365.589
    ##   ---                                                                  
    ## 1196:        2.423868 0.1501976    0.8156863 2120.266 2443.876 2447.494
    ## 1197:        2.423868 0.1501976    0.8156863 2120.266 2443.876 2447.494
    ## 1198:        2.423868 0.1501976    0.8156863 2120.266 2443.876 2447.494
    ## 1199:        2.423868 0.1501976    0.8156863 2120.266 2443.876 2447.494
    ## 1200:        2.423868 0.1501976    0.8156863 2120.266 2443.876 2447.494
    ##       no_24hr_avg pm2_5_fr_avg townname  sid race    agepft height weight
    ##    1:    15.53682     20.54245   Alpine  835    W 10.099932    143     69
    ##    2:    15.53682     20.54245   Alpine  840    W  9.965777    146     78
    ##    3:    15.53682     20.54245   Alpine  860    W  9.946612    142     64
    ##    4:    15.53682     20.54245   Alpine  865    W 10.039699    162    140
    ##    5:    15.53682     20.54245   Alpine  873    W  9.804244    140    101
    ##   ---                                                                    
    ## 1196:    15.86585     19.94291   Upland 1816    W  9.631759    134     60
    ## 1197:    15.86585     19.94291   Upland 1833    W  9.453799    139     73
    ## 1198:    15.86585     19.94291   Upland 1839    M  9.776865    141     85
    ## 1199:    15.86585     19.94291   Upland 1850    O  9.338809    143     86
    ## 1200:    15.86585     19.94291   Upland 1862    W        NA     NA     NA
    ##            bmi asthma active_asthma father_asthma mother_asthma wheeze hayfever
    ##    1: 15.33749      0             0             0             0      0        0
    ##    2: 16.63283      0             0             0             0      0        0
    ##    3: 14.42715      0             0             0             0      0        0
    ##    4: 24.24797      1             1             0             0      1        0
    ##    5: 23.42301      0             0             0             0      0        0
    ##   ---                                                                          
    ## 1196: 15.18864      0             0             0             0      0        0
    ## 1197: 17.17397      0             0             0             0      1        0
    ## 1198: 19.43381      0             0             0             0     NA        1
    ## 1199: 19.11629      0             0             0             0      1       NA
    ## 1200:       NA      0             0             1             0      1        0
    ##       allergy educ_parent smoke pets gasstove      fev      fvc     mmef
    ##    1:       1           3     0    1        0 2529.276 2826.316 3406.579
    ##    2:       0          NA    NA    0       NA 2466.791 2638.221 3466.464
    ##    3:       0           2     0    0        1 1759.866 2194.314 1695.652
    ##    4:       1           3     0    1        1 2583.934 3567.541 2071.475
    ##    5:       0           3     0    1        0 2226.421 2406.020 2734.114
    ##   ---                                                                   
    ## 1196:       1           3     0    1        1 1783.168 2001.980 2126.733
    ## 1197:       0           5     0    0        1 2000.662 2255.960 2396.026
    ## 1198:       0           3     0    1        1 2279.402 2537.542 2641.196
    ## 1199:       1           3    NA    1        1 2428.672 2653.055 3489.301
    ## 1200:       0           4     0    1        0       NA       NA       NA
    ##       pm25_mass pm25_so4 pm25_no3 pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc
    ##    1:      8.74     1.73     1.59     0.88    2.54    0.48    3.04    3.25
    ##    2:      8.74     1.73     1.59     0.88    2.54    0.48    3.04    3.25
    ##    3:      8.74     1.73     1.59     0.88    2.54    0.48    3.04    3.25
    ##    4:      8.74     1.73     1.59     0.88    2.54    0.48    3.04    3.25
    ##    5:      8.74     1.73     1.59     0.88    2.54    0.48    3.04    3.25
    ##   ---                                                                     
    ## 1196:     22.46     2.65     7.75     2.96    6.49    1.19    7.79    8.32
    ## 1197:     22.46     2.65     7.75     2.96    6.49    1.19    7.79    8.32
    ## 1198:     22.46     2.65     7.75     2.96    6.49    1.19    7.79    8.32
    ## 1199:     22.46     2.65     7.75     2.96    6.49    1.19    7.79    8.32
    ## 1200:     22.46     2.65     7.75     2.96    6.49    1.19    7.79    8.32
    ##       pm10_ec pm10_tc formic acetic  hcl hno3 o3_max o3106 o3_24   no2  pm10
    ##    1:    0.49    3.75   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73
    ##    2:    0.49    3.75   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73
    ##    3:    0.49    3.75   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73
    ##    4:    0.49    3.75   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73
    ##    5:    0.49    3.75   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73
    ##   ---                                                                       
    ## 1196:    1.22    9.54   2.67   4.73 0.46 4.03  63.83 46.50 22.20 37.97 40.80
    ## 1197:    1.22    9.54   2.67   4.73 0.46 4.03  63.83 46.50 22.20 37.97 40.80
    ## 1198:    1.22    9.54   2.67   4.73 0.46 4.03  63.83 46.50 22.20 37.97 40.80
    ## 1199:    1.22    9.54   2.67   4.73 0.46 4.03  63.83 46.50 22.20 37.97 40.80
    ## 1200:    1.22    9.54   2.67   4.73 0.46 4.03  63.83 46.50 22.20 37.97 40.80
    ##       no_24hr pm2_5_fr iacid oacid total_acids       lon      lat    m_bmi
    ##    1:    2.48    10.28  2.39  3.52        5.50 -116.7664 32.83505 18.05281
    ##    2:    2.48    10.28  2.39  3.52        5.50 -116.7664 32.83505 18.05281
    ##    3:    2.48    10.28  2.39  3.52        5.50 -116.7664 32.83505 18.05281
    ##    4:    2.48    10.28  2.39  3.52        5.50 -116.7664 32.83505 18.05281
    ##    5:    2.48    10.28  2.39  3.52        5.50 -116.7664 32.83505 18.05281
    ##   ---                                                                     
    ## 1196:   18.48    27.73  4.49  7.40       11.43 -117.6484 34.09751 19.41148
    ## 1197:   18.48    27.73  4.49  7.40       11.43 -117.6484 34.09751 19.41148
    ## 1198:   18.48    27.73  4.49  7.40       11.43 -117.6484 34.09751 19.41148
    ## 1199:   18.48    27.73  4.49  7.40       11.43 -117.6484 34.09751 19.41148
    ## 1200:   18.48    27.73  4.49  7.40       11.43 -117.6484 34.09751 19.41148

``` r
dat3[,
     c("agepft", "height", "weight", "bmi", "smoke", "asthma", "gasstove", "fev") :=
     .(
       fifelse(is.na(agepft), agepft_avg, agepft),
       fifelse(is.na(height), height_avg, height),
       fifelse(is.na(weight), weight_avg, weight),
       fifelse(is.na(bmi), bmi_avg, bmi),
       fifelse(is.na(smoke), smoke_avg, smoke),
       fifelse(is.na(asthma), asthma_avg, asthma),
       fifelse(is.na(gasstove), gasstove_avg, gasstove),
       fifelse(is.na(fev), fev_avg, fev)
     )
     ]


dat3[, ..cols]
```

    ##          agepft   height    weight      bmi asthma father_asthma mother_asthma
    ##    1: 10.099932 143.0000  69.00000 15.33749      0             0             0
    ##    2:  9.965777 146.0000  78.00000 16.63283      0             0             0
    ##    3:  9.946612 142.0000  64.00000 14.42715      0             0             0
    ##    4: 10.039699 162.0000 140.00000 24.24797      1             0             0
    ##    5:  9.804244 140.0000 101.00000 23.42301      0             0             0
    ##   ---                                                                         
    ## 1196:  9.631759 134.0000  60.00000 15.18864      0             0             0
    ## 1197:  9.453799 139.0000  73.00000 17.17397      0             0             0
    ## 1198:  9.776865 141.0000  85.00000 19.43381      0             0             0
    ## 1199:  9.338809 143.0000  86.00000 19.11629      0             0             0
    ## 1200:  9.966942 138.5984  82.76707 19.41148      0             1             0
    ##       wheeze hayfever allergy educ_parent     smoke  gasstove      fev      fvc
    ##    1:      0        0       1           3 0.0000000 0.0000000 2529.276 2826.316
    ##    2:      0        0       0          NA 0.1522989 0.7291066 2466.791 2638.221
    ##    3:      0        0       0           2 0.0000000 1.0000000 1759.866 2194.314
    ##    4:      1        0       1           3 0.0000000 1.0000000 2583.934 3567.541
    ##    5:      0        0       0           3 0.0000000 0.0000000 2226.421 2406.020
    ##   ---                                                                          
    ## 1196:      0        0       1           3 0.0000000 1.0000000 1783.168 2001.980
    ## 1197:      1        0       0           5 0.0000000 1.0000000 2000.662 2255.960
    ## 1198:     NA        1       0           3 0.0000000 1.0000000 2279.402 2537.542
    ## 1199:      1       NA       1           3 0.1501976 1.0000000 2428.672 2653.055
    ## 1200:      1        0       0           4 0.0000000 0.0000000 2120.266       NA
    ##           mmef no_24hr pm2_5_fr
    ##    1: 3406.579    2.48    10.28
    ##    2: 3466.464    2.48    10.28
    ##    3: 1695.652    2.48    10.28
    ##    4: 2071.475    2.48    10.28
    ##    5: 2734.114    2.48    10.28
    ##   ---                          
    ## 1196: 2126.733   18.48    27.73
    ## 1197: 2396.026   18.48    27.73
    ## 1198: 2641.196   18.48    27.73
    ## 1199: 3489.301   18.48    27.73
    ## 1200:       NA   18.48    27.73

``` r
#dat2[rowSums(is.na(dat2)) > 0, ]
```

### Looking at the Data (EDA)

The primary questions of interest are: 1. What is the association
between BMI and FEV (forced expiratory volume)? 2. What is the
association between smoke and gas exposure and FEV? 3. What is the
association between PM2.5 exposure and FEV?  

Follow the EDA checklist from week 3 and the previous assignment. Be
sure to focus on the key variables. Visualization Create the following
figures and interpret them. Be sure to include easily understandable
axes, titles, and legends.  

1.  Facet plot showing scatterplots with regression lines of BMI vs FEV
    by “townname”.  
2.  Stacked histograms of FEV by BMI category and FEV by smoke/gas
    exposure. Use different color schemes 3. than the ggplot default.  
3.  Barchart of BMI by smoke/gas exposure.  
4.  Statistical summary graphs of FEV by BMI and FEV by smoke/gas
    exposure category.  
5.  A leaflet map showing the concentrations of PM2.5 mass in each of
    the CHS communities.  
6.  Choose a visualization to examine whether PM2.5 mass is associated
    with FEV.  
