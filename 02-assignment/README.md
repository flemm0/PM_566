Assignment 02: Data Viz and Wrangling
================
Flemming Wu
2022-09-25

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

###### 1. After merging the data, make sure you don’t have any duplicates by counting the number of rows. Make sure it matches. 

In the case of missing values, impute data using the average within the
variables “male” and “hispanic.” If you are interested (and feel
adventurous) in the theme of Data Imputation, take a look at this paper
on “Multiple Imputation” using the Amelia R package here.  

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
cols
```

    ##  [1] "agepft"        "height"        "weight"        "bmi"          
    ##  [5] "asthma"        "father_asthma" "mother_asthma" "wheeze"       
    ##  [9] "hayfever"      "allergy"       "educ_parent"   "smoke"        
    ## [13] "gasstove"      "fev"           "fvc"           "mmef"         
    ## [17] "no_24hr"       "pm2_5_fr"

``` r
#select most important columns to impute NAs and assign to "in_names"
in_names  <- c("agepft", "height", "weight", "bmi", "smoke", "asthma", "gasstove", "fev")
out_names <- paste0(in_names, "_avg")
dat <- merge(
  
  #group by variables hispanic and male, then compute the mean of in_names for each group and assign to temporary variable x
  x = dat[,
  setNames(lapply(.SD, mean, na.rm = TRUE), out_names),
  .SDcols = in_names, 
  by  = .(hispanic, male)
  ],
  
  #merge temporary variable x back with main data table to add new columns containing averages
  y = dat,
  by.x = c("hispanic", "male"),
  by.y = c("hispanic", "male")
)

head(dat, n = 3)
```

    ##    hispanic male agepft_avg height_avg weight_avg  bmi_avg smoke_avg asthma_avg
    ## 1:        0    0   9.849789    138.972   77.39564 18.05281 0.1522989  0.1239193
    ## 2:        0    0   9.849789    138.972   77.39564 18.05281 0.1522989  0.1239193
    ## 3:        0    0   9.849789    138.972   77.39564 18.05281 0.1522989  0.1239193
    ##    gasstove_avg  fev_avg townname sid race    agepft height weight      bmi
    ## 1:    0.7291066 1945.743   Alpine 835    W 10.099932    143     69 15.33749
    ## 2:    0.7291066 1945.743   Alpine 840    W  9.965777    146     78 16.63283
    ## 3:    0.7291066 1945.743   Alpine 860    W  9.946612    142     64 14.42715
    ##    asthma active_asthma father_asthma mother_asthma wheeze hayfever allergy
    ## 1:      0             0             0             0      0        0       1
    ## 2:      0             0             0             0      0        0       0
    ## 3:      0             0             0             0      0        0       0
    ##    educ_parent smoke pets gasstove      fev      fvc     mmef pm25_mass
    ## 1:           3     0    1        0 2529.276 2826.316 3406.579      8.74
    ## 2:          NA    NA    0       NA 2466.791 2638.221 3466.464      8.74
    ## 3:           2     0    0        1 1759.866 2194.314 1695.652      8.74
    ##    pm25_so4 pm25_no3 pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc pm10_ec pm10_tc
    ## 1:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ## 2:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ## 3:     1.73     1.59     0.88    2.54    0.48    3.04    3.25    0.49    3.75
    ##    formic acetic  hcl hno3 o3_max o3106 o3_24   no2  pm10 no_24hr pm2_5_fr
    ## 1:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ## 2:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ## 3:   1.03   2.49 0.41 1.98  65.82 55.05 41.23 12.18 24.73    2.48    10.28
    ##    iacid oacid total_acids       lon      lat
    ## 1:  2.39  3.52         5.5 -116.7664 32.83505
    ## 2:  2.39  3.52         5.5 -116.7664 32.83505
    ## 3:  2.39  3.52         5.5 -116.7664 32.83505

``` r
#impute NAs with averages from _avg columns
dat[,
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

#check that there are no more NAs in the important columns
sum(is.na(dat[, ..in_names]))
```

    ## [1] 0

###### 2. Create a new categorical variable named “obesity_level” using the BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight BMI 22-24; obese BMI\>24). To make sure the variable is rightly coded, create a summary table that contains the minimum BMI, maximum BMI, and the total number of observations per category.

``` r
# removing the averages columns created for previous step
dat <- dat %>%
    select(!ends_with("_avg")) %>%
    collect()
dat <- as.data.table(dat)

dat[, `:=`(obesity_level, fifelse(bmi < 14, "underweight", fifelse(bmi >=
    14 & bmi < 22, "normal", fifelse(bmi >= 22 & bmi <= 24, "overweight",
    "obese"))))]
```

Create summary table to verify new column.

``` r
dat[, .(min_bmi = min(bmi), max_bmi = max(bmi)), by = obesity_level][order(min_bmi)]
```

    ##    obesity_level  min_bmi  max_bmi
    ## 1:   underweight 11.29640 13.98601
    ## 2:        normal 14.00380 21.96387
    ## 3:    overweight 22.02353 23.99650
    ## 4:         obese 24.00647 41.26613

###### 3. Create another categorical variable named “smoke_gas_exposure” that summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have four categories in total.

``` r
dat[, `:=`(smoke_gas_exposure, fifelse(smoke > 0 & gasstove >
    0, "both", fifelse(smoke == 0 & gasstove > 0, "gas", fifelse(smoke >
    0 & gasstove == 0, "smoke", "neither"))))]
```

###### 4. Create four summary tables showing the average (or proportion, if binary) and sd of “Forced expiratory volume in 1 second (ml)” and asthma indicator by town, sex, obesity level, and “smoke_gas_exposure.”

``` r
# fev and asthma by town name
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), avg_asthma = mean(asthma),
    asthma_std = sd(asthma)), by = townname]
```

    ##          townname  avg_fev  std_fev avg_asthma asthma_std
    ##  1:        Alpine 2087.101 291.1768  0.1144423  0.3139348
    ##  2:    Atascadero 2075.897 324.0935  0.2528408  0.4340107
    ##  3: Lake Elsinore 2038.849 303.6956  0.1274366  0.3255095
    ##  4:  Lake Gregory 2084.700 319.9593  0.1512392  0.3585609
    ##  5:     Lancaster 2003.044 317.1298  0.1640054  0.3674206
    ##  6:        Lompoc 2034.354 351.0454  0.1142335  0.3139431
    ##  7:    Long Beach 1985.861 319.4625  0.1359886  0.3370219
    ##  8:     Mira Loma 1985.202 324.9634  0.1582359  0.3572088
    ##  9:     Riverside 1989.881 277.5065  0.1100000  0.3144660
    ## 10:     San Dimas 2026.794 318.7845  0.1712392  0.3771647
    ## 11:   Santa Maria 2025.750 312.1725  0.1348240  0.3372912
    ## 12:        Upland 2024.266 343.1637  0.1212392  0.3263737

``` r
# fev and asthma by sex
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), avg_asthma = mean(asthma),
    asthma_std = sd(asthma)), by = male]
```

    ##    male  avg_fev  std_fev avg_asthma asthma_std
    ## 1:    0 1958.911 311.9181  0.1208035  0.3224043
    ## 2:    1 2103.787 307.5123  0.1726819  0.3728876

``` r
# fev and asthma by obesity level
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), avg_asthma = mean(asthma),
    asthma_std = sd(asthma)), by = obesity_level]
```

    ##    obesity_level  avg_fev  std_fev avg_asthma asthma_std
    ## 1:        normal 1999.794 295.1964 0.14036063  0.3426863
    ## 2:         obese 2266.154 325.4710 0.20819643  0.4034416
    ## 3:    overweight 2224.322 317.4261 0.16409910  0.3687886
    ## 4:   underweight 1698.327 303.3983 0.08571429  0.2840286

``` r
# fev and asthma by smoke and gas exposure
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), avg_asthma = mean(asthma),
    asthma_std = sd(asthma)), by = smoke_gas_exposure]
```

    ##    smoke_gas_exposure  avg_fev  std_fev avg_asthma asthma_std
    ## 1:            neither 2055.356 330.4169  0.1476213  0.3522319
    ## 2:               both 2020.516 306.5113  0.1443831  0.3373595
    ## 3:                gas 2023.639 318.7928  0.1460135  0.3511608
    ## 4:              smoke 2062.816 290.1996  0.1538265  0.3563238

### Looking at the Data (EDA)

The primary questions of interest are:  
1. What is the association between BMI and FEV (forced expiratory
volume)?  
2. What is the association between smoke and gas exposure and FEV?  
3. What is the association between PM2.5 exposure and FEV?  

Follow the EDA checklist from week 3 and the previous assignment. Be
sure to focus on the key variables. Visualization Create the following
figures and interpret them. Be sure to include easily understandable
axes, titles, and legends.  

###### 1. Facet plot showing scatterplots with regression lines of BMI vs FEV by “townname”. 

``` r
ggplot(dat, aes(bmi, fev)) + geom_jitter(size = 0.5) + geom_smooth(method = "lm",
    formula = y ~ x, size = 0.5) + facet_wrap(~townname, nrow = 3)
```

![](README_files/figure-gfm/facet%20plot%20of%20bmi%20vs%20fev-1.png)<!-- -->  
The plots above show that forced expiratory volume and body mass index
are positively correlated in each of the towns in the data set.

###### 2. Stacked histograms of FEV by BMI category and FEV by smoke/gas exposure. Use different color schemes than the ggplot default.

``` r
require(gridExtra)
```

    ## Loading required package: gridExtra

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
p1 <- ggplot(data = dat, mapping = aes(x = fev)) + geom_histogram(mapping = aes(fill = factor(obesity_level)),
    bins = 70) + scale_fill_manual(values = c("darkseagreen2",
    "lemonchiffon2", "lightsalmon1", "gold1"))

p2 <- ggplot(data = dat, mapping = aes(x = fev)) + geom_histogram(mapping = aes(fill = factor(smoke_gas_exposure)),
    bins = 70) + scale_fill_manual(values = c("dodgerblue4",
    "lightpink", "cadetblue1", "darkorange1"))

grid.arrange(p1, p2, ncol = 1)
```

![](README_files/figure-gfm/histograms%20of%20fev%20by%20bmi%20and%20smoke/gas-1.png)<!-- -->

###### 3. Barchart of BMI by smoke/gas exposure. 

``` r
ggplot(data = dat, mapping = aes(x = obesity_level, fill = factor(smoke_gas_exposure))) +
    geom_bar() + scale_fill_manual(values = c("dodgerblue4",
    "lightpink", "cadetblue1", "darkorange1"))
```

![](README_files/figure-gfm/bmi%20vs%20smoke/gas%20barchart-1.png)<!-- -->

###### 4. Statistical summary graphs of FEV by BMI and FEV by smoke/gas exposure category. 

###### 5. A leaflet map showing the concentrations of PM2.5 mass in each of the CHS communities. 

###### 6. Choose a visualization to examine whether PM2.5 mass is associated with FEV. 
