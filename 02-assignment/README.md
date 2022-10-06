Assignment 02: Data Viz and Wrangling
================
Flemming Wu
2022-10-06

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
location variable.

Load libraries

``` r
library(data.table)
library(tidyverse)
library(dtplyr)
library(leaflet)
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

##### 1. After merging the data, make sure you don’t have any duplicates by counting the number of rows. Make sure it matches. 

In the case of missing values, impute data using the average within the
variables “male” and “hispanic.” If you are interested (and feel
adventurous) in the theme of Data Imputation, take a look at this paper
on “Multiple Imputation” using the Amelia R package here.  

Merge data tables and ensure dimensions are correct.

``` r
# Check rows and columns
dim(individual)
```

    ## [1] 1200   23

``` r
dim(regional)
```

    ## [1] 12 27

The merged data set should contain 1200 observations and 23 + 27 - 1
variables.

``` r
dat <- merge(x = individual, y = regional, by.x = "townname",
    by.y = "townname")

# Check dimensions
dim(dat)
```

    ## [1] 1200   49

Merged data set has 1200 rows and 49 columns as expected.

Impute missing values with averages within variables “male” and
“hispanic”

``` r
# look at which columns have NA count greater than 0
names(which(colSums(is.na(dat)) > 0))
```

    ##  [1] "agepft"        "height"        "weight"        "bmi"          
    ##  [5] "asthma"        "father_asthma" "mother_asthma" "wheeze"       
    ##  [9] "hayfever"      "allergy"       "educ_parent"   "smoke"        
    ## [13] "gasstove"      "fev"           "fvc"           "mmef"         
    ## [17] "no_24hr"       "pm2_5_fr"

``` r
# select most important columns to impute NAs and assign to
# 'in_names'
in_names <- c("agepft", "height", "weight", "bmi", "smoke", "asthma",
    "gasstove", "fev")

# print how many values will be imputed
sum(is.na(dat[, ..in_names]))
```

    ## [1] 555

There are 555 values out of 58800 that will be imputed, which is
approximately 0.94% of the data

``` r
dat[, lapply(.SD, mean, na.rm = TRUE), .SDcols = in_names, by = .(hispanic,
    male)] %>%
    knitr::kable()
```

| hispanic | male |   agepft |   height |   weight |      bmi |     smoke |    asthma |  gasstove |      fev |
|---------:|-----:|---------:|---------:|---------:|---------:|----------:|----------:|----------:|---------:|
|        0 |    0 | 9.849789 | 138.9720 | 77.39564 | 18.05281 | 0.1522989 | 0.1239193 | 0.7291066 | 1945.743 |
|        1 |    0 | 9.907300 | 138.1941 | 79.04641 | 18.63201 | 0.1535270 | 0.1164659 | 0.8218623 | 1977.241 |
|        1 |    1 | 9.966942 | 138.5984 | 82.76707 | 19.41148 | 0.1501976 | 0.1601562 | 0.8156863 | 2120.266 |
|        0 |    1 | 9.979124 | 139.8388 | 78.78289 | 18.14035 | 0.1949686 | 0.1829653 | 0.7798742 | 2090.258 |

The above table shows what values are to be used to impute NAs in each
column. I will first have to merge these columns in order to add them to
the original data table.

``` r
out_names <- paste0(in_names, "_avg") #suffix for "out_names" will be _avg

dat <- merge(
  
  #group by variables "hispanic" and "male", then compute the mean of in_names for each group and assign to temporary table x
  x = dat[,
  setNames(lapply(.SD, mean, na.rm = TRUE), out_names),
  .SDcols = in_names, 
  by  = .(hispanic, male)
  ],
  
  #merge temporary table x back with main data table to add new columns containing averages
  y = dat,
  by.x = c("hispanic", "male"),
  by.y = c("hispanic", "male")
)

#check results
head(dat[, hispanic:educ_parent]) %>%
  knitr::kable()
```

| hispanic | male | agepft_avg | height_avg | weight_avg |  bmi_avg | smoke_avg | asthma_avg | gasstove_avg |  fev_avg | townname | sid | race |    agepft | height | weight |      bmi | asthma | active_asthma | father_asthma | mother_asthma | wheeze | hayfever | allergy | educ_parent |
|---------:|-----:|-----------:|-----------:|-----------:|---------:|----------:|-----------:|-------------:|---------:|:---------|----:|:-----|----------:|-------:|-------:|---------:|-------:|--------------:|--------------:|--------------:|-------:|---------:|--------:|------------:|
|        0 |    0 |   9.849789 |    138.972 |   77.39564 | 18.05281 | 0.1522989 |  0.1239193 |    0.7291066 | 1945.743 | Alpine   | 835 | W    | 10.099932 |    143 |     69 | 15.33749 |      0 |             0 |             0 |             0 |      0 |        0 |       1 |           3 |
|        0 |    0 |   9.849789 |    138.972 |   77.39564 | 18.05281 | 0.1522989 |  0.1239193 |    0.7291066 | 1945.743 | Alpine   | 840 | W    |  9.965777 |    146 |     78 | 16.63283 |      0 |             0 |             0 |             0 |      0 |        0 |       0 |          NA |
|        0 |    0 |   9.849789 |    138.972 |   77.39564 | 18.05281 | 0.1522989 |  0.1239193 |    0.7291066 | 1945.743 | Alpine   | 860 | W    |  9.946612 |    142 |     64 | 14.42715 |      0 |             0 |             0 |             0 |      0 |        0 |       0 |           2 |
|        0 |    0 |   9.849789 |    138.972 |   77.39564 | 18.05281 | 0.1522989 |  0.1239193 |    0.7291066 | 1945.743 | Alpine   | 865 | W    | 10.039699 |    162 |    140 | 24.24797 |      1 |             1 |             0 |             0 |      1 |        0 |       1 |           3 |
|        0 |    0 |   9.849789 |    138.972 |   77.39564 | 18.05281 | 0.1522989 |  0.1239193 |    0.7291066 | 1945.743 | Alpine   | 873 | W    |  9.804244 |    140 |    101 | 23.42301 |      0 |             0 |             0 |             0 |      0 |        0 |       0 |           3 |
|        0 |    0 |   9.849789 |    138.972 |   77.39564 | 18.05281 | 0.1522989 |  0.1239193 |    0.7291066 | 1945.743 | Alpine   | 894 | W    |  9.295003 |    150 |     89 | 17.97980 |      0 |             0 |             0 |             0 |     NA |        1 |       1 |           2 |

The imputed average columns have been successfully added to the data
table.  

Next, impute NA values with averages from \*\_avg columns

``` r
dat[, `:=`(c("agepft", "height", "weight", "bmi", "smoke", "asthma",
    "gasstove", "fev"), .(fifelse(is.na(agepft), agepft_avg,
    agepft), fifelse(is.na(height), height_avg, height), fifelse(is.na(weight),
    weight_avg, weight), fifelse(is.na(bmi), bmi_avg, bmi), fifelse(is.na(smoke),
    smoke_avg, smoke), fifelse(is.na(asthma), asthma_avg, asthma),
    fifelse(is.na(gasstove), gasstove_avg, gasstove), fifelse(is.na(fev),
        fev_avg, fev)))]

# check that there are no more NAs in the important columns
sum(is.na(dat[, ..in_names]))
```

    ## [1] 0

The NA values have been successfully imputed.

##### 2. Create a new categorical variable named “obesity_level” using the BMI measurement (underweight BMI\<14; normal BMI 14-22; overweight BMI 22-24; obese BMI\>24). To make sure the variable is rightly coded, create a summary table that contains the minimum BMI, maximum BMI, and the total number of observations per category.

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
dat[, .(min_bmi = min(bmi), max_bmi = max(bmi), num_observations = .N),
    by = obesity_level][order(min_bmi)] %>%
    knitr::kable()
```

| obesity_level |  min_bmi |  max_bmi | num_observations |
|:--------------|---------:|---------:|-----------------:|
| underweight   | 11.29640 | 13.98601 |               35 |
| normal        | 14.00380 | 21.96387 |              975 |
| overweight    | 22.02353 | 23.99650 |               87 |
| obese         | 24.00647 | 41.26613 |              103 |

The bmi variables have been categorized as intended.

##### 3. Create another categorical variable named “smoke_gas_exposure” that summarizes “Second Hand Smoke” and “Gas Stove.” The variable should have four categories in total.

Since the values for smoke and gasstove are both binary, but some of the
values will have been imputed with averages, I will consider how to
categorize the imputed values.

``` r
dat[(smoke %between% c(0.001, 0.999)) & (gasstove %between% c(0.001,
    0.999)), .(smoke, gasstove)]
```

    ##         smoke  gasstove
    ##  1: 0.1522989 0.7291066
    ##  2: 0.1522989 0.7291066
    ##  3: 0.1522989 0.7291066
    ##  4: 0.1949686 0.7798742
    ##  5: 0.1949686 0.7798742
    ##  6: 0.1535270 0.8218623
    ##  7: 0.1535270 0.8218623
    ##  8: 0.1535270 0.8218623
    ##  9: 0.1501976 0.8156863
    ## 10: 0.1501976 0.8156863
    ## 11: 0.1501976 0.8156863
    ## 12: 0.1501976 0.8156863
    ## 13: 0.1501976 0.8156863

It appears that the imputed average for smoke exposure is closer to 0,
and the imputed average for gas exposure is closer to 1. When creating
the smoke and gas exposure category, those that have smoke exposure = 1
will qualify. As for the gas exposure, those that have gasstove != 0
will qualify. Essentially, I will be rounding the imputed smoke exposure
values up to 1 and the imputed gasstove exposure values down to 0.

``` r
dat[, `:=`(smoke_gas_exposure, fifelse(smoke == 1 & gasstove !=
    0, "both", fifelse(smoke != 1 & gasstove != 0, "gas", fifelse(smoke ==
    1 & gasstove == 0, "smoke", "neither"))))]
```

##### 4. Create four summary tables showing the average (or proportion, if binary) and sd of “Forced expiratory volume in 1 second (ml)” and asthma indicator by town, sex, obesity level, and “smoke_gas_exposure.”

``` r
# fev and asthma by town name
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), prop_asthma = mean(asthma)),
    by = townname] %>%
    knitr::kable()
```

| townname      |  avg_fev |  std_fev | prop_asthma |
|:--------------|---------:|---------:|------------:|
| Alpine        | 2087.101 | 291.1768 |   0.1144423 |
| Atascadero    | 2075.897 | 324.0935 |   0.2528408 |
| Lake Elsinore | 2038.849 | 303.6956 |   0.1274366 |
| Lake Gregory  | 2084.700 | 319.9593 |   0.1512392 |
| Lancaster     | 2003.044 | 317.1298 |   0.1640054 |
| Lompoc        | 2034.354 | 351.0454 |   0.1142335 |
| Long Beach    | 1985.861 | 319.4625 |   0.1359886 |
| Mira Loma     | 1985.202 | 324.9634 |   0.1582359 |
| Riverside     | 1989.881 | 277.5065 |   0.1100000 |
| San Dimas     | 2026.794 | 318.7845 |   0.1712392 |
| Santa Maria   | 2025.750 | 312.1725 |   0.1348240 |
| Upland        | 2024.266 | 343.1637 |   0.1212392 |

``` r
# fev and asthma by sex
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), prop_asthma = mean(asthma)),
    by = male] %>%
    knitr::kable()
```

| male |  avg_fev |  std_fev | prop_asthma |
|-----:|---------:|---------:|------------:|
|    0 | 1958.911 | 311.9181 |   0.1208035 |
|    1 | 2103.787 | 307.5123 |   0.1726819 |

``` r
# fev and asthma by obesity level
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), prop_asthma = mean(asthma)),
    by = obesity_level] %>%
    knitr::kable()
```

| obesity_level |  avg_fev |  std_fev | prop_asthma |
|:--------------|---------:|---------:|------------:|
| normal        | 1999.794 | 295.1964 |   0.1403606 |
| obese         | 2266.154 | 325.4710 |   0.2081964 |
| overweight    | 2224.322 | 317.4261 |   0.1640991 |
| underweight   | 1698.327 | 303.3983 |   0.0857143 |

``` r
# fev and asthma by smoke and gas exposure
dat[, .(avg_fev = mean(fev), std_fev = sd(fev), prop_asthma = mean(asthma)),
    by = smoke_gas_exposure] %>%
    knitr::kable()
```

| smoke_gas_exposure |  avg_fev |  std_fev | prop_asthma |
|:-------------------|---------:|---------:|------------:|
| neither            | 2056.693 | 328.7843 |   0.1448168 |
| gas                | 2022.671 | 319.3449 |   0.1491750 |
| smoke              | 2055.714 | 295.6475 |   0.1717490 |
| both               | 2024.778 | 300.6313 |   0.1277738 |

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

Check dimensions, headers, footers, and variable types.

``` r
dim(dat)
```

    ## [1] 1200   51

Dimensions look correct, the number of observations remained the same as
the earlier check, and there are two additional columns (for the two
categorical variables that were created)

``` r
head(dat) %>%
    knitr::kable()
```

| hispanic | male | townname | sid | race |    agepft | height | weight |      bmi | asthma | active_asthma | father_asthma | mother_asthma | wheeze | hayfever | allergy | educ_parent |     smoke | pets |  gasstove |      fev |      fvc |     mmef | pm25_mass | pm25_so4 | pm25_no3 | pm25_nh4 | pm25_oc | pm25_ec | pm25_om | pm10_oc | pm10_ec | pm10_tc | formic | acetic |  hcl | hno3 | o3_max | o3106 | o3_24 |   no2 |  pm10 | no_24hr | pm2_5\_fr | iacid | oacid | total_acids |       lon |      lat | obesity_level | smoke_gas_exposure |
|---------:|-----:|:---------|----:|:-----|----------:|-------:|-------:|---------:|-------:|--------------:|--------------:|--------------:|-------:|---------:|--------:|------------:|----------:|-----:|----------:|---------:|---------:|---------:|----------:|---------:|---------:|---------:|--------:|--------:|--------:|--------:|--------:|--------:|-------:|-------:|-----:|-----:|-------:|------:|------:|------:|------:|--------:|----------:|------:|------:|------------:|----------:|---------:|:--------------|:-------------------|
|        0 |    0 | Alpine   | 835 | W    | 10.099932 |    143 |     69 | 15.33749 |      0 |             0 |             0 |             0 |      0 |        0 |       1 |           3 | 0.0000000 |    1 | 0.0000000 | 2529.276 | 2826.316 | 3406.579 |      8.74 |     1.73 |     1.59 |     0.88 |    2.54 |    0.48 |    3.04 |    3.25 |    0.49 |    3.75 |   1.03 |   2.49 | 0.41 | 1.98 |  65.82 | 55.05 | 41.23 | 12.18 | 24.73 |    2.48 |     10.28 |  2.39 |  3.52 |         5.5 | -116.7664 | 32.83505 | normal        | neither            |
|        0 |    0 | Alpine   | 840 | W    |  9.965777 |    146 |     78 | 16.63283 |      0 |             0 |             0 |             0 |      0 |        0 |       0 |          NA | 0.1522989 |    0 | 0.7291066 | 2466.791 | 2638.221 | 3466.464 |      8.74 |     1.73 |     1.59 |     0.88 |    2.54 |    0.48 |    3.04 |    3.25 |    0.49 |    3.75 |   1.03 |   2.49 | 0.41 | 1.98 |  65.82 | 55.05 | 41.23 | 12.18 | 24.73 |    2.48 |     10.28 |  2.39 |  3.52 |         5.5 | -116.7664 | 32.83505 | normal        | gas                |
|        0 |    0 | Alpine   | 860 | W    |  9.946612 |    142 |     64 | 14.42715 |      0 |             0 |             0 |             0 |      0 |        0 |       0 |           2 | 0.0000000 |    0 | 1.0000000 | 1759.866 | 2194.314 | 1695.652 |      8.74 |     1.73 |     1.59 |     0.88 |    2.54 |    0.48 |    3.04 |    3.25 |    0.49 |    3.75 |   1.03 |   2.49 | 0.41 | 1.98 |  65.82 | 55.05 | 41.23 | 12.18 | 24.73 |    2.48 |     10.28 |  2.39 |  3.52 |         5.5 | -116.7664 | 32.83505 | normal        | gas                |
|        0 |    0 | Alpine   | 865 | W    | 10.039699 |    162 |    140 | 24.24797 |      1 |             1 |             0 |             0 |      1 |        0 |       1 |           3 | 0.0000000 |    1 | 1.0000000 | 2583.934 | 3567.541 | 2071.475 |      8.74 |     1.73 |     1.59 |     0.88 |    2.54 |    0.48 |    3.04 |    3.25 |    0.49 |    3.75 |   1.03 |   2.49 | 0.41 | 1.98 |  65.82 | 55.05 | 41.23 | 12.18 | 24.73 |    2.48 |     10.28 |  2.39 |  3.52 |         5.5 | -116.7664 | 32.83505 | obese         | gas                |
|        0 |    0 | Alpine   | 873 | W    |  9.804244 |    140 |    101 | 23.42301 |      0 |             0 |             0 |             0 |      0 |        0 |       0 |           3 | 0.0000000 |    1 | 0.0000000 | 2226.421 | 2406.020 | 2734.114 |      8.74 |     1.73 |     1.59 |     0.88 |    2.54 |    0.48 |    3.04 |    3.25 |    0.49 |    3.75 |   1.03 |   2.49 | 0.41 | 1.98 |  65.82 | 55.05 | 41.23 | 12.18 | 24.73 |    2.48 |     10.28 |  2.39 |  3.52 |         5.5 | -116.7664 | 32.83505 | overweight    | neither            |
|        0 |    0 | Alpine   | 894 | W    |  9.295003 |    150 |     89 | 17.97980 |      0 |             0 |             0 |             0 |     NA |        1 |       1 |           2 | 0.0000000 |    1 | 1.0000000 | 2448.837 | 2763.787 | 3041.860 |      8.74 |     1.73 |     1.59 |     0.88 |    2.54 |    0.48 |    3.04 |    3.25 |    0.49 |    3.75 |   1.03 |   2.49 | 0.41 | 1.98 |  65.82 | 55.05 | 41.23 | 12.18 | 24.73 |    2.48 |     10.28 |  2.39 |  3.52 |         5.5 | -116.7664 | 32.83505 | normal        | gas                |

``` r
tail(dat) %>%
    knitr::kable()
```

| hispanic | male | townname |  sid | race |   agepft |   height |   weight |      bmi | asthma | active_asthma | father_asthma | mother_asthma | wheeze | hayfever | allergy | educ_parent |     smoke | pets | gasstove |      fev |      fvc |     mmef | pm25_mass | pm25_so4 | pm25_no3 | pm25_nh4 | pm25_oc | pm25_ec | pm25_om | pm10_oc | pm10_ec | pm10_tc | formic | acetic |  hcl | hno3 | o3_max | o3106 | o3_24 |   no2 | pm10 | no_24hr | pm2_5\_fr | iacid | oacid | total_acids |       lon |      lat | obesity_level | smoke_gas_exposure |
|---------:|-----:|:---------|-----:|:-----|---------:|---------:|---------:|---------:|-------:|--------------:|--------------:|--------------:|-------:|---------:|--------:|------------:|----------:|-----:|---------:|---------:|---------:|---------:|----------:|---------:|---------:|---------:|--------:|--------:|--------:|--------:|--------:|--------:|-------:|-------:|-----:|-----:|-------:|------:|------:|------:|-----:|--------:|----------:|------:|------:|------------:|----------:|---------:|:--------------|:-------------------|
|        1 |    1 | Upland   | 1799 | O    | 9.897331 | 138.0000 | 75.00000 | 17.90113 |      0 |             0 |             0 |             0 |      0 |        0 |       0 |           3 | 0.0000000 |    0 |        1 | 2084.428 | 2368.851 | 2630.158 |     22.46 |     2.65 |     7.75 |     2.96 |    6.49 |    1.19 |    7.79 |    8.32 |    1.22 |    9.54 |   2.67 |   4.73 | 0.46 | 4.03 |  63.83 |  46.5 |  22.2 | 37.97 | 40.8 |   18.48 |     27.73 |  4.49 |   7.4 |       11.43 | -117.6484 | 34.09751 | normal        | gas                |
|        1 |    1 | Upland   | 1816 | W    | 9.631759 | 134.0000 | 60.00000 | 15.18864 |      0 |             0 |             0 |             0 |      0 |        0 |       1 |           3 | 0.0000000 |    1 |        1 | 1783.168 | 2001.980 | 2126.733 |     22.46 |     2.65 |     7.75 |     2.96 |    6.49 |    1.19 |    7.79 |    8.32 |    1.22 |    9.54 |   2.67 |   4.73 | 0.46 | 4.03 |  63.83 |  46.5 |  22.2 | 37.97 | 40.8 |   18.48 |     27.73 |  4.49 |   7.4 |       11.43 | -117.6484 | 34.09751 | normal        | gas                |
|        1 |    1 | Upland   | 1833 | W    | 9.453799 | 139.0000 | 73.00000 | 17.17397 |      0 |             0 |             0 |             0 |      1 |        0 |       0 |           5 | 0.0000000 |    0 |        1 | 2000.662 | 2255.960 | 2396.026 |     22.46 |     2.65 |     7.75 |     2.96 |    6.49 |    1.19 |    7.79 |    8.32 |    1.22 |    9.54 |   2.67 |   4.73 | 0.46 | 4.03 |  63.83 |  46.5 |  22.2 | 37.97 | 40.8 |   18.48 |     27.73 |  4.49 |   7.4 |       11.43 | -117.6484 | 34.09751 | normal        | gas                |
|        1 |    1 | Upland   | 1839 | M    | 9.776865 | 141.0000 | 85.00000 | 19.43381 |      0 |             0 |             0 |             0 |     NA |        1 |       0 |           3 | 0.0000000 |    1 |        1 | 2279.402 | 2537.542 | 2641.196 |     22.46 |     2.65 |     7.75 |     2.96 |    6.49 |    1.19 |    7.79 |    8.32 |    1.22 |    9.54 |   2.67 |   4.73 | 0.46 | 4.03 |  63.83 |  46.5 |  22.2 | 37.97 | 40.8 |   18.48 |     27.73 |  4.49 |   7.4 |       11.43 | -117.6484 | 34.09751 | normal        | gas                |
|        1 |    1 | Upland   | 1850 | O    | 9.338809 | 143.0000 | 86.00000 | 19.11629 |      0 |             0 |             0 |             0 |      1 |       NA |       1 |           3 | 0.1501976 |    1 |        1 | 2428.672 | 2653.055 | 3489.301 |     22.46 |     2.65 |     7.75 |     2.96 |    6.49 |    1.19 |    7.79 |    8.32 |    1.22 |    9.54 |   2.67 |   4.73 | 0.46 | 4.03 |  63.83 |  46.5 |  22.2 | 37.97 | 40.8 |   18.48 |     27.73 |  4.49 |   7.4 |       11.43 | -117.6484 | 34.09751 | normal        | gas                |
|        1 |    1 | Upland   | 1862 | W    | 9.966942 | 138.5984 | 82.76707 | 19.41148 |      0 |             0 |             1 |             0 |      1 |        0 |       0 |           4 | 0.0000000 |    1 |        0 | 2120.266 |       NA |       NA |     22.46 |     2.65 |     7.75 |     2.96 |    6.49 |    1.19 |    7.79 |    8.32 |    1.22 |    9.54 |   2.67 |   4.73 | 0.46 | 4.03 |  63.83 |  46.5 |  22.2 | 37.97 | 40.8 |   18.48 |     27.73 |  4.49 |   7.4 |       11.43 | -117.6484 | 34.09751 | normal        | neither            |

``` r
str(dat)
```

    ## Classes 'data.table' and 'data.frame':   1200 obs. of  51 variables:
    ##  $ hispanic          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ male              : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ townname          : chr  "Alpine" "Alpine" "Alpine" "Alpine" ...
    ##  $ sid               : int  835 840 860 865 873 894 901 905 906 909 ...
    ##  $ race              : chr  "W" "W" "W" "W" ...
    ##  $ agepft            : num  10.1 9.97 9.95 10.04 9.8 ...
    ##  $ height            : num  143 146 142 162 140 150 132 142 126 141 ...
    ##  $ weight            : num  69 78 64 140 101 89 67 94 57 68 ...
    ##  $ bmi               : num  15.3 16.6 14.4 24.2 23.4 ...
    ##  $ asthma            : num  0 0 0 1 0 0 0 1 0 0 ...
    ##  $ active_asthma     : int  0 0 0 1 0 0 0 1 0 0 ...
    ##  $ father_asthma     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ mother_asthma     : int  0 0 0 0 0 0 0 1 1 0 ...
    ##  $ wheeze            : int  0 0 0 1 0 NA NA 1 0 0 ...
    ##  $ hayfever          : int  0 0 0 0 0 1 1 0 0 1 ...
    ##  $ allergy           : int  1 0 0 1 0 1 0 0 0 1 ...
    ##  $ educ_parent       : int  3 NA 2 3 3 2 2 3 3 3 ...
    ##  $ smoke             : num  0 0.152 0 0 0 ...
    ##  $ pets              : int  1 0 0 1 1 1 1 1 1 1 ...
    ##  $ gasstove          : num  0 0.729 1 1 0 ...
    ##  $ fev               : num  2529 2467 1760 2584 2226 ...
    ##  $ fvc               : num  2826 2638 2194 3568 2406 ...
    ##  $ mmef              : num  3407 3466 1696 2071 2734 ...
    ##  $ pm25_mass         : num  8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 8.74 ...
    ##  $ pm25_so4          : num  1.73 1.73 1.73 1.73 1.73 1.73 1.73 1.73 1.73 1.73 ...
    ##  $ pm25_no3          : num  1.59 1.59 1.59 1.59 1.59 1.59 1.59 1.59 1.59 1.59 ...
    ##  $ pm25_nh4          : num  0.88 0.88 0.88 0.88 0.88 0.88 0.88 0.88 0.88 0.88 ...
    ##  $ pm25_oc           : num  2.54 2.54 2.54 2.54 2.54 2.54 2.54 2.54 2.54 2.54 ...
    ##  $ pm25_ec           : num  0.48 0.48 0.48 0.48 0.48 0.48 0.48 0.48 0.48 0.48 ...
    ##  $ pm25_om           : num  3.04 3.04 3.04 3.04 3.04 3.04 3.04 3.04 3.04 3.04 ...
    ##  $ pm10_oc           : num  3.25 3.25 3.25 3.25 3.25 3.25 3.25 3.25 3.25 3.25 ...
    ##  $ pm10_ec           : num  0.49 0.49 0.49 0.49 0.49 0.49 0.49 0.49 0.49 0.49 ...
    ##  $ pm10_tc           : num  3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.75 3.75 ...
    ##  $ formic            : num  1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 1.03 ...
    ##  $ acetic            : num  2.49 2.49 2.49 2.49 2.49 2.49 2.49 2.49 2.49 2.49 ...
    ##  $ hcl               : num  0.41 0.41 0.41 0.41 0.41 0.41 0.41 0.41 0.41 0.41 ...
    ##  $ hno3              : num  1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 1.98 ...
    ##  $ o3_max            : num  65.8 65.8 65.8 65.8 65.8 ...
    ##  $ o3106             : num  55 55 55 55 55 ...
    ##  $ o3_24             : num  41.2 41.2 41.2 41.2 41.2 ...
    ##  $ no2               : num  12.2 12.2 12.2 12.2 12.2 ...
    ##  $ pm10              : num  24.7 24.7 24.7 24.7 24.7 ...
    ##  $ no_24hr           : num  2.48 2.48 2.48 2.48 2.48 2.48 2.48 2.48 2.48 2.48 ...
    ##  $ pm2_5_fr          : num  10.3 10.3 10.3 10.3 10.3 ...
    ##  $ iacid             : num  2.39 2.39 2.39 2.39 2.39 2.39 2.39 2.39 2.39 2.39 ...
    ##  $ oacid             : num  3.52 3.52 3.52 3.52 3.52 3.52 3.52 3.52 3.52 3.52 ...
    ##  $ total_acids       : num  5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5 ...
    ##  $ lon               : num  -117 -117 -117 -117 -117 ...
    ##  $ lat               : num  32.8 32.8 32.8 32.8 32.8 ...
    ##  $ obesity_level     : chr  "normal" "normal" "normal" "obese" ...
    ##  $ smoke_gas_exposure: chr  "neither" "gas" "gas" "gas" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

The variables appear to be stored in the correct data type.

Take closer look at key variables (fev, smoke and gas exposure, pm 2.5,
and bmi) and calculate initial summary statistics.

According to this article from NCBI:
 <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2643211/>  
the forced expiratory volume of an individual varies by age and gender.
First, I need to know what the age range is for individuals in this data
set.

``` r
summary(dat[, agepft])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   8.961   9.632   9.907   9.923  10.155  12.731

After verifying on
<https://github.com/USCbiostats/data-science-data/blob/master/01_chs/chs_codebook.docx>
that the age numbers are in fact in years, I looked back to the NCBI
article to find what range of fev I should expect to see for individuals
at ages 8 to 13. According to the plot in Figure 1, the median fev
values found for individuals ages 8 to 13 ranged from below 1 liter to
about 3 liters for females and 1 liter to 3.5 liters in males.

``` r
# according to the document describing the data, the fev is
# recorded in ml.

dat[, .(min = min(fev), median = median(fev), max = max(fev)),
    by = male]
```

    ##    male       min   median      max
    ## 1:    0  984.8485 1945.743 3179.798
    ## 2:    1 1146.4494 2090.258 3323.684

The stats for fev in males and females falls within the range that I
would expect them to.

``` r
summary(dat[, pm25_mass])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   5.960   7.615  10.545  14.362  20.988  29.970

There are no missing or implausible values in the PM 2.5 column.

``` r
summary(dat[, bmi])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.30   15.96   17.81   18.50   19.99   41.27

No missing or implausible values for bmi column. The values are within
the range that the CDC provides for bmi measurements in children:
<https://www.cdc.gov/healthyweight/assessing/bmi/childrens_bmi/about_childrens_bmi.html>

##### 1. Facet plot showing scatterplots with regression lines of BMI vs FEV by “townname”. 

``` r
ggplot(dat, aes(bmi, fev, color = townname)) + geom_jitter(size = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x, size = 0.5, se = FALSE,
        color = "red") + facet_wrap(~townname, nrow = 3)
```

![](README_files/figure-gfm/facet%20plot%20of%20bmi%20vs%20fev-1.png)<!-- -->  
The plots above show that forced expiratory volume and body mass index
are positively correlated in each of the towns in the data set.
Additionally, towns such as Alpine and Riverside have a relatively low
positive slope, whereas towns such as San Dimas, Lake Elsinore, and
Upland have steeper positive slopes. This suggests that the positive
correlation between body mass index and forced expiratory volume varies
slightly by region.

##### 2. Stacked histograms of FEV by BMI category and FEV by smoke/gas exposure. Use different color schemes than the ggplot default.

``` r
ggplot(data = dat, mapping = aes(x = fev)) + geom_histogram(mapping = aes(fill = factor(obesity_level)),
    bins = 70, alpha = 0.8) + scale_fill_manual(values = c("darkseagreen2",
    "dodgerblue", "firebrick2", "gold1")) + guides(fill = guide_legend(title = "Obesity Category"))
```

![](README_files/figure-gfm/histograms%20of%20fev%20by%20bmi-1.png)<!-- -->  
The plot above reveals that most people in the data set fall under the
normal obesity level category, and that the average forced expiratory
volume (fev) for people in this category lies at about 2000 with a
normal distribution. As for people in the underweight category, the
average fev is lower than that of those in the normal category, at about
1750. For people in both the overweight and obese categories, the
average fev lies about at 2300, which is higher than the average for the
people in the normal category.

``` r
ggplot(data = dat, mapping = aes(x = fev)) + geom_histogram(mapping = aes(fill = factor(smoke_gas_exposure)),
    bins = 70, alpha = 0.8) + scale_fill_manual(values = c("dodgerblue4",
    "lightpink", "cadetblue1", "yellow1")) + guides(fill = guide_legend(title = "Obesity Category"))
```

![](README_files/figure-gfm/histogram%20of%20fev%20by%20smoke/gas-1.png)<!-- -->  
The plot above reveals that most people in the data set are classified
as either having exposure to both gas and smoke, or gas only.
Additionally, the fev for each of the obesity categories are centered
around 2050, with a standard deviation of about 300.

##### 3. Barchart of BMI by smoke/gas exposure.

``` r
ggplot(data = dat, mapping = aes(x = obesity_level, fill = factor(smoke_gas_exposure))) +
    geom_bar(position = "dodge") + scale_fill_manual(values = c("dodgerblue4",
    "lightpink", "cadetblue1", "darkorange1")) + labs(fill = "Smoke and Gas Exposure")
```

![](README_files/figure-gfm/bmi%20vs%20smoke/gas%20barchart-1.png)<!-- -->  
From this bar chart of bmi category counts by obesity level, it is
evident that the proportions of people that are exposed to gas, smoke,
both, or neither are mostly consistent across the bmi categories. The
highest proportion of people in each category have some exposure to gas,
and the smallest proportion of people have some exposure to smoke. For
the other types of exposure (both or neither), the proportion of people
in those categories sits at about one third of that of the proportion of
people exposed to gas only.

##### 4. Statistical summary graphs of FEV by BMI and FEV by smoke/gas exposure category.

``` r
ggplot(data = dat, mapping = aes(x = forcats::fct_reorder(factor(obesity_level),
    fev, median), y = fev, color = obesity_level)) + stat_summary(fun.min = min,
    fun.max = max, fun = median) + xlab("Obesity Category")
```

![](README_files/figure-gfm/plot%20stat%20summary%20of%20fev%20by%20obesity%20category-1.png)<!-- -->  
The above plot reveals that there is a positive correlation between
increase in bmi and increase in forced expiratory volume. The median fev
for those that are underweight is about 1700, and increases to 1950 for
those that are normal, and up to 2200 and 2250 for those that are
overweight and obese, respectively. Additionally, the range of values
for the normal and obese categories are larger than the underweight and
overweight categories.

``` r
ggplot(data = dat, mapping = aes(y = fev, x = smoke_gas_exposure,
    color = smoke_gas_exposure)) + stat_summary(fun.min = min,
    fun.max = max, fun = median)
```

![](README_files/figure-gfm/plot%20stat%20summary%20of%20fev%20by%20smoke/gas%20exposure-1.png)<!-- -->  
The stat summary above reveals that when looking at fev by smoke and gas
exposure, the median fev for the groups all lie at approximately 2000.
The groups having no exposure to smoke or gas, as well as the group
exposed to smoke only, have medians slightly higher than the other two
groups. Additionally, the range of observations for the groups exposed
to neither smoke nor gas, and gas only, are greater than for the other
two groups.

##### 5. A leaflet map showing the concentrations of PM2.5 mass in each of the CHS communities. 

``` r
qpal <- colorQuantile("YlOrRd", dat[, pm25_mass], n = 12)

qpal_colors <- unique(qpal(sort(dat[, pm25_mass])))
qpal_labs <- unique(sort(dat[, pm25_mass]))

leaflet(dat) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircles(lat = ~lat, lng = ~lon, color = ~qpal(pm25_mass),
        radius = 400, fillOpacity = 1) %>%
    addLegend(colors = qpal_colors, labels = qpal_labs, title = "conc. of PM 2.5 mass",
        opacity = 0.5)
```

![](README_files/figure-gfm/plot%20leaflet%20map-1.png)<!-- -->  
The leaflet plot above reveals that higher concentration levels of pm
2.5 are associated with large, urban population centers, in this case
around the greater Los Angeles area. Only in areas such as Santa Barbara
or San Luis Obispo that are further from large population centers and
closer to the coastline are low concentration levels of pm 2.5 of below
8 observed.

##### 6. Choose a visualization to examine whether PM2.5 mass is associated with FEV. 

``` r
ggplot(data = dat, mapping = aes(y = fev, x = pm25_mass, group = factor(pm25_mass))) +
    geom_boxplot(width = 0.5) + geom_smooth(mapping = aes(group = 1),
    col = "red", formula = y ~ x, method = "lm", alpha = 0.01,
    linetype = "twodash")
```

![](README_files/figure-gfm/pm25%20mass%20and%20fev%20association%20grouped%20boxplots-1.png)<!-- -->  
The above boxplots show that there is a small negative correlation
between forced expiratory volume and concentration of pm 2.5 mass, with
a slight decrease in fev with increasing concentration of pm 2.5 mass.
