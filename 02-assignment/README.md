Assignment 02: Data Viz and Wrangling
================
Flemming Wu
2022-09-27

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
which(colSums(is.na(dat)) > 0)
```

    ##        agepft        height        weight           bmi        asthma 
    ##             6             7             8             9            10 
    ## father_asthma mother_asthma        wheeze      hayfever       allergy 
    ##            12            13            14            15            16 
    ##   educ_parent         smoke      gasstove           fev           fvc 
    ##            17            18            20            21            22 
    ##          mmef       no_24hr      pm2_5_fr 
    ##            23            43            44

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
will qualify.

``` r
dat[, `:=`(smoke_gas_exposure, fifelse(smoke == 1 & gasstove !=
    0, "both", fifelse(smoke != 1 & gasstove != 0, "gas", fifelse(smoke ==
    1 & gasstove == 0, "smoke", "neither"))))]
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
    ## 1:            neither 2056.693 328.7843  0.1448168  0.3487352
    ## 2:                gas 2022.671 319.3449  0.1491750  0.3519572
    ## 3:              smoke 2055.714 295.6475  0.1717490  0.3768879
    ## 4:               both 2024.778 300.6313  0.1277738  0.3291856

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
ggplot(dat, aes(bmi, fev, color = townname)) + geom_jitter(size = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x, size = 0.5, se = FALSE,
        color = "red") + facet_wrap(~townname, nrow = 3)
```

![](README_files/figure-gfm/facet%20plot%20of%20bmi%20vs%20fev-1.png)<!-- -->  
The plots above show that forced expiratory volume and body mass index
are positively correlated in each of the towns in the data set.
Additionally, towns such as Alpine and Riverside have a relatively low
positive slope, whereas towns such as San Dimas, Lake Elsinore, and
Upland much steeper positive slopes. This suggests that the positive
correlation between bmi and fev varies by region.

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
Of the two histograms above, the one on top reveals that most people in
the data set fall under the normal obesity level category, and that the
average forced expiratory volume (fev) for people in this category lies
at about 2000 with a normal distribution. As for people in the
underweight category, the average fev is lower than that of those in the
normal category, at about 1750. For people in both the overweight and
obese categories, the average fev lies about at 2300, which is higher
than the average for the people in the normal category.  
The histogram on the bottom reveals that most people in the data set are
classified as either having exposure to both gas and smoke, or gas only.
Additionally, most of the people in each category have an average fev of
about 2000, with a standard deviation of about 300.

###### 3. Barchart of BMI by smoke/gas exposure. 

``` r
ggplot(data = dat, mapping = aes(x = obesity_level, fill = factor(smoke_gas_exposure))) +
    geom_bar(position = "dodge") + scale_fill_manual(values = c("dodgerblue4",
    "lightpink", "cadetblue1", "darkorange1"))
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

###### 4. Statistical summary graphs of FEV by BMI and FEV by smoke/gas exposure category. 

``` r
ggplot(data = dat, mapping = aes(x = fev, y = bmi, col = obesity_level)) +
    geom_jitter() + geom_smooth(formula = y ~ x, method = "lm",
    se = FALSE)
```

![](README_files/figure-gfm/plot%20stat%20summary%20of%20fev%20by%20bmi-1.png)<!-- -->

``` r
ggplot(data = dat, mapping = aes(y = fev, x = smoke_gas_exposure,
    color = smoke_gas_exposure)) + stat_summary(fun.min = min,
    fun.max = max, fun = median)
```

![](README_files/figure-gfm/plot%20stat%20summary%20of%20fev%20by%20smoke/gas%20exposure-1.png)<!-- -->

###### 5. A leaflet map showing the concentrations of PM2.5 mass in each of the CHS communities. 

``` r
qpal <- colorQuantile("YlOrRd", dat[, pm25_mass], n = 12)

qpal_colors <- unique(qpal(sort(dat[, pm25_mass])))
qpal_labs <- unique(sort(dat[, pm25_mass]))

leaflet(dat) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircles(lat = ~lat, lng = ~lon, color = ~qpal(pm25_mass),
        radius = 200, fillOpacity = 1) %>%
    addLegend(colors = qpal_colors, labels = qpal_labs, title = "conc. of PM 2.5 mass")
```

![](README_files/figure-gfm/plot%20leaflet%20map-1.png)<!-- -->

###### 6. Choose a visualization to examine whether PM2.5 mass is associated with FEV. 

``` r
library(ggbeeswarm)

ggplot(data = dat, mapping = aes(x = pm25_mass, y = fev, col = pm25_mass)) +
    geom_beeswarm() + geom_smooth(formula = y ~ x, method = "lm",
    col = "black")
```

![](README_files/figure-gfm/pm25%20mass%20and%20fev%20association%20visualization-1.png)<!-- -->
