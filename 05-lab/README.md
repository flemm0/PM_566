05-Lab Data Wrangling
================
Flemming Wu
2022-09-22

### Learning goals:

##### Use the merge() function to join two datasets. 

##### Deal with missings and impute data. 

##### Identify relevant observations using quantile(). 

##### Practice your GitHub skills. 

### Lab description

For this lab we will be, again, dealing with the meteorological dataset
downloaded from the NOAA, the met. In this case, we will use data.table
to answer some questions regarding the met dataset, while at the same
time practice your Git+GitHub skills for this project.

This markdown document should be rendered using github_document
document.

Load the data.table (and the dtplyr and dplyr packages if you plan to
work with those).

Load the met data from
<https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz>,
and also the station data. For the later, you can use the code we used
during lecture to pre-process the stations data:

``` r
library(data.table)
library(dtplyr)
library(dplyr)
library(lubridate)
```

``` r
# Load in the met_all data
met <- fread("../04-lab/met_all.gz")
met <- met[met$temp > -17][elev == 9999.0, elev := NA]

# Download the stations data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

Merge the data as we did during the lecture.

``` r
dat <- merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  )
```

#### Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and
atmospheric pressure? Look for the three weather stations that best
represent continental US using the quantile() function. Do these three
coincide?  
  
Knit the document, commit your changes, and Save it on GitHub. Don’t
forget to add README.md to the tree, the first time you render it.

``` r
#Summarize the average temperature, wind speed, and atmospheric pressure for each station 
(station_averages <- dat[, .(
  temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = .(USAFID, STATE)])
```

    ##       USAFID STATE     temp  wind.sp atm.press
    ##    1: 690150    CA 33.18763 3.483560  1010.379
    ##    2: 720110    TX 31.22003 2.138348       NaN
    ##    3: 720113    MI 23.29317 2.470298       NaN
    ##    4: 720120    SC 27.01922 2.503079       NaN
    ##    5: 720137    IL 21.88823 1.979335       NaN
    ##   ---                                         
    ## 1584: 726777    MT 19.15492 4.673878  1014.299
    ## 1585: 726797    MT 18.78980 2.858281  1014.902
    ## 1586: 726798    MT 19.47014 4.449337  1014.072
    ## 1587: 726810    ID 25.03549 3.037356  1011.730
    ## 1588: 726813    ID 23.47809 2.435372  1012.315

The above computes the mean by weather station. Now let’s compute the
median value for each variable.

``` r
(us_medians <- dat[ , .(
      temp50        = median(temp, na.rm = TRUE),
      windsp50      = median(wind.sp, na.rm = TRUE),
      atmpress50    = median(atm.press, na.rm = TRUE)
)])
```

    ##    temp50 windsp50 atmpress50
    ## 1:   23.5      2.1     1014.1

Now create a new column in the station averages data table that holds
the differences between the station averages and the mean across the
entire US.

``` r
(station_averages <- station_averages[,
                 c("temp50_dist", "wind.sp50_dist", "atmpress50_dist") := 
                     .(abs(temp - us_medians$temp50), 
                       abs(wind.sp - us_medians$windsp50), 
                       abs(atm.press - us_medians$atmpress50))
                 ][order(temp50_dist, wind.sp50_dist, atmpress50_dist)])
```

    ##       USAFID STATE      temp   wind.sp atm.press  temp50_dist wind.sp50_dist
    ##    1: 725830    NV 23.498627  2.966084  1012.675  0.001372998      0.8660839
    ##    2: 720549    NV 23.496276  1.953681       NaN  0.003724395      0.1463187
    ##    3: 724769    CO 23.489852  3.214057  1013.090  0.010148233      1.1140571
    ##    4: 723114    VA 23.520054  1.846268       NaN  0.020053957      0.2537320
    ##    5: 726813    ID 23.478088  2.435372  1012.315  0.021912351      0.3353723
    ##   ---                                                                       
    ## 1584: 722788    AZ 36.852459  3.393852       NaN 13.352459016      1.2938525
    ## 1585: 722787    AZ 37.258907  2.847381       NaN 13.758907363      0.7473810
    ## 1586: 723805    CA 37.625391  3.532935  1005.207 14.125390625      1.4329349
    ## 1587: 726130    NH  9.189602 12.239908       NaN 14.310397554     10.1399083
    ## 1588: 720385    CO  8.044959  7.298963       NaN 15.455040872      5.1989635
    ##       atmpress50_dist
    ##    1:        1.425146
    ##    2:             NaN
    ##    3:        1.009677
    ##    4:             NaN
    ##    5:        1.784812
    ##   ---                
    ## 1584:             NaN
    ## 1585:             NaN
    ## 1586:        8.892876
    ## 1587:             NaN
    ## 1588:             NaN

A helpful function ‘which.min()’ to find the station that holds the
closest average readings to the median of the whole data set for each of
the three variables.

``` r
station_averages[which.min(temp50_dist)] #median station in terms of temperature
```

    ##    USAFID STATE     temp  wind.sp atm.press temp50_dist wind.sp50_dist
    ## 1: 725830    NV 23.49863 2.966084  1012.675 0.001372998      0.8660839
    ##    atmpress50_dist
    ## 1:        1.425146

``` r
station_averages[which.min(wind.sp50_dist)] #median station in terms of wind speed
```

    ##    USAFID STATE     temp  wind.sp atm.press temp50_dist wind.sp50_dist
    ## 1: 725479    IA 22.50564 2.099955       NaN   0.9943567   4.528986e-05
    ##    atmpress50_dist
    ## 1:             NaN

``` r
station_averages[which.min(atmpress50_dist)] #median station in terms of atmospheric pressure
```

    ##    USAFID STATE     temp  wind.sp atm.press temp50_dist wind.sp50_dist
    ## 1: 725340    IL 23.40676 3.621518  1014.099  0.09324324       1.521518
    ##    atmpress50_dist
    ## 1:     0.000811124

The stations closest to national median in temperature, wind speed, and
atmospheric pressure seem to be spread around the country, and not
consolidated in one state or region.

#### Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.  

Knit the doc and save it on GitHub.

``` r
(state_medians <- dat[ , .(
      temp50        = median(temp, na.rm = TRUE),
      windsp50      = median(wind.sp, na.rm = TRUE),
      atmpress50    = median(atm.press, na.rm = TRUE)),
      by = STATE])
```

    ##     STATE temp50 windsp50 atmpress50
    ##  1:    CA   21.1      2.6     1012.8
    ##  2:    TX   29.0      3.1     1012.6
    ##  3:    MI   20.6      2.1     1014.4
    ##  4:    SC   25.0      1.5     1015.3
    ##  5:    IL   22.2      2.1     1014.6
    ##  6:    MO   23.3      2.1     1014.8
    ##  7:    AR   25.6      2.1     1014.5
    ##  8:    OR   17.2      2.1     1015.4
    ##  9:    WA   18.0      0.0         NA
    ## 10:    GA   26.0      1.5     1014.9
    ## 11:    MN   19.0      2.1     1014.7
    ## 12:    AL   25.3      1.5     1014.8
    ## 13:    IN   22.0      2.1     1014.8
    ## 14:    NC   24.0      1.5     1015.7
    ## 15:    VA   23.4      1.5     1015.2
    ## 16:    IA   21.0      2.6     1014.9
    ## 17:    PA   21.1      1.5     1015.6
    ## 18:    NE   21.7      3.1     1014.3
    ## 19:    ID   20.0      2.1     1013.1
    ## 20:    WI   18.6      2.1     1014.6
    ## 21:    WV   21.1      1.5     1015.7
    ## 22:    MD   24.4      2.1     1015.3
    ## 23:    AZ   29.0      3.1     1010.8
    ## 24:    OK   26.7      3.1     1012.8
    ## 25:    WY   18.3      3.6     1014.0
    ## 26:    LA   27.8      1.5     1014.6
    ## 27:    KY   23.0      1.5     1015.3
    ## 28:    FL   27.0      2.6     1015.1
    ## 29:    CO   18.9      2.6     1013.7
    ## 30:    OH   21.7      2.6     1015.0
    ## 31:    NJ   23.3      1.5     1015.1
    ## 32:    NM   24.4      3.1     1012.0
    ## 33:    KS   23.3      3.6     1013.5
    ## 34:    ND   18.0      3.6         NA
    ## 35:    VT   18.9      1.5     1014.5
    ## 36:    MS   26.0      1.5     1014.9
    ## 37:    CT   22.2      2.1     1015.2
    ## 38:    NV   27.0      2.6     1011.8
    ## 39:    UT   26.1      4.1     1012.1
    ## 40:    SD   20.0      3.6     1014.3
    ## 41:    TN   24.0      1.5     1014.9
    ## 42:    NY   20.6      2.1     1014.9
    ## 43:    RI   22.2      2.6     1014.0
    ## 44:    MA   21.7      2.6     1014.9
    ## 45:    DE   24.4      2.6     1015.4
    ## 46:    NH   18.9      1.5     1014.6
    ## 47:    ME   18.9      2.1     1014.1
    ## 48:    MT   18.3      3.1     1014.3
    ##     STATE temp50 windsp50 atmpress50

``` r
#Remove distance columns from question 1
station_averages[, c("temp50_dist", "wind.sp50_dist", "atmpress50_dist") := NULL]


#Merge state median values back with station averages to add three new columns
(station_averages <- merge(
  x = station_averages,
  y = state_medians,
  by.x = "STATE",
  by.y = "STATE",
  all.x = TRUE,
  all.y = FALSE
))
```

    ##       STATE USAFID     temp   wind.sp atm.press temp50 windsp50 atmpress50
    ##    1:    AL 720413 24.48892 1.4319361  1049.886   25.3      1.5     1014.8
    ##    2:    AL 722031 24.88190 0.9408126       NaN   25.3      1.5     1014.8
    ##    3:    AL 723231 24.95596 1.4908629  1015.070   25.3      1.5     1014.8
    ##    4:    AL 720376 24.97884 1.2960442       NaN   25.3      1.5     1014.8
    ##    5:    AL 722279 25.07919 1.9837526  1015.289   25.3      1.5     1014.8
    ##   ---                                                                     
    ## 1584:    WY 726710 16.86569 3.4922179  1014.945   18.3      3.6     1014.0
    ## 1585:    WY 720522 16.10256 4.6162089       NaN   18.3      3.6     1014.0
    ## 1586:    WY 725776 15.91732 2.9382716  1017.804   18.3      3.6     1014.0
    ## 1587:    WY 720345 15.33827 2.9487410       NaN   18.3      3.6     1014.0
    ## 1588:    WY 726664 12.24103 1.0831429  1019.024   18.3      3.6     1014.0

``` r
(station_averages <- station_averages[,
                 c("temp50_dist", "wind.sp50_dist", "atmpress50_dist") := 
                     .(abs(temp - temp50), 
                       abs(wind.sp - windsp50), 
                       abs(atm.press - atmpress50))
                 ])


#(station_averages[, temp50_dist := abs(temp - temp50)])
#station_averages[, wind.sp50_dist := abs(wind.sp - wind.sp50)]
#station_averages[, atm.press50_dist := abs(atm.press - atm.press50)]
```

``` r
(station_averages[ , eucdist := sqrt(temp50_dist^2 + wind.sp50_dist^2 + atmpress50_dist^2)])
```

``` r
#Group by state, and compute min on eucdist column, which will be the .SD column
#Assign to temporary table
state_eucdist <- station_averages[, lapply(.SD, min, na.rm = T), by = STATE, .SDcols = c("eucdist")]
```

    ## Warning in gmin(eucdist, na.rm = TRUE): No non-missing values found in at least
    ## one group. Returning 'Inf' for such groups to be consistent with base

``` r
#search the euclidean distances in the station averages to get the USAFID associated with the stations of interest
station_averages <- station_averages[eucdist %in% state_eucdist[, eucdist]]
```

``` r
merge(station_averages, dat[ , .(STATE, lat, lon)], by.x = "STATE", by.y = "STATE", all.y = FALSE)
```

    ##          STATE USAFID     temp  wind.sp atm.press temp50 windsp50 atmpress50
    ##       1:    AL 722267 25.34049 1.543094  1014.992   25.3      1.5     1014.8
    ##       2:    AL 722267 25.34049 1.543094  1014.992   25.3      1.5     1014.8
    ##       3:    AL 722267 25.34049 1.543094  1014.992   25.3      1.5     1014.8
    ##       4:    AL 722267 25.34049 1.543094  1014.992   25.3      1.5     1014.8
    ##       5:    AL 722267 25.34049 1.543094  1014.992   25.3      1.5     1014.8
    ##      ---                                                                    
    ## 2271640:    WY 726654 19.85844 3.775443  1014.107   18.3      3.6     1014.0
    ## 2271641:    WY 726654 19.85844 3.775443  1014.107   18.3      3.6     1014.0
    ## 2271642:    WY 726654 19.85844 3.775443  1014.107   18.3      3.6     1014.0
    ## 2271643:    WY 726654 19.85844 3.775443  1014.107   18.3      3.6     1014.0
    ## 2271644:    WY 726654 19.85844 3.775443  1014.107   18.3      3.6     1014.0
    ##          temp50_dist wind.sp50_dist atmpress50_dist   eucdist    lat      lon
    ##       1:  0.04048653     0.04309441       0.1920699 0.2009655 32.915  -85.963
    ##       2:  0.04048653     0.04309441       0.1920699 0.2009655 32.915  -85.963
    ##       3:  0.04048653     0.04309441       0.1920699 0.2009655 32.915  -85.963
    ##       4:  0.04048653     0.04309441       0.1920699 0.2009655 32.915  -85.963
    ##       5:  0.04048653     0.04309441       0.1920699 0.2009655 32.915  -85.963
    ##      ---                                                                     
    ## 2271640:  1.55844005     0.17544274       0.1071023 1.5719372 43.064 -108.458
    ## 2271641:  1.55844005     0.17544274       0.1071023 1.5719372 43.064 -108.458
    ## 2271642:  1.55844005     0.17544274       0.1071023 1.5719372 43.064 -108.458
    ## 2271643:  1.55844005     0.17544274       0.1071023 1.5719372 43.064 -108.458
    ## 2271644:  1.55844005     0.17544274       0.1071023 1.5719372 43.064 -108.458

#### Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use leaflet() to visualize all \~100 points in
the same figure, applying different colors for those identified in this
question.  

Knit the doc and save it on GitHub.

``` r
#Find the physical/geographic midpoint of each state using median of the longitudes and latitudes

(state_midpoints <- dat[,
                   .(
                     lat50 = quantile(lat, prob = 0.5, na.rm = TRUE),
                     lon50 = quantile(lon, prob = 0.5, na.rm = TRUE)
                   ),
                   by = "STATE"])
```

    ##     STATE  lat50    lon50
    ##  1:    CA 37.285 -120.448
    ##  2:    TX 31.178  -97.691
    ##  3:    MI 43.067  -84.688
    ##  4:    SC 34.181  -80.634
    ##  5:    IL 40.520  -88.751
    ##  6:    MO 38.583  -93.183
    ##  7:    AR 35.333  -92.767
    ##  8:    OR 42.600 -123.364
    ##  9:    WA 47.104 -122.416
    ## 10:    GA 32.564  -83.241
    ## 11:    MN 45.117  -94.382
    ## 12:    AL 32.915  -86.557
    ## 13:    IN 40.711  -86.296
    ## 14:    NC 35.633  -79.101
    ## 15:    VA 37.321  -77.558
    ## 16:    IA 41.717  -93.619
    ## 17:    PA 40.435  -76.922
    ## 18:    NE 41.189  -98.054
    ## 19:    ID 43.650 -116.240
    ## 20:    WI 44.614  -89.774
    ## 21:    WV 38.885  -80.400
    ## 22:    MD 39.167  -76.667
    ## 23:    AZ 34.257 -111.733
    ## 24:    OK 35.534  -97.350
    ## 25:    WY 42.796 -108.389
    ## 26:    LA 30.521  -91.983
    ## 27:    KY 37.591  -84.672
    ## 28:    FL 28.290  -81.876
    ## 29:    CO 39.217 -105.861
    ## 30:    OH 40.333  -83.078
    ## 31:    NJ 40.617  -74.417
    ## 32:    NM 34.067 -105.990
    ## 33:    KS 38.329  -97.430
    ## 34:    ND 48.301  -99.621
    ## 35:    VT 44.567  -72.562
    ## 36:    MS 33.433  -90.078
    ## 37:    CT 41.384  -72.682
    ## 38:    NV 39.417 -116.891
    ## 39:    UT 38.427 -113.012
    ## 40:    SD 44.045  -99.318
    ## 41:    TN 35.593  -86.246
    ## 42:    NY 42.241  -74.846
    ## 43:    RI 41.597  -71.433
    ## 44:    MA 42.098  -70.918
    ## 45:    DE 39.133  -75.467
    ## 46:    NH 43.278  -71.503
    ## 47:    ME 44.316  -69.667
    ## 48:    MT 45.788 -110.440
    ##     STATE  lat50    lon50

``` r
#Merge back to original datatable to obtain station numbers
(state_midpoints <- merge(
  x = state_midpoints,
  y = dat,
  by.x = "STATE",
  by.y = "STATE"
))
```

    ##          STATE  lat50    lon50 USAFID  WBAN year month day hour min    lat
    ##       1:    AL 32.915  -86.557 720265 63833 2019     8   1    0  15 32.915
    ##       2:    AL 32.915  -86.557 720265 63833 2019     8   1    0  35 32.915
    ##       3:    AL 32.915  -86.557 720265 63833 2019     8   1    0  55 32.915
    ##       4:    AL 32.915  -86.557 720265 63833 2019     8   1    1  15 32.915
    ##       5:    AL 32.915  -86.557 720265 63833 2019     8   1    1  35 32.915
    ##      ---                                                                  
    ## 2317200:    WY 42.796 -108.389 726720 24061 2019     8  31   19  53 43.064
    ## 2317201:    WY 42.796 -108.389 726720 24061 2019     8  31   20  53 43.064
    ## 2317202:    WY 42.796 -108.389 726720 24061 2019     8  31   21  53 43.064
    ## 2317203:    WY 42.796 -108.389 726720 24061 2019     8  31   22  53 43.064
    ## 2317204:    WY 42.796 -108.389 726720 24061 2019     8  31   23  53 43.064
    ##               lon elev wind.dir wind.dir.qc wind.type.code wind.sp wind.sp.qc
    ##       1:  -85.963  209       NA           9              C     0.0          5
    ##       2:  -85.963  209       NA           9              C     0.0          5
    ##       3:  -85.963  209       NA           9              C     0.0          5
    ##       4:  -85.963  209       NA           9              C     0.0          5
    ##       5:  -85.963  209       NA           9              C     0.0          5
    ##      ---                                                                     
    ## 2317200: -108.458 1684       50           5              N     3.6          5
    ## 2317201: -108.458 1684      330           5              N     2.1          5
    ## 2317202: -108.458 1684       NA           9              V     1.5          5
    ## 2317203: -108.458 1684       90           5              N     3.1          5
    ## 2317204: -108.458 1684       90           5              N     3.1          5
    ##          ceiling.ht ceiling.ht.qc ceiling.ht.method sky.cond vis.dist
    ##       1:      22000             5                 9        N    16093
    ##       2:      22000             5                 9        N    16093
    ##       3:      22000             5                 9        N    16093
    ##       4:      22000             5                 9        N    16093
    ##       5:      22000             5                 9        N    16093
    ##      ---                                                             
    ## 2317200:      22000             5                 9        N    16093
    ## 2317201:      22000             5                 9        N    16093
    ## 2317202:      22000             5                 9        N    16093
    ## 2317203:      22000             5                 9        N    16093
    ## 2317204:      22000             5                 9        N    16093
    ##          vis.dist.qc vis.var vis.var.qc temp temp.qc dew.point dew.point.qc
    ##       1:           5       N          5 29.2       5      22.0            5
    ##       2:           5       N          5 27.7       5      22.3            5
    ##       3:           5       N          5 27.1       5      23.0            5
    ##       4:           5       N          5 26.2       5      22.8            5
    ##       5:           5       N          5 26.0       5      23.0            5
    ##      ---                                                                   
    ## 2317200:           5       N          5 27.2       5       5.6            5
    ## 2317201:           5       N          5 28.9       5       3.3            5
    ## 2317202:           5       N          5 29.4       5       4.4            5
    ## 2317203:           5       N          5 30.6       5       2.8            5
    ## 2317204:           5       N          5 30.0       5       2.8            5
    ##          atm.press atm.press.qc       rh CTRY
    ##       1:        NA            9 65.19533   US
    ##       2:        NA            9 72.47858   US
    ##       3:        NA            9 78.33501   US
    ##       4:        NA            9 81.61330   US
    ##       5:        NA            9 83.59005   US
    ##      ---                                     
    ## 2317200:    1014.9            5 25.20016   US
    ## 2317201:    1014.1            5 19.37114   US
    ## 2317202:    1013.3            5 20.33048   US
    ## 2317203:    1012.4            5 16.90751   US
    ## 2317204:    1012.3            5 17.51524   US

``` r
#Group new data frame by station
(state_midpoints <- state_midpoints[, .(STATE, lat50, lon50, lat, lon), by = "USAFID"])
```

    ##          USAFID STATE  lat50    lon50    lat      lon
    ##       1: 720265    AL 32.915  -86.557 32.915  -85.963
    ##       2: 720265    AL 32.915  -86.557 32.915  -85.963
    ##       3: 720265    AL 32.915  -86.557 32.915  -85.963
    ##       4: 720265    AL 32.915  -86.557 32.915  -85.963
    ##       5: 720265    AL 32.915  -86.557 32.915  -85.963
    ##      ---                                             
    ## 2317200: 726720    WY 42.796 -108.389 43.064 -108.458
    ## 2317201: 726720    WY 42.796 -108.389 43.064 -108.458
    ## 2317202: 726720    WY 42.796 -108.389 43.064 -108.458
    ## 2317203: 726720    WY 42.796 -108.389 43.064 -108.458
    ## 2317204: 726720    WY 42.796 -108.389 43.064 -108.458

``` r
#Calculate distance from state midpoint for each station
state_midpoints[, c("lat50_dist", "lon50_dist") := 
                  .(abs(lat - lat50), abs(lon - lon50))]

state_midpoints[, eucdist := sqrt(lat50_dist^2 + lon50_dist^2)]

(state_midpoints <- state_midpoints[, .SD[which.min(eucdist)], by = "STATE"])
```

    ##     STATE USAFID  lat50    lon50    lat      lon lat50_dist lon50_dist
    ##  1:    AL 722300 32.915  -86.557 33.177  -86.783      0.262      0.226
    ##  2:    AR 723429 35.333  -92.767 35.259  -93.093      0.074      0.326
    ##  3:    AZ 723745 34.257 -111.733 34.257 -111.339      0.000      0.394
    ##  4:    CA 724815 37.285 -120.448 37.285 -120.512      0.000      0.064
    ##  5:    CO 722061 39.217 -105.861 39.467 -106.150      0.250      0.289
    ##  6:    CT 720545 41.384  -72.682 41.384  -72.506      0.000      0.176
    ##  7:    DE 724088 39.133  -75.467 39.133  -75.467      0.000      0.000
    ##  8:    FL 721042 28.290  -81.876 28.228  -82.156      0.062      0.280
    ##  9:    GA 722217 32.564  -83.241 32.564  -82.985      0.000      0.256
    ## 10:    IA 725466 41.717  -93.619 41.691  -93.566      0.026      0.053
    ## 11:    ID 726810 43.650 -116.240 43.567 -116.240      0.083      0.000
    ## 12:    IL 724397 40.520  -88.751 40.477  -88.916      0.043      0.165
    ## 13:    IN 720961 40.711  -86.296 40.711  -86.375      0.000      0.079
    ## 14:    KS 724509 38.329  -97.430 38.068  -97.275      0.261      0.155
    ## 15:    KY 720448 37.591  -84.672 37.578  -84.770      0.013      0.098
    ## 16:    LA 720468 30.521  -91.983 30.558  -92.099      0.037      0.116
    ## 17:    MA 725068 42.098  -70.918 41.876  -71.021      0.222      0.103
    ## 18:    MD 724060 39.167  -76.667 39.175  -76.668      0.008      0.001
    ## 19:    ME 726185 44.316  -69.667 44.316  -69.797      0.000      0.130
    ## 20:    MI 725405 43.067  -84.688 43.322  -84.688      0.255      0.000
    ## 21:    MN 726583 45.117  -94.382 45.097  -94.507      0.020      0.125
    ## 22:    MO 724453 38.583  -93.183 38.704  -93.183      0.121      0.000
    ## 23:    MS 725023 33.433  -90.078 33.761  -90.758      0.328      0.680
    ## 24:    MT 726798 45.788 -110.440 45.699 -110.448      0.089      0.008
    ## 25:    NC 722201 35.633  -79.101 35.584  -79.101      0.049      0.000
    ## 26:    ND 720867 48.301  -99.621 48.390 -100.024      0.089      0.403
    ## 27:    NE 725513 41.189  -98.054 40.893  -97.997      0.296      0.057
    ## 28:    NH 726050 43.278  -71.503 43.205  -71.503      0.073      0.000
    ## 29:    NJ 720581 40.617  -74.417 40.617  -74.250      0.000      0.167
    ## 30:    NM 722683 34.067 -105.990 33.463 -105.535      0.604      0.455
    ## 31:    NV 724770 39.417 -116.891 39.600 -116.010      0.183      0.881
    ## 32:    NY 725145 42.241  -74.846 41.702  -74.795      0.539      0.051
    ## 33:    OH 720928 40.333  -83.078 40.280  -83.115      0.053      0.037
    ## 34:    OK 723540 35.534  -97.350 35.417  -97.383      0.117      0.033
    ## 35:    OR 725975 42.600 -123.364 42.600 -123.364      0.000      0.000
    ## 36:    PA 725118 40.435  -76.922 40.217  -76.851      0.218      0.071
    ## 37:    RI 725074 41.597  -71.433 41.597  -71.412      0.000      0.021
    ## 38:    SC 720603 34.181  -80.634 34.283  -80.567      0.102      0.067
    ## 39:    SD 726530 44.045  -99.318 43.767  -99.318      0.278      0.000
    ## 40:    TN 721031 35.593  -86.246 35.380  -86.246      0.213      0.000
    ## 41:    TX 722570 31.178  -97.691 31.150  -97.717      0.028      0.026
    ## 42:    UT 724750 38.427 -113.012 38.427 -113.012      0.000      0.000
    ## 43:    VA 720498 37.321  -77.558 37.400  -77.517      0.079      0.041
    ## 44:    VT 726114 44.567  -72.562 44.535  -72.614      0.032      0.052
    ## 45:    WA 720388 47.104 -122.416 47.104 -122.287      0.000      0.129
    ## 46:    WI 726465 44.614  -89.774 44.778  -89.667      0.164      0.107
    ## 47:    WV 720328 38.885  -80.400 39.000  -80.274      0.115      0.126
    ## 48:    WY 726720 42.796 -108.389 43.064 -108.458      0.268      0.069
    ##     STATE USAFID  lat50    lon50    lat      lon lat50_dist lon50_dist
    ##         eucdist
    ##  1: 0.346005780
    ##  2: 0.334293284
    ##  3: 0.394000000
    ##  4: 0.064000000
    ##  5: 0.382126942
    ##  6: 0.176000000
    ##  7: 0.000000000
    ##  8: 0.286782147
    ##  9: 0.256000000
    ## 10: 0.059033889
    ## 11: 0.083000000
    ## 12: 0.170510997
    ## 13: 0.079000000
    ## 14: 0.303555596
    ## 15: 0.098858485
    ## 16: 0.121757957
    ## 17: 0.244730464
    ## 18: 0.008062258
    ## 19: 0.130000000
    ## 20: 0.255000000
    ## 21: 0.126589889
    ## 22: 0.121000000
    ## 23: 0.754972847
    ## 24: 0.089358827
    ## 25: 0.049000000
    ## 26: 0.412710552
    ## 27: 0.301438219
    ## 28: 0.073000000
    ## 29: 0.167000000
    ## 30: 0.756201693
    ## 31: 0.899805535
    ## 32: 0.541407425
    ## 33: 0.064637450
    ## 34: 0.121564798
    ## 35: 0.000000000
    ## 36: 0.229270583
    ## 37: 0.021000000
    ## 38: 0.122036880
    ## 39: 0.278000000
    ## 40: 0.213000000
    ## 41: 0.038209946
    ## 42: 0.000000000
    ## 43: 0.089005618
    ## 44: 0.061057350
    ## 45: 0.129000000
    ## 46: 0.195818794
    ## 47: 0.170590152
    ## 48: 0.276739950
    ##         eucdist

``` r
#plot_data <- rbind(state_midpoints[, .(STATE, lat, lon)], station_averages[, .(STATE, lat, lon)])

#Need to create new column for indicating if it is an average or midpoint station
```

#### Question 4: Means of means

Using the quantile() function, generate a summary table that shows the
number of states included, average temperature, wind-speed, and
atmospheric pressure by the variable “average temperature level,” which
you’ll need to create.  
  
Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:  
  
low: temp \< 20  
Mid: temp \>= 20 and temp \< 25  
High: temp \>= 25  
Once you are done with that, you can compute the following:  
  

``` r
temp_summary <- dat[, .(avg_temp = mean(temp)), by = STATE]

(temp_summary[, temp_factor := fifelse(avg_temp < 20, "low", 
                             fifelse(avg_temp >= 20 & avg_temp < 25, "mid", "high"))])
```

Number of entries (records),  
Number of NA entries,  
Number of stations,  
Number of states included, and  
Mean temperature, wind-speed, and atmospheric pressure.  
All by the levels described before.  
  
Knit the document, commit your changes, and push them to GitHub. If
you’d like, you can take this time to include the link of the issue of
the week so that you let us know when you are done, e.g.,
