05-Lab Data Wrangling
================
Flemming Wu
2022-09-21

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
(station_averages <- met[, .(
  temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = USAFID])
```

    ##       USAFID     temp  wind.sp atm.press
    ##    1: 690150 33.18763 3.483560  1010.379
    ##    2: 720110 31.22003 2.138348       NaN
    ##    3: 720113 23.29317 2.470298       NaN
    ##    4: 720120 27.01922 2.503079       NaN
    ##    5: 720137 21.88823 1.979335       NaN
    ##   ---                                   
    ## 1584: 726777 19.15492 4.673878  1014.299
    ## 1585: 726797 18.78980 2.858281  1014.902
    ## 1586: 726798 19.47014 4.449337  1014.072
    ## 1587: 726810 25.03549 3.037356  1011.730
    ## 1588: 726813 23.47809 2.435372  1012.315

The above computes the mean by weather station. Now let’s compute the
median value for each variable.

``` r
(statmedians <- station_averages[ , .(
      temp50        = median(temp, na.rm = TRUE),
      windsp50      = median(wind.sp, na.rm = TRUE),
      atmpress50    = median(atm.press, na.rm = TRUE)
)])
```

    ##      temp50 windsp50 atmpress50
    ## 1: 23.68406 2.463685   1014.691

``` r
#summary(station_averages[, temp])
```

A helpful function ‘which.min()’:

``` r
station_averages[ , 
                  temp_dist50 := abs(temp - statmedians$temp50)][order(temp_dist50)]
```

    ##       USAFID      temp   wind.sp atm.press  temp_dist50
    ##    1: 720458 23.681730  1.209682       NaN  0.002328907
    ##    2: 725515 23.686388  2.709164       NaN  0.002328907
    ##    3: 725835 23.678347  2.652381       NaN  0.005712423
    ##    4: 724509 23.675100  4.066833  1013.863  0.008959632
    ##    5: 720538 23.665932  1.907897       NaN  0.018127186
    ##   ---                                                  
    ## 1584: 722788 36.852459  3.393852       NaN 13.168399783
    ## 1585: 722787 37.258907  2.847381       NaN 13.574848130
    ## 1586: 723805 37.625391  3.532935  1005.207 13.941331392
    ## 1587: 726130  9.189602 12.239908       NaN 14.494456787
    ## 1588: 720385  8.044959  7.298963       NaN 15.639100105

``` r
                  #windsp_dist5050 := abs(wind.sp - statmedian$windsp50),
                  #atmpress_dist50 := abs(atm.press - statmedian$atmpress50)]
station_averages[ which.min(temp_dist50)]
```

    ##    USAFID     temp  wind.sp atm.press temp_dist50
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

``` r
summary(dat[, wind.sp], na.rm = TRUE)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    0.00    2.10    2.46    3.60   36.00   31743

``` r
summary(dat[, atm.press], na.rm = TRUE)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   960.5  1011.8  1014.1  1014.2  1016.4  1059.9 1606783

#### Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.  

Knit the doc and save it on GitHub.

#### Question 3: In the middle?

For each state, identify what is the station that is closest to the
mid-point of the state. Combining these with the stations you identified
in the previous question, use leaflet() to visualize all \~100 points in
the same figure, applying different colors for those identified in this
question.  

Knit the doc and save it on GitHub.

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
  
Number of entries (records),  
Number of NA entries,  
Number of stations,  
Number of states included, and  
Mean temperature, wind-speed, and atmospheric pressure.  
All by the levels described before.  
  
Knit the document, commit your changes, and push them to GitHub. If
you’d like, you can take this time to include the link of the issue of
the week so that you let us know when you are done, e.g.,
