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
quantile(dat[, temp])
```

    ##   0%  25%  50%  75% 100% 
    ## -3.0 19.6 23.5 27.8 56.0

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
