Lab 01: Hello R
================
Flemming Wu
2022-09-08

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
c("2004 dimensions: ", dim(data_2004), "2019 dimensions: ", dim(data_2019))
```

    ## [1] "2004 dimensions: " "19233"             "20"               
    ## [4] "2019 dimensions: " "53156"             "20"
