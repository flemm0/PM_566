Assignment 04: HPC and SQL
================
Flemming Wu
2022-11-17

``` r
library(microbenchmark)
library(parallel)
library(RSQLite)
library(DBI)
```

## HPC

### Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums sum each row in a matrix
fun1 <- function(mat) {
    n <- nrow(mat)
    ans <- double(n)
    for (i in 1:n) {
        ans[i] <- sum(mat[i, ])
    }
    ans
}

fun1alt <- function(mat) {
    rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
    n <- nrow(mat)
    k <- ncol(mat)
    ans <- mat
    for (i in 1:n) {
        for (j in 2:k) {
            ans[i, j] <- mat[i, j] + ans[i, j - 1]
        }
    }
    ans
}

fun2alt <- function(mat) {
    t(apply(mat, 1, cumsum))
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
mb1 <- microbenchmark::microbenchmark(fun1(dat), fun1alt(dat),
    check = "equivalent")

# Test for the second
mb2 <- microbenchmark::microbenchmark(fun2(dat), fun2alt(dat),
    check = "equivalent")


summary(mb1, unit = "relative")
```

    ##           expr      min      lq     mean  median       uq       max neval
    ## 1    fun1(dat) 8.273028 10.8592 8.379098 10.8335 10.28461 0.7280283   100
    ## 2 fun1alt(dat) 1.000000  1.0000 1.000000  1.0000  1.00000 1.0000000   100

``` r
summary(mb2, unit = "relative")
```

    ##           expr      min       lq    mean   median       uq       max neval
    ## 1    fun2(dat) 4.044125 2.472166 1.87645 2.386436 2.184968 0.2772443   100
    ## 2 fun2alt(dat) 1.000000 1.000000 1.00000 1.000000 1.000000 1.0000000   100

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

### Problem 2: Make things run faster with parallel computing

The following function allows simulating pi

``` r
sim_pi <- function(n = 1000, i = NULL) {
    p <- matrix(runif(n * 2), ncol = 2)
    mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000)  # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
time, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000
# points
set.seed(1231)
system.time({
    ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
    print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   4.503   1.147   5.746

Rewrite the previous code using `parLapply()` to make it run faster.
Make sure you set the seed using `clusterSetRNGStream()`:

``` r
n.cores <- detectCores()
cl <- makePSOCKcluster(n.cores)

clusterSetRNGStream(cl, 1231)  # set seed

clusterEvalQ(cl, {
    paste0("Hello from process #", Sys.getpid())
})
```

    ## [[1]]
    ## [1] "Hello from process #28087"
    ## 
    ## [[2]]
    ## [1] "Hello from process #28088"
    ## 
    ## [[3]]
    ## [1] "Hello from process #28086"
    ## 
    ## [[4]]
    ## [1] "Hello from process #28089"

``` r
system.time({
    clusterExport(cl, "sim_pi")
    ans <- unlist(parLapply(cl, 1:4000, sim_pi, n = 10000))
    print(mean(ans))
    stopCluster(cl)
})
```

    ## [1] 3.141578

    ##    user  system elapsed 
    ##   0.008   0.002   2.176

## SQL

Set up a temporary database by running the following chunk

``` r
# install.packages(c('RSQLite', 'DBI'))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

When you write a new chunk, remember to replace the r with sql,
connection=con. Some of these questions will reqruire you to use an
inner join. Read more about them here
<https://www.w3schools.com/sql/sql_join_inner.asp>

### Question 1

How many many movies is there available in each rating category.

``` sql
SELECT rating, COUNT(rating) num_films  
FROM film
GROUP BY rating
ORDER BY num_films DESC
```

| rating | num_films |
|:-------|----------:|
| PG-13  |       223 |
| NC-17  |       210 |
| R      |       195 |
| PG     |       194 |
| G      |       180 |

5 records

### Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql


SELECT c.name rating_cat, AVG(replacement_cost) avg_repl_cost, AVG(rental_rate) avg_rent_rate
FROM film f
JOIN film_category fc
ON f.film_id = fc.film_id
JOIN category c
ON fc.category_id = c.category_id
GROUP BY c.name
ORDER BY avg_repl_cost DESC, avg_rent_rate DESC
```

| rating_cat | avg_repl_cost | avg_rent_rate |
|:-----------|--------------:|--------------:|
| Sci-Fi     |      21.15393 |      3.219508 |
| Drama      |      21.08677 |      3.022258 |
| Classics   |      21.00754 |      2.744386 |
| Action     |      20.91187 |      2.646250 |
| Sports     |      20.39541 |      3.125135 |
| Games      |      20.28508 |      3.252295 |
| Animation  |      20.12636 |      2.808182 |
| Children   |      20.05667 |      2.890000 |
| Horror     |      19.86500 |      3.025714 |
| Family     |      19.72913 |      2.758116 |

Displaying records 1 - 10

### Question 3

Use table film_category together with film to find the how many films
there are with each category ID

``` sql
SELECT fc.category_id, COUNT(*) num_films
FROM film_category fc
JOIN film f
ON fc.film_id = f.film_id
GROUP BY fc.category_id
```

| category_id | num_films |
|:------------|----------:|
| 1           |        64 |
| 2           |        66 |
| 3           |        60 |
| 4           |        57 |
| 5           |        58 |
| 6           |        68 |
| 7           |        62 |
| 8           |        69 |
| 9           |        73 |
| 10          |        61 |

Displaying records 1 - 10

### Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT fc.category_id, c.name category, COUNT(*) AS num_films
FROM film_category fc
JOIN film f
ON fc.film_id = f.film_id
LEFT JOIN category c
ON fc.category_id = c.category_id
GROUP BY fc.category_id
ORDER BY num_films DESC
```

| category_id | category    | num_films |
|------------:|:------------|----------:|
|          15 | Sports      |        74 |
|           9 | Foreign     |        73 |
|           8 | Family      |        69 |
|           6 | Documentary |        68 |
|           2 | Animation   |        66 |
|           1 | Action      |        64 |
|          13 | New         |        63 |
|           7 | Drama       |        62 |
|          14 | Sci-Fi      |        61 |
|          10 | Games       |        61 |

Displaying records 1 - 10

``` r
dbDisconnect(con)
```
