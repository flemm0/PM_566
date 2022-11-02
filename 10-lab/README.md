Lab 10 - SQL
================
Flemming Wu
2022-11-01

## Setup

``` r
if (!require("RSQLite")) {
    install.packages("RSQLite")
}
if (!require("DBI")) {
    install.packages("DBL")
}

library(RSQLite)
library(DBI)
```

``` r
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
```

``` r
dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

TIP: You can use the following QUERY to see the structure of a table

``` sql
PRAGMA table_info(actor)
```

| cid | name        | type    | notnull | dflt_value |  pk |
|:----|:------------|:--------|--------:|:-----------|----:|
| 0   | actor_id    | INTEGER |       0 | NA         |   0 |
| 1   | first_name  | TEXT    |       0 | NA         |   0 |
| 2   | last_name   | TEXT    |       0 | NA         |   0 |
| 3   | last_update | TEXT    |       0 | NA         |   0 |

4 records

SQL references:

<https://www.w3schools.com/sql/>

### Exercise 1

Retrive the actor ID, first name and last name for all actors using the
actor table. Sort by last name and then by first name.

``` sql
SELECT actor_id, first_name, last_name
FROM actor
ORDER BY last_name, first_name
```

| actor_id | first_name | last_name |
|---------:|:-----------|:----------|
|       58 | CHRISTIAN  | AKROYD    |
|      182 | DEBBIE     | AKROYD    |
|       92 | KIRSTEN    | AKROYD    |
|      118 | CUBA       | ALLEN     |
|      145 | KIM        | ALLEN     |
|      194 | MERYL      | ALLEN     |
|       76 | ANGELINA   | ASTAIRE   |
|      112 | RUSSELL    | BACALL    |
|      190 | AUDREY     | BAILEY    |
|       67 | JESSICA    | BAILEY    |

Displaying records 1 - 10

## Exercise 2

Retrive the actor ID, first name, and last name for actors whose last
name equals ‘WILLIAMS’ or ‘DAVIS’.

## Exercise 3

Write a query against the rental table that returns the IDs of the
customers who rented a film on July 5, 2005 (use the rental.rental_date
column, and you can use the date() function to ignore the time
component). Include a single row for each distinct customer ID.

## Exercise 4

### Exercise 4.1

Construct a query that retrives all rows from the payment table where
the amount is either 1.99, 7.99, 9.99.

### Exercise 4.2

Construct a query that retrives all rows from the payment table where
the amount is greater then 5

### Exercise 4.3

Construct a query that retrives all rows from the payment table where
the amount is greater then 5 and less then 8

## Exercise 5

Retrive all the payment IDs and their amount from the customers whose
last name is ‘DAVIS’.

## Exercise 6

### Exercise 6.1

Use COUNT(\*) to count the number of rows in rental

### Exercise 6.2

Use COUNT(\*) and GROUP BY to count the number of rentals for each
customer_id

### Exercise 6.3

Repeat the previous query and sort by the count in descending order

### Exercise 6.4

Repeat the previous query but use HAVING to only keep the groups with 40
or more.

## Exercise 7

The following query calculates a number of summary statistics for the
payment table using MAX, MIN, AVG and SUM

### Exercise 7.1

Modify the above query to do those calculations for each customer_id

### Exercise 7.2

Modify the above query to only keep the customer_ids that have more then
5 payments

## Cleanup

Run the following chunk to disconnect from the connection.

``` r
dbDisconnect(con)
```
