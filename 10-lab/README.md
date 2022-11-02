Lab 10 - SQL
================
Flemming Wu
2022-11-02

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
ORDER BY last_name, first_name;
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

Retrieve the actor ID, first name, and last name for actors whose last
name equals ‘WILLIAMS’ or ‘DAVIS’.

``` sql
SELECT actor_id, first_name, last_name
FROM actor
WHERE last_name IN ('WILLIAMS', 'DAVIS');
```

| actor_id | first_name | last_name |
|---------:|:-----------|:----------|
|        4 | JENNIFER   | DAVIS     |
|       72 | SEAN       | WILLIAMS  |
|      101 | SUSAN      | DAVIS     |
|      110 | SUSAN      | DAVIS     |
|      137 | MORGAN     | WILLIAMS  |
|      172 | GROUCHO    | WILLIAMS  |

6 records

## Exercise 3

Write a query against the rental table that returns the IDs of the
customers who rented a film on July 5, 2005 (use the rental.rental_date
column, and you can use the date() function to ignore the time
component). Include a single row for each distinct customer ID.

``` sql
-- PRAGMA table_info(rental)

SELECT customer_id
FROM rental
WHERE DATE(rental_date) = '2005-07-05';
```

| customer_id |
|------------:|
|         565 |
|         242 |
|          37 |
|          60 |
|         594 |
|           8 |
|         490 |
|         476 |
|         322 |
|         298 |

Displaying records 1 - 10

## Exercise 4

### Exercise 4.1

Construct a query that retrieves all rows from the payment table where
the amount is either 1.99, 7.99, 9.99.

``` sql
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99);
```

| payment_id | customer_id | staff_id | rental_id | amount | payment_date               |
|-----------:|------------:|---------:|----------:|-------:|:---------------------------|
|      16050 |         269 |        2 |         7 |   1.99 | 2007-01-24 21:40:19.996577 |
|      16056 |         270 |        1 |       193 |   1.99 | 2007-01-26 05:10:14.996577 |
|      16081 |         282 |        2 |        48 |   1.99 | 2007-01-25 04:49:12.996577 |
|      16103 |         294 |        1 |       595 |   1.99 | 2007-01-28 12:28:20.996577 |
|      16133 |         307 |        1 |       614 |   1.99 | 2007-01-28 14:01:54.996577 |
|      16158 |         316 |        1 |      1065 |   1.99 | 2007-01-31 07:23:22.996577 |
|      16160 |         318 |        1 |       224 |   9.99 | 2007-01-26 08:46:53.996577 |
|      16161 |         319 |        1 |        15 |   9.99 | 2007-01-24 23:07:48.996577 |
|      16180 |         330 |        2 |       967 |   7.99 | 2007-01-30 17:40:32.996577 |
|      16206 |         351 |        1 |      1137 |   1.99 | 2007-01-31 17:48:40.996577 |

Displaying records 1 - 10

### Exercise 4.2

Construct a query that retrieves all rows from the payment table where
the amount is greater then 5

``` sql
SELECT *
FROM payment
WHERE amount > 5;
```

| payment_id | customer_id | staff_id | rental_id | amount | payment_date               |
|-----------:|------------:|---------:|----------:|-------:|:---------------------------|
|      16052 |         269 |        2 |       678 |   6.99 | 2007-01-28 21:44:14.996577 |
|      16058 |         271 |        1 |      1096 |   8.99 | 2007-01-31 11:59:15.996577 |
|      16060 |         272 |        1 |       405 |   6.99 | 2007-01-27 12:01:05.996577 |
|      16061 |         272 |        1 |      1041 |   6.99 | 2007-01-31 04:14:49.996577 |
|      16068 |         274 |        1 |       394 |   5.99 | 2007-01-27 09:54:37.996577 |
|      16073 |         276 |        1 |       860 |  10.99 | 2007-01-30 01:13:42.996577 |
|      16074 |         277 |        2 |       308 |   6.99 | 2007-01-26 20:30:05.996577 |
|      16082 |         282 |        2 |       282 |   6.99 | 2007-01-26 17:24:52.996577 |
|      16086 |         284 |        1 |      1145 |   6.99 | 2007-01-31 18:42:11.996577 |
|      16087 |         286 |        2 |        81 |   6.99 | 2007-01-25 10:43:45.996577 |

Displaying records 1 - 10

### Exercise 4.3

Construct a query that retrieves all rows from the payment table where
the amount is greater than 5 and less than 8

``` sql
SELECT *
FROM payment
WHERE amount BETWEEN 5 AND 8;
```

| payment_id | customer_id | staff_id | rental_id | amount | payment_date               |
|-----------:|------------:|---------:|----------:|-------:|:---------------------------|
|      16052 |         269 |        2 |       678 |   6.99 | 2007-01-28 21:44:14.996577 |
|      16060 |         272 |        1 |       405 |   6.99 | 2007-01-27 12:01:05.996577 |
|      16061 |         272 |        1 |      1041 |   6.99 | 2007-01-31 04:14:49.996577 |
|      16068 |         274 |        1 |       394 |   5.99 | 2007-01-27 09:54:37.996577 |
|      16074 |         277 |        2 |       308 |   6.99 | 2007-01-26 20:30:05.996577 |
|      16082 |         282 |        2 |       282 |   6.99 | 2007-01-26 17:24:52.996577 |
|      16086 |         284 |        1 |      1145 |   6.99 | 2007-01-31 18:42:11.996577 |
|      16087 |         286 |        2 |        81 |   6.99 | 2007-01-25 10:43:45.996577 |
|      16092 |         288 |        2 |       427 |   6.99 | 2007-01-27 14:38:30.996577 |
|      16094 |         288 |        2 |       565 |   5.99 | 2007-01-28 07:54:57.996577 |

Displaying records 1 - 10

## Exercise 5

Retrieve all the payment IDs and their amount from the customers whose
last name is ‘DAVIS’.

``` sql
SELECT p.payment_id, p.amount
FROM payment p
JOIN customer c
ON p.customer_id = c.customer_id
WHERE c.last_name LIKE 'DAVIS';
```

| payment_id | amount |
|:-----------|-------:|
| 16685      |   4.99 |
| 16686      |   2.99 |
| 16687      |   0.99 |

3 records

## Exercise 6

### Exercise 6.1

Use COUNT(\*) to count the number of rows in rental

``` sql
SELECT COUNT(*) num_rows
FROM rental;
```

| num_rows |
|---------:|
|    16044 |

1 records

### Exercise 6.2

Use COUNT(\*) and GROUP BY to count the number of rentals for each
customer_id

``` sql
SELECT customer_id, COUNT(*) num_rentals
FROM rental
GROUP BY customer_id;
```

| customer_id | num_rentals |
|:------------|------------:|
| 1           |          32 |
| 2           |          27 |
| 3           |          26 |
| 4           |          22 |
| 5           |          38 |
| 6           |          28 |
| 7           |          33 |
| 8           |          24 |
| 9           |          23 |
| 10          |          25 |

Displaying records 1 - 10

### Exercise 6.3

Repeat the previous query and sort by the count in descending order

``` sql
SELECT customer_id, COUNT(*) num_rentals
FROM rental
GROUP BY customer_id
ORDER BY num_rentals DESC;
```

| customer_id | num_rentals |
|------------:|------------:|
|         148 |          46 |
|         526 |          45 |
|         236 |          42 |
|         144 |          42 |
|          75 |          41 |
|         469 |          40 |
|         197 |          40 |
|         468 |          39 |
|         178 |          39 |
|         137 |          39 |

Displaying records 1 - 10

### Exercise 6.4

Repeat the previous query but use HAVING to only keep the groups with 40
or more.

``` sql
SELECT customer_id, COUNT(*) num_rentals
FROM rental
GROUP BY customer_id
HAVING num_rentals > 40
ORDER BY num_rentals DESC;
```

| customer_id | num_rentals |
|------------:|------------:|
|         148 |          46 |
|         526 |          45 |
|         236 |          42 |
|         144 |          42 |
|          75 |          41 |

5 records

## Exercise 7

The following query calculates a number of summary statistics for the
payment table using MAX, MIN, AVG and SUM

``` sql
SELECT MAX(amount), MIN(amount), AVG(amount), SUM(amount)
FROM payment;
```

| MAX(amount) | MIN(amount) | AVG(amount) | SUM(amount) |
|------------:|------------:|------------:|------------:|
|       11.99 |        0.99 |    4.169775 |     4824.43 |

1 records

### Exercise 7.1

Modify the above query to do those calculations for each customer_id

``` sql
SELECT customer_id, MAX(amount), MIN(amount), AVG(amount), SUM(amount)
FROM payment
GROUP BY customer_id;
```

| customer_id | MAX(amount) | MIN(amount) | AVG(amount) | SUM(amount) |
|------------:|------------:|------------:|------------:|------------:|
|           1 |        2.99 |        0.99 |    1.990000 |        3.98 |
|           2 |        4.99 |        4.99 |    4.990000 |        4.99 |
|           3 |        2.99 |        1.99 |    2.490000 |        4.98 |
|           5 |        6.99 |        0.99 |    3.323333 |        9.97 |
|           6 |        4.99 |        0.99 |    2.990000 |        8.97 |
|           7 |        5.99 |        0.99 |    4.190000 |       20.95 |
|           8 |        6.99 |        6.99 |    6.990000 |        6.99 |
|           9 |        4.99 |        0.99 |    3.656667 |       10.97 |
|          10 |        4.99 |        4.99 |    4.990000 |        4.99 |
|          11 |        6.99 |        6.99 |    6.990000 |        6.99 |

Displaying records 1 - 10

### Exercise 7.2

Modify the above query to only keep the customer_ids that have more then
5 payments

``` sql
SELECT customer_id, MAX(amount), MIN(amount), AVG(amount), SUM(amount)
FROM payment
GROUP BY customer_id
HAVING COUNT(*) > 5;
```

| customer_id | MAX(amount) | MIN(amount) | AVG(amount) | SUM(amount) |
|------------:|------------:|------------:|------------:|------------:|
|          19 |        9.99 |        0.99 |    4.490000 |       26.94 |
|          53 |        9.99 |        0.99 |    4.490000 |       26.94 |
|         109 |        7.99 |        0.99 |    3.990000 |       27.93 |
|         161 |        5.99 |        0.99 |    2.990000 |       17.94 |
|         197 |        3.99 |        0.99 |    2.615000 |       20.92 |
|         207 |        6.99 |        0.99 |    2.990000 |       17.94 |
|         239 |        7.99 |        2.99 |    5.656667 |       33.94 |
|         245 |        8.99 |        0.99 |    4.823333 |       28.94 |
|         251 |        4.99 |        1.99 |    3.323333 |       19.94 |
|         269 |        6.99 |        0.99 |    3.156667 |       18.94 |

Displaying records 1 - 10

## Cleanup

Run the following chunk to disconnect from the connection.

``` r
dbDisconnect(con)
```
