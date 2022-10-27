Assignment 03: Web Scraping and Text Mining
================
Flemming Wu
2022-10-27

``` r
library(httr)
library(rvest)
library(stringr)
library(xml2)
library(tidyverse)
```

### APIs

-   Using the NCBI API, look for papers that show up under the term
    “sars-cov-2 trial vaccine.” Look for the data in the pubmed
    database, and then retrieve the details of the paper as shown in
    lab 7. How many papers were you able to find?

-   Using the list of pubmed ids you retrieved, download each papers’
    details using the query parameter rettype = abstract. If you get
    more than 250 ids, just keep the first 250.

-   As we did in lab 7. Create a dataset containing the following:

    1.  Pubmed ID number,

    2.  Title of the paper,

    3.  Name of the journal where it was published,

    4.  Publication date, and

    5.  Abstract of the paper (if any).

``` r
# using rvest
count1 <- rvest::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine") %>%
  html_node(xpath = "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]") %>%
  as.character() %>%
  str_extract_all("([:digit:]{1,3},)+?[:digit:]{3}") 

paste0("I have found ", count1[[1]], " papers that show up under the term: sars-cov-2 trial vaccine using rvest")
```

    ## [1] "I have found 3,985 papers that show up under the term: sars-cov-2 trial vaccine using rvest"

``` r
# using xml2
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine")

count2 <- xml2::xml_find_first(website, xpath = "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]") %>%
  as.character() %>%
  str_extract("([:digit:]{1,3},)+?[:digit:]{3}")


paste0("I have found ", count2, " papers that show up under the term: sars-cov-2 trial vaccine using xml2")
```

    ## [1] "I have found 3,985 papers that show up under the term: sars-cov-2 trial vaccine using xml2"

``` r
query_ids <- GET(
  url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    term = "sars-cov-2 trial vaccine",
    retmax = 250)
)

# Extracting the content of the response of GET
ids <- httr::content(query_ids) %>% as.character()

ids <- stringr::str_extract_all(ids, "<Id>[:digit:]*</Id>")[[1]]

ids <- stringr::str_remove_all(ids, "</?Id>")

publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
  query = list(
    db = "pubmed",
    id = str_c(ids, collapse = ","),
    retmax = 250,
    rettype = "abstract"
    )
)

# Turning the output into character vector
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```

``` r
titles <- str_extract_all(publications_txt, "<ArticleTitle>.*</ArticleTitle>")[[1]] %>% str_remove_all("</?[:alpha:]*>")

journals <- str_extract_all(publications_txt, "<Title>.*</Title>")[[1]] %>% str_remove_all("</?[:alpha:]*>")

date <- str_extract_all(publications_txt, "<PubDate>.*</PubDate>")[[1]] %>% str_remove_all("</?[:alpha:]*>")

abstracts <- str_extract_all(publications_txt, "<AbstractText.*>.*</AbstractText.*>")[[1]] %>% str_remove_all("</?[:alpha:]*>")
```

### Text Mining

A new dataset has been added to the data science data repository
(<https://github.com/USCbiostats/data-science-data/tree/master/03_pubmed>)\[<https://github.com/USCbiostats/data-science-data/tree/master/03_pubmed>\].
The dataset contains 3241 abstracts from articles across 5 search terms.
Your job is to analyse these abstracts to find interesting insights.

1.  Tokenize the abstracts and count the number of each token. Do you
    see anything interesting? Does removing stop words change what
    tokens appear as the most frequent? What are the 5 most common
    tokens for each search term after removing stopwords?

2.  Tokenize the abstracts into bigrams. Find the 10 most common bigram
    and visualize them with ggplot2. Calculate the TF-IDF value for each
    word-search term combination. (here you want the search term to be
    the “document”)

3.  What are the 5 tokens from each search term with the highest TF-IDF
    value? How are the results different from the answers you got in
    question 1?
