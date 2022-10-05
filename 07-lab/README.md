07-Lab Web scraping and Regular Expressions
================
Flemming Wu
2022-10-05

``` r
library(rvest)
```

# Learning goals:

-   Use a real world API to make queries and process the data
-   Use regular expressions to parse the information
-   Practice your GitHub skills

# Lab description

In this lab, we will be working with the NCBI API to make queries and
extract information using XML and regular expressions. For this lab, we
will be using the httr, xml2, and stringr R packages.

This markdown document should be rendered using github_document
document.

## Question 1: How many Sars-CoV-2 papers?

Build an automatic counter of sars-cov-2 papers using PubMed. You will
need to apply XPath as we did during the lecture to extract the number
of results returned by PubMed in the following web address:
`https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2`

Complete the lines of code:

``` r
# Downloading the website

website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

# Finding the counts
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/section[1]/div[2]/div[1]")

counts <- as.character(counts)

counts
```

    ## [1] "<div class=\"results-amount\">\n  \n    <span class=\"value\">179,009</span>\n    results\n  \n</div>"

``` r
stringr::str_extract(counts, "([0-9]{3},)+[0-9]{3}")
```

    ## [1] "179,009"

## Question 2: Academic publications on COVID-19 and Hawaii

You need to query the following The parameters passed to the query are
documented here.

Use the function `httr::GET()` to make the following query:

1.  Baseline URL:
    <https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi>

2.  Query parameters:

-   db: pubmed
-   term: covid19 hawaii
-   retmax: 1000

## Question 3: Get details about the articles

The Ids are wrapped around text in the following way: <Id>… id number
…</Id>. we can use a regular expression that extract that information.
Fill out the following lines of code:

With the ids in hand, we can now try to get the abstracts of the papers.
As before, we will need to coerce the contents (results) to a list
using:

1.  Baseline url:
    <https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi>

2.  Query parameters:

-   db: pubmed
-   id: A character with all the ids separated by comma, e.g.,
    “1232131,546464,13131”
-   retmax: 1000
-   rettype: abstract

Pro-tip: If you want GET() to take some element literal, wrap it around
I() (as you would do in a formula in R). For example, the text “123,456”
is replaced with “123%2C456”. If you don’t want that behavior, you would
need to do the following I(“123,456”).

With this in hand, we can now analyze the data. This is also a good time
for committing and pushing your work!

## Question 4: Distribution of universities, schools, and departments

Using the function `stringr::str_extract_all()` applied on
publications_txt, capture all the terms of the form:

1.  University of …
2.  … Institute of …

Write a regular expression that captures all such instances

Repeat the exercise and this time focus on schools and departments in
the form of

1.  School of…
2.  Department of…

And tabulate the results

## Question 5: Form a database

We want to build a dataset which includes the title and the abstract of
the paper. The title of all records is enclosed by the HTML tag
ArticleTitle, and the abstract by Abstract.

Before applying the functions to extract text directly, it will help to
process the XML a bit. We will use the xml2::xml_children() function to
keep one element per id. This way, if a paper is missing the abstract,
or something else, we will be able to properly match PUBMED IDS with
their corresponding records.

Now, extract the abstract and article title for each one of the elements
of pub_char_list. You can either use sapply() as we just did, or simply
take advantage of vectorization of stringr::str_extract

Finally, put everything together into a single data.frame and use
knitr::kable() to print the results

Knit the document, commit, and push.
