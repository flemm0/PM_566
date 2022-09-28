06-Lab Text Mining
================
Flemming Wu
2022-09-28

# Learning goals

-   Use `unnest_tokens()` and `unnest_ngrams()` to extract tokens and
    ngrams from text.
-   Use dplyr and ggplot2 to analyze text data

# Lab description

For this lab we will be working with a new dataset. The dataset contains
transcription samples from <https://www.mtsamples.com/>. And is loaded
and “fairly” cleaned at
<https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv>.

This markdown document should be rendered using `github_document`
document.

### Setup packages

You should load in `dplyr`, (or `data.table` if you want to work that
way), `ggplot2` and `tidytext`.

``` r
library(tidyverse)
library(tidytext)
```

### read in Medical Transcriptions

Loading in reference transcription samples from
<https://www.mtsamples.com/>

``` r
if(!file.exists("mtsamples.csv")){
  download.file(url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv",
                destfile = "mtsamples.csv",
                method = "libcurl",
                timeout = 60)
}

mtsamples <- read.csv("mtsamples.csv")

#str(mtsamples) 

#change to tibble format
mtsamples <- as.tibble(mtsamples)

str(mtsamples)
```

    ## tibble [4,999 × 6] (S3: tbl_df/tbl/data.frame)
    ##  $ X                : int [1:4999] 0 1 2 3 4 5 6 7 8 9 ...
    ##  $ description      : chr [1:4999] " A 23-year-old white female presents with complaint of allergies." " Consult for laparoscopic gastric bypass." " Consult for laparoscopic gastric bypass." " 2-D M-Mode. Doppler.  " ...
    ##  $ medical_specialty: chr [1:4999] " Allergy / Immunology" " Bariatrics" " Bariatrics" " Cardiovascular / Pulmonary" ...
    ##  $ sample_name      : chr [1:4999] " Allergic Rhinitis " " Laparoscopic Gastric Bypass Consult - 2 " " Laparoscopic Gastric Bypass Consult - 1 " " 2-D Echocardiogram - 1 " ...
    ##  $ transcription    : chr [1:4999] "SUBJECTIVE:,  This 23-year-old white female presents with complaint of allergies.  She used to have allergies w"| __truncated__ "PAST MEDICAL HISTORY:, He has difficulty climbing stairs, difficulty with airline seats, tying shoes, used to p"| __truncated__ "HISTORY OF PRESENT ILLNESS: , I have seen ABC today.  He is a very pleasant gentleman who is 42 years old, 344 "| __truncated__ "2-D M-MODE: , ,1.  Left atrial enlargement with left atrial diameter of 4.7 cm.,2.  Normal size right and left "| __truncated__ ...
    ##  $ keywords         : chr [1:4999] "allergy / immunology, allergic rhinitis, allergies, asthma, nasal sprays, rhinitis, nasal, erythematous, allegr"| __truncated__ "bariatrics, laparoscopic gastric bypass, weight loss programs, gastric bypass, atkin's diet, weight watcher's, "| __truncated__ "bariatrics, laparoscopic gastric bypass, heart attacks, body weight, pulmonary embolism, potential complication"| __truncated__ "cardiovascular / pulmonary, 2-d m-mode, doppler, aortic valve, atrial enlargement, diastolic function, ejection"| __truncated__ ...

------------------------------------------------------------------------

## Question 1: What specialties do we have?

We can use `count()` from `dplyr` to figure out how many different
categories do we have? Are these catagories related? overlapping? evenly
distributed?

``` r
mtsamples %>%
  count(medical_specialty, sort = TRUE)
```

    ## # A tibble: 40 × 2
    ##    medical_specialty                    n
    ##    <chr>                            <int>
    ##  1 " Surgery"                        1103
    ##  2 " Consult - History and Phy."      516
    ##  3 " Cardiovascular / Pulmonary"      372
    ##  4 " Orthopedic"                      355
    ##  5 " Radiology"                       273
    ##  6 " General Medicine"                259
    ##  7 " Gastroenterology"                230
    ##  8 " Neurology"                       223
    ##  9 " SOAP / Chart / Progress Notes"   166
    ## 10 " Obstetrics / Gynecology"         160
    ## # … with 30 more rows

``` r
mtsamples %>%
  count(medical_specialty) %>%
  unique()
```

    ## # A tibble: 40 × 2
    ##    medical_specialty                 n
    ##    <chr>                         <int>
    ##  1 " Allergy / Immunology"           7
    ##  2 " Autopsy"                        8
    ##  3 " Bariatrics"                    18
    ##  4 " Cardiovascular / Pulmonary"   372
    ##  5 " Chiropractic"                  14
    ##  6 " Consult - History and Phy."   516
    ##  7 " Cosmetic / Plastic Surgery"    27
    ##  8 " Dentistry"                     27
    ##  9 " Dermatology"                   29
    ## 10 " Diets and Nutritions"          10
    ## # … with 30 more rows

------------------------------------------------------------------------

## Question 2

-   Tokenize the the words in the `transcription` column
-   Count the number of times each token appears
-   Visualize the top 20 most frequent words

Explain what we see from this result. Does it makes sense? What insights
(if any) do we get?

``` r
mtsamples %>%
  unnest_tokens(token, transcription)
```

    ## # A tibble: 2,403,563 × 6
    ##        X description                               medic…¹ sampl…² keywo…³ token
    ##    <int> <chr>                                     <chr>   <chr>   <chr>   <chr>
    ##  1     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… subj…
    ##  2     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… this 
    ##  3     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… 23   
    ##  4     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… year 
    ##  5     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… old  
    ##  6     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… white
    ##  7     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… fema…
    ##  8     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… pres…
    ##  9     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… with 
    ## 10     0 " A 23-year-old white female presents wi… " Alle… " Alle… allerg… comp…
    ## # … with 2,403,553 more rows, and abbreviated variable names
    ## #   ¹​medical_specialty, ²​sample_name, ³​keywords

------------------------------------------------------------------------

## Question 3

-   Redo visualization but remove stopwords before
-   Bonus points if you remove numbers as well

What do we see know that we have removed stop words? Does it give us a
better idea of what the text is about?

------------------------------------------------------------------------

# Question 4

repeat question 2, but this time tokenize into bi-grams. how does the
result change if you look at tri-grams?

------------------------------------------------------------------------

# Question 5

Using the results you got from questions 4. Pick a word and count the
words that appears after and before it.

------------------------------------------------------------------------

# Question 6

Which words are most used in each of the specialties. you can use
`group_by()` and `top_n()` from `dplyr` to have the calculations be done
within each specialty. Remember to remove stopwords. How about the most
5 used words?

# Question 7 - extra

Find your own insight in the data:

Ideas:

-   Interesting ngrams
-   See if certain words are used more in some specialties then others
