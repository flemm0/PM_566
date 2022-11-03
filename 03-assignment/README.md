Assignment 03: Web Scraping and Text Mining
================
Flemming Wu
2022-11-03

``` r
library(httr)
library(rvest)
library(stringr)
library(xml2)
library(tidyverse)
library(tidytext)
library(forcats)
require(gridExtra)
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
    rvest::html_nodes(xpath = "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]") %>%
    html_text2() %>%
    str_extract("([:digit:]{1,3},)+?[:digit:]{3}")

paste0("I have found ", count1, " papers that show up under the term: sars-cov-2 trial vaccine using rvest")
```

    ## [1] "I have found 4,007 papers that show up under the term: sars-cov-2 trial vaccine using rvest"

``` r
# using xml2
count2 <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine") %>%
    xml2::xml_find_first(xpath = "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]") %>%
    as.character() %>%
    str_extract("([:digit:]{1,3},)+?[:digit:]{3}")

paste0("I have found ", count2, " papers that show up under the term: sars-cov-2 trial vaccine using xml2")
```

    ## [1] "I have found 4,007 papers that show up under the term: sars-cov-2 trial vaccine using xml2"

``` r
# get list of ID numbers
query_ids <- GET(
  url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    term = "sars-cov-2 trial vaccine",
    retmax = 250) 
)

# Extracting the content of the response of GET
ids <- httr::content(query_ids) %>% as.character()

ids <- stringr::str_extract_all(ids, "<Id>[:digit:]*</Id>")[[1]] %>% stringr::str_remove_all("</?Id>")

publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi", # the link now will be using efetch, instead of esearch
  query = list(
    db = "pubmed",
    id = str_c(ids, collapse = ","),
    rettype = "abstract"
    )
)

# Turning the output into character vector
publications <- httr::content(publications) %>% xml2::xml_children()
publications_txt <- as.character(publications)
```

``` r
# article titles
titles <- str_extract(publications_txt, "<ArticleTitle>.*</ArticleTitle>") %>%
    str_remove_all("</?[:alpha:]*>")

# names of journals
journals <- str_extract(publications_txt, "<ISOAbbreviation>.*</ISOAbbreviation>") %>%
    str_remove_all("</?[:alpha:]+>")


# publication dates
dates <- str_remove_all(publications_txt, "\\n") %>%
    str_extract("<PubDate>.*</PubDate>") %>%
    str_squish() %>%
    str_remove_all("</?[:alpha:]+>") %>%
    str_trim(side = "both")


# abstract texts
abstracts <- str_remove_all(publications_txt, "\\n") %>%
    str_extract("<Abstract>.{1,}?</Abstract>") %>%
    str_remove_all("(</?AbstractText Label.{1,}?>|</?Abstract>|</?AbstractText>)") %>%
    str_trim(side = "both")
```

``` r
df <- data.frame(PubMed_Id = ids, title = titles, journal = journals,
    publication_date = dates, abstract = abstracts, check.rows = FALSE)
```

``` r
head(df, n = 3) %>%
    knitr::kable()
```

| PubMed_Id | title                                                                                                                                         | journal             | publication_date | abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|:----------|:----------------------------------------------------------------------------------------------------------------------------------------------|:--------------------|:-----------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 36307830  | Improving pediatric COVID-19 vaccine uptake using an mHealth tool (MoVeUp): study protocol for a randomized, controlled trial.                | Trials              | 2022 Oct 28      | Coronavirus disease 2019 (COVID-19) vaccines demonstrate excellent effectiveness against infection, severe disease, and death. However, pediatric COVID-19 vaccination rates lag among individuals from rural and other medically underserved communities. The research objective of the current protocol is to determine the effectiveness of a vaccine communication mobile health (mHealth) application (app) on parental decisions to vaccinate their children against COVID-19. Custodial parents/caregivers with ≥ 1 child eligible for COVID-19 vaccination who have not yet received the vaccine will be randomized to download one of two mHealth apps. The intervention app will address logistical and motivational barriers to pediatric COVID-19 vaccination. Participants will receive eight weekly push notifications followed by two monthly push notifications (cues to action) regarding vaccinating their child. Through branching logic, users will access customized content based on their locality, degree of rurality-urbanicity, primary language (English/Spanish), race/ethnicity, and child’s age to address COVID-19 vaccine knowledge and confidence gaps. The control app will provide push notifications and information on general pediatric health and infection prevention and mitigation strategies based on recommendations from the American Academy of Pediatrics (AAP) and the Centers for Disease Control and Prevention (CDC). The primary outcome is the proportion of children who complete COVID-19 vaccination series. Secondary outcomes include the proportion of children who receive ≥ 1 dose of COVID-19 vaccine and changes in parent/caregiver scores from baseline to immediately post-intervention on the modified WHO SAGE Vaccine Hesitancy Scale adapted for the COVID-19 vaccine. The COVID-19 pandemic inflicts disproportionate harm on individuals from underserved communities, including those in rural settings. Maximizing vaccine uptake in these communities will decrease infection rates, severe illness, and death. Given that most US families from these communities use smart phones, mHealth interventions hold the promise of broad uptake. Bundling multiple mHealth vaccine uptake interventions into a single app may maximize the impact of deploying such a tool to increase COVID-19 vaccination. The new knowledge to be gained from this study will directly inform future efforts to increase COVID-19 vaccination rates across diverse settings and provide an evidentiary base for app-based vaccine communication tools that can be adapted to future vaccine-deployment efforts. ClinicalTrials.gov NCT05386355 . Registered on May 23, 2022. <CopyrightInformation>© 2022. The Author(s).</CopyrightInformation>                                                                                                                                                                                                           |
| 36305195  | Deep learning in drug discovery: a futuristic modality to materialize the large datasets for cheminformatics.                                 | J Biomol Struct Dyn | 2022 Oct 28      | Artificial intelligence (AI) development imitates the workings of the human brain to comprehend modern problems. The traditional approaches such as high throughput screening (HTS) and combinatorial chemistry are lengthy and expensive to the pharmaceutical industry as they can only handle a smaller dataset. Deep learning (DL) is a sophisticated AI method that uses a thorough comprehension of particular systems. The pharmaceutical industry is now adopting DL techniques to enhance the research and development process. Multi-oriented algorithms play a crucial role in the processing of QSAR analysis, de novo drug design, ADME evaluation, physicochemical analysis, preclinical development, followed by clinical trial data precision. In this study, we investigated the performance of several algorithms, including deep neural networks (DNN), convolutional neural networks (CNN) and multi-task learning (MTL), with the aim of generating high-quality, interpretable big and diverse databases for drug design and development. Studies have demonstrated that CNN, recurrent neural network and deep belief network are compatible, accurate and effective for the molecular description of pharmacodynamic properties. In Covid-19, existing pharmacological compounds has also been repurposed using DL models. In the absence of the Covid-19 vaccine, remdesivir and oseltamivir have been widely employed to treat severe SARS-CoV-2 infections. In conclusion, the results indicate the potential benefits of employing the DL strategies in the drug discovery process.Communicated by Ramaswamy H. Sarma.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 36301821  | Immunogenicity and reactogenicity of SARS-CoV-2 vaccines in people living with HIV in the Netherlands: A nationwide prospective cohort study. | PLoS Med            | 2022 Oct         | Vaccines can be less immunogenic in people living with HIV (PLWH), but for SARS-CoV-2 vaccinations this is unknown. In this study we set out to investigate, for the vaccines currently approved in the Netherlands, the immunogenicity and reactogenicity of SARS-CoV-2 vaccinations in PLWH. We conducted a prospective cohort study to examine the immunogenicity of BNT162b2, mRNA-1273, ChAdOx1-S, and Ad26.COV2.S vaccines in adult PLWH without prior COVID-19, and compared to HIV-negative controls. The primary endpoint was the anti-spike SARS-CoV-2 IgG response after mRNA vaccination. Secondary endpoints included the serological response after vector vaccination, anti-SARS-CoV-2 T-cell response, and reactogenicity. Between 14 February and 7 September 2021, 1,154 PLWH (median age 53 \[IQR 44-60\] years, 85.5% male) and 440 controls (median age 43 \[IQR 33-53\] years, 28.6% male) were included in the final analysis. Of the PLWH, 884 received BNT162b2, 100 received mRNA-1273, 150 received ChAdOx1-S, and 20 received Ad26.COV2.S. In the group of PLWH, 99% were on antiretroviral therapy, 97.7% were virally suppressed, and the median CD4+ T-cell count was 710 cells/μL (IQR 520-913). Of the controls, 247 received mRNA-1273, 94 received BNT162b2, 26 received ChAdOx1-S, and 73 received Ad26.COV2.S. After mRNA vaccination, geometric mean antibody concentration was 1,418 BAU/mL in PLWH (95% CI 1322-1523), and after adjustment for age, sex, and vaccine type, HIV status remained associated with a decreased response (0.607, 95% CI 0.508-0.725, p \< 0.001). All controls receiving an mRNA vaccine had an adequate response, defined as \>300 BAU/mL, whilst in PLWH this response rate was 93.6%. In PLWH vaccinated with mRNA-based vaccines, higher antibody responses were predicted by CD4+ T-cell count 250-500 cells/μL (2.845, 95% CI 1.876-4.314, p \< 0.001) or \>500 cells/μL (2.936, 95% CI 1.961-4.394, p \< 0.001), whilst a viral load \> 50 copies/mL was associated with a reduced response (0.454, 95% CI 0.286-0.720, p = 0.001). Increased IFN-γ, CD4+ T-cell, and CD8+ T-cell responses were observed after stimulation with SARS-CoV-2 spike peptides in ELISpot and activation-induced marker assays, comparable to controls. Reactogenicity was generally mild, without vaccine-related serious adverse events. Due to the control of vaccine provision by the Dutch National Institute for Public Health and the Environment, there were some differences between vaccine groups in the age, sex, and CD4+ T-cell counts of recipients. After vaccination with BNT162b2 or mRNA-1273, anti-spike SARS-CoV-2 antibody levels were reduced in PLWH compared to HIV-negative controls. To reach and maintain the same serological responses as HIV-negative controls, additional vaccinations are probably required. The trial was registered in the Netherlands Trial Register (NL9214). <https://www.trialregister.nl/trial/9214>. |

### Text Mining

A new dataset has been added to the data science data repository
<https://github.com/USCbiostats/data-science-data/tree/master/03_pubmed>.
The dataset contains 3241 abstracts from articles across 5 search terms.
Your job is to analyse these abstracts to find interesting insights.

1.  Tokenize the abstracts and count the number of each token. Do you
    see anything interesting? Does removing stop words change what
    tokens appear as the most frequent? What are the 5 most common
    tokens for each search term after removing stopwords?

``` r
pm <- read_csv("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/03_pubmed/pubmed.csv",
    col_types = c("c", "c"))
```

``` r
pm %>%
    unnest_tokens(token, abstract) %>%
    count(token, sort = TRUE) %>%
    top_n(n = 20) %>%
    ggplot(aes(x = n, y = fct_reorder(token, n))) + geom_col() +
    labs(y = "Token", title = "Top 20 Most Frequent Words in PubMed Data Set")
```

![](README_files/figure-gfm/tokenize%20abstracts-1.png)<!-- -->

There is nothing particularly interesting about the most frequent words
in the abstract texts, as most of them contain stop words. Some of the
non-stop words include “patients”, “cancer”, and “covid”.

``` r
pm %>%
    unnest_tokens(token, abstract) %>%
    anti_join(stop_words, by = c(token = "word")) %>%
    filter(!grepl(pattern = "^[0-9]+$", x = token)) %>%
    count(token, sort = TRUE) %>%
    top_n(n = 20) %>%
    ggplot(aes(x = n, y = fct_reorder(token, n))) + geom_col() +
    labs(y = "Token", title = "Top 20 Most Frequent Words in PubMed Data Set (Stop Words and Numbers Removed)") +
    theme(plot.title = element_text(size = 10))
```

![](README_files/figure-gfm/removing%20stop%20words%20and%20numbers-1.png)<!-- -->

After removing stop words and digits, it is evident that the text
contained in the data set are about health related topics, with the most
frequent words including: “covid”, “patients”, “disease”, and
“eclampsia”.

``` r
top_5 <- pm %>%
    unnest_tokens(token, abstract) %>%
    anti_join(stop_words, by = c(token = "word")) %>%
    filter(!grepl(pattern = "^[0-9]+$", x = token)) %>%
    group_by(term) %>%
    count(token, sort = TRUE) %>%
    top_n(n, n = 5) %>%
    arrange(term)

p1 <- ggplot(top_5 %>%
    filter(term == "covid"), aes(n, fct_reorder(token, n))) +
    geom_col() + labs(y = "token", title = "Term: covid")

p2 <- ggplot(top_5 %>%
    filter(term == "cystic fibrosis"), aes(n, fct_reorder(token,
    n))) + geom_col() + labs(y = "token", title = "Term: cystic fibrosis")

p3 <- ggplot(top_5 %>%
    filter(term == "meningitis"), aes(n, fct_reorder(token, n))) +
    geom_col() + labs(y = "token", title = "Term: meningitis")

p4 <- ggplot(top_5 %>%
    filter(term == "preeclampsia"), aes(n, fct_reorder(token,
    n))) + geom_col() + labs(y = "token", title = "Term: preeclampsia")

p5 <- ggplot(top_5 %>%
    filter(term == "prostate cancer"), aes(n, fct_reorder(token,
    n))) + geom_col() + labs(y = "token", title = "Term: prostate cancer")

grid.arrange(p1, p2, p3, p4, p5, ncol = 2)
```

<img src="README_files/figure-gfm/top 5 tokens for each search term after removing stop words-1.png" style="display: block; margin: auto;" />

The top 5 words for each of the search terms contain the search terms
themselves, but other than that the word “disease” appears frequently
under the terms prostate cancer, cystic fibrosis, and covid. The word
“patient” appears frequently under the terms prostate cancer,
meningitis, cystic fibrosis, and covid.

``` r
top_5 %>%
    knitr::kable()
```

| term            | token        |    n |
|:----------------|:-------------|-----:|
| covid           | covid        | 7275 |
| covid           | patients     | 2293 |
| covid           | disease      |  943 |
| covid           | pandemic     |  800 |
| covid           | coronavirus  |  647 |
| covid           | health       |  647 |
| cystic fibrosis | fibrosis     |  867 |
| cystic fibrosis | cystic       |  862 |
| cystic fibrosis | cf           |  625 |
| cystic fibrosis | patients     |  586 |
| cystic fibrosis | disease      |  400 |
| meningitis      | patients     |  446 |
| meningitis      | meningitis   |  429 |
| meningitis      | meningeal    |  219 |
| meningitis      | csf          |  206 |
| meningitis      | clinical     |  187 |
| preeclampsia    | pre          | 2038 |
| preeclampsia    | eclampsia    | 2005 |
| preeclampsia    | preeclampsia | 1863 |
| preeclampsia    | women        | 1196 |
| preeclampsia    | pregnancy    |  969 |
| prostate cancer | cancer       | 3840 |
| prostate cancer | prostate     | 3832 |
| prostate cancer | patients     |  934 |
| prostate cancer | treatment    |  926 |
| prostate cancer | disease      |  652 |

2.  Tokenize the abstracts into bigrams. Find the 10 most common bigram
    and visualize them with ggplot2.

``` r
pm %>%
    unnest_ngrams(bigram, abstract, n = 2) %>%
    count(bigram, sort = TRUE) %>%
    top_n(10, n) %>%
    ggplot(aes(x = n, y = fct_reorder(bigram, n))) + geom_col() +
    labs(y = "bigram", title = "Top 10 Most Common Bigrams in PubMed Data Set") +
    theme_classic()
```

![](README_files/figure-gfm/top%2010%20bigrams-1.png)<!-- -->

All but two of the bigrams in the top 10 contain stop words, making the
graph above not very useful in describing the text in the data set. I
will remove them along with numbers and visualize again.

``` r
pm %>%
    unnest_ngrams(bigram, abstract, n = 2) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    anti_join(stop_words, by = c(word1 = "word")) %>%
    anti_join(stop_words, by = c(word2 = "word")) %>%
    filter(!grepl(pattern = "^[0-9]+$", x = word1)) %>%
    filter(!grepl(pattern = "^[0-9]+$", x = word2)) %>%
    unite(bigram, c("word1", "word2"), sep = " ") %>%
    count(bigram, sort = TRUE) %>%
    top_n(10, n) %>%
    ggplot(aes(x = n, y = fct_reorder(bigram, n))) + geom_col() +
    labs(y = "bigram", title = "Top 10 Most Common Bigrams in PubMed Data Set (Stop Words Removed)") +
    theme_classic() + theme(plot.title = element_text(size = 10))
```

![](README_files/figure-gfm/top%2010%20bigrams%20after%20removing%20stop%20words-1.png)<!-- -->

Now the graph better describes the text data. Above, we can see
health-related bigrams such as “prostate cancer” and “pre eclampsia” are
common terms in the data set.

3.  Calculate the TF-IDF value for each word-search term combination.
    (here you want the search term to be the “document”) What are the 5
    tokens from each search term with the highest TF-IDF value? How are
    the results different from the answers you got in question 1?

``` r
tf_idf_top_5 <- pm %>%
    unnest_tokens(token, abstract) %>%
    anti_join(stop_words, by = c(token = "word")) %>%
    filter(!grepl(pattern = "^[0-9]+$", x = token)) %>%
    count(token, term, sort = TRUE) %>%
    bind_tf_idf(token, term, n) %>%
    group_by(term) %>%
    top_n(5, tf_idf) %>%
    arrange(term, -tf_idf)

p1 <- ggplot(tf_idf_top_5 %>%
    filter(term == "covid"), aes(tf_idf, fct_reorder(token, tf_idf))) +
    geom_col() + labs(y = "token", title = "Term: covid")

p2 <- ggplot(tf_idf_top_5 %>%
    filter(term == "cystic fibrosis"), aes(tf_idf, fct_reorder(token,
    tf_idf))) + geom_col() + labs(y = "token", title = "Term: cystic fibrosis")

p3 <- ggplot(tf_idf_top_5 %>%
    filter(term == "meningitis"), aes(tf_idf, fct_reorder(token,
    tf_idf))) + geom_col() + labs(y = "token", title = "Term: meningitis")

p4 <- ggplot(tf_idf_top_5 %>%
    filter(term == "preeclampsia"), aes(tf_idf, fct_reorder(token,
    tf_idf))) + geom_col() + labs(y = "token", title = "Term: preeclampsia")

p5 <- ggplot(tf_idf_top_5 %>%
    filter(term == "prostate cancer"), aes(tf_idf, fct_reorder(token,
    tf_idf))) + geom_col() + labs(y = "token", title = "Term: prostate cancer")

grid.arrange(p1, p2, p3, p4, p5, ncol = 2)
```

<img src="README_files/figure-gfm/calculate tf-idf frequency for each word-search term combination-1.png" style="display: block; margin: auto;" />

We can see that by giving weight to words that occur more frequently in
one group than others, the words “patient” and “disease”, which are
associated with all five of the terms, have been replaced in the top 5
most frequent words in each group by words more specific to the term.

``` r
tf_idf_top_5 %>%
    knitr::kable()
```

| token           | term            |    n |        tf |       idf |    tf_idf |
|:----------------|:----------------|-----:|----------:|----------:|----------:|
| covid           | covid           | 7275 | 0.0721733 | 1.6094379 | 0.1161585 |
| pandemic        | covid           |  800 | 0.0079366 | 1.6094379 | 0.0127734 |
| coronavirus     | covid           |  647 | 0.0064187 | 1.6094379 | 0.0103305 |
| sars            | covid           |  372 | 0.0036905 | 1.6094379 | 0.0059397 |
| cov             | covid           |  334 | 0.0033135 | 1.6094379 | 0.0053329 |
| cf              | cystic fibrosis |  625 | 0.0242089 | 0.9162907 | 0.0221823 |
| fibrosis        | cystic fibrosis |  867 | 0.0335825 | 0.5108256 | 0.0171548 |
| cystic          | cystic fibrosis |  862 | 0.0333889 | 0.5108256 | 0.0170559 |
| cftr            | cystic fibrosis |   86 | 0.0033311 | 1.6094379 | 0.0053613 |
| sweat           | cystic fibrosis |   83 | 0.0032149 | 1.6094379 | 0.0051742 |
| meningitis      | meningitis      |  429 | 0.0172462 | 1.6094379 | 0.0277567 |
| meningeal       | meningitis      |  219 | 0.0088040 | 1.6094379 | 0.0141695 |
| pachymeningitis | meningitis      |  149 | 0.0059899 | 1.6094379 | 0.0096405 |
| csf             | meningitis      |  206 | 0.0082814 | 0.9162907 | 0.0075882 |
| meninges        | meningitis      |  106 | 0.0042613 | 1.6094379 | 0.0068583 |
| eclampsia       | preeclampsia    | 2005 | 0.0266803 | 1.6094379 | 0.0429403 |
| preeclampsia    | preeclampsia    | 1863 | 0.0247907 | 1.6094379 | 0.0398992 |
| pregnancy       | preeclampsia    |  969 | 0.0128944 | 0.5108256 | 0.0065868 |
| maternal        | preeclampsia    |  797 | 0.0106056 | 0.5108256 | 0.0054176 |
| gestational     | preeclampsia    |  191 | 0.0025416 | 1.6094379 | 0.0040906 |
| prostate        | prostate cancer | 3832 | 0.0576943 | 1.6094379 | 0.0928554 |
| androgen        | prostate cancer |  305 | 0.0045921 | 1.6094379 | 0.0073906 |
| psa             | prostate cancer |  282 | 0.0042458 | 1.6094379 | 0.0068333 |
| prostatectomy   | prostate cancer |  215 | 0.0032370 | 1.6094379 | 0.0052098 |
| castration      | prostate cancer |  148 | 0.0022283 | 1.6094379 | 0.0035863 |
