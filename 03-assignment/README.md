Assignment 03: Web Scraping and Text Mining
================
Flemming Wu
2022-10-29

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
    html_node(xpath = "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]") %>%
    as.character() %>%
    str_extract_all("([:digit:]{1,3},)+?[:digit:]{3}")

paste0("I have found ", count1[[1]], " papers that show up under the term: sars-cov-2 trial vaccine using rvest")
```

    ## [1] "I have found 3,993 papers that show up under the term: sars-cov-2 trial vaccine using rvest"

``` r
# using xml2
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine")

count2 <- xml2::xml_find_first(website, xpath = "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]") %>%
    as.character() %>%
    str_extract("([:digit:]{1,3},)+?[:digit:]{3}")


paste0("I have found ", count2, " papers that show up under the term: sars-cov-2 trial vaccine using xml2")
```

    ## [1] "I have found 3,993 papers that show up under the term: sars-cov-2 trial vaccine using xml2"

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
| 36305195  | Deep learning in drug discovery: a futuristic modality to materialize the large datasets for cheminformatics.                                 | J Biomol Struct Dyn | 2022 Oct 28      | Artificial intelligence (AI) development imitates the workings of the human brain to comprehend modern problems. The traditional approaches such as high throughput screening (HTS) and combinatorial chemistry are lengthy and expensive to the pharmaceutical industry as they can only handle a smaller dataset. Deep learning (DL) is a sophisticated AI method that uses a thorough comprehension of particular systems. The pharmaceutical industry is now adopting DL techniques to enhance the research and development process. Multi-oriented algorithms play a crucial role in the processing of QSAR analysis, de novo drug design, ADME evaluation, physicochemical analysis, preclinical development, followed by clinical trial data precision. In this study, we investigated the performance of several algorithms, including deep neural networks (DNN), convolutional neural networks (CNN) and multi-task learning (MTL), with the aim of generating high-quality, interpretable big and diverse databases for drug design and development. Studies have demonstrated that CNN, recurrent neural network and deep belief network are compatible, accurate and effective for the molecular description of pharmacodynamic properties. In Covid-19, existing pharmacological compounds has also been repurposed using DL models. In the absence of the Covid-19 vaccine, remdesivir and oseltamivir have been widely employed to treat severe SARS-CoV-2 infections. In conclusion, the results indicate the potential benefits of employing the DL strategies in the drug discovery process. Communicated by Ramaswamy H. Sarma.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 36303764  | Predicted B Cell Epitopes Highlight the Potential for COVID-19 to Drive Self-Reactive Immunity.                                               | Front Bioinform     | 2021             | COVID-19, caused by the Severe Acute Respiratory Syndrome Coronavirus 2 (SARS-CoV-2), whilst commonly characterised as a respiratory disease, is reported to have extrapulmonary manifestations in multiple organs. Extrapulmonary involvement in COVID-19 includes autoimmune-like diseases such as Guillain-Barré syndrome and Kawasaki disease, as well as the presence of various autoantibodies including those associated with autoimmune diseases such a systemic lupus erythematosus (e.g. ANA, anti-La). Multiple strains of SARS-CoV-2 have emerged globally, some of which are found to be associated with increased transmissibility and severe disease. We performed an unbiased comprehensive mapping of the potential for cross-reactivity with self-antigens across multiple SARS-CoV-2 proteins and compared identified immunogenic regions across multiples strains. Using the Immune Epitope Database (IEDB) B cell epitope prediction tool, regions predicted as antibody epitopes with high prediction scores were selected. Epitope sequences were then blasted to eight other global strains to identify mutations within these regions. Of the 15 sequences compared, eight had a mutation in at least one other global strain. Predicted epitopes were then compared to human proteins using the NCBI blast tool. In contrast to studies focusing on short sequences of peptide identity, we have taken an immunological approach to selection criteria for further analysis and have identified 136 alignments of 6-23 amino acids (aa) in 129 human proteins that are immunologically likely to be cross-reactive with SARS-CoV-2. Additionally, to identify regions with significant potential to interfere with host cell function-or promote immunopathology, we identified epitope regions more likely to be accessible to pathogenic autoantibodies in the host, selected using a novel combination of sequence similarity, and modelling protein and alignment localization with a focus on extracellular regions. Our analysis identified 11 new predicted B-cell epitopes in host proteins, potentially capable of explaining key aspects of COVID-19 extrapulmonary pathology, and which were missed in other <i>in silico</i> studies which used direct identity rather than immunologically related functional criteria. <CopyrightInformation>Copyright © 2021 Moody, Wilson, Boer, Holien, Flanagan, Jaworowski and Plebanski.</CopyrightInformation>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 36301821  | Immunogenicity and reactogenicity of SARS-CoV-2 vaccines in people living with HIV in the Netherlands: A nationwide prospective cohort study. | PLoS Med            | 2022 Oct         | Vaccines can be less immunogenic in people living with HIV (PLWH), but for SARS-CoV-2 vaccinations this is unknown. In this study we set out to investigate, for the vaccines currently approved in the Netherlands, the immunogenicity and reactogenicity of SARS-CoV-2 vaccinations in PLWH. We conducted a prospective cohort study to examine the immunogenicity of BNT162b2, mRNA-1273, ChAdOx1-S, and Ad26.COV2.S vaccines in adult PLWH without prior COVID-19, and compared to HIV-negative controls. The primary endpoint was the anti-spike SARS-CoV-2 IgG response after mRNA vaccination. Secondary endpoints included the serological response after vector vaccination, anti-SARS-CoV-2 T-cell response, and reactogenicity. Between 14 February and 7 September 2021, 1,154 PLWH (median age 53 \[IQR 44-60\] years, 85.5% male) and 440 controls (median age 43 \[IQR 33-53\] years, 28.6% male) were included in the final analysis. Of the PLWH, 884 received BNT162b2, 100 received mRNA-1273, 150 received ChAdOx1-S, and 20 received Ad26.COV2.S. In the group of PLWH, 99% were on antiretroviral therapy, 97.7% were virally suppressed, and the median CD4+ T-cell count was 710 cells/μL (IQR 520-913). Of the controls, 247 received mRNA-1273, 94 received BNT162b2, 26 received ChAdOx1-S, and 73 received Ad26.COV2.S. After mRNA vaccination, geometric mean antibody concentration was 1,418 BAU/mL in PLWH (95% CI 1322-1523), and after adjustment for age, sex, and vaccine type, HIV status remained associated with a decreased response (0.607, 95% CI 0.508-0.725, p \< 0.001). All controls receiving an mRNA vaccine had an adequate response, defined as \>300 BAU/mL, whilst in PLWH this response rate was 93.6%. In PLWH vaccinated with mRNA-based vaccines, higher antibody responses were predicted by CD4+ T-cell count 250-500 cells/μL (2.845, 95% CI 1.876-4.314, p \< 0.001) or \>500 cells/μL (2.936, 95% CI 1.961-4.394, p \< 0.001), whilst a viral load \> 50 copies/mL was associated with a reduced response (0.454, 95% CI 0.286-0.720, p = 0.001). Increased IFN-γ, CD4+ T-cell, and CD8+ T-cell responses were observed after stimulation with SARS-CoV-2 spike peptides in ELISpot and activation-induced marker assays, comparable to controls. Reactogenicity was generally mild, without vaccine-related serious adverse events. Due to the control of vaccine provision by the Dutch National Institute for Public Health and the Environment, there were some differences between vaccine groups in the age, sex, and CD4+ T-cell counts of recipients. After vaccination with BNT162b2 or mRNA-1273, anti-spike SARS-CoV-2 antibody levels were reduced in PLWH compared to HIV-negative controls. To reach and maintain the same serological responses as HIV-negative controls, additional vaccinations are probably required. The trial was registered in the Netherlands Trial Register (NL9214). <https://www.trialregister.nl/trial/9214>. |

### Text Mining

A new dataset has been added to the data science data repository
(<https://github.com/USCbiostats/data-science-data/tree/master/03_pubmed>)\[<https://github.com/USCbiostats/data-science-data/tree/master/03_pubmed>\].
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
    top_n(n = 30) %>%
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
    top_n(5) %>%
    arrange(term)
```

    ## Selecting by n

``` r
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

![](README_files/figure-gfm/top%2010%20bigrams%20by%20tf-idf-1.png)<!-- -->

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
