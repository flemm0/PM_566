07-Lab Web scraping and Regular Expressions
================
Flemming Wu
2022-10-05

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

``` r
library(rvest)
library(stringr)
```

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

#counts

stringr::str_extract(counts, "[0-9,]+")
```

    ## [1] "179,021"

## Question 2: Academic publications on COVID-19 and Hawaii

You need to query the following The parameters passed to the query are
documented here.

Use the function `httr::GET()` to make the following query:

1.  Baseline URL:
    `https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi`

2.  Query parameters:

-   db: pubmed
-   term: covid19 hawaii
-   retmax: 1000

``` r
library(httr)
query_ids <- GET(
  url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    term = "covid19 hawaii",
    retmax = 1000)
)

# Extracting the content of the response of GET
ids <- httr::content(query_ids)
```

## Question 3: Get details about the articles

The Ids are wrapped around text in the following way: <Id>… id number
…</Id>. we can use a regular expression that extract that information.
Fill out the following lines of code:

``` r
# Turn the result into a character vector
ids <- as.character(ids)

# Find all the ids 
ids <- stringr::str_extract_all(ids, "<Id>[0-9]*</Id>")[[1]]

# Remove all the leading and trailing <Id> </Id>. Make use of "|"
ids <- stringr::str_remove_all(ids, "<Id>|</Id>")
```

With the ids in hand, we can now try to get the abstracts of the papers.
As before, we will need to coerce the contents (results) to a list
using:

1.  Baseline url:
    `https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi`

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

``` r
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
  query = list(
    db = "pubmed",
    id = str_c(ids, collapse = ","),
    retmax = 1000,
    rettype = "abstract"
    )
)

# Turning the output into character vector
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```

With this in hand, we can now analyze the data. This is also a good time
for committing and pushing your work!

## Question 4: Distribution of universities, schools, and departments

Using the function `stringr::str_extract_all()` applied on
publications_txt, capture all the terms of the form:

1.  University of …
2.  … Institute of …

Write a regular expression that captures all such instances

``` r
institution <- str_extract_all(
  publications_txt, 
  "University of ([A-Z][a-zA-Z]+)+|[A-Z][a-z]* Institute of ([A-Z][a-zA-Z]+)+")

institution <- unlist(institution)
table(institution)
```

    ## institution
    ##              Army Institute of Research        Australian Institute of Tropical 
    ##                                       1                                      15 
    ##       Beijing Institute of Pharmacology              Berlin Institute of Health 
    ##                                       2                                       4 
    ##              Broad Institute of Harvard               Cancer Institute of Emory 
    ##                                       2                                       2 
    ##                 Cancer Institute of New           Genome Institute of Singapore 
    ##                                       1                                       1 
    ##    Graduate Institute of Rehabilitation         Health Institute of Montpellier 
    ##                                       3                                       1 
    ##          Heidelberg Institute of Global            Indian Institute of Tropical 
    ##                                       1                                       5 
    ##            Leeds Institute of Rheumatic                   M Institute of Health 
    ##                                       2                                       1 
    ##   Massachusetts Institute of Technology            Mayo Institute of Technology 
    ##                                       1                                       1 
    ##          Medanta Institute of Education Mediterranean Institute of Oceanography 
    ##                                       1                                       1 
    ##       Monterrey Institute of Technology           National Institute of Allergy 
    ##                                       1                                       3 
    ##        National Institute of Biomedical     National Institute of Biostructures 
    ##                                       1                                       1 
    ##             National Institute of Child     National Institute of Environmental 
    ##                                       2                                       3 
    ##           National Institute of General            National Institute of Public 
    ##                                       1                                       1 
    ##        National Institute of Technology        Nordic Institute of Chiropractic 
    ##                                       1                                       1 
    ##      Prophylactic Institute of Southern               Research Institute of New 
    ##                                       2                                       4 
    ##      Research Institute of Tuberculosis                   University of Alberta 
    ##                                       2                                       2 
    ##                   University of Applied                   University of Arizona 
    ##                                       3                                       5 
    ##                  University of Arkansas                     University of Basel 
    ##                                      25                                       8 
    ##                     University of Benin                  University of Botswana 
    ##                                       1                                       1 
    ##                  University of Bradford                   University of Bristol 
    ##                                       1                                       4 
    ##                   University of British                   University of Calgary 
    ##                                       4                                      23 
    ##                University of California                  University of Campinas 
    ##                                      78                                       3 
    ##                   University of Chicago                   University of Chinese 
    ##                                      13                                       1 
    ##                University of Cincinnati                  University of Colorado 
    ##                                      11                                       4 
    ##               University of Connecticut                University of Copenhagen 
    ##                                       3                                       7 
    ##                    University of Dayton                 University of Education 
    ##                                       1                                       1 
    ##                    University of Exeter                   University of Florida 
    ##                                       1                                       9 
    ##                   University of Georgia                     University of Ghana 
    ##                                       2                                       1 
    ##                   University of Granada                     University of Haifa 
    ##                                       2                                       1 
    ##                      University of Hawa                     University of Hawai 
    ##                                       1                                     260 
    ##                    University of Hawaii                    University of Health 
    ##                                     350                                      11 
    ##                      University of Hong                  University of Honolulu 
    ##                                       3                                       6 
    ##                  University of Illinois               University of Information 
    ##                                       6                                       2 
    ##                      University of Iowa                      University of Juiz 
    ##                                       4                                       4 
    ##                    University of Kansas                  University of Kentucky 
    ##                                       4                                       1 
    ##                  University of Lausanne                     University of Leeds 
    ##                                       1                                       2 
    ##                University of Louisville                     University of Maine 
    ##                                       1                                       2 
    ##                    University of Malaya                  University of Maryland 
    ##                                       2                                      11 
    ##             University of Massachusetts                   University of Medical 
    ##                                      22                                       2 
    ##                  University of Medicine                 University of Melbourne 
    ##                                       4                                       1 
    ##                     University of Miami                  University of Michigan 
    ##                                       2                                       8 
    ##                 University of Minnesota                  University of Missouri 
    ##                                       1                                       2 
    ##                   University of Montana                     University of Mount 
    ##                                       3                                       1 
    ##                    University of Murcia                  University of Nebraska 
    ##                                       1                                       5 
    ##                    University of Nevada                       University of New 
    ##                                       3                                      14 
    ##                     University of North                      University of Ohio 
    ##                                       8                                       1 
    ##                   University of Ontario                      University of Oslo 
    ##                                       1                                       6 
    ##                    University of Oxford                   University of Palermo 
    ##                                       9                                       1 
    ##                     University of Paris              University of Pennsylvania 
    ##                                       1                                      68 
    ##                University of Pittsburgh                     University of Porto 
    ##                                      13                                       2 
    ##                    University of Puerto                University of Queensland 
    ##                                       3                                       2 
    ##                     University of Rhode                  University of Richmond 
    ##                                       3                                       1 
    ##                       University of Rio                 University of Rochester 
    ##                                       1                                       4 
    ##                       University of Sao                   University of Science 
    ##                                       2                                      34 
    ##                   University of Sergipe                 University of Singapore 
    ##                                       1                                       1 
    ##                     University of South                  University of Southern 
    ##                                       4                                      24 
    ##                    University of Sydney                University of Technology 
    ##                                       1                                       5 
    ##                     University of Texas                   University of Toronto 
    ##                                       9                                      15 
    ##                    University of Toulon                      University of Utah 
    ##                                       1                                       8 
    ##                University of Washington                      University of West 
    ##                                       9                                       1 
    ##                   University of Western                 University of Wisconsin 
    ##                                       1                                       8 
    ##                   University of Wyoming                      University of York 
    ##                                       1                                       1

Repeat the exercise and this time focus on schools and departments in
the form of

1.  School of…
2.  Department of…

And tabulate the results

``` r
schools_and_deps <- str_extract_all(
  publications_txt,
  "School of ([A-Z][a-zA-Z]+)+|[A-Z][a-z]* Department of ([A-Z][a-zA-Z]+)+"
  )
table(schools_and_deps)
```

    ## schools_and_deps
    ## Abramson Department of Rehabilitation             Alaska Department of Fish 
    ##                                     1                                     1 
    ##      Carilion Department of Emergency     Faillace Department of Psychiatry 
    ##                                     1                                     2 
    ##     Florida Department of Agriculture         Georgia Department of Natural 
    ##                                     1                                     2 
    ##       Government Department of Health          Hawaii Department of Defense 
    ##                                     2                                     1 
    ##        Hawaii Department of Education           Hawaii Department of Health 
    ##                                     6                                    30 
    ##        Hospital Department of Surgery            Maine Department of Inland 
    ##                                     1                                     1 
    ##       Manoa Department of Mathematics        Michigan Department of Natural 
    ##                                     5                                     1 
    ##       Milliken Department of Medicine        Ministers Department of Health 
    ##                                     1                                     1 
    ##       Nuffield Department of Medicine     Nuffield Department of Population 
    ##                                     1                                     3 
    ##     Pennsylvania Department of Health              S Department of Commerce 
    ##                                     1                                     1 
    ##                S Department of Health              S Department of Veterans 
    ##                                     2                                     8 
    ##            Samoa Department of Health                   School of Aerospace 
    ##                                     2                                     5 
    ##                 School of Agriculture                     School of Applied 
    ##                                     1                                     1 
    ##                     School of Aquatic                  School of Biological 
    ##                                     1                                     4 
    ##                  School of Biomedical                       School of Brown 
    ##                                     3                                     2 
    ##                    School of Business                    School of Chemical 
    ##                                     4                                     1 
    ##                   School of Education                  School of Electronic 
    ##                                     2                                     1 
    ##                      School of Energy               School of Environmental 
    ##                                     2                                     2 
    ##                School of Epidemiology                      School of Forest 
    ##                                     6                                     1 
    ##                    School of Forestry                      School of Health 
    ##                                     4                                    14 
    ##                     School of Hygiene                  School of Immunology 
    ##                                     1                                     1 
    ##                         School of Law                        School of Life 
    ##                                     1                                     3 
    ##                    School of Medicine                     School of Natural 
    ##                                   588                                     4 
    ##                     School of Nursing                       School of Ocean 
    ##                                    70                                     1 
    ##                    School of Pharmacy                    School of Physical 
    ##                                     1                                     6 
    ##               School of Physiotherapy                  School of Population 
    ##                                     1                                     2 
    ##                      School of Public                    School of Sciences 
    ##                                    92                                     4 
    ##                      School of Social              School of Transportation 
    ##                                    55                                     1 
    ##                  School of Veterinary         State Department of Education 
    ##                                     1                                     1 
    ##            State Department of Health          State Department of Taxation 
    ##                                    38                                     1 
    ##         States Department of Veterans      University Department of Surgery 
    ##                                     1                                     2 
    ##       Wisconsin Department of Natural        Zealand Department of Internal 
    ##                                     2                                    22

## Question 5: Form a database

We want to build a dataset which includes the title and the abstract of
the paper. The title of all records is enclosed by the HTML tag
ArticleTitle, and the abstract by Abstract.

Before applying the functions to extract text directly, it will help to
process the XML a bit. We will use the xml2::xml_children() function to
keep one element per id. This way, if a paper is missing the abstract,
or something else, we will be able to properly match PUBMED IDS with
their corresponding records.

``` r
pub_char_list <- xml2::xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)
```

Now, extract the abstract and article title for each one of the elements
of pub_char_list. You can either use sapply() as we just did, or simply
take advantage of vectorization of stringr::str_extract

``` r
abstracts <- str_extract(pub_char_list, "<AbstractText>.*</AbstractText>")
abstracts <- str_replace_all(abstracts, "</?[[:alpha:]]+>", "")

titles <- str_extract(pub_char_list, "<ArticleTitle>.*</ArticleTitle>")
titles <- str_replace_all(titles, "</?[[:alpha:]]+>", "")
```

Finally, put everything together into a single data.frame and use
knitr::kable() to print the results

``` r
database <- data.frame(
  PubMedId = ids,
  Title    = titles,
  Abstract = abstracts
) 

database %>%
  head(n = 10) %>%
  knitr::kable()
```

| PubMedId | Title                                                                                                                                  | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:---------|:---------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 36186620 | Racial/Ethnic Disparities in Getting COVID-19 Vaccine: Do Age, Gender, and Education Matter?                                           | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| 36173983 | Hydroxychloroquine/chloroquine for the treatment of hospitalized patients with COVID-19: An individual participant data meta-analysis. | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| 36146513 | Dynamics of Trust and Consumption of COVID-19 Information Implicate a Mechanism for COVID-19 Vaccine and Booster Uptake.               | Vaccine hesitancy remains a significant barrier to achieving herd immunity and preventing the further spread of COVID-19. Understanding contributors to vaccine hesitancy and how they change over time may improve COVID-19 mitigation strategies and public health policies. To date, no mechanism explains how trust in and consumption of different sources of information affect vaccine uptake. A total of 1594 adults enrolled in our COVID-19 testing program completed standardized surveys on demographics, vaccination status, use, reliance, and trust in sources of COVID-19 information, from September to October 2021, during the COVID-19 Delta wave. Of those, 802 individuals (50.3%) completed a follow-up survey, from January to February 2022, during the Omicron-wave. Regression analyses were performed to understand contributors to vaccine and booster uptake over time. Individuals vaccinated within two months of eligibility (early vaccinees) tended to have more years of schooling, with greater trust in and consumption of official sources of COVID-19 information, compared to those who waited 3-6 months (late vaccinees), or those who remained unvaccinated at 6 months post-eligibility (non-vaccinees). Most (70.1%) early vaccinees took the booster shot, compared to only 30.5% of late vaccinees, with the latter group gaining trust and consumption of official information after four months. These data provide the foundation for a mechanism based on the level of trust in and consumption of official information sources, where those who increased their level of trust in and consumption of official information sources were more likely to receive a booster. This study shows that social factors, including education and individual-level degree of trust in (and consumption of) sources of COVID-19 information, interact and change over time to be associated with vaccine and booster uptakes. These results are critical for the development of effective public health policies and offer insights into hesitancy over the course of the COVID-19 vaccine and booster rollout. |
| 36128780 | Effects of the COVID 19 Pandemic on School Nurses’ Resiliency and Ability to Cope: A Mixed Methods Study in the State of Hawaii.       | This mixed-method study examined school nurses’ experiences during the Coronavirus Disease 2019 pandemic related to role change, psychological feelings, and coping/resiliency in the State of Hawaii. A total of 30 school nurses completed a Brief Resilience Coping Scale plus a series of open-ended questions in January 2022. On the coping scale, over 40% of participants scored high, 52% scored medium, and 7% scored a low resilient/coping level. We did not identify any association between coping level and participant characteristics. Three qualitative themes emerged: 1) school nurses experience chronic negative emotions related to the pandemic, 2) school nurses demonstrate attributes of resilience, and 3) school nurses utilize positive coping techniques. The pandemic created significant stresses and negative emotions among school nurses. Yet, school nurses reported effective coping strategies and demonstrated strength/resilience. Support and open communication between school nurses, their employers, and other school-based stakeholders is needed to provide continued support for school nurses.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 36126272 | Veterans’ Use of Telehealth for Veterans Health Administration Community Care Urgent Care During the Early COVID-19 Pandemic.          | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| 36108254 | Vaccine-Associated Shifts in SARS-CoV-2 Infectivity Among the Native Hawaiian and Other Pacific Islander Population in Hawaii.         | Native Hawaiians and other Pacific Islanders (NHPIs) across the country have experienced significant disparities because of the COVID-19 pandemic. The Pacific Alliance Against COVID-19 used a community-based participatory approach involving academic and community partners to expand sustainable COVID-19 testing capacity and mitigate the severe consequences among NHPI communities in Hawaii. We describe the approach of this one-year study, some of the results, and how the data are being used to inform next steps for the communities. Clinical Trials.gov identifier: NCT04766333. (Am J Public Health. Published online ahead of print September 15, 2022:e1-e4. <https://doi.org/10.2105/AJPH.2022.306973>).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 36091468 | Resilience of breadfruit agro-ecosystems in Hawai’i during the COVID-19 pandemic.                                                      | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| 36081413 | Effect of a Condensed NBA Season on Injury Risk: An Analysis of the 2020 Season and Player Safety.                                     | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| 36065487 | The mental health impact of COVID-19-related stressors among treatment-seeking trauma-exposed veterans.                                | Trauma-exposed veterans receiving mental health care may have an elevated risk of experiencing COVID-19-related difficulties. Using data from several ongoing clinical trials (N = 458), this study examined exposure to COVID-19-related stressors and their associations with key sociodemographic factors and mental health outcomes. The results showed that exposure to COVID-19-related stressors was common, higher among veterans who were racial/ethnic minorities d = 0.32, and associated with elevated posttraumatic stress disorder (PTSD), r = .288, and depressive symptom severity, r = .246. Women veterans experienced more difficulty accessing social support, d = 0.31, and higher levels of COVID-19-related distress, d = 0.31, than men. Qualitative data were consistent with survey findings and highlighted the broader societal context in veterans’ experience of COVID-19-related distress. These findings may inform future research on the impact of the pandemic on veterans, particularly those who are women and members of minoritized racial/ethnic groups, as well as mental health treatment planning for this population.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 36051401 | Defining and Addressing Anesthesiology Needs in Simulation-based Medical Education.                                                    | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
