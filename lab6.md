PM566lab6
================
Mingzhi Ye
9/24/2020

``` r
library(data.table)
library(leaflet)
library(tidyverse)
```

    ## -- Attaching packages ---------------------------- tidyverse 1.3.0 --

    ## √ ggplot2 3.3.2     √ purrr   0.3.4
    ## √ tibble  3.0.3     √ dplyr   1.0.2
    ## √ tidyr   1.1.1     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.5.0

    ## -- Conflicts ------------------------------- tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
library(dplyr)
library(readr)
library(ggplot2)
library(tidytext)
```

Step in
-------

``` r
mt <- read.csv("C:/Users/yemin/Desktop/PM566/week6/mtsamples.csv")
```

``` r
mt<-mt %>%
  select(description,medical_specialty,transcription)
```

question1
---------

``` r
mt %>%
  count(medical_specialty,sort = TRUE)
```

    ##                 medical_specialty    n
    ## 1                         Surgery 1103
    ## 2      Consult - History and Phy.  516
    ## 3      Cardiovascular / Pulmonary  372
    ## 4                      Orthopedic  355
    ## 5                       Radiology  273
    ## 6                General Medicine  259
    ## 7                Gastroenterology  230
    ## 8                       Neurology  223
    ## 9   SOAP / Chart / Progress Notes  166
    ## 10        Obstetrics / Gynecology  160
    ## 11                        Urology  158
    ## 12              Discharge Summary  108
    ## 13           ENT - Otolaryngology   98
    ## 14                   Neurosurgery   94
    ## 15          Hematology - Oncology   90
    ## 16                  Ophthalmology   83
    ## 17                     Nephrology   81
    ## 18         Emergency Room Reports   75
    ## 19          Pediatrics - Neonatal   70
    ## 20                Pain Management   62
    ## 21        Psychiatry / Psychology   53
    ## 22                   Office Notes   51
    ## 23                       Podiatry   47
    ## 24                    Dermatology   29
    ## 25     Cosmetic / Plastic Surgery   27
    ## 26                      Dentistry   27
    ## 27                        Letters   23
    ## 28      Physical Medicine - Rehab   21
    ## 29                 Sleep Medicine   20
    ## 30                  Endocrinology   19
    ## 31                     Bariatrics   18
    ## 32         IME-QME-Work Comp etc.   16
    ## 33                   Chiropractic   14
    ## 34           Diets and Nutritions   10
    ## 35                   Rheumatology   10
    ## 36              Speech - Language    9
    ## 37                        Autopsy    8
    ## 38       Lab Medicine - Pathology    8
    ## 39           Allergy / Immunology    7
    ## 40      Hospice - Palliative Care    6

``` r
mt %>%
  count(medical_specialty,sort = TRUE) %>%
  nrow()
```

    ## [1] 40

There are 40 categories of medical specialty.

question2
---------

``` r
mt %>%
  unnest_tokens(token, transcription) %>%
  count(token, sort = TRUE) %>%
  top_n(n=20,wt=n) %>%
  mutate(token = reorder(token, n)) %>%
  ggplot(aes(y=token,x=n)) +
  geom_col()
```

![](lab6_files/figure-markdown_github/unnamed-chunk-5-1.png) Many of the most frequent words are not important, it's normal, we shold strip these words(stop\_words)

question 3
----------

``` r
mt %>%
  unnest_tokens(token, transcription) %>%
  anti_join(stop_words,by=c("token"="word"))%>% #word is a column in stop_words
  filter(!(str_detect(token,"\\d+$")))%>%
  count(token, sort = TRUE) %>%
  top_n(n=20,wt=n) %>%
  mutate(token = reorder(token, n)) %>%
  ggplot(aes(y=token,x=n)) +
  geom_col()
```

![](lab6_files/figure-markdown_github/unnamed-chunk-6-1.png) Now they are usually n. and v. , so it's more useful for us

question4
---------

``` r
mt %>%
  unnest_ngrams(ngram, transcription, n = 2) %>%
  count(ngram, sort = TRUE) %>%
  top_n(n=20,wt=n) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(y=ngram,x=n)) +
  geom_col()
```

![](lab6_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
mt %>%
  unnest_ngrams(ngram, transcription, n = 3) %>%
  count(ngram, sort = TRUE) %>%
  top_n(n=20,wt=n) %>%
  mutate(ngram = reorder(ngram, n)) %>%
  ggplot(aes(y=ngram,x=n)) +
  geom_col()
```

![](lab6_files/figure-markdown_github/unnamed-chunk-7-2.png)

question 5
==========

I choose "developed" to check

After "developed"

``` r
mtbi<-mt %>%
  unnest_ngrams(ngram, transcription, n = 2) %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  select(word1,word2)
```

``` r
mtbi1<-mtbi %>%
  anti_join(tidytext::stop_words,by=c("word1"="word")) %>%
  anti_join(tidytext::stop_words %>% select(word),by=c("word2"="word"))
```

``` r
mtbi1 %>%
  filter(word1=="developed")%>%
  count(word2, sort = TRUE) %>%
  top_n(n=20,wt=n) %>%
  mutate(word2 = reorder(word2, n)) %>%
  ggplot(aes(y=word2,x=n)) +
  geom_col()
```

![](lab6_files/figure-markdown_github/unnamed-chunk-10-1.png)

Before "developed"

``` r
mtbi1 %>%
  filter(word2=="developed")%>%
  count(word1, sort = TRUE) %>%
  top_n(n=20,wt=n) %>%
  mutate(word1 = reorder(word1, n)) %>%
  ggplot(aes(y=word1,x=n)) +
  geom_col()
```

![](lab6_files/figure-markdown_github/unnamed-chunk-11-1.png)

question 6
==========

Rank by medical\_specialty at first

``` r
mt %>%
  unnest_tokens(token, transcription) %>%
  anti_join(stop_words,by=c("token"="word"))%>% #word is a column in stop_words
  filter(!(str_detect(token,"\\d+$")))%>%
  count(medical_specialty,token, sort = TRUE) %>%
  group_by(medical_specialty)%>%
  top_n(n=5,wt=n) %>%
  arrange(medical_specialty,desc(n))
```

    ## # A tibble: 210 x 3
    ## # Groups:   medical_specialty [40]
    ##    medical_specialty       token         n
    ##    <chr>                   <chr>     <int>
    ##  1 " Allergy / Immunology" history      38
    ##  2 " Allergy / Immunology" noted        23
    ##  3 " Allergy / Immunology" patient      22
    ##  4 " Allergy / Immunology" allergies    21
    ##  5 " Allergy / Immunology" nasal        13
    ##  6 " Allergy / Immunology" past         13
    ##  7 " Autopsy"              left         83
    ##  8 " Autopsy"              inch         59
    ##  9 " Autopsy"              neck         55
    ## 10 " Autopsy"              anterior     47
    ## # ... with 200 more rows

Rank by n at first

``` r
mt %>%
  unnest_tokens(token, transcription) %>%
  anti_join(stop_words,by=c("token"="word"))%>% #word is a column in stop_words
  filter(!(str_detect(token,"\\d+$")))%>%
  count(medical_specialty,token, sort = TRUE) %>%
  group_by(medical_specialty)%>%
  top_n(n=5,wt=n) %>%
  arrange(desc(n),medical_specialty)
```

    ## # A tibble: 210 x 3
    ## # Groups:   medical_specialty [40]
    ##    medical_specialty             token          n
    ##    <chr>                         <chr>      <int>
    ##  1 " Surgery"                    patient     4856
    ##  2 " Surgery"                    left        3263
    ##  3 " Surgery"                    procedure   3243
    ##  4 " Consult - History and Phy." patient     3057
    ##  5 " Consult - History and Phy." history     2820
    ##  6 " Orthopedic"                 patient     1713
    ##  7 " Surgery"                    anesthesia  1687
    ##  8 " Surgery"                    incision    1641
    ##  9 " Cardiovascular / Pulmonary" left        1550
    ## 10 " Cardiovascular / Pulmonary" patient     1532
    ## # ... with 200 more rows

question 7
==========

1.  "in the", "at the" are very common, and the "the patient" is the most common. It is consistent with my common sense

2.  Sure, for example, tooth and teeth is very common in the dentistry speciality, because these two words is really frequently-mentioned in dentistry
