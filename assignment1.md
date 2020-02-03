Statistical assignment 1
================
\[visuth yanamorn, 660071411\]
\[02/02/2020\]

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code
    below.

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.0       ✔ purrr   0.3.0  
    ## ✔ tibble  2.0.1       ✔ dplyr   0.8.0.1
    ## ✔ tidyr   0.8.3       ✔ stringr 1.3.1  
    ## ✔ readr   1.3.1       ✔ forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.2

    ## Warning: package 'tibble' was built under R version 3.5.2

    ## Warning: package 'tidyr' was built under R version 3.5.2

    ## Warning: package 'purrr' was built under R version 3.5.2

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## Warning: package 'forcats' was built under R version 3.5.2

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
Data <- read_tsv("/Users/vy210/Documents/data analysis/assignment1/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
Data <- Data %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
?filter
```

    ## Help on topic 'filter' was found in the following packages:
    ## 
    ##   Package               Library
    ##   dplyr                 /Library/Frameworks/R.framework/Versions/3.5/Resources/library
    ##   stats                 /Library/Frameworks/R.framework/Versions/3.5/Resources/library
    ## 
    ## 
    ## Using the first match ...

``` r
Data <- Data %>%
        filter(h_memorig == 1)
```

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 
    ##  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51 
    ## 314 332 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 
    ##  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69 
    ## 427 414 432 422 408 413 416 434 369 398 358 399 354 412 345 358 412 434 
    ##  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87 
    ## 431 334 326 293 275 251 219 231 211 205 181 162 138 117 117 108  89  78 
    ##  88  89  90  91  92  93  94  95  96  97  98  99 101 102 
    ##  77  48  41  27  15  18  15   7   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
        mutate(EU = case_when(h_eumem == 1 ~ "1", h_eumem == 2 ~ "0", h_eumem < 1 ~ NA_character_)
      
        ) %>%
        mutate(sex = case_when(h_sex_dv == 1 ~ "male", h_sex_dv == 2 ~ "female", h_sex_dv == 0 ~ NA_character_)
               ) %>%
        mutate(agegr = case_when(
          between(h_age_dv, 16, 25) ~ "16 to 25",
          between(h_age_dv, 26, 40) ~ "26 to 40",
          between(h_age_dv, 41, 55) ~ "41 to 55",
          between(h_age_dv, 56, 70) ~ "56 to 70",
          h_age_dv > 70 ~ "over 70"
        )
        )
```

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
?count
Data %>%
        count(EU) %>% mutate(percentages = n/ sum(n) * 100)
```

    ## # A tibble: 3 x 3
    ##   EU        n percentages
    ##   <chr> <int>       <dbl>
    ## 1 <NA>   2501        10.9
    ## 2 0      9338        40.7
    ## 3 1     11118        48.4

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum?
Why?

## comparing to the result of the 2016 referendum, UK voters had voted to leave the EU by 51.9%, whereas this result shows approxiamtely 41% of voters voting to leave the EU. the remain percentages is still remains at 48%.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
Data %>%
        count(EU, sex) %>% mutate(percentages = n/ sum(n) * 100)
```

    ## # A tibble: 7 x 4
    ##   EU    sex        n percentages
    ##   <chr> <chr>  <int>       <dbl>
    ## 1 <NA>  female  1256     5.47   
    ## 2 <NA>  male    1245     5.42   
    ## 3 0     female  4859    21.2    
    ## 4 0     male    4479    19.5    
    ## 5 1     <NA>       1     0.00436
    ## 6 1     female  6371    27.8    
    ## 7 1     male    4746    20.7

``` r
Data %>%
        count(EU, agegr) %>% mutate(percentages = n/ sum(n) * 100)
```

    ## # A tibble: 15 x 4
    ##    EU    agegr        n percentages
    ##    <chr> <chr>    <int>       <dbl>
    ##  1 <NA>  16 to 25   373        1.62
    ##  2 <NA>  26 to 40   436        1.90
    ##  3 <NA>  41 to 55   628        2.74
    ##  4 <NA>  56 to 70   558        2.43
    ##  5 <NA>  over 70    506        2.20
    ##  6 0     16 to 25   763        3.32
    ##  7 0     26 to 40  1508        6.57
    ##  8 0     41 to 55  2509       10.9 
    ##  9 0     56 to 70  2737       11.9 
    ## 10 0     over 70   1821        7.93
    ## 11 1     16 to 25  1749        7.62
    ## 12 1     26 to 40  2440       10.6 
    ## 13 1     41 to 55  3013       13.1 
    ## 14 1     56 to 70  2646       11.5 
    ## 15 1     over 70   1270        5.53

Write a couple of sentences interpreting your results. \#\# the result
shows that female were more likely to vote remain than male and people
aged 41-55 were more likely to vote to remain and other age group. the
percentage of people aged 16-25 is much lower if compare to other age
groups. this could be because people aged under 18 are not able to vote
and also the younger people are less interested in voting and politics.
