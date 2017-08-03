---
layout: post
title:  "Github Docs test"
date:   2017-08-03 19:19:32 +0200
categories: jekyll update
---


Where are the voters?
================

Overview of the data
--------------------

The data used here can be found here: [link](data.gouv.fr)

``` r
library(readr)
library(gplots) 
```

    ## 
    ## Attaching package: 'gplots'

    ## The following object is masked from 'package:stats':
    ## 
    ##     lowess

``` r
library(maps)
communeSortedByCandidat <- read_csv("/home/mathieu/Documents/data/EP2017/Presidentielle_2017_Resultats_Communes_T1_clean_def.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   CodeInsee = col_character(),
    ##   CodeDepartement = col_character(),
    ##   Département = col_character(),
    ##   Commune = col_character(),
    ##   Inscrits = col_integer(),
    ##   Abstentions = col_integer(),
    ##   Votants = col_integer(),
    ##   Blancs = col_integer(),
    ##   Nuls = col_integer(),
    ##   Exprimés = col_integer(),
    ##   `LE PEN` = col_integer(),
    ##   MÉLENCHON = col_integer(),
    ##   MACRON = col_integer(),
    ##   FILLON = col_integer(),
    ##   `DUPONT-AIGNAN` = col_integer(),
    ##   LASSALLE = col_integer(),
    ##   HAMON = col_integer(),
    ##   ASSELINEAU = col_integer(),
    ##   POUTOU = col_integer(),
    ##   ARTHAUD = col_integer()
    ##   # ... with 1 more columns
    ## )

    ## See spec(...) for full column specifications.

This is how our data look like:

``` r
head(communeSortedByCandidat)
```

    ## # A tibble: 6 x 51
    ##   CodeInsee CodeDepartement Département                 Commune Inscrits
    ##       <chr>           <chr>       <chr>                   <chr>    <int>
    ## 1     01001              01         Ain L'Abergement-Clémenciat      598
    ## 2     01002              01         Ain   L'Abergement-de-Varey      209
    ## 3     01004              01         Ain       Ambérieu-en-Bugey     8586
    ## 4     01005              01         Ain     Ambérieux-en-Dombes     1172
    ## 5     01006              01         Ain                 Ambléon       99
    ## 6     01007              01         Ain                Ambronay     1880
    ## # ... with 46 more variables: Abstentions <int>, Abstentions_ins <dbl>,
    ## #   Votants <int>, Votants_ins <dbl>, Blancs <int>, Blancs_ins <dbl>,
    ## #   Blancs_vot <dbl>, Nuls <int>, Nuls_ins <dbl>, Nuls_vot <dbl>,
    ## #   Exprimés <int>, Exprimés_ins <dbl>, Exprimés_vot <dbl>, `LE
    ## #   PEN` <int>, MÉLENCHON <int>, MACRON <int>, FILLON <int>,
    ## #   `DUPONT-AIGNAN` <int>, LASSALLE <int>, HAMON <int>, ASSELINEAU <int>,
    ## #   POUTOU <int>, ARTHAUD <int>, CHEMINADE <int>, `LE PEN.ins` <dbl>,
    ## #   MÉLENCHON.ins <dbl>, MACRON.ins <dbl>, FILLON.ins <dbl>,
    ## #   `DUPONT-AIGNAN.ins` <dbl>, LASSALLE.ins <dbl>, HAMON.ins <dbl>,
    ## #   ASSELINEAU.ins <dbl>, POUTOU.ins <dbl>, ARTHAUD.ins <dbl>,
    ## #   CHEMINADE.ins <dbl>, `LE PEN.exp` <dbl>, MÉLENCHON.exp <dbl>,
    ## #   MACRON.exp <dbl>, FILLON.exp <dbl>, `DUPONT-AIGNAN.exp` <dbl>,
    ## #   LASSALLE.exp <dbl>, HAMON.exp <dbl>, ASSELINEAU.exp <dbl>,
    ## #   POUTOU.exp <dbl>, ARTHAUD.exp <dbl>, CHEMINADE.exp <dbl>

We have the departements and commmunes codes and name, the number of registered people, numbers of non-voters, the invalid votes, counted votes and the results for each candidate.

for security check, let's make sure that registered voters and non-voters are equal to registered people all together.

``` r
controllingPop <- communeSortedByCandidat$Inscrits-(communeSortedByCandidat$Abstentions + communeSortedByCandidat$Votants)
ctrl<-controllingPop!=0
ctrl=TRUE
#controllingPop
```

if there would be a mistake, this code should return a 'FALSE'. Here it doesn't.

### The registered voters

One of the specificities of France is it's geographical repartition in 35719 villages, which is by far the largest number in the European Union. Here we will show the importance of small villages in the political organization of the country.

``` r
inscritsParCommune <- table(cut(communeSortedByCandidat$Inscrits, breaks = 20))
hist(inscritsParCommune, 
     col = 'blue', 
     main = 'Figure1: Frequency of small communes in France',
     xlab = "Voters per Commune")
```

![](repartitionDesInscrits_files/figure-markdown_github/unnamed-chunk-4-1.png) Figure1 shows the repartition of the voters: very high concentration of small commune, a couple of large cities, and nearly nothing in between. The summary of the registered voters per commune gives the following result:

``` r
commune <- tapply(communeSortedByCandidat$Inscrits, communeSortedByCandidat$Commune, sum)
summary(commune)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     169     379    1426     948 1302000

Here we see that even the third quartile is under 1000 voters. Meaning that 75% of the communes have less than 948 voters.

### Voters per departement

``` r
inscrits <- communeSortedByCandidat$Inscrits
departement <- communeSortedByCandidat$Département
france<-map(database="france")
```

![](repartitionDesInscrits_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
dep <- tapply(inscrits, departement, sum)
matche <- match.map(france, departement)

map(database = 'france', fill = TRUE, col = rev(heat.colors(10)), resolution = 0)
title("Geographical repartition of the voters ")
```

![](repartitionDesInscrits_files/figure-markdown_github/unnamed-chunk-6-2.png)

Reminder: this is the world repartition of the french voters and not the french people. The french people might not be registered on the voter's lists
