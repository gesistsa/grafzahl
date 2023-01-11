Dobbrick et al. (2021)
================

The following is to analyse the same data used in Dobbrick et al. (2021)
“Enhancing Theory-Informed Dictionary Approaches with “Glass-box”
Machine Learning: The Case of Integrative Complexity in Social Media
Comments” \[[doi](https://doi.org/10.1080/19312458.2021.1999913)\]. The
data is available from [osf](https://doi.org/10.17605/OSF.IO/578MG).

Dobbrick et al. present a study of comparing various methods to learn
and predict integrative complexity of English and German online user
comments from Facebook, Twitter, and news website comment sections.
According to the original paper, “Integrative complexity is a
psychological measure that researchers increasingly implement to assess
the argumentative quality of public debate contributions.” (p. 3)
Comments were coded with a standard coding scheme into a 7-point Likert
scale from 1 (lowest complexity) to 7 (highest complexity). The paper
presents two approaches: Assumption-based approach and Shotgun approach.
The Shotgun approach is similar to the traditional full-text machine
learning approach. Dobbrick et al. report that CNN with word embeddings
provides the best out-of-sample performance. The original paper reports
10-fold cross-validation. Root mean squared error (RMSE) of .75
(English) and .84 (German) were reported. It is also important to note
that Dobbrick et al. trained an individual model for each language. The
human annotated data and the original training-and-test split are
publicly available. In total, there are 4,800 annotated comments.

Please note that this is a regression example.

# Obtain the data from OSF

``` r
temp <- tempdir()
require(osfr)
osf_retrieve_file("https://osf.io/m6a9n") %>%
  osf_download(path = temp)
## sanity check
file.exists(file.path(temp, "glassbox.zip"))
## goldstandard_ic_en.csv
## goldstandard_ic_de.csv
unzip(file.path(temp, "glassbox.zip"), files = c("glassbox/data/goldstandard_ic_de.csv", "glassbox/data/goldstandard_ic_en.csv"), exdir = temp)

## sanity check
file.exists(file.path(temp, "glassbox/data/goldstandard_ic_de.csv"))
file.exists(file.path(temp, "glassbox/data/goldstandard_ic_en.csv"))

file.copy(file.path(temp, "glassbox/data/goldstandard_ic_de.csv"), here::here("goldstandard_ic_de.csv"))
file.copy(file.path(temp, "glassbox/data/goldstandard_ic_en.csv"), here::here("goldstandard_ic_en.csv"))
```

# Read the data

``` r
require(quanteda)
#> Loading required package: quanteda
#> Package version: 3.2.4
#> Unicode version: 13.0
#> ICU version: 66.1
#> Parallel computing: 16 of 16 threads used.
#> See https://quanteda.io for tutorials and examples.
require(grafzahl)
#> Loading required package: grafzahl
require(rio)
#> Loading required package: rio
#> 
#> Attaching package: 'rio'
#> The following object is masked from 'package:quanteda':
#> 
#>     convert
require(dplyr)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

## The csv file is actually the "European" variant; can't use readtext
## https://github.com/quanteda/readtext/issues/170
en_data <- rio::import(here::here("goldstandard_ic_en.csv")) %>% tibble::as_tibble() %>% filter(!is_redacted & main_language == "en" & WC > 0)
en_data %>% pull(post) %>% corpus -> en_corpus
docvars(en_corpus, "icom") <- as.numeric(en_data$ic_ordinal)
docnames(en_corpus) <- paste0("en", seq_along(en_corpus))

de_data <- rio::import(here::here("goldstandard_ic_de.csv")) %>% tibble::as_tibble() %>% filter(!is_redacted & main_language == "de" & WC > 0)
de_data %>% pull(post) %>% corpus -> de_corpus
docvars(de_corpus, "icom") <- as.numeric(de_data$ic_ordinal)

docnames(de_corpus) <- paste0("de", seq_along(de_corpus))
```

# Generate the 10-fold cross validation setup

``` r
set.seed(2020)
en_ranid <- sample(1:10, size = ndoc(en_corpus), replace = TRUE)

set.seed(2020)
de_ranid <- sample(1:10, size = ndoc(de_corpus), replace = TRUE)
```

# Do the 10-fold cross validation

The distil mBERT is used in this case to model the two languages
simultanesouly, whereas the original paper modeled the two languages
seperately.

Also, we can see here that the quanteda `corpus` objects can be combined
by `+`. In this example, as there is only `docvar` in the corpus
(“icom”), that docvar is used as the label to be predicted.

``` r
rmse <- function(x, y) {
    sqrt(mean((x - y)^2))
}

fold <- function(i, en_corpus, de_corpus, en_ranid, de_ranid) {
    mod <- grafzahl(en_corpus[en_ranid != i] + de_corpus[de_ranid != i],
                    model_name = "distilbert-base-multilingual-cased",
                    output_dir = here::here(paste0("dobbrick", i)),
                    regression = TRUE)
    pred_en <- predict(mod, en_corpus[en_ranid == i])
    pred_de <- predict(mod, de_corpus[de_ranid == i])
    x1 <- cor(docvars(en_corpus, "icom")[en_ranid == i], pred_en)
    x2 <- rmse(docvars(en_corpus, "icom")[en_ranid == i], pred_en)
    x3 <- cor(docvars(de_corpus, "icom")[de_ranid == i], pred_de)
    x4 <- rmse(docvars(de_corpus, "icom")[de_ranid == i], pred_de)
    return(tibble(i = i, en_cor = x1, en_rmse = x2, de_cor = x3, de_rmse = x4))
}

res <- purrr::map_dfr(1:10, fold, en_corpus = en_corpus, de_corpus = de_corpus, en_ranid = en_ranid, de_ranid = de_ranid)
```

# Results

Apply the same 10-fold cross-validation setup, the RMSE for English and
German are .67 and .74 respectively (vs. .75 and .84 from the original
paper, lower is better).

``` r
res %>% select(-i) %>% summarise_all(mean)
#> # A tibble: 1 × 4
#>   en_cor en_rmse de_cor de_rmse
#>    <dbl>   <dbl>  <dbl>   <dbl>
#> 1  0.740   0.671  0.726   0.738
```
