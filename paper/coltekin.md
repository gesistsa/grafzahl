Çöltekin (2020)
================

OffensEval-TR 2020 is a [shared
task](https://sites.google.com/site/offensevalsharedtask/results-and-paper-submission).
The Turkish social media dataset by Çöltekin (2020)
\[[link](https://aclanthology.org/2020.lrec-1.758)\] is available here.

In this subtask, Turkish tweets, 31,756 and 3,528 in the training and
test sets respectively, were coded as “Offensive” or “Not Offensive”.
The state-of-the-art performance by the world’s best NLP experts for
this subtask is 82.58% (Marco F1). Of course, it is quite impossible for
this R package with default settings to obtain this performance. But it
would be interesting to see how well the performance this package could
get.

## Obtaining the data

``` r
url <- "https://coltekin.github.io/offensive-turkish/offenseval2020-turkish.zip"
temp <- tempfile(fileext = ".zip")
download.file(url, temp)
unzip(temp, exdir = here::here("paper"))
```

## Create the training corpus

``` r
require(quanteda)
#> Loading required package: quanteda
#> Package version: 3.2.4
#> Unicode version: 13.0
#> ICU version: 66.1
#> Parallel computing: 16 of 16 threads used.
#> See https://quanteda.io for tutorials and examples.
require(readtext)
#> Loading required package: readtext
input <- readtext::readtext(here::here("offenseval2020-turkish/offenseval-tr-training-v1/offenseval-tr-training-v1.tsv"), text_field = "tweet", quote = "") %>% corpus
```

## Train a classifer

The model is based on the BERTurk model by the *Bayerische
Staatsbibliothek* [^1].

``` r
set.seed(721)
model <- grafzahl(x = input,
                  y = "subtask_a",
                  model_type = "bert",
                  model_name = "dbmdz/bert-base-turkish-cased",
                  output_dir = here::here("turkmodel"))
saveRDS(model, here::here("turkmodel.RDS"))
```

## Create the test corpus

``` r
test <- rio::import(here::here("offenseval2020-turkish/offenseval-tr-testset-v1/offenseval-tr-testset-v1.tsv"), quote = "")

labels <- rio::import(here::here("offenseval2020-turkish/offenseval-tr-testset-v1/offenseval-tr-labela-v1.tsv"), quote = "")

colnames(labels)[1] <- "id"
colnames(labels)[2] <- "subtask_a"

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
test %>% left_join(labels) -> test
#> Joining, by = "id"


corpus(test, text_field = "tweet") -> test_corpus
```

## Calculation of Macro-F1

``` r
preds <- predict(model, newdata = test_corpus)

sum(caret::confusionMatrix(table(preds, docvars(test_corpus, "subtask_a")), mode = "prec_recall", positive = "OFF")$byClass["F1"], caret::confusionMatrix(table(preds, docvars(test_corpus, "subtask_a")), mode = "prec_recall", positive = "NOT")$byClass["F1"]) / 2
#> [1] 0.7972064
```

Not bad (vs the SOTA: 82.58%)!

[^1]: https://huggingface.co/dbmdz/bert-base-turkish-cased
