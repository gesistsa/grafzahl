Azime & Mohammed. (2021)
================

Amharic is a Semitic language mainly spoken in Ethiopia. After Arabic,
Amharic is the second most-spoken Semitic language. Unlike many Semitic
languages using the *abjad* (consonant-only) writing system, Amharic is
written in a unique alphasyllabary writing system called *Ge’ez*.
Syntactically, Amharic is also different from many Germanic languages
for its SOV (subject-object-verb) word order [^1]. It is in general
considered to be a “low resource” language. Only recently, the first
news classification dataset called “Amharic News Text classification
Dataset” is available \[[link](https://arxiv.org/abs/2103.05639)\].

Amharic News Text classification Dataset contains 50,706 news articles
curated from various Amharic websites. The original paper reports the
baseline out-of-sample accuracy of 62.2% using Naive Bayes. The released
data also contains the training-and-test split [^2]. It is a much bigger
dataset than the two previous examples (training set: 41,185 articles,
test set: 10,287). News articles were annotated into the following
categories (originally written in *Ge’ez*, transliterated to Latin
characters here): *hāgeri āk’efi zēna* (national news), *mezinanya*
(entertainment), *siporiti* (sport), *bīzinesi* (business), *’alemi
āk’efi zēna* (international news), and *poletīka* (politics).

In this example, the AfriBERTa is used as the pretrained model. The
AfriBERTa model was trained with a small corpus of 11 African languages.

# Obtain the data

``` r
download.file("https://huggingface.co/datasets/israel/Amharic-News-Text-classification-Dataset/resolve/main/train.csv", destfile = here::here("am_train.csv"))

download.file("https://huggingface.co/datasets/israel/Amharic-News-Text-classification-Dataset/resolve/main/test.csv", destfile = here::here("am_test.csv"))
```

# Preserve a model

We can directly use the AfriBERTa model online. We can also preserve a
local copy of a pretrained model. As all models on Hugging Face are
stored as a Git repository, one can use git to clone the model locally.
A cloned model usually takes around 1G of local storage.

``` bash
## make sure you have installed git lfs
## https://git-lfs.github.com/
git lfs install
git clone https://huggingface.co/castorini/afriberta_base localafriberta
```

# Train a classifer using the preserved AfriBERTa model

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
require(grafzahl)
#> Loading required package: grafzahl
input <- readtext::readtext(here::here("am_train.csv"), text_field = "article") %>%
    corpus %>% corpus_subset(category != "")
```

``` r
model <- grafzahl(x = input,
                  y = "category",
                  model_name = here::here("localafriberta"))
```

# Evaluate

Accuracy: 84%

``` r
testset_corpus <- readtext::readtext(here::here("am_test.csv"), text_field = "article") %>% corpus %>% corpus_subset(category != "")

preds <- predict(model, newdata = testset_corpus)
caret::confusionMatrix(table(preds, docvars(testset_corpus, "category")))
#> Confusion Matrix and Statistics
#> 
#>             
#> preds        ሀገር አቀፍ ዜና መዝናኛ ስፖርት ቢዝነስ ዓለም አቀፍ ዜና ፖለቲካ
#>   ሀገር አቀፍ ዜና       3434   23   57  194         88  234
#>   መዝናኛ               25  100    0    7          4    0
#>   ስፖርት               33    4 2052    4          7    4
#>   ቢዝነስ              130    2    0  454          7  104
#>   ዓለም አቀፍ ዜና        115    4    4    5       1136   17
#>   ፖለቲካ              351    2    2  120         82 1492
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8418          
#>                  95% CI : (0.8346, 0.8488)
#>     No Information Rate : 0.397           
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7878          
#>                                           
#>  Mcnemar's Test P-Value : 1.794e-15       
#> 
#> Statistics by Class:
#> 
#>                      Class: ሀገር አቀፍ ዜና Class: መዝናኛ Class: ስፖርት Class: ቢዝነስ
#> Sensitivity                     0.8400    0.740741      0.9702     0.57908
#> Specificity                     0.9040    0.996457      0.9936     0.97446
#> Pos Pred Value                  0.8521    0.735294      0.9753     0.65136
#> Neg Pred Value                  0.8956    0.996555      0.9923     0.96562
#> Prevalence                      0.3970    0.013111      0.2054     0.07614
#> Detection Rate                  0.3335    0.009712      0.1993     0.04409
#> Detection Prevalence            0.3914    0.013208      0.2043     0.06769
#> Balanced Accuracy               0.8720    0.868599      0.9819     0.77677
#>                      Class: ዓለም አቀፍ ዜና Class: ፖለቲካ
#> Sensitivity                     0.8580      0.8061
#> Specificity                     0.9838      0.9341
#> Pos Pred Value                  0.8868      0.7282
#> Neg Pred Value                  0.9791      0.9565
#> Prevalence                      0.1286      0.1798
#> Detection Rate                  0.1103      0.1449
#> Detection Prevalence            0.1244      0.1990
#> Balanced Accuracy               0.9209      0.8701
```

[^1]: Actually, majority of the languages are SOV, while SVO (many
    Germanic languages) are slightly less common.

[^2]: https://huggingface.co/datasets/israel/Amharic-News-Text-classification-Dataset/tree/main
