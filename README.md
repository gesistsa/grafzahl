
<!-- README.md is generated from README.Rmd. Please edit that file -->

# grafzahl <img src="man/figures/grafzahl_logo.svg" align="right" height="200" />

<!-- badges: start -->

<!-- badges: end -->

WIP: The goal of grafzahl (**G**racious **R** **A**nalytical
**F**ramework for **Z**appy **A**nalysis of **H**uman **L**anguages
\[1\]) is to duct tape the
[quanteda](https://github.com/quanteda/quanteda) ecosystem to modern
[Transformer-based text classification
models](https://simpletransformers.ai/), e.g. BERT, RoBERTa, etc. The
model object looks and feels like the textmodel S3 object from the
package
[quanteda.textmodels](https://github.com/quanteda/quanteda.textmodels).

If you don’t know what I am talking about, don’t worry, this package is
gracious. You don’t need to know a lot about Transformers to use this
package. See the examples below.

## Installation

You can install the development version of grafzahl like so:

``` r
remotes::install_github("chainsawriot/grafzahl")
```

After that, you need to setup your conda environment

``` r
require(grafzahl)
setup_grafzahl(cuda = TRUE) ## if you have GPU(s)
```

## Some common choices

| Your data         | model\_type | model\_name                        |
| ----------------- | ----------- | ---------------------------------- |
| English tweets    | bertweet    | vinai/bertweet-base                |
| Lightweight       | mobilebert  | google/mobilebert-uncased          |
|                   | distilbert  | distilbert-base-uncased            |
| Long Text         | longformer  | allenai/longformer-base-4096       |
|                   | bigbird     | google/bigbird-roberta-base        |
| English (General) | bert        | bert-base-uncased                  |
|                   | bert        | bert-base-cased                    |
|                   | electra     | google/electra-small-discriminator |
|                   | roberta     | roberta-base                       |
| Multilingual      | xlm         | xlm-mlm-17-1280                    |
|                   |             | xlm-mlm-100-1280                   |
|                   | bert        | bert-base-multilingual-uncased     |
|                   |             | bert-base-multilingual-cased       |
|                   | xlmroberta  | xlm-roberta-base                   |
|                   |             | xlm-roberta-large                  |

-----

1.  Yes, I totally made up the meaningless long name. Actually, it is
    the German name of the *Sesame Street* character [Count von
    Count](https://de.wikipedia.org/wiki/Sesamstra%C3%9Fe#Graf_Zahl),
    meaning “Count (the noble title) Number”. And it seems to be so that
    it is compulsory to name absolutely everything related to
    Transformers after Seasame Street characters.
