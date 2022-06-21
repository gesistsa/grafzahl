
<!-- README.md is generated from README.Rmd. Please edit that file -->

# grafzahl

<!-- badges: start -->

<!-- badges: end -->

WIP: The goal of grafzahl (**G**racious **R** **A**nalytical
**F**ramework for **Z**ero-shot **A**nalysis of **H**uman **L**anguages
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

-----

1.  Yes, I totally made up the meaningless long name. Actually, it is
    the German name of the *Sesame Street* character [Count von
    Count](https://de.wikipedia.org/wiki/Sesamstra%C3%9Fe#Graf_Zahl),
    meaning “Count (the noble title) Number”. And it seems to be so that
    it is compulsory to name absolutely everything related to
    Transformer after Seasame Street characters.
