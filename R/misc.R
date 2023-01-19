utils::globalVariables(c("py_train", "py_predict", "py_detect_cuda"))

#' @importFrom stats predict runif
#' @importFrom utils download.file installed.packages tail
NULL

#' A Corpus Of Tweets With Incivility Labels
#'
#' This is a dataset from the paper "The Dynamics of Political Incivility on Twitter". The tweets were by Members of Congress elected to the 115th Congress (2017–2018). It is important to note that not all the incivility labels were coded by human. Majority of the labels were coded by the Google Perspective API. All mentions were removed. The dataset is available from Pablo Barbera's Github. <https://github.com/pablobarbera/incivility-sage-open>
#' 
#' @references
#' Theocharis, Y., Barberá, P., Fazekas, Z., & Popa, S. A. (2020). The dynamics of political incivility on Twitter. Sage Open, 10(2), 2158244020919447.
"unciviltweets"

#' A Corpus Of Dutch News Headlines 
#'
#' This is a dataset from the paper "The Validity of Sentiment Analysis: Comparing Manual Annotation, Crowd-Coding, Dictionary Approaches, and Machine Learning Algorithms."
#' The data frame contains four columns: id (identifier), headline (the actual text data), value (sentiment: 0 Neutral, +1 Positive, -1 Negative), gold (whether or not this row is "gold standard", i.e. test set). The data is available from Wouter van Atteveldt's Github. <https://github.com/vanatteveldt/ecosent>
#' @references
#' Van Atteveldt, W., Van der Velden, M. A., & Boukes, M. (2021). The validity of sentiment analysis: Comparing manual annotation, crowd-coding, dictionary approaches, and machine learning algorithms. Communication Methods and Measures, 15(2), 121-140.
"ecosent"

#' Download The Amharic News Text Classification Dataset
#'
#' This function downloads the training and test sets of the Amharic News Text Classification Dataset from Hugging Face.
#'
#' @return A named list of two corpora: training and test
#' @references Azime, Israel Abebe, and Nebil Mohammed (2021). "An Amharic News Text classification Dataset." arXiv preprint arXiv:2103.05639
#' @export
get_amharic_data <- function() {
    current_tempdir <- tempdir()
    download.file("https://huggingface.co/datasets/israel/Amharic-News-Text-classification-Dataset/resolve/main/train.csv",
                  destfile = file.path(current_tempdir, "train.csv"))
    download.file("https://huggingface.co/datasets/israel/Amharic-News-Text-classification-Dataset/resolve/main/test.csv",
                  destfile = file.path(current_tempdir, "test.csv"))
    training <- utils::read.csv(file.path(current_tempdir, "train.csv"))
    test <- utils::read.csv(file.path(current_tempdir, "test.csv"))
    training <- training[training$category != "",]
    test <- test[test$category != "",]
    training_corpus <- quanteda::corpus(training$article)
    quanteda::docvars(training_corpus, "category") <- training$category
    test_corpus <- quanteda::corpus(test$article)
    quanteda::docvars(test_corpus, "category") <- test$category
    return(list(training = training_corpus, test = test_corpus))
}

## .count_lang <- function(x, force_cld2 = FALSE, sampl = NULL, safety_multiplier = 1.5) {
##     if (isTRUE("cld3" %in% row.names(installed.packages())) & !force_cld2) {
##         trans_fun <- cld3::detect_language
##     } else {
##         trans_fun <- cld2::detect_language
##     }
##     if (length(x) > 5000 & !is.null(sampl)) {
##         if (sampl >= 1 | sampl <= 0) {
##             stop("`sampl` must be < 1 and > 0.")
##         }
##         x <- sample(x, ceiling(length(x) * sampl))
##     }
##     longest <- max(quanteda::ntoken(x, what = "word"), na.rm = TRUE) * safety_multiplier
##     res <- trans_fun(x)
##     non_na_res <- res[!is.na(res)]
##     freqcount <- table(non_na_res)
##     majority_lang <- tail(names(sort(freqcount)), 1)
##     majority_n <- max(table(non_na_res))
##     ratio <- majority_n / length(non_na_res)
##     caseness <- stringi::stri_detect_charclass(x, "\\p{Lu}")
##     output <- list(n = length(res), valid_n = length(non_na_res),
##                    maj_lang = majority_lang, maj_ratio = ratio,
##                    caseness = any(caseness), longest = longest)
##     return(output)
## }

