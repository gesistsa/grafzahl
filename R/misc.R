#' A Corpus Of Tweets With Incivility Labels
#'
#' This is a dataset from the paper "The Dynamics of Political Incivility on Twitter". The tweets were by Members of Congress elected to the 115th Congress (2017–2018). It is important to note that not all the incivility labels were coded by human. Majority of the labels were coded by the Google Perspective API. All mentions were removed. The dataset is available from Pablo Barberá's Github. https://github.com/pablobarbera/incivility-sage-open 
#' 
#' @references
#' Theocharis, Y., Barberá, P., Fazekas, Z., & Popa, S. A. (2020). The dynamics of political incivility on Twitter. Sage Open, 10(2), 2158244020919447.
"unciviltweets"

.count_lang <- function(x, force_cld2 = FALSE, sampl = NULL, safety_multiplier = 1.5) {
    if (isTRUE("cld3" %in% row.names(installed.packages())) & !force_cld2) {
        trans_fun <- cld3::detect_language
    } else {
        trans_fun <- cld2::detect_language
    }
    if (length(x) > 5000 & !is.null(sampl)) {
        if (sampl >= 1 | sampl <= 0) {
            stop("`sampl` must be < 1 and > 0.")
        }
        x <- sample(x, ceiling(length(x) * sampl))
    }
    longest <- max(quanteda::ntoken(x, what = "word"), na.rm = TRUE) * safety_multiplier
    res <- trans_fun(x)
    non_na_res <- res[!is.na(res)]
    freqcount <- table(non_na_res)
    majority_lang <- tail(names(sort(freqcount)), 1)
    majority_n <- max(table(non_na_res))
    ratio <- majority_n / length(non_na_res)
    caseness <- stringi::stri_detect_charclass(x, "\\p{Lu}")
    output <- list(n = length(res), valid_n = length(non_na_res),
                   maj_lang = majority_lang, maj_ratio = ratio,
                   caseness = any(caseness), longest = longest)
    return(output)
}

