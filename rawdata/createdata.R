
d <- readr::read_csv(here::here("rawdata/training-data.csv"), col_types="cccc")

# adding synthetic labels
d2 <- readr::read_csv(here::here("rawdata/synthetic-labels.csv"), col_types="cccc")
d <- rbind(d, d2)


d$text <- gsub('@[0-9_A-Za-z]+', '@', d$text)
d$uncivil_dummy <- ifelse(d$uncivil=="yes", 1, 0)

unciviltweets <- quanteda::corpus(d$text)

quanteda::docvars(unciviltweets, "uncivil") <- d$uncivil_dummy
usethis::use_data(unciviltweets)

set.seed(123)
smallunciviltweets <- quanteda::corpus_sample(unciviltweets, 200)
usethis::use_data(smallunciviltweets)

download.file(url <- "https://raw.githubusercontent.com/vanatteveldt/ecosent/master/data/intermediate/sentences_ml.csv", "rawdata/sentences_ml.csv")

ecosent <- read.csv("rawdata/sentences_ml.csv", encoding = "UTF-8")[c("id", "headline", "value", "gold")]
save(ecosent, file = "data/ecosent.rda", ascii = FALSE, compress = "xz")

supported_model_types <- c("albert", "bert", "bertweet", "bigbird", "camembert", "deberta", "distilbert", "electra", "flaubert",
                           "herbert", "layoutlm", "layoutlmv2", "longformer", "mpnet", "mobilebert", "rembert", "roberta", "squeezebert",
                           "squeezebert", "xlm", "xlmroberta", "xlnet", "debertav2")
usethis::use_data(supported_model_types)
