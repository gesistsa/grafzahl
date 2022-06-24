
d <- readr::read_csv(here::here("rawdata/training-data.csv"), col_types="cccc")

# adding synthetic labels
d2 <- readr::read_csv(here::here("rawdata/synthetic-labels.csv"), col_types="cccc")
d <- rbind(d, d2)


d$text <- gsub('@[0-9_A-Za-z]+', '@', d$text)
d$uncivil_dummy <- ifelse(d$uncivil=="yes", 1, 0)

unciviltweets <- quanteda::corpus(d$text)

quanteda::docvars(unciviltweets, "uncivil") <- d$uncivil_dummy
usethis::use_data(unciviltweets)
