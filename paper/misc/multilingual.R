require(readr)

all_dirs <- tail(list.dirs(here::here("paper/sentiment")), -1)

.reading <- function(dir, file = "train.tsv") {
    res <- readr::read_tsv(file.path(dir, file), col_names = FALSE)
    res$dir <- dir
    return(res)
}

training_set <- suppressMessages(purrr::map_dfr(all_dirs, .reading))

test_set <- suppressMessages(purrr::map_dfr(all_dirs, .reading, file = "test.tsv"))


devtools::load_all()

require(quanteda)
training_corpus <- corpus(training_set$X2)
docvars(training_corpus, "sentiment") <- training_set$X1

set.seed(1212121)
##model <- grafzahl(training_corpus, model_type = "distilbert", model_name = "distilbert-base-multilingual-cased", output_dir = here::here("paper/multi"))
##saveRDS(model, here::here("paper/multimodel.RDS"))
model <- readRDS(here::here("paper/multimodel.RDS"))
training_pred <- predict(model, training_corpus)

table(docvars(training_corpus, "sentiment"), training_pred)

test_pred <- predict(model, corpus(test_set$X2))

dir <- unique(test_set$dir)

dir[4]

i <- 1
.acc <- function(i) {
    F1 <- caret::confusionMatrix(table(test_set$X1[test_set$dir == dir[i]], test_pred[test_set$dir == dir[i]]))$byClass["F1"]
    lang <- dir[i]
    tibble::tibble(lang, F1)
}

purrr::map_dfr(seq_along(dir), .acc)
