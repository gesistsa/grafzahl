## https://arxiv.org/pdf/2101.06949.pdf
require(quanteda)
x <- readr::read_tsv(here::here("paper/hindi-train.csv"), col_names = FALSE)

input_corpus <- corpus(x$X2)
docvars(input_corpus, "outcome") <- x$X1

devtools::load_all()
set.seed(721)
## model <- grafzahl(input_corpus, model_type = "albert", model_name = "ai4bharat/indic-bert", output_dir = here::here("paper/hindi"), num_train_epoch = 20, train_size = 1)
## saveRDS(model, here::here("paper/hindi_model.RDS"))
model <- readRDS(here::here("paper/hindi_model.RDS"))
x2 <- readr::read_tsv(here::here("paper/hindi-test.csv"), col_names = FALSE)

test_corpus <- corpus(x2$X2)
docvars(test_corpus, "outcome") <- x2$X1

pred <- predict(model, newdata = test_corpus)

u <- union(pred, x2$X1)
t <- table(factor(pred, u), factor(x2$X1, u))
mean(caret::confusionMatrix(t)$byClass[,11])

caret::confusionMatrix(t)$byClass[,11]
