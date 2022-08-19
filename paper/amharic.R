require(quanteda)
## require(grafzahl)
devtools::load_all()
## there is only one article in the category of "". It is removed here
## It is too big and needs to be run on Jarvis.

set.seed(721)
input <- readtext::readtext(here::here("paper/am_train.csv"), text_field = "article") %>% corpus %>% corpus_subset(category != "")

model <- grafzahl(x = input, y = "category",
                  model_type = "xlmroberta",
                  model_name = "castorini/afriberta_base",
                  output_dir = here::here("paper/amharic"),
                  manual_seed = 721)
saveRDS(model, "amharic.RDS")

testset_corpus <- readtext::readtext(here::here("paper/am_test.csv"), text_field = "article") %>% corpus %>% corpus_subset(category != "")

preds <- predict(model, newdata = testset_corpus)
caret::confusionMatrix(table(preds, docvars(testset_corpus, "category")))

input <- readtext::readtext(here::here("paper/am_train.csv"), text_field = "article") %>% corpus %>% corpus_subset(category != "")

model <- grafzahl(x = input,
                  y = "category",
                  model_name = here::here("paper/localafriberta"),
                  output_dir = here::here("paper/amharic"),
                  manual_seed = 721)
testset_corpus <- readtext::readtext(here::here("paper/am_test.csv"), text_field = "article") %>% corpus %>% corpus_subset(category != "")

preds <- predict(model, newdata = testset_corpus)
caret::confusionMatrix(table(preds, docvars(testset_corpus, "category")))
