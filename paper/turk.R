require(quanteda)
input <- readtext::readtext(here::here("paper/offenseval2020-turkish/offenseval-tr-training-v1/offenseval-tr-training-v1.tsv"), text_field = "tweet", quote = "") %>% corpus

set.seed(721)
model <- grafzahl(x = input,
                  y = "subtask_a",
                  model_type = "bert",
                  model_name = "dbmdz/bert-base-turkish-cased",
                  output_dir = here::here("paper/turkmodel"))

test <- rio::import(here::here("paper/offenseval2020-turkish/offenseval-tr-testset-v1/offenseval-tr-testset-v1.tsv"), quote = "")

labels <- rio::import(here::here("paper/offenseval2020-turkish/offenseval-tr-testset-v1/offenseval-tr-labela-v1.tsv"), quote = "")

colnames(labels)[1] <- "id"
colnames(labels)[2] <- "subtask_a"

require(dplyr)
test %>% left_join(labels) -> test


corpus(test, text_field = "tweet") -> test_corpus

preds <- predict(model, newdata = test_corpus)

sum(caret::confusionMatrix(table(preds, docvars(test_corpus, "subtask_a")), mode = "prec_recall", positive = "OFF")$byClass["F1"], caret::confusionMatrix(table(preds, docvars(test_corpus, "subtask_a")), mode = "prec_recall", positive = "NOT")$byClass["F1"]) / 2
