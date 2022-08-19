require(grafzahl)

require(quanteda.textmodels)
require(quanteda)
set.seed(20190721)
model <- grafzahl(x = data_corpus_moviereviews, y = "sentiment",
                  model_type = "bert", model_name = "bert-base-uncased",
                  train_size = 1, num_train_epochs = 2)
preds <- predict(model)
table(preds, docvars(data_corpus_moviereviews, "sentiment"))

mr_vec <- as.vector(data_corpus_moviereviews)

sent <- docvars(data_corpus_moviereviews, "sentiment")

grafzahl(x = mr_vec, y = sent,
                  model_type = "bert", model_name = "bert-base-uncased",
                  train_size = 1, num_train_epochs = 2)
