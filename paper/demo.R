devtools::load_all()

require(quanteda)

unciviltweets

##
## x_cor <- corpus(x)
## docvars(x_cor, "outcome") <- y

docvars(unciviltweets, "uncivil")

seed <- 123
set.seed(seed)

y <- docvars(unciviltweets, "uncivil")

## Carbon copy from Barbera. Don't blame me for the ugliness.
training <- sample(1:length(y), floor(.80 * length(y)))
test <- (1:length(y))[1:length(y) %in% training == FALSE]

origin_training <- setdiff(1:length(y), test)

set.seed(721)
model <- grafzahl(unciviltweets[origin_training],
                  model_type = "bertweet", model_name = "vinai/bertweet-base",
                  output_dir = here::here("paper/output"))

## https://simpletransformers.ai/docs/classification-specifics/#supported-model-types

## https://huggingface.co/models

## planned feature

.count_lang(unciviltweets)

## model is like a regular glm or textmodel_* object

pred_bert <- predict(model, unciviltweets[test])

caret::confusionMatrix(table(y[test], pred_bert))$byClass

