
## This code is modified from the follow two files by Pablo Barbera
## https://github.com/pablobarbera/incivility-sage-open/blob/master/02-classifier.R
## https://github.com/pablobarbera/incivility-sage-open/blob/master/functions.R

## I just make it compatible with R 4.3 and qanteda 3.x.

devtools::load_all()
require(quanteda)
uncivildfm <- unciviltweets %>% tokens(remove_url = TRUE, remove_numbers = TRUE) %>% tokens_wordstem() %>% dfm() %>% dfm_remove(stopwords("english")) %>% dfm_trim(min_docfreq = 2)
y <- docvars(unciviltweets)[,1]
seed <- 123
set.seed(seed)
training <- sample(1:length(y), floor(.80 * length(y)))
test <- (1:length(y))[1:length(y) %in% training == FALSE]

## so-called downsample

small_class <- which.min(table(y[training])) - 1
n_small_class <- sum(y[training] == small_class)
downsample <- sample(training[y[training] != small_class], n_small_class, replace = TRUE)
training <- c(training[y[training] == small_class], downsample)

X <- as(uncivildfm, "dgCMatrix")

lasso <- glmnet::cv.glmnet(x = X[training,], y = y[training], alpha = 1, nfold = 5, family = "binomial")

preds <- predict(lasso, uncivildfm[test,], type="response")

pROC::auc(as.vector((y[test])*1), as.vector((preds)*1))


cm <- caret::confusionMatrix(table(y[test], ifelse(preds > .5, 1, 0)))
cm$byClass

## For future paper writing.
tibble::tibble(truth = y[test], preds = as.vector(preds)) -> res
saveRDS(res, here::here("paper/theocharis.RDS"))

origin_training <- setdiff(1:length(y), test)

## Don't train again
set.seed(721)
model <- grafzahl(unciviltweets[origin_training], model_type = "bertweet", model_name = "vinai/bertweet-base", output_dir = here::here("paper/output"))
saveRDS(model, here::here("paper/theo.RDS"))
##model <- hydrate(here::here("paper/output"), model_type = "bertweet")
##model <- readRDS(here::here("paper/theo.RDS"))
pred_bert <- predict(model, unciviltweets[test])
caret::confusionMatrix(table(y[test], pred_bert))$byClass

pred_bert2 <- predict(model, unciviltweets[test], return_raw = TRUE)
##pROC::auc(as.vector((y[test])*1), pred_bert2[,1])
##pROC::auc(as.vector((y[test])*1), preds)

##plot(pROC::roc(as.vector((y[test])*1), pred_bert2[,1]))

## require(ROCR)

## performance_bert <- performance(prediction(pred_bert2[,2], y[test]), "tpr", "fpr")
## performance_origin <- performance(prediction(preds, y[test]), "tpr", "fpr")


## plot(performance_origin)
## abline(a = 0, b = 1, col = "grey")
## plot(performance_bert, add = TRUE, col = "red")
