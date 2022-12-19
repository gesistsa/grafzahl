require(RTextTools)
devtools::load_all()
data(USCongress)
USCongress$text <- as.character(USCongress$text)
USCongress$major <- as.factor(USCongress$major)
## training <- USCongress[1:4000,]
## test <- USCongress[4001:4449,]

require(quanteda)
USCongress_corpus <- corpus(USCongress$text)
docvars(USCongress_corpus, "major") <- as.numeric(USCongress$major) - 1
set.seed(721)
model <- grafzahl(USCongress_corpus[1:4000], model_type = "bert", model_name = "bert-base-cased", output_dir = here::here("paper/rtt"))
saveRDS(model, here::here("paper/tm_mod.RDS"))
output <- predict(model, newdata = USCongress_corpus[1:4000])

testset_out <- predict(model, newdata = USCongress_corpus[4001:4449])

u <- union(testset_out, docvars(USCongress_corpus, "major")[4001:4449])

tb <- table(factor(docvars(USCongress_corpus, "major")[4001:4449], u), factor(testset_out, u))
cb <- caret::confusionMatrix(tb)

mean(cb$byClass[,"F1"], na.rm = TRUE)

doc_matrix <- create_matrix(USCongress$text, language="english", removeNumbers=TRUE, stemWords=TRUE, removeSparseTerms=.998)
container <- create_container(doc_matrix, USCongress$major, trainSize=1:4000, testSize=4001:4449, virgin=FALSE)
SVM <- train_model(container,"SVM")


SVM_CLASSIFY <- classify_model(container, SVM)

analytics <- create_analytics(container, cbind(SVM_CLASSIFY))
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary

mean(alg_summary$SVM_FSCORE, na.rm = TRUE)

