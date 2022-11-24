require(quanteda)
devtools::load_all()
input <- rio::import(here::here("paper/sentences_ml.csv")) %>% tibble::as_tibble()


require(readtext)

input <- readtext(here::here("paper/sentences_ml.csv"), text_field = "headline") %>% corpus


## table(input$gold)

## training <- input$headline[!input$gold]
## y <- input$value[!input$gold]

training_corpus <- corpus_subset(input, !gold)

n <- rep(seq(500, 6000, by = 500), 10)
res <- list()
test_corpus<- corpus_subset(input, gold)
set.seed(721831)
for (i in seq_along(n)) {
    current_corpus <- corpus_sample(training_corpus, n[i])
    model <- grafzahl(x = current_corpus, y = "value", model_name = "GroNLP/bert-base-dutch-cased", output_dir = here::here("paper/va_size"))
        predicted_sentiment <- predict(model, newdata = test_corpus)
    res[[i]] <- caret::confusionMatrix(table(predicted_sentiment, gt = docvars(test_corpus, "value")), mode = "prec_recall")
}
saveRDS(res, here::here("paper/va_learning.RDS"))


