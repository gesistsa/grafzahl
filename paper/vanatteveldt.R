require(quanteda)
devtools::load_all()
input <- rio::import(here::here("paper/sentences_ml.csv")) %>% tibble::as_tibble()


require(readtext)

input <- readtext(here::here("paper/sentences_ml.csv"), text_field = "headline") %>% corpus


## table(input$gold)

## training <- input$headline[!input$gold]
## y <- input$value[!input$gold]

training_corpus <- corpus_subset(input, !gold)


system.time(model <- grafzahl(x = training_corpus, y = "value", model_name = "GroNLP/bert-base-dutch-cased", output_dir = here::here("paper/va_output"), manual_seed = 721))
saveRDS(model, here::here("paper/va_model.RDS"))


##model <- hydrate(here::here("paper/va_output"), model_type = "bert")


## pred_bert <- predict(model, newdata = training)
## table(pred_bert, training_value)
model <- readRDS(here::here("paper/va_model.RDS"))

test_corpus<- corpus_subset(input, gold)
predicted_sentiment <- predict(model, newdata = test_corpus)

saveRDS(table(predicted_sentiment, gt = docvars(test_corpus, "value")), here::here("paper/va_cm.RDS"))


caret::confusionMatrix(readRDS(here::here("paper/va_cm.RDS")), mode = "prec_recall")

require(lime)
require(quanteda)
devtools::load_all()
input <- rio::import(here::here("paper/sentences_ml.csv")) %>% tibble::as_tibble()


require(readtext)

input <- readtext(here::here("paper/sentences_ml.csv"), text_field = "headline") %>% corpus


## table(input$gold)

## training <- input$headline[!input$gold]
## y <- input$value[!input$gold]

training_corpus <- corpus_subset(input, !gold)

model <- readRDS(here::here("paper/va_model.RDS"))

sentences <- c("Dijsselbloem pessimistisch over snelle stappen Grieken",
               "Aandelenbeurzen zetten koersopmars voort")
explainer <- lime(training_corpus, model)
explanations <- explain(sentences, explainer, n_labels = 1,
                        n_features = 5)
plot_text_explanations(explanations)
