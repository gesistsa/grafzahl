require(lime)
devtools::load_all()
library(caret)
library(lime)

# Split up the data set
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Create Random Forest model on iris data
model <- train(iris_train, iris_lab, method = 'nb')

predict_model(model, newdata = iris_test, type = "prob")

# Create an explainer object
explainer <- lime(iris_train, model)

model <- readRDS(here::here("paper/va_model.RDS"))


y <- predict(model, return_raw = TRUE)

input <- rio::import(here::here("paper/sentences_ml.csv")) %>% tibble::as_tibble()

table(input$gold)

training <- input$headline[!input$gold]
y <- input$value[!input$gold]
testset <- input$headline[input$gold]
predict_model(model, testset, type = "rawaa")


explainer <- lime(training, model)

explanation <- explain(testset[c(2, 4, 284)], explainer, n_labels = 1, n_features = 5)

