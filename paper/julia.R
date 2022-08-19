require(dplyr)
require(quanteda)

en_data <- rio::import(here::here("paper/goldstandard_ic_en.csv")) %>% tibble::as_tibble() %>% filter(!is_redacted & main_language == "en" & WC > 0)
en_data %>% pull(post) %>% corpus -> en_corpus
docvars(en_corpus, "icom") <- as.numeric(en_data$ic_ordinal)

docnames(en_corpus) <- paste0("en", seq_along(en_corpus))

de_data <- rio::import(here::here("paper/goldstandard_ic_de.csv")) %>% tibble::as_tibble() %>% filter(!is_redacted & main_language == "de" & WC > 0)
de_data %>% pull(post) %>% corpus -> de_corpus
docvars(de_corpus, "icom") <- as.numeric(de_data$ic_ordinal)

docnames(de_corpus) <- paste0("de", seq_along(de_corpus))


set.seed(2020)
en_ranid <- sample(1:10, size = ndoc(en_corpus), replace = TRUE)

set.seed(2020)
de_ranid <- sample(1:10, size = ndoc(de_corpus), replace = TRUE)
devtools::load_all()

set.seed(2020)

rmse <- function(x, y) {
    sqrt(mean((x - y)^2))
}

fold <- function(i, en_corpus, de_corpus, en_ranid, de_ranid) {
    mod <- grafzahl(en_corpus[en_ranid != i] + de_corpus[de_ranid != i],
                    model_type = "distilbert",
                    model_name = "distilbert-base-multilingual-cased",
                    output_dir = here::here(paste0("paper/julia", i)),
                    regression = TRUE)
    pred_en <- predict(mod, en_corpus[en_ranid == i])
    pred_de <- predict(mod, de_corpus[de_ranid == i])
    x1 <- cor(docvars(en_corpus, "icom")[en_ranid == i], pred_en)
    x2 <- rmse(docvars(en_corpus, "icom")[en_ranid == i], pred_en)
    x3 <- cor(docvars(de_corpus, "icom")[de_ranid == i], pred_de)
    x4 <- rmse(docvars(de_corpus, "icom")[de_ranid == i], pred_de)
    return(tibble(i = i, en_cor = x1, en_rmse = x2, de_cor = x3, de_rmse = x4))
}

res <- purrr::map_dfr(1:10, fold, en_corpus = en_corpus, de_corpus = de_corpus, en_ranid = en_ranid, de_ranid = de_ranid)

saveRDS(res, here::here("paper/julia_cv.RDS"))
