require(tidyverse)

res <- readRDS("va_learning.RDS")
n <- rep(seq(500, 6000, by = 500), 10)

acc <- purrr::map_dbl(res, ~.$overall['Accuracy'])

## Downright stole from Van Atteveldt.
## Except their Amsterdam style of R programming.
## https://github.com/vanatteveldt/ecosent/blob/36b84628ec908666ea8280593cb335c89c4e5e7e/src/analysis/performance.md

curve <- rbind(read_csv(here::here("paper/cnn_curve.csv")) %>% add_column(method="CNN", .before=1), read_csv(here::here("paper/svm_curve.csv")) %>% add_column(method="SVM", .before=1)) %>% group_by(method, perc) %>% summarize(n=mean(n), acc=mean(acc)) %>% ungroup

tibble::tibble(n, acc, method = "Transformer (BERTje)", perc = 0) %>% group_by(method, n) %>% summarise(acc = mean(acc)) %>% ungroup %>% add_column(perc = 1, .before = "n") %>% bind_rows(curve) -> curve


plot <- ggplot(curve, aes(x=n, y=acc, group=method, lty=method)) + geom_line() + 
  scale_linetype(name="Method") +
  xlab("Number of training examples") + ylab("Accuracy") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ggthemes::theme_clean() + theme(legend.position = "top", legend.background = element_blank(),
                                  plot.background  = element_blank())
saveRDS(plot, here::here("paper/learning.RDS"))
