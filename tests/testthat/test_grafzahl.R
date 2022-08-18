Sys.setenv(KILL_SWITCH = "KILL")
txt <- c(d1 = "Chinese Beijing Chinese",
          d2 = "Chinese Chinese Shanghai",
          d3 = "Chinese",
          d4 = "Tokyo Japan Chinese",
          d5 = "Chinese Chinese Chinese Tokyo Japan")
y <- factor(c("Y", "Y", "Y", "N", "Y"), ordered = TRUE)

test_that("basic", {
    expect_error(grafzahl(x = txt, y = y, model_name = "bert-base-cased", train_size = 1, num_train_epochs = 1), NA)
})
