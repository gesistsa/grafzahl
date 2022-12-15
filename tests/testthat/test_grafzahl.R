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

test_that(".infer local", {
    expect_error(.infer_model_type("../testdata/fake"), NA)
    expect_equal(.infer_model_type("../testdata/fake"), "xlm-roberta")
    ## Integration with .check_model_type
    expect_equal(.check_model_type(model_type = NULL, model_name = "../testdata/fake"), "xlmroberta")
    ## Integration with grafzahl
    expect_error(grafzahl(x = txt, y = y, model_name = "../testdata/fake", train_size = 1, num_train_epochs = 1), NA)
})

test_that(".check_model_type", {
    expect_error(.check_model_type(model_type = NULL))
    expect_error(.check_model_type(model_type = "idk"))
    expect_error(.check_model_type(model_type = "xlmroberta", model_name = "xlm-roberta-base"), NA)
    expect_error(.check_model_type(model_type = "xlm-roberta", model_name = "xlm-roberta-base"), NA)
    expect_error(.check_model_type(model_type = "XLM-roberta", model_name = "xlm-roberta-base"), NA)
    ## Integration with grafzahl
    expect_error(grafzahl(x = txt, y = y, model_type = "idk", model_name = "bert-base-cased", train_size = 1, num_train_epochs = 1))
    expect_error(grafzahl(x = txt, y = y, model_type = "xlmroberta", model_name = "xlm-roberta-base", train_size = 1, num_train_epochs = 1), NA)
    expect_error(grafzahl(x = txt, y = y, model_type = "xlm-roberta", model_name = "xlm-roberta-base", train_size = 1, num_train_epochs = 1), NA)
    expect_error(grafzahl(x = txt, y = y, model_type = "XLM-roberta", model_name = "xlm-roberta-base", train_size = 1, num_train_epochs = 1), NA)    
})

Sys.setenv(KILL_SWITCH = "")
