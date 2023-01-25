## test_that("setup", {
##     skip_on_cran()
##     testbed <- file.path(tempdir(), "testbed")
##     dir.create(testbed)
##     withr::local_envvar(GRAFZAHL_MINICONDA_PATH = testbed)
##     expect_error(setup_grafzahl(force = TRUE), NA)
##     expect_true(detect_conda())
##     expect_false(detect_cuda())
##     txt <- c(d1 = "Chinese Beijing Chinese",
##              d2 = "Chinese Chinese Shanghai",
##              d3 = "Chinese",
##              d4 = "Tokyo Japan Chinese",
##              d5 = "Chinese Chinese Chinese Tokyo Japan")
##     y <- factor(c("Y", "Y", "Y", "N", "Y"), ordered = TRUE)

##     expect_error(model <- grafzahl(x = txt, y = y, train_size = 1,
##                                    num_train_epochs = 1,
##                                    model_name = "bert-base-cased",
##                                    cuda = FALSE), NA)
##     expect_error(predict(model, cuda = FALSE), NA)
## })
