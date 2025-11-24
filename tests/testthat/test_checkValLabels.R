
dfSAV <- import_spss(file = test_path("helper_spss_missings.sav"))
load(file = test_path("helper_data.rda"))

df3 <- df2
df3$dat[1, 1:2] <- 8

iris2 <- iris
iris2[, "charVar"] <- "test"
suppressMessages(iris_g <- import_DF(iris2))

