## currently not complete since originally these files were created manually

## helper_data.rda
load(file = test_path("helper_data.rda"))
# df1
df1_new <- import_DF(data.frame(ID1 = c(1, 2), V1 = c(3, 5)))
df1_new <- changeSPSSformat(df1_new, varName = "ID1", format = "F8.2")
df1_new <- changeSPSSformat(df1_new, varName = "V1", format = "F8.2")
# Hotfix to make data set comparable to initial data set
row.names(df1_new$labels) <- NULL
all.equal(df1_new, df1)

# df2
df2_new <- import_DF(data.frame(ID1 = c(1, 1), V2 = c(4, 8)))
df2_new <- changeSPSSformat(df2_new, varName = "ID1", format = "F8.2")
df2_new <- changeSPSSformat(df2_new, varName = "V2", format = "F10.2")
df2_new <- changeVarLabels(df2_new, varName = "V2", varLabel = "Variable 2")
df2_new <- changeValLabels(df2_new, varName = "V2", value = 99, valLabel = "mis")
df2_new <- changeMissings(df2_new, varName = "V2", value = 99, missings = "miss")
# Hotfix to make data set comparable to initial data set
row.names(df2_new$labels) <- NULL
all.equal(df2_new, df2)

# df1_2
df1_2_new <- import_DF(data.frame(ID1 = c(1, 2), V1 = c(3, 5), V3 = c(8, 9)))
all.equal(df1_2_new, df1_2)

# expected_bigList
expected_bigList <- mergeLabels(df1 = df1, df2 = df2)
expected_labels <- expected_bigList$allLabels

df1 <- df1_new
df2 <- df2_new
df1_2 <- df1_2_new
save(df1, df1_2, df2, expected_bigList, expected_labels, file = test_path("helper_data.rda"))
