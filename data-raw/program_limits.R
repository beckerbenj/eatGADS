program_limits_matlist1 <- list(x = matrix(c(64, 256, 120, 32767, 2^31-1, 2^31-1,
                                             32, 80, 30000, 2*10^6, 2^31-29, 32767,
                                             32, 80, 30000, 2*10^6, 2^31-29, 2048,
                                             32, 80, 30000, 2*10^6, 2^40-1, 120000),
                                           ncol = 4, nrow = 6,
                                           dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                             "nrows", "ncols"),
                                                           c("SPSS", "Stata", "Stata 19/BE", "Stata 19/MP"))),
                                unit = matrix(c("byte", "char", "char", "byte", "generic", "generic",
                                                "char", "char", "char", "byte", "generic", "generic",
                                                "char", "char", "char", "byte", "generic", "generic",
                                                "char", "char", "char", "byte", "generic", "generic"),
                                              ncol = 4, nrow = 6,
                                              dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                                "nrows", "ncols"),
                                                              c("SPSS", "Stata", "Stata 19/BE", "Stata 19/MP"))))

program_limits_matlist2 <- list(SPSS = matrix(c(64, 256, 120, 32767, 2^31-1, 2^31-1,
                                                "byte", "char", "char", "byte", "generic", "generic"),
                                              ncol = 2, nrow = 6,
                                              dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                                "nrows", "ncols"),
                                                              c("x", "unit"))),
                                Stata = matrix(c(32, 80, 30000, 2*10^6, 2^31-29, 32767,
                                               "char", "char", "char", "byte", "generic", "generic"),
                                                 ncol = 2, nrow = 6,
                                                 dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                                   "nrows", "ncols"),
                                                                 c("x", "unit"))),
                                Stata_19_BE = matrix(c(32, 80, 30000, 2*10^6, 2^31-29, 2048,
                                                     "char", "char", "char", "byte", "generic", "generic"),
                                                     ncol = 2, nrow = 6,
                                                     dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                                       "nrows", "ncols"),
                                                                     c("x", "unit"))),
                                Stata_19_MP = matrix(c(32, 80, 30000, 2*10^6, 2^40-1, 120000,
                                                     "char", "char", "char", "byte", "generic", "generic"),
                                                     ncol = 2, nrow = 6,
                                                     dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                                       "nrows", "ncols"),
                                                                     c("x", "unit"))))

program_limits_df1 <- data.frame(SPSS_x = c(64, 256, 120, 32767, 2^31-1, 2^31-1),
                                 SPSS_unit = c("byte", "char", "char", "byte", "generic", "generic"),
                                 Stata_x = c(32, 80, 30000, 2*10^6, 2^31-29, 32767),
                                 Stata_unit = c("char", "char", "char", "byte", "generic", "generic"),
                                 Stata_19_BE_x = c(32, 80, 30000, 2*10^6, 2^31-29, 2048),
                                 Stata_19_BE_unit = c("char", "char", "char", "byte", "generic", "generic"),
                                 Stata_19_MP_x = c(32, 80, 30000, 2*10^6, 2^40-1, 120000),
                                 Stata_19_MP_unit = c("char", "char", "char", "byte", "generic", "generic"))
rownames(program_limits_df1) <- c("varNames", "varLabels", "valLabels", "stringvars", "nrows", "ncols")

program_limits_df2 <- data.frame(SPSS_x = c(64, "byte", 256, "char", 120, "char", 32767, "byte",
                                            2^31-1, "generic", 2^31-1, "generic"),
                                 Stata_x = c(32, "char", 80, "char", 30000, "char", 2*10^6, "byte",
                                             2^31-29, "generic", 32767, "generic"),
                                 Stata_19_BE = c(32, "char", 80, "char", 30000, "char", 2*10^6, "byte",
                                                   2^31-29, "generic", 2048, "generic"),
                                 Stata_19_MP = c(32, "char", 80, "char", 30000, "char", 2*10^6, "byte",
                                                   2^40-1, "generic", 120000, "generic"))
rownames(program_limits_df2) <- c("varNames_x", "varNames_unit", "varLabels_x", "varLabels_unit",
                                  "valLabels_x", "valLabels_unit", "stringvars_x", "stringvars_unit",
                                  "nrows_x", "nrows_unit", "ncols_x", "ncols_unit")
program_limits_matlist1
program_limits_matlist2
program_limits_df1
program_limits_df2
program_limits <- program_limits_matlist1
save(program_limits, file = "data/program_limits.rda")
