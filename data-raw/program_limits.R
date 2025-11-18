program_limits <- data.frame(component = unlist(lapply(c("varNames", "varLabels", "valLabels", "stringvars", "nrows", "ncols"),
                                                       rep, 4)),
                             program = c("SPSS", "Stata", "Stata 19/BE", "Stata 19/MP"),
                             value = c(64, 32, 32, 32,
                                       256, 80, 80, 80,
                                       120, 30000, 30000, 30000,
                                       32767, 2*10^6, 2*10^6, 2*10^6,
                                       2^31-1, 2^31-29, 2^31-29, 2^40-1,
                                       2^31-1, 32767, 2048, 120000),
                             unit = c("byte", "char", "char", "char",
                                      "char", "char", "char", "char",
                                      "char", "char", "char", "char",
                                      "byte", "byte", "byte", "byte",
                                      "generic", "generic", "generic", "generic",
                                      "generic", "generic", "generic", "generic"),
                             stringsAsFactors = FALSE)
class(program_limits) <- c("program_limits_df", class(program_limits))
save(program_limits, file = "data/program_limits.rda")
