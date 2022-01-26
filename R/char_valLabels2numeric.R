

## Transform labeled character values (possible in SPSS and haven) to numeric
char_valLabels2numeric <- function(rawDat, labeledStrings) {
  UseMethod("char_valLabels2numeric")
}


## Function works for labelled and unlabelled time and date variables (which are handeled very inconsistently by haven under user_na = T)
#'@export
char_valLabels2numeric.savDat<- function(rawDat, labeledStrings) {

  char_values_marker <- unlist(lapply(rawDat, function(single_var) {
    ## should I inspect missing tags as well here?
    labs <- extract_attribute_vector(single_var, attr_type = "labels")
    na_values <- extract_attribute_vector(single_var, attr_type = "na_values")
    !can_be_numeric(c(labs, na_values))
  }))

  if(all(!char_values_marker)) return(rawDat)

  for(char_var in names(char_values_marker)[char_values_marker]) {
    old_attributes <- attributes(rawDat[[char_var]])
    new_attributes <- old_attributes

    if(identical(labeledStrings, "keep")) { ## corresponding behavior in extract_labels has to be adapted!
      warning("Some values with value labels or missing tags of variable ", char_var,
              " cannot be coerced to numeric. This possibly corrupts all meta data.")
    }

    if(identical(labeledStrings, "drop")) {
      #browser()
      warning("Some values with value labels or missing tags of variable ", char_var,
       " cannot be coerced to numeric and are therefore changed to NA.")

      if(!is.null(new_attributes$labels)) {
        suppressWarnings(new_attributes$labels <- eatTools::asNumericIfPossible(x = new_attributes$labels,
                                                    maintain.factor.scores = TRUE, force.string = TRUE, transform.factors = TRUE))
        names(new_attributes$labels) <- names(old_attributes$labels)
      }

      if(!is.null(new_attributes$na_values)){
        suppressWarnings(new_attributes$na_values <- eatTools::asNumericIfPossible(x = new_attributes$na_values,
                                                    maintain.factor.scores = TRUE, force.string = TRUE, transform.factors = TRUE))
        #names(new_attributes$labels) <- names(old_attributes$labels)
      }
    }
    if(identical(labeledStrings, "transform")) {
      warning("Some values with value labels or missing tags of variable ", char_var,
              " cannot be coerced to numeric. Therefore all underlying values are recoded to numeric.")

      # create lookup table for alle values
      oldValues <- unique(c(old_attributes$labels, old_attributes$na_values))
      lookup <- data.frame(oldValues = oldValues,
                           newValues = seq(length(oldValues)))

      if(!is.null(new_attributes$labels)) {
        new_attributes$labels <- eatTools::recodeLookup(new_attributes$labels, lookup)
        names(new_attributes$labels) <- names(old_attributes$labels)
      }
      if(!is.null(new_attributes$na_values)) {
        new_attributes$na_values <- eatTools::recodeLookup(new_attributes$na_values, lookup)
      }

      rawDat[[char_var]] <- eatTools::recodeLookup(rawDat[[char_var]], lookup)
    }

    attributes(rawDat[[char_var]]) <- new_attributes
  }
  rawDat
}

#'@export
char_valLabels2numeric.data.frame <- function(rawDat, labeledStrings = labeledStrings) {
  rawDat
}



# extract attributes and produce NA for not given attributes
extract_attribute_vector <- function(var, attr_type) {
  out <- attr(var, attr_type, exact = TRUE)
  if(is.null(out)) out <- NA
  out
}

can_be_numeric <- function(x) {
  stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  numNAs <- sum(is.na(x))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
  return(numNAs_new == numNAs)
}
