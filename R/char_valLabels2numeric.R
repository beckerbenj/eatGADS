

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
              " cannot be coerced to numeric. This possibly corrupts all meta data. For other import behavior check out the 'labeledStrings' argument.")
    }

    if(identical(labeledStrings, "drop")) {
      #browser()
      warning("Some values with value labels or missing tags of variable ", char_var,
       " cannot be coerced to numeric and are therefore changed to NA. For other import behavior check out the 'labeledStrings' argument.")

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
      # create lookup table for all values
      #if(char_var == "TR_KURS_DEU") browser()
      all_oldValues <- unique(c(old_attributes$labels, old_attributes$na_values))
      all_oldValues <- all_oldValues[all_oldValues != ""] ## hotfix to circumvent issues with missing tags for empty strings; in other circumstances these are silently dropped anyway

      na_char_oldValues <- suppressWarnings(eatTools::asNumericIfPossible(x = all_oldValues, force.string = TRUE))
      num_oldValues <- na_char_oldValues[!is.na(na_char_oldValues)]
      char_oldValues <- all_oldValues[is.na(na_char_oldValues)]
      ## incorporate numeric values in actual values!
      raw_values <- rawDat[[char_var]]
      attributes(raw_values) <- NULL
      num_from_data <- suppressWarnings(eatTools::asNumericIfPossible(raw_values))
      #browser()

      # if no transformation is necessary move to next variable (e.g., only missing values are tagged & labelled)
      if(length(char_oldValues) == 0) next

      # assign former character values new numbers but skip already used numbers (prevent conflicts)
      lookup <- data.frame(oldValues = char_oldValues,
                           newValues = seq_but_skip(to = length(char_oldValues), skip = c(num_oldValues, num_from_data)))

      warning("Some values with value labels or missing tags of variable ", char_var,
              " cannot be coerced to numeric. These string values are recoded to numeric.")

      if(!is.null(new_attributes$labels)) {
        new_attributes$labels <- eatTools::recodeLookup(new_attributes$labels, lookup)
        new_attributes$labels <- as.numeric(new_attributes$labels)
        names(new_attributes$labels) <- names(old_attributes$labels)
      }
      if(!is.null(new_attributes$na_values)) {
        new_attributes$na_values <- eatTools::recodeLookup(new_attributes$na_values, lookup)
        new_attributes$na_values <- as.numeric(new_attributes$na_values)
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

seq_but_skip <- function(to, skip) {
  x <- seq(from = 1, to = to + length(skip))
  x2 <- setdiff(x, skip)
  x2[seq(from = 1, to = to)]
}
