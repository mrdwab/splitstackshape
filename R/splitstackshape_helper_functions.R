# Trim Whitespace Fallback -- Don't Export --------------------------------
.tws <- trim <- function(vec) {
  sw <- startsWith(vec, " ")
  ew <- endsWith(vec, " ")
  if (any(sw, na.rm = TRUE)) {
    vec[which(sw)] <- sub("^ +", "", vec[which(sw)])
  }
  if (any(ew, na.rm = TRUE)) {
    vec[which(ew)] <- sub(" +$", "", vec[which(ew)])
  }
  vec
}
NULL

# stri_flatten Fallback -- Don't Export -----------------------------------
.strflat <- function(invec) paste(invec, collapse = "\n")
NULL

# Make Equal Columns -- Don't Export --------------------------------------
make_equal <- function(indt, splitCols, verbose = TRUE) {
  check <- all_names(names(indt), stubs = splitCols, end_stub = FALSE, verbose)
  
  if (length(check[["miss"]]) > 0L) {
    nat <- vapply(unname(check[["stubs"]]), function(x) {
      typeof(indt[[grep(x, names(indt), fixed = TRUE)[[1L]]]])
    }, character(1L))
    
    for (i in seq_along(nat)) {
      COLS <- grep(names(nat[i]), check[["miss"]], value = TRUE, fixed = TRUE)
      if (length(COLS) > 0L) indt[, (COLS) := NA_type(nat[i])]
    } 
  } 
  
  setcolorder(indt, check[["full_names"]])
}
NULL

# NA Types -- Don't Export ------------------------------------------------
NA_type <- function(string) {
  switch(string,
         double = NA_real_,
         integer = NA_integer_,
         complex = NA_complex_,
         character = NA_character_,
         NA)
}
NULL

# All Names for a Balanced Dataset -- Don't Export ------------------------
all_names <- function(current_names, stubs, end_stub = FALSE, ids = NULL, 
                      keep_all = TRUE, verbose = TRUE) {
  V1 <- V2 <- NULL
  stub_names <- grep(paste(stubs, collapse = "|"), current_names, value = TRUE)
  stub_names <- stub_names[!stub_names %in% stubs]
  stub_list <- setNames(
    lapply(stubs, function(x) stub_names[grepl(x, stub_names)]), stubs)
  
  if (is.null(ids)) {
    if (verbose) message("All non-stub names being used as ids")
    ids <- setdiff(current_names, stub_names)
  }
  
  if (!is.null(ids)) {
    ids <- if (keep_all) setdiff(current_names, stub_names) else ids
  }
  
  id_names <- ids

  levs <- unique(gsub(paste(stubs, collapse = "|"), "", stub_names))
  stub_levs <- trim_vec(CJ(stubs, levs)[, if (end_stub) paste0(V2, V1) else paste0(V1, V2)])
  full_names <- c(id_names, stub_levs[order(stub_levs)])
  levs <- gsub("^[[:punct:]]|[[:punct:]]$", "", levs)
  levs <- levs[order(levs)]
  miss <- setdiff(full_names, current_names)
  list(stubs = stubs,
       stub_names = stub_names, 
       stub_list = stub_list,
       id_names = id_names, 
       levs = levs, 
       stub_levs = stub_levs,
       full_names = full_names,
       miss = miss)
}
NULL

# Long Fixer -- Don't Export ----------------------------------------------
long_fixer <- function(indt, cols) {
  temp <- rowSums(is.na(indt[, ..cols])) + rowSums(indt[, ..cols] == "", na.rm = TRUE)
  temp == length(cols)
}
NULL



#' @name dataset_names
#' @rdname dataset_names
#' @title Name Convenience Functions
#' 
#' @description `othernames` is a convenience function for 
#' `setdiff(names(data), -some_vector_of_names-)`. `Names` is a convenience 
#' function using either character vectors or numeric vectors to specify a 
#' subset of [base::names()] of a dataset, returning the names as a character 
#' vector, always.
#' 
#' @param data The input dataset.
#' @param toremove The [base::names()] you want to exclude.
#' @param invec The [base::names()] you want to keep.
#' @return A character vector of the remaining names.
#' @author Ananda Mahto
#' @seealso [base::setdiff()]
NULL


#' @rdname dataset_names
#' 
#' @examples
#' 
#' mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' splitstackshape:::othernames(mydf, "a")
#' 
#' @aliases othernames
othernames <- function(data, toremove) {
  setdiff(names(data), Names(data, toremove))
}
NULL

#' @rdname dataset_names
#' 
#' @examples
#' 
#' mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' splitstackshape:::Names(mydf, c("a", "c"))
#' splitstackshape:::Names(mydf, c(1, 3))
#' 
#' @aliases Names 
Names <- function(data, invec) {
  if (!is.numeric(invec)) invec <- match(invec, names(data))
  names(data)[invec]
}
NULL




#' Split Basic Alphanumeric Strings Which Have No Separators
#' 
#' Used to split strings like "Abc8" into "Abc" and "8".
#' 
#' @param data The vector of strings to be split.
#' @param charfirst Is the string constructed with characters at the start or
#' numbers? Defaults to `TRUE`.
#' @return A `data.frame` with two columns, "variable" and "measure".
#' @author Ananda Mahto
#' @seealso [utils::strcapture()]
#' @examples
#' 
#' x <- paste0("Var", LETTERS[1:3], 1:3)
#' splitstackshape:::NoSep(x)
#' 
#' y <- paste0(1:3, "Var", LETTERS[1:3])
#' splitstackshape:::NoSep(y, charfirst = FALSE)
#' 
NoSep <- function(data, charfirst = TRUE) {
  if (isTRUE(charfirst)) {
    pattern <- "([[:alpha:]]+)([[:digit:]]+)"
    proto <- data.frame(variable = character(), measure = integer(), 
                        stringsAsFactors = FALSE)
  } else {
    pattern <- "([[:digit:]]+)([[:alpha:]]+)"
    proto <- data.frame(measure = integer(), variable = character(),
                        stringsAsFactors = FALSE)
  }
  strcapture(pattern, data, proto)
}
NULL




#' Convert All Factor Columns to Character Columns
#' 
#' Sometimes, we forget to use the \code{stringsAsFactors} argument when using
#' \code{\link{read.table}} and related functions. By default, R converts
#' character columns to factors. Instead of re-reading the data, the
#' \code{\link{FacsToChars}} function will identify which columns are currently
#' factors, and convert them all to characters.
#' 
#' 
#' @param mydf The name of your \code{data.frame}
#' @author Ananda Mahto
#' @seealso \code{\link{read.table}}
#' @examples
#' 
#' ## Some example data
#' dat <- data.frame(title = c("title1", "title2", "title3"),
#'          author = c("author1", "author2", "author3"),
#'          customerID = c(1, 2, 1))
#' 
#' str(dat) # current structure
#' dat2 <- splitstackshape:::FacsToChars(dat)
#' str(dat2) # Your new object
#' str(dat)  # Original object is unaffected
#' 
#' \dontshow{rm(dat, dat2, dat_copy)}
#' 
FacsToChars <- function(mydf) {
  mydf[sapply(mydf, is.factor)] <- 
    lapply(mydf[sapply(mydf, is.factor)], as.character)
  mydf
}
NULL


.collapseMe <- function(invec, atStart = TRUE) {
  if (isTRUE(atStart)) paste(sprintf("^%s", invec), collapse = "|")
  else paste(sprintf("%s$", invec), collapse = "|")
}
NULL

.stripWhite <- function(invec, delim = ",") {
  gsub("^\\s+|\\s+$", "",
       gsub(sprintf("\\s+[%s]\\s+|\\s+[%s]|[%s]\\s+",
                    delim, delim, delim), delim, invec))
}
NULL

vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)
NULL

.noEmpty <- function(invec) {
  invec[invec != ""]
}
NULL

.pad <- function(invec) {
  nchars <- max(nchar(invec))
  sprintf(paste0("%0", nchars, "d"), invec)
}
NULL
