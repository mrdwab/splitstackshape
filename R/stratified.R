#' Take a Stratified Sample From a Dataset
#' 
#' The `stratified` function samples from a `data.table` in which one or more 
#' columns can be used as a "stratification" or "grouping" variable. The result 
#' is a new `data.table` with the specified number of samples from each group.
#' 
#' @param indt The input `data.table`. 
#' @param group The column or columns that should be used to create the groups. 
#' Can be a character vector of column names (recommended) or a numeric vector 
#' of column positions. Generally, if you are using more than one variable to 
#' create your "strata", you should list them in the order of *slowest* varying 
#' to *quickest* varying. This can be a vector of names or column indexes.
#' @param size The desired sample size.
#' * If `size` is a value between `0` and `1` expressed as a decimal, size is 
#' set to be proportional to the number of observations per group.
#' * If `size` is a single positive integer, it will be assumed that you want
#' the same number of samples from each group.
#' * If `size` is a named vector, the function will check to see whether the 
#' length of the vector matches the number of groups and that the names match 
#' the group names.
#' @param select A named list containing levels from the `"group"` variables in
#' which you are interested. The list names must be present as variable names
#' for the input dataset.
#' @param replace Logical. Should sampling be with replacement? Defaults to `FALSE`.
#' @param keep.rownames Logical. If the input is a `data.frame` with `rownames`, 
#' `as.data.table` would normally drop the rownames. If `TRUE`, the rownames 
#' would be retained in a column named `rn`. Defaults to `FALSE`.
#' @param bothSets Logical. Should both the sampled and non-sampled sets be 
#' returned as a `list`? Defaults to `FALSE`.
#' @param \dots Optional arguments to [base::sample()].
#' @return If `bothSets = TRUE`, a `list` of two `data.tables`; otherwise, a `data.table`.
#' @note *Slightly different sizes than requested*: Because of how computers 
#' deal with floating-point arithmetic, and because R uses a "round to even" 
#' approach, the size per strata that results when specifying a proportionate 
#' sample may be one sample higher or lower per strata than you might have expected.
#' @author Ananda Mahto
#' @seealso [sampling::strata()] from the "strata" package; [dplyr::sample_n()] 
#' and [dplyr::sample_frac()] from "dplyr".
#' @examples
#' 
#' # Generate a sample data.frame to play with
#' set.seed(1)
#' DF <- data.frame(
#'   ID = 1:100,
#'   A = sample(c("AA", "BB", "CC", "DD", "EE"), 100, replace = TRUE),
#'   B = rnorm(100), C = abs(round(rnorm(100), digits=1)),
#'   D = sample(c("CA", "NY", "TX"), 100, replace = TRUE),
#'   E = sample(c("M", "F"), 100, replace = TRUE))
#'
#' # Take a 10% sample from all -A- groups in DF
#' stratified(DF, "A", .1)
#' 
#' # Take a 10% sample from only "AA" and "BB" groups from -A- in DF
#' stratified(DF, "A", .1, select = list(A = c("AA", "BB")))
#' 
#' # Take 5 samples from all -D- groups in DF, specified by column number
#' stratified(DF, group = 5, size = 5)
#' 
#' # Use a two-column strata: -E- and -D-
#' stratified(DF, c("E", "D"), size = .15)
#' 
#' # Use a two-column strata (-E- and -D-) but only use cases where -E- == "M"
#' stratified(DF, c("E", "D"), .15, select = list(E = "M"))
#' 
#' ## As above, but where -E- == "M" and -D- == "CA" or "TX"
#' stratified(DF, c("E", "D"), .15, select = list(E = "M", D = c("CA", "TX")))
#' 
#' # Use a three-column strata: -E-, -D-, and -A-
#' stratified(DF, c("E", "D", "A"), size = 2)
#' 
#' \dontrun{
#' # The following will produce errors
#' stratified(DF, "D", c(5, 3))
#' stratified(DF, "D", c(5, 3, 2))
#' }
#' 
#' # Sizes using a named vector
#' stratified(DF, "D", c(CA = 5, NY = 3, TX = 2))
#' 
#' # Works with multiple groups as well
#' stratified(DF, c("D", "E"), 
#'            c("NY F" = 2, "NY M" = 3, "TX F" = 1, "TX M" = 1,
#'              "CA F" = 5, "CA M" = 1))
#' 
#' @export stratified
stratified <- function(indt, group, size, select = NULL, replace = FALSE,
                       keep.rownames = FALSE, bothSets = FALSE, ...) {
  
  indt <- setDT(copy(indt), keep.rownames = keep.rownames)
  if (is.numeric(group)) group <- names(indt)[group]
  
  temp_grp <- temp_ind <- NULL

  indt <- if (is.null(select)) indt else strat_sub(indt, select)
  indt[, temp_ind := .I]
  indt[, temp_grp := do.call(paste, .SD), .SDcols = group]
  
  if (any(size < 1L)) {
    if (!all(size < 1L)) stop("Incompatible sizes supplied") else type <- "frac"
  }
  
  if (any(size >= 1L)) {
    if (!all(size >= 1L)) stop("Incompatible sizes supplied") else type <- "n"
  }
  
  check <- switch(type,
                  frac = s_frac(indt, group, size),
                  n = s_n(indt, group, size))

  inds <- split(indt$temp_ind, indt$temp_grp)[check$temp_grp]
  samples <- check$ss
  
  ROWS <- Map(function(x, y) x[sample(seq_along(x), y, replace = replace, ...)], 
              inds, samples)
  ROWS <- unlist(ROWS, use.names = FALSE)
  
  out1 <- indt[ROWS]
  if (isTRUE(bothSets)) {
    out2 <- indt[!temp_ind %in% ROWS]
    list(SAMP1 = out1[, temp_ind := NULL][, temp_grp := NULL][],
         SAMP2 = out2[, temp_ind := NULL][, temp_grp := NULL][])
  } else {
    out1[, temp_ind := NULL][, temp_grp := NULL][]
  }
  
}
NULL
