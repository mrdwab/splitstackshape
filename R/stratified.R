#' Take a Stratified Sample From a Dataset
#' 
#' The \code{\link{stratified}} function samples from a
#' \code{\link{data.frame}} or a \code{\link{data.table}} in which one or more columns can be used as a
#' "stratification" or "grouping" variable. The result is a new
#' \code{data.table} with the specified number of samples from each group.
#' 
#' @param indt The input \code{data.frame} or \code{data.table}. 
#' @param group The column or columns that should be used to create the groups. Can be a character vector of column names (recommended) or a numeric vector of column positions. Generally, if you are using more than
#' one variable to create your "strata", you should list them in the order of
#' \emph{slowest} varying to \emph{quickest} varying. This can be a vector of
#' names or column indexes.
#' @param size The desired sample size. \itemize{ \item If \code{size} is a
#' value between \code{0} and \code{1} expressed as a decimal, size is set to
#' be proportional to the number of observations per group. \item If
#' \code{size} is a single positive integer, it will be assumed that you want
#' the same number of samples from each group. \item If \code{size} is a named
#' vector, the function will check to see whether the length of the vector
#' matches the number of groups and that the names match the group names.}
#' @param select A named list containing levels from the "group" variables in
#' which you are interested. The list names must be present as variable names
#' for the input dataset.
#' @param replace Logical. Should sampling be with replacement? Defaults to \code{FALSE}.
#' @param keep.rownames Logical. If the input is a \code{data.frame} or a \code{matrix}, \code{as.data.table} would normally drop the rownames. If \code{TRUE}, the rownames would be retained in a column named \code{rn}. Defaults to \code{FALSE}.
#' @param bothSets Logical. Should both the sampled and non-sampled sets be returned as a \code{list}? Defaults to \code{FALSE}.
#' @param \dots Optional arguments to \code{\link{sample}}.
#' @return If \code{bothSets = FALSE}, a \code{list} of two \code{data.tables}; otherwise, a \code{data.table}.
#' @note \emph{Slightly different sizes than requested}: Because of how computers deal with floating-point arithmetic, and because R uses a "round to even" approach, the size per strata that results when specifying a proportionate sample may be slightly higher or lower per strata than you might have expected.
#' @author Ananda Mahto
#' @seealso \code{\link[sampling:strata]{strata}} from the "strata" package; \code{\link[dplyr:sample_n]{sample_n}} and \code{\link[dplyr:sample_frac]{sample_frac}} from "dplyr". 
#' @examples
#' 
#' # Generate a sample data.frame to play with
#' set.seed(1)
#' dat1 <- data.frame(ID = 1:100,
#'               A = sample(c("AA", "BB", "CC", "DD", "EE"), 
#'                          100, replace = TRUE),
#'               B = rnorm(100), C = abs(round(rnorm(100), digits=1)),
#'               D = sample(c("CA", "NY", "TX"), 100, replace = TRUE),
#'               E = sample(c("M", "F"), 100, replace = TRUE))
#'
#' # Let's take a 10% sample from all -A- groups in dat1
#' stratified(dat1, "A", .1)
#' 
#' # Let's take a 10% sample from only "AA" and "BB" groups from -A- in dat1
#' stratified(dat1, "A", .1, select = list(A = c("AA", "BB")))
#' 
#' # Let's take 5 samples from all -D- groups in dat1,
#' #   specified by column number
#' stratified(dat1, group = 5, size = 5)
#' 
#' # Use a two-column strata: -E- and -D-
#' #   -E- varies more slowly, so it is better to put that first
#' stratified(dat1, c("E", "D"), size = .15)
#' 
#' # Use a two-column strata (-E- and -D-) but only interested in
#' #   cases where -E- == "M"
#' stratified(dat1, c("E", "D"), .15, select = list(E = "M"))
#' 
#' ## As above, but where -E- == "M" and -D- == "CA" or "TX"
#' stratified(dat1, c("E", "D"), .15,
#'      select = list(E = "M", D = c("CA", "TX")))
#' 
#' # Use a three-column strata: -E-, -D-, and -A-
#' s.out <- stratified(dat1, c("E", "D", "A"), size = 2)
#' 
#' @export stratified
stratified <- function(indt, group, size, select = NULL, 
                       replace = FALSE, keep.rownames = FALSE,
                       bothSets = FALSE, ...) {
  if (is.numeric(group)) group <- names(indt)[group]
  if (!is.data.table(indt)) {
    indt <- as.data.table(indt, keep.rownames = keep.rownames)
  } else {
    indt <- copy(indt)
  }
  if (is.null(select)) {
    indt <- indt
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(indt)))
      stop("Please verify your 'select' argument")
    temp <- vapply(names(select), function(x)
      indt[[x]] %in% select[[x]], logical(nrow(indt)))
    indt <- indt[rowSums(temp) == length(select), ]
  }
  .SD <- .N <- .RNID <- .EACHI <- ss <- N <- NULL
  df.table <- indt[, .N, by = group]
  df.table
  if (length(size) > 1) {
    if (length(size) != nrow(df.table))
      stop("Number of groups is ", nrow(df.table),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      stop("size should be entered as a named vector")
    } else {
      ifelse(all(names(size) %in% do.call(
        paste, df.table[, group, with = FALSE])),
        n <- merge(
          df.table, 
          setnames(data.table(names(size), ss = size), 
                   c(group, "ss")), by = group),
        stop("Named vector supplied with names ",
             paste(names(size), collapse = ", "),
             "\n but the names for the group levels are ",
             do.call(paste, c(unique(
               df.table[, group, with = FALSE]), collapse = ", "))))
    }
  } else if (size < 1) {
    n <- df.table[, ss := round(N * size, digits = 0)]
  } else if (size >= 1) {
    if (all(df.table$N >= size) || isTRUE(replace)) {
      n <- cbind(df.table, ss = size)
    } else {
      message(
        "Some groups\n---",
        do.call(paste, c(df.table[df.table$N < size][, group, with = FALSE], 
                         sep = ".", collapse = ", ")),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- cbind(df.table, ss = pmin(df.table$N, size))
    }
  }
  setkeyv(indt, group)
  setkeyv(n, group)
  indt[, .RNID := sequence(nrow(indt))]
  out1 <- indt[indt[n, list(
    .RNID = sample(.RNID, ss, replace, ...)), by = .EACHI]$`.RNID`]
  
  if (isTRUE(bothSets)) {
    out2 <- indt[!.RNID %in% out1$`.RNID`]
    out1[, .RNID := NULL]
    out2[, .RNID := NULL]
    list(SAMP1 = out1, SAMP2 = out2)
  } else {
    out1[, .RNID := NULL][]
  }
}
NULL
