s_frac <- function(indt, group, size) {
  ss <- N <- temp_grp <- NULL
  samp_sizes <- indt[, .N, by = group]
  samp_sizes[, ss := round(N * size, digits = 0)]
  samp_sizes[, temp_grp := do.call(paste, .SD), .SDcols = group]
}
NULL

s_n <- function(indt, group, size) {
  ss <- N <- temp_grp <- NULL
  samp_sizes <- indt[, .N, by = group]
  samp_sizes[, temp_grp := do.call(paste, .SD), .SDcols = group]
  
  if (length(size) == 1L) {
    samp_sizes[, ss := min(N, size), by = group]
    check <- samp_sizes$ss < size
    if (any(check)) {
      grps <- samp_sizes[check]$temp_grp
      message(sprintf("Groups %s contain fewer rows than requested. %s",
                      toString(grps), "Returning all rows."))
    }
  } else {
    if (length(size) > 1L) {
      if (length(size) != nrow(samp_sizes)) {
        stop(sprintf("Number of groups is %s but number of sizes supplied is %s.",
                     nrow(samp_sizes), length(size)))
      }
      if (is.null(names(size))) {
        stop("'size' should be entered as a named vector.")
      }
      samp_sizes <- samp_sizes[data.table(ss = size, temp_grp = names(size)),
                               on = "temp_grp"]
    }
  }
  samp_sizes[]
}
NULL

strat_sub <- function(indt, select) {
  if (is.null(names(select))) stop("'select' must be a named list")
  temp <- vapply(names(select), function(x)
    indt[[x]] %in% select[[x]], logical(nrow(indt)))
  indt[rowSums(temp) == length(select), ]
}
NULL