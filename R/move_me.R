#' Reorders the Contents of a Vector
#' 
#' Shuffle the order of a vector around using natural language statements.
#' 
#' This can be a useful function for reordering the columns of a `data.table` in 
#' a convenient manner. In such cases, the `invec` would be `names(your_data)`. 
#' 
#' @details 
#' 
#' The `movecommand` argument is specified in the form of `"a, b
#' before f"`. The positions to move are:
#' 
#' * **`first`**: move the specified items to the first postion. 
#' * **`last`**: move the specified items to the last position. 
#' * **`before`**: move the specified items before the value mentioned. 
#' * **`after`**: move the specified items after the value mentioned.
#' 
#' Multiples are allowed:
#' 
#' * Specify multiple values to be moved by separating them with a comma.
#' * Chain multiple move commands by separating them with a semicolon.
#' 
#' @param invec The input vector
#' @param movecommand The command that describes how you want to shuffle the
#' vector. See *Details*.
#' @return A vector.
#' @author Ananda Mahto
#' @references <http://stackoverflow.com/a/18420673/1270695>
#' @examples
#' 
#' myvec <- letters[1:10]
#' myvec
#' move_me(myvec, "a last; b, e, g before d; c first; h after j")
#' 
#' new_col_order <- move_me(
#'   names(mtcars), "hp first; cyl after drat; vs, am, gear before mpg; wt last")
#' mtcars[new_col_order]
#' 
#' @export 
move_me <- function(invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1L]], ",|\\s+"), 
                        function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1L]])
    A <- movelist[[i]][[2L]][1L]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2L]][2L]
      if (A == "before") {
        after <- match(ba, temp)-1L
      } else if (A == "after") {
        after <- match(ba, temp)
      }    
    } else if (A == "first") {
      after <- 0L
    } else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1L]], after = after)
  }
  myVec
}
NULL