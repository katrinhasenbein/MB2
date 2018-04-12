# shift vector by a certain number of positions
#
#
#' @title Shift a vector by a given number of positions
#' @description Shifts a vector by a given number of positions. Unfilled positions are replaced by \emph{NA}.
#' @param vec vector that is to be shifted
#' @param shift number of positions that the vector should be shifted. Positive and negative values possible. New position of the value: original position + \emph{shift}
#' @examples #get the previous value
#' @examples previous <- shift.vec(vector, 1)
#' @examples #get the next value
#' @examples next <- shift.vec(vector, -1)

shift.vec <- function(vec, shift){
  if (length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}

