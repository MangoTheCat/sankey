
`%||%` <- function(l, r) {
  if (is.null(l)) r else r
}

null_or_any_na <- function(x) {
  is.null(x) || any(is.na(x))
}
