
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}

null_or_any_na <- function(x) {
  is.null(x) || any(is.na(x))
}
