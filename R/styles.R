
default.style <- function() {

  ret <- list(
    nodestyle = "regular",
    col =       "grey",
    srt =       "90",
    lty =       1,
    textcol =   "black",
    edgecol =   "gradient",
    edgestyle = "sin"
  )

  ret
}

## function for updating styles. s is filled up with default values if these
## values are empty. If update.missing is TRUE, update also these fields
## which are missing from the global default style.

getstyle   <- function(s, defaults = NULL, update.missing = FALSE) {

  if (is.null(s)) s <- list()

  if (is.null(defaults)) defaults <- default.style()

  for (n in names(defaults)) {
    if (is.null(s[[n]])) s[[n]] <- defaults[[n]]
  }

  if (update.missing) {
    defaults <- default.style()
    for (n in names(defaults)) {
      if (is.null(s[[n]])) s[[n]] <- defaults[[n]]
    }
  }

  s
}


## copy attribute from id.from to id.to

copyattr <- function(styles, id.from, id.to, attr) {

  val <- getattr(styles, id.from, attr)
  styles <- setattr(styles, id.to, attr, val)
  styles
}

setattr <- function(styles, id, attr, value) {

  if (is.null(styles)) styles <- list()
  if (is.null(styles[[id]])) styles[[id]] <- list()
  styles[[id]][[attr]] <- value
  styles
}

## return attribute for id in styles. If NULL, return the default

getattr <- function(styles, id, attr) {

  def <- TRUE

  if (is.null(styles) ||
     is.null(styles[[id]]) ||
     is.null(styles[[id]][[attr]])) {
    tmp <- default.style()

  } else {
    tmp <- styles[[id]]
  }

  tmp[[attr]]
}
