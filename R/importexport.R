
checkedges <- function(edges, nnames) {
  if (ncol(edges) < 3
       || ! all(c("N1", "N2", "Value") %in% colnames(edges)))
    stop("edges must have the columns N1, N2 and Value")

  if (! "ID" %in% colnames(edges)) {
    edges$ID <- paste0(edges$N1, "->", edges$N2)
  }

  if (any(edges$ID %in% nnames))
    stop("edges must not have the same IDs as nodes")

  if (! all(c(as.character(edges$N1), as.character(edges$N2)) %in% nnames)) {
    sel <- (! edges$N1 %in% nnames) | (! edges$N2 %in% nnames)
    n <- sum(sel)
    warning(sprintf("unknown nodes present in the edges parameter, removing %d edges", n))
    edges <- edges[! sel, ]
  }

  # search for duplicated *pairs*
  tmp <- t(apply(edges[, c("N1", "N2")], 1, sort))
  sel <- duplicated(tmp)
  if (any(sel)) {
    n <- sum(sel)
    warning(sprintf("duplicated edge information, removing %d edges ", n))
    edges <- edges[! sel, ]
  }

  if (any(duplicated(edges$ID))) {
    n <- sum(duplicated(edges$ID))
    warning(sprintf("duplicated edge information, removing %d edges ", n))
    edges <- edges[! duplicated(edges$ID), ]
  }

  if (any(is.na(edges$Value))) {
    n <- sum(is.na(edges$Value))
    warning(sprintf("NA's in edges, removing %d edges", n))
    edges <- edges[! is.na(edges$Value), ]
  }

  if (! is.numeric(edges$Value))
    stop("Non-numeric edge sizes")

  if (nrow(edges) == 0) stop("No edges to draw")
  rownames(edges) <- edges$ID

  edges
}
