
## We can assume that nodes and edges have color at
## this point. We add labels here, if not present,
## to hide the extra nodes.

do_break_edges <- function(nodes, edges) {

  ## Color of new nodes is the mean of the two incident nodes
  col1 <- nodes$col[match(edges[,1], nodes[,1])]
  col2 <- nodes$col[match(edges[,2], nodes[,1])]
  col <- mean_colors(col1, col2)

  new_nodes <- data.frame(
    stringsAsFactors = FALSE,
    id = make.unique(paste(edges[,1], sep = "-", edges[,2]), sep = "_"),
    label = "",
    shape = "invisible",
    boxw = 0,
    col = col
  )
  names(new_nodes)[1] <- names(nodes)[1]

  edges1 <- edges2 <- edges
  edges1[,2] <- new_nodes[,1]
  edges2[,1] <- new_nodes[,1]

  edges <- rbind(edges1, edges2)
  nodes <- merge(nodes, new_nodes, all = TRUE)

  list(nodes = nodes, edges = edges)
}

mean_colors <- function(col1, col2) {
  vapply(seq_along(col1), FUN.VALUE = "", function(i) {
    mrgb <- rowMeans(cbind(col2rgb(col1[i]), col2rgb(col2[i])))
    do.call(rgb, as.list(mrgb / 255))
  })
}
