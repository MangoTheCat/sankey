
optimize_y <- function(nodes, edges, mode = c("optimal", "simple")) {

  mode <- match.arg(mode)

  if (mode == "simple") {
    optimize_y_simple(nodes, edges)

  } else if (mode == "optimal") {
    optimize_y_optim(nodes, edges)
  }
}


optimize_y_simple <- function(nodes, edges) {

  ## 10 percent of max total node size at a level
  interstop <- 0.3 * max(tapply(nodes$size, nodes$x, sum))

  nodes$center <- nodes$top <- nodes$bottom <- NA_real_

  for (pos in sort(unique(nodes$x))) {
    cur_y <- 0
    nodes_here <- rev(which(nodes$x == pos))

    for (node in nodes_here) {

      if (! is.null(nodes$y) && ! is.na(nodes$y[node])) {
        nodes$center[node] <- nodes$y[node]
        nodes$top[node]    <- nodes$y[node] + nodes$size[node] / 2
        nodes$bottom[node] <- nodes$y[node] - nodes$size[node] / 2

      } else {
        nodes$bottom[node] <- cur_y
        nodes$center[node] <- cur_y - nodes$size[node] / 2
        nodes$top   [node] <- cur_y - nodes$size[node]
        cur_y <- cur_y - nodes$size[node] - interstop
      }
    }
  }

  nodes
}
