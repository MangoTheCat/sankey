
#' Create an object that describes a sankey plot
#'
#' @param nodes A data frame of nodes on the plot, and possibly
#'   their visual style. The first column must be the ids of the
#'   nodes. If this argument is \code{NULL}, then the ids of the
#'   nodes are determined from \code{edges}.
#' @param edges A data frame of the edges. The first two columns
#'   must be node ids, and they define the edges. The rest of the columns
#'   contain the visual style of the edges.
#' @return A \code{sankey} object that can be plotted via the
#'   \code{\link{sankey}} function.x
#'
#' @importFrom simplegraph graph
#' @export

make_sankey <- function(nodes = NULL, edges) {

  if (is.null(nodes)) {
    nodes <- data.frame(
      stringAsFactors = FALSE,
      id = unique(c(edges[,1], edges[,2]))
    )
  }

  nodes$col     <- nodes$col     %||% color_nodes(nodes, edges)
  nodes$size    <- nodes$size    %||% optimize_sizes(nodes, edges)
  nodes$x       <- nodes$x       %||% optimize_x(nodes, edges)
  nodes$shape   <- nodes$shape   %||% "rectangle"
  nodes$lty     <- nodes$lty     %||% 1
  nodes$srt     <- nodes$srt     %||% 0
  nodes$textcol <- nodes$textcol %||% "black"
  nodes$label   <- nodes$label   %||% nodes[,1]

  if (null_or_any_na(nodes$y)      ||
      null_or_any_na(nodes$top)    ||
      null_or_any_na(nodes$center) ||
      null_or_any_na(nodes$bottom)) {
    nodes <- optimize_y(nodes, edges)
  }

  edges$colorstyle <- edges$colorstyle %||% "col"
  edges$curvestyle <- edges$curvestyle %||% "sin"
  edges$col        <- edges$col        %||% color_edges(nodes, edges)
  edges$weight     <- edges$weight     %||% 1

  graph(nodes, edges)
}

color_edges <- function(nodes, edges) {
  "#99d8c9"
}

color_nodes <- function(nodes, edges) {
  "#2ca25f"
}

#' @importFrom simplegraph predecessors successors

optimize_sizes <- function(nodes, edges) {

  sgraph <- graph(nodes, edges)

  lefts  <- vapply(predecessors(sgraph), length, 1L)
  rights <- vapply(successors(sgraph), length, 1L)

  pmax(lefts, rights)
}

#' @importFrom simplegraph topological_sort order vertex_ids

optimize_x <- function(nodes, edges) {

  ## `simplegraph` object
  sgraph <- graph(nodes, edges)

  ## Reverse adjacency list of the graph
  adj <- predecessors(sgraph)

  levels <- structure(rep(-1, order(sgraph)), names = vertex_ids(sgraph))

  order <- topological_sort(sgraph)

  for (n in order) {
    pred_levels <- levels[ adj[[n]] ]
    levels[[n]] <- if (length(pred_levels) == 0) 0 else max(pred_levels) + 1
  }

  levels
}

#' @importFrom simplegraph graph

optimize_y <- function(nodes, edges) {

  ## 10 percent of max total node size at a level
  interstop <- 0.1 * max(tapply(nodes$size, nodes$x, sum))

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
