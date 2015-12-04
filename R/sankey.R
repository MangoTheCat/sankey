
#' @importFrom simplegraph vertices edges strength

draw.edges <- function(x, nsteps = 50, boxw = 0.2) {
  # for each node, we need to to store the position of the current slot, on
  # the right and on the left

  nodes <- vertices(x)
  edges <- edges(x)

  nodes$lpos <- nodes$center - strength(x, mode = "in")  / 2
  nodes$rpos <- nodes$center - strength(x, mode = "out") / 2

  disp <- c(invisible = 0, point = 0, rectangle = boxw / 2)

  for (i in seq_len(nrow(edges))) {

    n1 <- edges$from[i]
    n2 <- edges$to[i]

    sel <- function(node, attr) nodes[ nodes[,1] == node, attr]

    curveseg(
      sel(n1, "x") + disp[sel(n1, "shape")],
      sel(n2, "x") - disp[sel(n2, "shape")],
      sel(n1, "rpos"),
      sel(n2, "lpos"),
      width = edges$weight[i],
      col = edges$col[i],
      nsteps = nsteps,
      curvestyle = edges$curvestyle[i]
    )

    nodes[nodes[,1] == n1, "rpos"] <- sel(n1, "rpos") + edges$weight[i]
    nodes[nodes[,1] == n2, "lpos"] <- sel(n2, "lpos") + edges$weight[i]
  }
}

draw.nodes <- function(x, width = 0.2, boxw = 0.2) {

  nodes <- vertices(x)

  for (n in seq_len(nrow(nodes))) {

    if (nodes$shape[n] == "invisible") next

    if (nodes$shape[n] == "point") {
      points(nodes$x[n], nodes$center[n], pch = 19, col = nodes$col[n])

    } else if (nodes$shape[n] == "rectangle") {
      rect(
        nodes$x[n] - boxw / 2, nodes$bottom[n],
        nodes$x[n] + boxw / 2, nodes$top[n],
        lty = nodes$lty[n], col = nodes$col[n]
      )
    }

    text(nodes$x[n], nodes$center[n], nodes$label[n],
         col = nodes$textcol[n], srt = nodes$srt[n])
  }
}

#' @rdname sankey
#' @method plot sankey
#' @export

plot.sankey <- function(x, ...) sankey(x, ...)

#'@export

sankey <- function(x) {

  plot.new()

  V <- vertices(x)
  E <- edges(x)

  xrange <- range(V$x)
  xlim <- xrange + (xrange[2] - xrange[1]) * c(-0.1, 0.1)
  yrange <- range(V[, c("bottom", "top")])
  ylim <- yrange + (yrange[2] - yrange[1]) * c(-0.1, 0.1)

  par(mar = c(0, 0, 0, 0) + 0.2)
  par(usr = c(xlim, ylim))

  dev.hold()
  on.exit(dev.flush())

  w <- strwidth("hjKg") * 0.75

  draw.edges(x, nsteps = 50, boxw = w)
  draw.nodes(x, boxw = w)

  invisible()
}
