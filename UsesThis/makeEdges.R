
require("plyr")

getSplitList <- function(x) {
  x <- gsub('[ \']', '', substr(x, 2, nchar(x)-1))
  x <- unlist(strsplit(x, ','))
}

makeEdgesFor <- function(entry) {
  entry <- as.list(entry)
  wares <- getSplitList(as.character(entry$wares))
  slug  <- as.character(entry$slug)
  categ <- getSplitList(as.character(entry$categories))
  if (length(wares) == 0) {
    return(NULL)
  }
  cartProd <- expand.grid(categ, wares)
  colnames(cartProd) <- c("categories", "wares")
  cbind(cartProd, slug)
}

printEdge <- function(edge) {
  edge <- as.list(edge)
  paste(sep=',', edge$categories, edge$wares, edge$slug)
}

args <- commandArgs(trailingOnly = TRUE)
fname <- args[1]

usesthis <- read.csv(fname)
edges <- ldply(apply(usesthis, 1, makeEdgesFor))
##apply(edges, 1, printEdge)
write.csv(file="uses_this_edges.csv", edges)
write.table(file="uses_this_edges_bare.csv",
            edges[,c("categories", "wares")],
            row.names=FALSE, col.names=FALSE,
            sep=','
           )
