#' Dijkstra's algorithm
#'
#' @param graph A data frame that contains the edges of the graph and the weight of the edge
#' @param init_node A numeric scalar that exists in the graph
#' @return The shortest distance from \code{init_node} to every other node
#' @description The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph
#' @author Simon and Mohamed
#' @examples
#' dijkstra(data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5), w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 1)
#' dijkstra(data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5), w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 3)
#' @references \url{https://en.wikipedia.org/wiki/Dijkstras_algorithm/}
#' @export
dijkstra <- function(graph, init_node) {
  nodes <- unique(graph[,1])
  dist <- rep(Inf, length(nodes))
  unvisited <- rep(TRUE, length(nodes))
  res <- data.frame(nodes, dist, unvisited)
  
  res[init_node,2] <- 0
  
  while(sum(res[,3], na.rm = TRUE) > 0) {
    u <- res[which(res[,3] == TRUE),][which.min(res[which(res[,3] == TRUE),][,2]),1]
    res[u,3] <- FALSE
    neighbors <- graph[which(graph[,2] == u),1]
    
    for(neighbor in neighbors) {
      alt <- res[u,2] + graph[which(graph[,1] == u & graph[,2] == neighbor),3]
      if(alt < res[neighbor,2]) {
        res[neighbor,2] <- alt
      }
    }
  }
  return(res[,2])
}