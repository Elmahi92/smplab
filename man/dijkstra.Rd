\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dijkstras algorithm
  %%  ~~function to do ... ~~
}
\description{
The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{The graph is a data.frame with three variables (v1, v2 and w) that containsthe edges of the graph (from v1 to v2) with the weight of the edge (w)
    %%     ~~Describe \code{a} here~~
  }
  \item{init_node}{
  init_node is a numeric scalar that exist in the graph.
    %%     ~~Describe \code{b} here~~
  }
}
\details{For a given source node in the graph, the algorithm finds the shortest path between that node and every other. It can also be used for finding the shortest paths from a single node to a single destination node by stopping the algorithm once the shortest path to the destination node has been determined. For example, if the nodes of the graph represent cities and edge path costs represent driving distances between pairs of cities connected by a direct road (for simplicity, ignore red lights, stop signs, toll roads and other obstructions), Dijkstra's algorithm can be used to find the shortest route between one city and all other cities. A widely used application of shortest path algorithm is network routing protocols, most notably IS-IS (Intermediate System to Intermediate System) and Open Shortest Path First (OSPF). It is also employed as a subroutine in other algorithms such as Johnson's.
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
The dijkstra function should return the shortest path to every other node from the starting node as a vector.
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
\url{https://en.wikipedia.org/wiki/Dijkstra}
  %% ~put references to the literature/web site here ~
}
\author{
Simon & Mohamed
  %%  ~~who you are~~
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
  If you’re not very familiar with graphs, vertices and edges, see \url{https://en.wikipedia.org/wiki/Graph#Mathematics}
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }
\examples{
  ##---- Should be DIRECTLY executable !! ----
  ##-- ==>  Define data, use random,
  ##--	or do  help(data=index)  for the standard data sets.
  
  ## The function is currently defined as
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
