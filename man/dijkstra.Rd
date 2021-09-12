\name{dijkstra}
\alias{dijkstra}
\title{Dijkstras algorithm
}
\description{
The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph
}
\usage{
  dijkstra(graph, init_node)
}
\arguments{
  \item{graph}{The graph is a data.frame with three variables (v1, v2 and w) that containsthe edges of the graph (from v1 to v2) with the weight of the edge (w)
  }
  \item{init_node}{
  init_node is a numeric scalar that exist in the graph
  }
}
\details{For a given source node in the graph, the algorithm finds the shortest path between that node and every other. It can also be used for finding the shortest paths from a single node to a single destination node by stopping the algorithm once the shortest path to the destination node has been determined. For example, if the nodes of the graph represent cities and edge path costs represent driving distances between pairs of cities connected by a direct road (for simplicity, ignore red lights, stop signs, toll roads and other obstructions), Dijkstra's algorithm can be used to find the shortest route between one city and all other cities. A widely used application of shortest path algorithm is network routing protocols, most notably IS-IS (Intermediate System to Intermediate System) and Open Shortest Path First (OSPF). It is also employed as a subroutine in other algorithms such as Johnson's
}
\references{
\url{https://en.wikipedia.org/wiki/Dijkstra}
}
\author{
Simon & Mohamed
}
  
\seealso{If you’re not very familiar with graphs, vertices and edges, see \url{https://en.wikipedia.org/wiki/Graph#Mathematics}
  }
\examples{
dijkstra(data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 1)
  }