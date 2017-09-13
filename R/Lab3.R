name <- "Ugurcan Lacin"
liuid <- "ugula687"

#' @title Euclidean Algorithm Implementation
#' @name  euclidean
#' @param one number
#' @param two number
#' @return Greatest Common Divisor of two Numbers by Euclidean Algorithm
#' @description Euclidean Algorithm Implementation to find the Greatest Common Divisor(GCD) of two numbers one and two param 
#' @references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}

## euclidean
euclidean <- function(m,n){
  if(!is.numeric(m) || !is.numeric(n)){
    stop()
  }
  if(n == 0) return(m)
  return(euclidean(n, m %% n))
}

# aa <- euclidean(123612, 13892347912)
# aa <-  euclidean(100, 1000)

#' @title Dijkstra Algorithm Implementation
#' @name  dijkstra 
#' @param Graph Data Frame of Graph
#' @param  Init_node Numeric Scalar Input
#' @return Return a Numeric Vector with the shortest path between nodes in the graph.
#' @description Dijkstra Algorithm to find the shortest path between nodes in the graph giving a shortest path vector. 
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}

## dijkstra
dijkstra <- function(graph, init_node){
  
  if(is.numeric(init_node) &
     is.atomic(init_node) &
     is.data.frame(graph) &
     (length(graph[[1]]) == length(graph[[2]])) &
     (length(graph[[2]]) == length(graph[[3]])) &
     all(colnames(graph) == c("v1", "v2", "w")) &
     length(colnames(graph)) == 3 &
     (init_node %in% graph[[1]] || init_node %in% graph[[2]])
     
  ){
    
    # Initialize
    previous <- c()
    distance <- c()
    Q <- c()
    
    for(i in unique(c(graph$v1, graph$v2))){
      previous[i] <- NA
      distance[i] <- Inf
      Q <- c(Q,i) # Node which are not part of the path yet
    }
    
    distance[init_node] <- 0
    
    while(length(Q) != 0){
      u <- Q[which.min(distance[Q])]
      Q <- Q[Q != u]
      
      for(i in graph$v1[graph$v2 == u]){
        dist <- graph$w[graph$v1 ==i & graph$v2 == u]
        alt <- distance[u] + dist
        
        if(alt < distance[i]){
          distance[i] <- alt
          previous[i] <- u
        }
      }
    }
  }else{
    stop()
  }
  return(distance)
}

#' @title wiki_graph
#' @name  wiki_graph 
#' @param Vector v1 A numeric scalar
#' @param Vector v2 A numeric scalar
#' #' @param Vector w A numeric scalar
#' @description wiki_graph
#' @references \url{https://en.wikipedia.org/wiki/Graph}
## wiki_graph
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
aa <- dijkstra(wiki_graph, 1)