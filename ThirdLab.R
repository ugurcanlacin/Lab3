name <- "Ugurcan Lacin"
liuid <- "ugula687"

# library(markmyassignment)
# lab_path <-
#   "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab3.yml"
# set_assignment(lab_path)

# 1.1.1
euclidean <- function(m,n){
  if(!is.numeric(m) || !is.numeric(n)){
    stop()
  }
  if(n == 0) return(m)
  return(euclidean(n, m %% n))
}

# aa <- euclidean(123612, 13892347912)
# aa <-  euclidean(100, 1000)

dijkstra <- function(graph,init_node){

  
}




wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

aa <- dijkstra(wiki_graph, 1)