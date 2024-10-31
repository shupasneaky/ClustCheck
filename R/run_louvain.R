#' Louvain Clustering for a vectors of Parameters
#' @description
#' conducts Louvain clustering for multiple vectors of parameter choices.
#'
#' @param data_input Either a diagonal numeric matrix or a distance object from [stats::dist()]. The distance_object output from [dimension_reduction_list()].
#' @param k_nearest_neighbors number of neighbors to be used when constructing the nearest neighbor graph.
#' @param resolution A scalar between 0 and 1. The `resolution` parameter from [igraph::cluster_louvain()].
#'
#' @return a named list containing classification vectors of the parameters provided.
#' @seealso [igraph::cluster_louvain()], [cccd::nng()]
#'
#' @examples
#' # load base iris dataset from rstudio
#' data("iris")
#' x <- t(iris[,1:4])
#' y <- dimension_reduction_list(x)
#'
#' run_louvain(y$pca_distance_object, c(20, 25), c(0.5, 0.8))
#'
#' @export
run_louvain <- function(data_input,
                       k_nearest_neighbors,
                       resolution){

  by_cluster = list()
  for(j in k_nearest_neighbors) {

    graph <- cccd::nng(dx = as.matrix(data_input), mutual = TRUE, k = j)
    for (i in resolution) {

      h <- igraph::cluster_louvain(graph, resolution = i)
      by_cluster[[paste0('knn_',j,'&resolution_',i)]] <- h$membership
    }
  }
  return(by_cluster)
}
