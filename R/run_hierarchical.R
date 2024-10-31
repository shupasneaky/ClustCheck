#' Hierarchical Clustering for a vector of Cluster Sizes
#' @description
#' A function that runs hclust from [stats::hclust()] through a loop using the dimension reduction taken from [dimension_reduction_list()].
#'
#' @param data_input A distance matrix. Can be one of the distance objects from [dimension_reduction_list()].
#' @param cluster_sizes a vector of cluster sizes.
#' The value is used as the number of centers within [stats::kmeans()].
#' @inheritParams stats::hclust
#' @inheritParams stats::hclust
#'
#' @return a named list containing classification vectors of the number
#'  of cluster sizes provided.
#' @seealso [stats::hclust()]
#'
#' @examples
#' # load base iris dataset from rstudio
#' data("iris")
#' x <- t(iris[,1:4])
#' y <- dimension_reduction_list(x)
#'
#' # for a single data structure
#' run_hierarchical(y$pca_distance_object, 1:5)
#'
#' @export
run_hierarchical <- function(data_input,
                             cluster_sizes,
                             method = "ward.D",
                             members = NULL){

  by_cluster = list()

  for (i in cluster_sizes) {
    tree <- stats::hclust(d = data_input,
                   method = method,
                   members = members)
    by_cluster[[paste0('cluster_size_',i)]] <- stats::cutree(tree = tree, k = i)
  }
  return(by_cluster)
}

