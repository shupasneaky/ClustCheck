#' Hierarchical Clustering for a vector of Cluster Sizes
#' @description
#' A function that runs hclust from [stats::hclust()] through a loop using the dimension reduction taken from [dimension_reduction_list()].
#'
#' @param data_input one of the resulting data structures from [dimension_reduction_list()].
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
#' run_hierarchical(y$pca, 1:5)
#'
#' # for the entire list of data structures
#' lapply(y, function(t) run_hierarchical(t,1:5))
#'
#' @export
run_hierarchical <- function(data_input,
                             cluster_sizes,
                             method = "ward.D",
                             members = NULL){

  by_cluster = list()

  for (i in cluster_sizes) {
    tree <- hclust(d = data_input,
                   method = method,
                   members = members)
    bycluster[[paste0('cluster_size_',i)]] <- cutree(tree = tree, k = i)
  }
  return(by_cluster)
}

