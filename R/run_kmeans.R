#' Kmeans Clustering for a vector of Cluster Sizes
#' @description
#' A function that runs kmeans from [stats::kmeans()] through a loop using the dimension reduction taken from [dimension_reduction_list()].
#'
#' @param data_input one of the resulting data structures from [dimension_reduction_list()].
#' @param cluster_sizes a vector of cluster sizes.
#' The value is used as the number of centers within [stats::kmeans()].
#'
#' @inheritParams stats::kmeans
#' @inheritParams stats::kmeans
#' @inheritParams stats::kmeans
#' @inheritParams stats::kmeans
#'
#' @return a named list containing classification vectors of the number
#'  of cluster sizes provided.
#' @seealso [stats::kmeans()]
#'
#' @examples
#' # load base iris dataset from rstudio
#' data("iris")
#' x <- t(iris[,1:4])
#' y <- dimension_reduction_list(x)
#'
#' # for a single data structure
#' run_kmeans(y$pca, 1:5)
#'
#' # for the entire list of data structures
#' lapply(y, function(t) run_kmeans(t,1:5))
#'
#' @export
run_kmeans <- function(data_input,
                       cluster_sizes,
                       iter.max = 10,
                       nstart = 1,
                       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                       trace=FALSE){

  by_cluster = list()

  for (i in cluster_sizes) {
    km <- stats::kmeans(data_input,
                        centers = i,
                        iter.max = iter.max,
                        nstart = nstart,
                        algorithm = algorithm,
                        trace = trace)
    by_cluster[[paste0('cluster_size_',i)]] <- km$cluster
  }
  return(by_cluster)
}

