#' Kmeans Clustering for a vector of Cluster Sizes
#' @description
#' A function that runs kmeans from [stats::kmeans()] through a loop using the dimension reduction taken from [dimension_reduction_list()].
#'
#' @param data_input A numeric matrix. Can be one of the pca or tsne structures from [dimension_reduction_list()].
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
#' run_kmeans(y$pca, 1:5)
#'
#' @export
run_kmeans <- function(data_input,
                       cluster_sizes,
                       iter.max = 10,
                       nstart = 1,
                       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                       trace=FALSE){

  by_cluster = list()

  if("dist" %in% class(data_input)) data_input <- as.matrix(data_input)

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

