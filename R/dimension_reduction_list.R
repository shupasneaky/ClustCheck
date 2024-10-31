#' creates a list of different levels of dimension reduction.
#'
#' @param numeric_matrix numeric matrix.
#' @inheretParams stats::prcomp
#' @inheretParams Rtsne::Rtsne
#' @param ... Additional [Rtsne::Rtsne()] parameters.
#' @return A list of all different dimension reductions recorded. Takes the form of a pca object, pca distance object, pca distance matrix, tsne object, tsne distance object, tsne distance matrix. Each of the following are used in the clustering procedures in `ClustCheck`.
#' @seealso [stats:prcomp()], [Rtsne::Rtsne()], [stats::dist()]
#' @examples
#' # load base iris dataset from rstudio
#' data("iris")
#' x <- iris[,1:4]
#'
#' dimension_reduction_list(x)
#' @export
dimension_reduction_list <- function(numeric_matrix, pca_dims = 50, tsne_dims = 3,...){

  pca <- stats::prcomp(x = t(numeric_matrix), rank. = pca_dims)$x
  pca_distance_object <- stats::dist(pca, method = "euclidean")
  pca_distance_matrix <- as.matrix(pca_distance_object)

  tsne <- Rtsne::Rtsne(
    X = pca_distance_matrix,
    dims = tsne_dims,
    perplexity = (ncol(pca_distance_matrix)-1)/3,
    check_duplicates = FALSE,
    pca = FALSE,
    is_distance = TRUE,
    Y_init = NULL,
    normalize = FALSE,...
  )$Y
  tsne_distance_object <- stats::dist(tsne, method = "euclidean")
  tsne_distance_matrix <- as.matrix(tsne_distance_object)

  # remove dimnames
  dimnames(pca) <-
    dimnames(pca_distance_object) <-
    dimnames(pca_distance_matrix) <-
    dimnames(tsne) <-
    dimnames(tsne_distance_object) <-
    dimnames(tsne_distance_matrix) <- NULL

  return(list(pca = pca,
              pca_distance_object = pca_distance_object,
              pca_distance_matrix = pca_distance_matrix,
              tsne = tsne,
              tsne_distance_object = tsne_distance_object,
              tsne_distance_matrix = tsne_distance_matrix))
}
