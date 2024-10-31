#' PCA and tSNE Dimension Reduction
#' @description creates a list of different levels of dimension reduction.
#'
#' @param numeric_matrix numeric matrix with samples as columns and variables as rows.
#' @param rank. The maximal number of principle components to be used. See [stats::prcomp()]
#'  for more details.
#' @param dims An integer output for dimensionality of the Rtsne reduction, either 2 or 3. usually 2. See [Rtsne::Rtsne()] for more details.
#' @param ... Allows input of additional [Rtsne::Rtsne()] parameters.
#' @return A list of all different dimension reductions recorded.
#' Takes the form of a pca object, pca distance object, tsne object,
#'  tsne distance object. Each of the following are used in the
#'  clustering procedures in `ClustCheck`.
#'
#' @seealso [stats::prcomp()], [Rtsne::Rtsne()], [stats::dist()]
#' @examples
#' # load base iris dataset from rstudio
#' data("iris")
#' x <- t(iris[,1:4])
#'
#' set.seed(1)
#' dimension_reduction_list(x)
#'
#' @export
dimension_reduction_list <- function(numeric_matrix, rank. = 50, dims = 3,...){

  pca <- stats::prcomp(x = t(numeric_matrix), rank. = rank.)$x
  pca_distance_object <- stats::dist(pca, method = "euclidean")
  pca_distance_matrix <- as.matrix(pca_distance_object)
  tsne <- Rtsne::Rtsne(
    X = pca_distance_matrix,
    dims = dims,
    perplexity = (ncol(pca_distance_matrix)-1)/3,
    check_duplicates = FALSE,
    pca = FALSE,
    is_distance = TRUE,
    Y_init = NULL,
    normalize = FALSE,...
  )$Y
  tsne_distance_object <- stats::dist(tsne, method = "euclidean")

  # remove dimnames
  dimnames(pca) <-
    dimnames(pca_distance_object) <-
    dimnames(tsne) <-
    dimnames(tsne_distance_object) <-NULL

  return(list(pca = pca,
              pca_distance_object = pca_distance_object,
              tsne = tsne,
              tsne_distance_object = tsne_distance_object))
}
