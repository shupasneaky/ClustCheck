% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_kmeans.R
\name{run_kmeans}
\alias{run_kmeans}
\title{Kmeans Clustering for a vector of Cluster Sizes}
\usage{
run_kmeans(
  data_input,
  cluster_sizes,
  iter.max = 10,
  nstart = 1,
  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
  trace = FALSE
)
}
\arguments{
\item{data_input}{A numeric matrix. Can be one of the pca or tsne structures from \code{\link[=dimension_reduction_list]{dimension_reduction_list()}}.}

\item{cluster_sizes}{a vector of cluster sizes.
The value is used as the number of centers within \code{\link[stats:kmeans]{stats::kmeans()}}.}

\item{iter.max}{the maximum number of iterations allowed.}

\item{nstart}{if \code{centers} is a number, how many random sets
    should be chosen?}

\item{algorithm}{character: may be abbreviated.  Note that
    \code{"Lloyd"} and \code{"Forgy"} are alternative names for one
    algorithm.}

\item{trace}{logical or integer number, currently only used in the
    default method (\code{"Hartigan-Wong"}): if positive (or true),
    tracing information on the progress of the algorithm is
    produced.  Higher values may produce more tracing information.}
}
\value{
a named list containing classification vectors of the number
of cluster sizes provided.
}
\description{
A function that runs kmeans from \code{\link[stats:kmeans]{stats::kmeans()}} through a loop using the dimension reduction taken from \code{\link[=dimension_reduction_list]{dimension_reduction_list()}}.
}
\examples{
# load base iris dataset from rstudio
data("iris")
x <- t(iris[,1:4])
y <- dimension_reduction_list(x)

run_kmeans(y$pca, 1:5)

}
\seealso{
\code{\link[stats:kmeans]{stats::kmeans()}}
}
