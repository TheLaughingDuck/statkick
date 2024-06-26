% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmeans.R
\name{kmeans}
\alias{kmeans}
\title{Cluster data using the k-means algorithm}
\usage{
kmeans(data, K, init = "random", repetitions = 1)
}
\arguments{
\item{data}{A dataframe where each column will be considered in the clustering.}

\item{K}{A numeric, the number of clusters to create.}

\item{init}{A string indicating which initialization algorithm to use.}

\item{repetitions}{A numeric, specifying number of times to repeat the whole k-means algorithm (including initialization).}
}
\value{
The original dataframe, but with an additional column indicating cluster belonging.
}
\description{
A function taking a set of data, and then clusters that data into K clusters using the K-means algorithm. Multiple initialization algorithms are available (some experimental).
}
