#' Cluster data using the k-means algorithm
#'
#' @description A function taking a set of data, and then clusters that data into K clusters using the K-means algorithm. Multiple initialization algorithms are available (some experimental).
#'
#' @param data A dataframe where each column will be considered in the clustering.
#'
#' @param K A numeric, the number of clusters to create.
#'
#' @param init A string indicating which initialization algorithm to use.
#'
#' @param repetitions A numeric, specifying number of times to repeat the whole k-means algorithm (including initialization).
#'
#' @return The original dataframe, but with an additional column indicating cluster belonging.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange filter select
#' @importFrom utils tail
#'
#' @export kmeans

kmeans <- function(data, K, init = "random", repetitions=1){

  # ---V--- CHECK INPUT ---V---
  # Check arg1 argument
  #stopifnot("argument \"arg1\" is not ---" = is.---(arg1))

  # Check arg2 argument
  #stopifnot("argument \"arg2\" is not ---" = is.---(arg2))
  # ---^--- CHECK INPUT ---^---

  # Initialize K-means
  if (init == "random"){centroids <- random_init(data, K)}

  # Setup K-means
  Costs <- c()
  current_cost <- 0 # Keep track of cost of currently assigned datapoints (saves comp. time)
  iteration <- 0
  data$class <- 0 # Default class placeholder

  ## Run K-means until convergence
  while (length(Costs) < 2 || tail(Costs, n=2)[1] - tail(Costs, n=1) != 0){
    # Iteration tracking
    iteration <- iteration + 1
    cat("Iteration", iteration, "\r")


    # Assign datapoints
    current_cost <- 0
    for (data_index in data){
      assigned_class <- 1
      min_distance <- sum((data[data_index]-centroids[[1]])**2)

      for (k_index in 2:K){
        new_dist <- sum((data[data_index]-centroids[[k_index]])**2)

        cat("newdist", new_dist, "\n")
        cat("mindist", min_distance, "\n")
        if (new_dist < min_distance){
          assigned_class <- k_index
          min_distance <- new_dist
          current_cost <- current_cost + min_distance
        }
      }

      # Assign the closest centroid index to this datapoint
      data$class[data_index] <- assigned_class
    }


    # Calclate new cost
    Costs <- c(Costs, current_cost)


    # Recalculate centroids
    for (k_index in 1:K){
      # Save centroid k_index as the mean of the datapoints in class k_index
      centroids[[k_index]] <- data %>%
        filter(class == k_index) %>%
        select(-class) %>%
        colMeans()
    }
  }

  return(centroids)
}


random_init <- function(data, K){
  # Create column of the necessary assignments

  # Setup
  points_left <- nrow(data)
  belongings <- c()
  k_index <- 1

  # Cycle around and append the k indices until all datapoints have
  # an assignment.
  while (points_left != 0){
    # Save a point belonging
    belongings <- c(belongings, k_index)
    points_left <- points_left - 1
    k_index <- k_index + 1

    # Reset k_index
    if (k_index == K+1){k_index <- 1}
  }

  # Shuffle cluster belonging
  belongings <- sample(belongings, nrow(data))

  # Assign belongings
  data$class <- belongings

  # Calculate centroids
  centroid <- list()
  for (k_index in 1:K){
    # Save centroid k_index as the mean of the datapoints in class k_index
    centroids[[k_index]] <- data %>%
      filter(class == k_index) %>%
      select(-class) %>%
      colMeans()
  }

  return(centroid)
}

#thing <- statkick_kmeans(iris, 3, init="random")
#print(thing)

#for (i in 1:4){
#  cat("Class", i, " size:", sum(thing == i), "\n")
#}


forgy_init <- function(data, K){
  # Forgy initialization
  ## Randomly choose K datapoints as initial centroids


}
