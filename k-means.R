kmeans <- function(k,data,rep) {
  centroids <- matrix(nrow = k, ncol = ncol(data))
  for ( i in 1:nrow(centroids)) {
    centroids[i,1] <- sample(data[,1], 1)
    centroids[i,2] <- sample(data[,2], 1)
  }
  
  error <- matrix(nrow = rep, ncol = 1)
  
  for ( r in 1:rep ) {
    # compute distance between n points and k centroids
    distance <- matrix(nrow = nrow(data), ncol = nrow(centroids))
    for ( j in 1:ncol(distance)) {
      for ( i in 1:nrow(distance)) {
        x <- c(data[i,1], data[i,2])
        y <- c(centroids[j,1], centroids[j,2])
        distance[i,j] <- dist(rbind(x,y))
      }
    }
    
    # update centroids as means of n in k
    nrow(distance[which(distance[,1] > distance[,2]),])
    nrow(distance[which(distance[,1] < distance[,2]),])
    xsum1 <- 0
    xsum2 <- 0
    ysum1 <- 0
    ysum2 <- 0
    for (i in 1:nrow(data)) {
      if (distance[i,1] > distance[i,2]) {
        xsum1 <- xsum1 + data[i,1]
        ysum1 <- ysum1 + data[i,2]
      }
      else 
        xsum2 <- xsum2 + data[i,1]
      ysum2 <- ysum2 + data[i,2]
    }
    centroids[1,1] <- xsum1/nrow(distance[which(distance[,1] > distance[,2]),])
    centroids[1,2] <- ysum1/nrow(distance[which(distance[,1] > distance[,2]),])
    centroids[2,1] <- xsum2/nrow(distance[which(distance[,1] < distance[,2]),])
    centroids[2,2] <- ysum2/nrow(distance[which(distance[,1] < distance[,2]),])
    
    error[r,1] <- sum(distance[which(distance[,1] > distance[,2]),2]) + sum(distance[which(distance[,1] < distance[,2]),1])
    
  }
  labels <- matrix(nrow = nrow(data), ncol = 1)
  for ( i in 1:nrow(labels) ) {
    x <- c(data[i,1], data[i,2])
    y1 <- c(centroids[1,1], centroids[1,2])
    y2 <- c(centroids[2,1], centroids[2,2])
    d1 <- dist(rbind(x,y1))
    d2 <- dist(rbind(x,y2))
    if (d1 > d2) {
      labels[i,1] <- "two"
    }
    else
      labels[i,1] <- "one"
  }
  return_list <- list("centroids" = centroids, "labels" = labels, "error" = error)
  return(return_list)
}