#Cached matrix will be stored in veriable "inv.matrix"
makeCacheMatrix <- function(x = matrix()) {
      inv.matrix <- NULL #setting NULL for an inverse matrix
      set <- function(y) {
            x <<- y
            inv.matrix <<- NULL
      }
      get <- function() {
            x
      }
      set.inverse <- function(inv) inv.matrix <<- inv
      get.inverse <- function() inv.matrix
#a list of functions related to a matrix that I will pass      
      list(set = set, get = get,
           set.inverse = set.inverse,
           get.inverse = get.inverse)
}


cacheSolve <- function(x, ...) {
      inv.matrix <- x$get.inverse() #checking the value of "inv.matrix"
      if(!is.null(inv.matrix)) {
            message("getting cached data")
            return(inv.matrix)
      }
      data <- x$get()
      #inv.matrix <- x$set.inverse(solve(data))
      inv.matrix <- solve(data)
      x$setmean(inv.matrix)
      inv.matrix
      ## Return a matrix that is the inverse of 'x'
}
