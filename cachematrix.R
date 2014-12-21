#Cached matrix will be stored in veriable "inv.matrix"
#-----
#Note: When working with the code, assign
      #m1 <- matrix(c(4,2,7,5),2,2)
      #x<-makeCacheMatrix(m1) 
#or
      #x<-makeCacheMatrix(matrix(c(4,2,7,5),2,2)
#in order to use cacheSolve().
#-----
makeCacheMatrix <- function(x = matrix()) {
      inv.matrix <- NULL #setting NULL if there is no inverse matrix
      #defining the "set" function
      set <- function(y) {
            x <<- y
            inv.matrix <<- NULL
      }
      #defining the "get" function
      get <- function() {
            x  #get the matrix
      }
      #defining the "set.inverse" and "get.inverse" functios
      set.inverse <- function(inv) inv.matrix <<- inv
      get.inverse <- function() inv.matrix
      #a list of functions related to a matrix that I will pass      
      list(set = set, get = get,
           set.inverse = set.inverse,
           get.inverse = get.inverse)
}


cacheSolve <- function(x, ...) {
      inv.matrix <- x$get.inverse() #checking the value of "inv.matrix"
      if(!is.null(inv.matrix)) { #if there is an inverse matrix already
            message("getting cached data") #cache it
            return(inv.matrix) #and return
      }
      data <- x$get() #othervise get the matrix
      inv.matrix <- x$set.inverse(solve(data)) #calculate it's inverse
      inv.matrix #and return it
}
