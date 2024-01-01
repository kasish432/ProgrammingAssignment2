# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) {
    x <<- y       
    inv <<- NULL  
  }
  get <- function() x   # Get matrix from cache matrix object
  set.inver <- function(inverse) inv <<- inverse  # Set the cached inverse in the cache matrix object
  get.inver <- function() inv     
  list(set = set, get = get,      
       set.inver = set.inver,
       get.inver = get.inver)
}

# Function to compute the inverse of the special "matrix" and use caching
cacheinverse <- function(x, ...) {
  inv <- x$get.inver()    
  if(!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  matrix_inverse <- x$get()   
  inv <- solve(matrix_inverse, ...)
  x$set.inver(inv)
  inv                       
}
