
#makeCacheMatrix creates a special "matrix" object that can cache its inverse. It
# contains functions to: Set the matrix, get the matrix, set the inverse, and get the 
#inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse as NULL
  inv <- NULL
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate the cache when the matrix changes
  }
  # Function to get the matrix
  get <- function() x
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  # Function to get the inverse
  getInverse <- function() inv
  # Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse is already cached and the matrix hasn’t changed, it retrieves 
# the inverse from the cache; if not, it computes the inverse, caches it, and returns it.
cacheSolve <- function(x, ...) {
  # Get the cached inverse
  inv <- x$getInverse()
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  # If not, compute the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse using solve()
  # Cache the inverse
  x$setInverse(inv)
  # Return the inverse
  inv
}

#Test case:
# Create a square invertible matrix
mat <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
# Create the special "matrix" object
special_mat <- makeCacheMatrix(mat)
# Compute the inverse (first time, it will calculate)
inverse1 <- cacheSolve(special_mat)
print(inverse1)
# Retrieve the inverse from the cache (second time, it will use the cache)
inverse2 <- cacheSolve(special_mat)
print(inverse2)
