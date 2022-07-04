# Cached calculation of the inverse of a matrix

# Example usage
# > r1 <- c(2, -1, 0)
# > r2 <- c(1, 2, -2)
# > r3 <- c(0, -1, 1)
# > m1 <- rbind(r1, r2, r3)
# Create the 
# > m2 <- makeCacheMatrix(m1)

# > cacheSolve(m2)
# r1 r2 r3
# [1,] -1.110223e-16  1  2
# [2,] -1.000000e+00  2  4
# [3,] -1.000000e+00  2  5

# (the inverse matrix is now cached)

makeCacheMatrix <- function(x = matrix()) {
  ## x: a matrix
  ## return: the inverse matrix and a list containing functions to
  ##              1. set matrix
  ##              2. get matrix
  ##              3. set inverse matrix
  ##              4. get inverse matrix
  
  inv <- NULL # Initialize the variable holding the cached inverse matrix
  
  # set the value of the matrix
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment.
   
    # Set the value of the matrix 
    x <<- y
    # Initialize the variable holding the cached inverse matrix
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # setinv to set the value of the inverse matrix
  setinv <- function(solve) inv <<- solve

  # getinv to return the value of the inverse matrix
  getinv <- function() inv

  # Build a list with the getter and setter functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  ## x: a matrix
  ## return: the inverse matrix
  ##     If already calculated earlier, the inverse is retrieved from cache
  ##     Otherwise computed from scratch
  
  # get the value (NULL if empty, otherwise cached value of inverse matrix)
  inv <- x$getinv()
  
  # return cached matrix if available (= already computed before)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # the cache is empty:
  # compute the inverse of the matrix
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse matrix
  x$setinv(inv)
  
  # return the inverse of the matrix
  inv
}
