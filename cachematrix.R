## Matrix compuation are usually time consuming and caching helps to avoid repeated operations
## Computing the inverse of the Matrix is time comsuming and caching the 
## inverse will help to avoid the repeated compuation
## functions does cache the inverse of the Matrix

## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(y) {
		x <<- y
		minv <<- NULL
    }
	get <- function() x
	setinverse <- function(solve) minv <<- solve
	getinverse <- function() minv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it does inverse of the Matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  # get the inverse from the cache
  minv <- x$getinverse()
  # If cache holds the inverse of the current matrix, just returns the inverse
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  # if Inverse is not solved then do the inverse and cache
  data <- x$get()
  minv <- solve(data, ...)
  x$setinverse(minv)
  minv
}
