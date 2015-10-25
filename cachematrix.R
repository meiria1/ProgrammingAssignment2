# This code enables caching the inverse of a matrix uppon calculation
# When the inverse is to be calculated a second time, the cached matrix is
# returned, rather than calculating it again

# Create a matrix with the ability to cache it's inverse
# The function receives a square matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(solve) inv<<- solve
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


# Calculate the inverse of the matrix
# if the inverse was previously calculated, then use the cached value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}

