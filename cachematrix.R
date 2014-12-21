## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) mat_inv <<- solve
  getinv <- function() mat_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  matrix <- x$get()
  mat_inv <- solve(matrix,...)
  x$setinv(mat_inv)
  mat_inv
}
