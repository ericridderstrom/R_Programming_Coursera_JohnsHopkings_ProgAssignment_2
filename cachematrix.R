## This first function creates an object (a matrix) to cache it's inverse.
## We follow the assumption that the matrix will always be invertible.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse_matrix <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse_matrix <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inverse_matrix <<- inv;
  getinv <- function() return(inverse_matrix);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This second function computes the inverse matrix of the output (special matrix) from the first function.
## If the inverse matric has already been calculated, then it retrieves it from the cache. The 'if' function takes care of 
## reviewing that condition.

cacheSolve <- function(mtx, ...) {
  inverse_matrix <- mtx$getinv()
  if(!is.null(inverse_matrix)) {
    message("Getting cached data...")
    return(inverse_matrix)
  }
  data <- mtx$get()
  invserse_matrix <- solve(data, ...)
  mtx$setinv(inverse_matrix)
  return(inverse_matrix)
}
