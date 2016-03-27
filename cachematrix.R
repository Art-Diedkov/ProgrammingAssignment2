## As  the matrix inversion is usually a costly computation there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly


## The function below is storing a special matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  rev <- NULL
  set <-function(y) {
    x<<-y
    rev<<- NULL
  }
  get <- function () x
  setinv <- function(inv) rev <<- inv
  getinv <- function() rev
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## A function that computes the inverse of the special matrix
## returned by the function above (using "$"). 
## In case if the inverse has been already calculated and 
## contents did not change, the cachesolve below will return
## the inverse form cache (and while retriving 
## it will print "getting casched data")

cacheSolve <- function(x, ...) {
  ## Return matrix that is inverse of 'x'
        rev <- x$getinv()
        if(!is.null(rev)) {
          message("getting cahed data")
          return(rev)
        }
      mx <- x$get()
      rev <- solve(mx, ...)
      x$setinv(rev)
      rev
}
