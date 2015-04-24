
## The function creates one matrix object which is stored in the cache and then inverse that fuction.
makeCacheMatrix <- function(x = matrix()) {
  flag <- NULL
  set <- function(y) {
    x <<- y
    flag <<- NULL
}

get <- function() x
setinv <- function(inverse) flag <<- inverse
getinv <- function() flag
list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The cacheSolve function return the inverse of the matrix (makeCacheMatrix). 
##If the inverse has been calculatedthen the cachesolve should retrieve the inverse from the cache
## just if the matrix has not changed.


cacheSolve <- function(x, ...) {
  flag <- x$getinverse()
  if(!is.null(flag)) {
    message("Getting Cached Data :)")
    return(flag)
  }
  data <- x$get()
  flag <- solve(data)
  x$setinverse(flag)
  flag
}
