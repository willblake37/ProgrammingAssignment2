## The following code contains two functions that cache the inverse of a matrix
## Calculating the inverse of a matrix can be costly in terms of processing time
## Caching allows a pre-calcuated result to be retrieved from the cache 

## The makeCacheMatirx function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    ## Sets the value of the matrix  
    s <- NULL
    set <- function(y) {
    x <<- y
    s <<- NULL
}

## The following gets the value of the matrix
get <- function() x

## The following sets the value of the inverse of the matrix
setmatrixinverse <- function(solve) s <<- solve
getmatrixinverse <- function() s

## The following gets the value of the inverse of the matrix
list(set = set, get = get,
     setmatrixinverse = setmatrixinverse,
     getmatrixinverse = getmatrixinverse)
}

## The cacheSolve function checks whether a result is stored in the cache
## If no result stored it calculates the inverse of the matrix

cacheSolve <- function(x = matrix(), ...) {
  
  ## Gets the inverse of the matrix
  s <- x$getmatrixinverse()
  
  ## Checks whether there is a result stored in the cache
  if(!is.null(s)) {
    message("Retreiving Cached Data")
    return(s)
  }
  
  ## If no cached result the following code calculates the inverse of the matrix
  result <- x$get()
  
  ## The following calculates the inverse of the matrix
  s <- solve(result, ...)
  
  x$setmatrixinverse(s)
  
  ## Result displayed
  s
}