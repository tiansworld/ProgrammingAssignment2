## Put comments here that give an overall description of what your
## functions do

## Two functions below are used to create a special object that 
## stores a matrix and cache its inverse matrix.

## makeCahematrix returns a list containing:
## 1. set matrix x
## 2. get matrix x
## 3. set the inverse of x
## 4. get the inverse of x
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Caculates the inverse matrix of the matrix x created with
## the function above, if inverse matrix has been caculated,
## it get the inverse matrix from the cache, and skip rest of
## the computation. Otherwise, it caculates the inverse matrix
## and set that inverse matrix to the cache via setinverse
## function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
