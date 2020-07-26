## ASSIGNMENT 2 - LEXICAL SCOPING
## ==============================

## OBJECTIVE: 
## A pair of functions that, between them, create and cache the inverse of a matrix.

## FUNCTION 1: makeCacheMatrix
## ---------------------------
## Creates a special matrix object that can cache its inverse.
## The input matrix is assumed to be a square, invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverted <<- solve
    getinverse <- function() inverted
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    print(set,get,setinverse,getinverse)
}

## FUNCTION 2: cacheSolve
## ----------------------
## Computes the inverse of a special matrix returned by makeCacheMatrix
## If the inverse has already been calculated and cached, and the matrix hasn't been 
## changed, the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
    inverted <- x$getinverse()
    if(!is.null(inverted)) {
        message("Getting cached data")
        return(inverted)
    }
    matrixdata <- x$get()
    inverted <- solve(matrixdata, ...)
    x$setinverse(inverted)
    inverted
}
