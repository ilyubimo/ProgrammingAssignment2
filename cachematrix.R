## This two functions allow to save time and memory when 
## matrix inversion operation is frequently used. 
## Whith this setup, matrix invertion is applied only if 
## inverted matrix is not in cache already.

## This function creates a "special vector" (which technically is a list)
## for input sqaured invertable matrix. "Special vector" defines functions to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function takes the input "special vector" created by makeCacheMatrix 
## based on invertable square matrix. and returns its inverse matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' (but x is not matrix here)
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
