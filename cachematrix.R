## Anil Satram
## Date: 26-Sep-2015
## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix  <- NULL
    
    set  <- function(y){
        x <<- y
        invMatrix <<- NULL 
    }
    
    get  <- function() x
    
    setinverse  <- function(inverse){
        invMatrix  <<- inverse
    }
    
    getinverse  <- function() invMatrix
    
    list(set= set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## Provides inverse of square matrix. If the inverse is available already in cache,
## then the cachesolve retrieves the inverse from the cach

cacheSolve <- function(x, ...) {
    invMatrix  <- x$getinverse()
    if (!is.null(invMatrix)){
        message("getting cached data")
        return(invMatrix)
    }
    data  <- x$get()
    invMatrix  <- solve(data, ...)
    x$setinverse(invMatrix)
    invMatrix
}
