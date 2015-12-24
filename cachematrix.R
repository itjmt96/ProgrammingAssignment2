## Matrix inversion is usually a costly computation 
##     and there may be some benefit to caching the inverse 
##     of a matrix rather than computing it repeatedly

## These 2 functions can be use to cache the inverse of a matrix

## The 1st function creates the matrix object and caches its inverse
## It returns a list of functions used by cacheSolve function to
##    to return the inverse of matrix:
##      1. set the matrix
##      2. get the matrix
##      3. set inverse of the matrix
##      4. get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        cacheX <- NULL
        set    <- function(y) {
                                x      <<- y
                                cacheX <<- NULL
        }
        
        get        <- function() x
        setinverse <- function(inverse) cacheX <<- inverse
        getinverse <- function() cacheX
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The 2nd function checks returns the inverse of the matrix 'x'
## If the inverse of the matrix is cached it returns the cached information
## If the inverse of the matrix is not cached it will calculate the inverse
##    of the matrix, set it in cache and return the information
##      1. Get cached data if it exists
##      2. Get data to cache if it does not exist in cache
##      3. Cache data
##      4. Return cached data

cacheSolve <- function(x, ...) {
        cacheX <- x$getinverse()
        
        ## Getting cached data if it exists
        if(!is.null(cacheX)) {
                print("Getting cached data.")
                return(cacheX)
        }
        ## Getting data to cache if it does not exist
        data <- x$get()
        cacheX <- solve(data, ...)
        
        ## Cache data
        x$setinverse(cacheX)
        print("Caching data")
        
        ## Return cached data
        cacheX
}
