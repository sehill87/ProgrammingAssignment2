## The following two functions demonstrate how nested functions can be utilized
## to demonstrate the scoping rules of the R language 

## makeCacheMatrix creates a matrix object that cacheSolve utilitzes to find
## the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
    m <-NULL
    set <- function(y) {
        x <<- y
        m <<-NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the Matrix retruned by makeCacheMatrix.
## if cacheSolve is called again on the same output from makeCasheMatrix, 
## the inverse is retrieved from the cache. 

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}        ## Return a matrix that is the inverse of 'x'

