#makeCacheMatrix creates a list of functions for setting and getting the inverse of a matrix
#input: square matrix
#output: list of functions with the input matrix as argument

makeCacheMatrix <- function(x = matrix()) {

    #allocate variable for inverse matrix
    m <- NULL
    #define set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #define get function
    get <- function() x
    #define inverse function
    setinverse <- function(solve) m <<- solve
    #output function
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}



## cacheSolve caculates inverse of a matrix but checks first if inverse was already calculated
#input: list of functions from makeCacheMatrix
#output: inverse Matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    #check if inverse was already calculated
    if(!is.null(m)) {
        message("getting cached data")
        #output:
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    #output:
    m
        
}