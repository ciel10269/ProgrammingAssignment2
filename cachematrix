
##Overview: An R function that is able to cache potentially time-consuming computations.
##An R function to caching the inverse of a matrix rather than compute it repeatedly.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()){

    invs <- NULL
    
    ##set the value of the matrix
    set <- function( y ) {
        m <<- y
        invs <<- NULL
    }
    
    ##get the value of the matrix
    get <- function() m
    
    ##set the value of the mean
    setinvs <- function(i) invs <<- i
    
    ##get the value of the mean
    getinvs <- function() invs
    list(set = set, get = get,
         setinvs = setinvs,
         getinvs = getinvs)
    
}

##cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(m,...){
    
    invs <- m$getinvs()
    ##If the inverse has already been calculated (and the matrix has not changed), 
    ##then the cachesolve should retrieve the inverse from the cache.
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    data <- m$get()
    invs <- solve(data, ...)
    m$setinvs(invs)
    invs
}
