## The function cacheMatrix calculates the inverse matrix 
##calculated from a matrix x created by the function
##makeCacheMatrix.


## The function makeCacheMatrix creates a special matrix object
##that can cache its inverse and contains functions to:
##-set and get the value of the matrix,
##-set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv<<- NULL
        set <- function(y) {
                x <<- y
                inv<<- NULL
        }
        get <- function() x
        setinverse<-function(solve) inv<<-solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## The function cacheSolve computes the inverse 
##of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv              
}
