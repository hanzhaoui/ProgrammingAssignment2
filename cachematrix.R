## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(i) inv <<- i
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cashed data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
