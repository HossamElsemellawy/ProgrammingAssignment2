## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverM <- NULL
        set <- function(y) {
                x <<- y
                inverM <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverM <<- inverse
        getinverse <- function() inverM
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverM <- x$getinverse()
        if(!is.null(inverM)) {
                message("getting cached data")
                return(inverM)
        }
        data <- x$get()
        inverM <- solve(data, ...)
        x$setinverse(inverM)
        inverM
}
