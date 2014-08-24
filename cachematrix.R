## These functions allow you to cache the inverse of a matrix, so it is only 
## computed once.

## makeMatrix takes as input a matrix, and returns a list-object consisting of 
## methods to get and set data and inverse of the matrix.

makeMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve takes as input the makeMatrix object and uses the methods inthe object 
## to check if the inverse already exsists, or computes the inverse if it doesn't

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


