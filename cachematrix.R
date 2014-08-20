## Thes functions allow you to cache the inverse of a matrix, so it is only 
## computed once.

## makeMatrix takes as input a matrix, and returns a list-object consisting of 
## methods to get and compute the matrix data and inverse

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


## cacheSolve takes as input the makeMatrix object and returns the inverse if it 
## alread exsists, or computes the inverse if it doesn't

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

