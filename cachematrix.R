## Coursera R Programming class - Programming assignment 2

## Cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) c <<- solve
        getInverse <- function() c
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## solve for inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        c <- x$getInverse()
        if(!is.null(c)) {
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c<- solve(data, ...)
        x$setInverse(c)
        c
}
