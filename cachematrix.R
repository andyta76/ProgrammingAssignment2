
## makeCacheMatrix is a function which allows a matrix to be cache
## Contains sub-methods: get(), set(matrix), setsolve(matrix)
## and getsolve()

## written by: Andy Ta
## Date: 18/09/2014 for R Programming Assignment 2


makeCacheMatrix <- function(x = numeric()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <- NULL
        }
        get <- function() {
                x
        }
        setsolve <- function(inverse) {
                i <<- inverse
        }
        getsolve <- function() {
                i
        }
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is a function which caches the inverse of a matrix input
## Uses makeCacheMatrix function

## written by: Andy Ta
## Date: 18/09/2014 for R Programming Assignment 2

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}