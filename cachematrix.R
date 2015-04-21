## This function is made to calculate the inverse of a matrix and store it
## in the cache, so if the same value is recalled, it take it directly from
## the cache, not recomputing the entire calculation.

## The first function, `makeCacheMatrix` creates a special "matrix",
## which is really a list containing a function to

## 1.  Set the value of the matrix
## 2.  Get the value of the matrix
## 3.  Set the value of the mean
## 4.  Get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set,
                     get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}

## The cacheSolve function calculate the inverse of the special "matrix"
## created in the makeCacheMatrix function above.
## First, it checks if the inverse of the matrix has already been calculated.
## If so, it returns it from the cache with the 'get' and skip computation.
## Otherwise, it calculates the inverse of the matrix and set the value in the
## cache with the 'setsolve' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}