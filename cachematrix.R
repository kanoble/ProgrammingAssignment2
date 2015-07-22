## Programmimg Assignment 2 - A pair of functions that cache
## the inverse of a matrix.

## Code structure is built upon the example provided in the course:
## https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3/submissions

## The following function creates a matrix that can
## cache its inverse. It sets the value of the matrix, gets
## the value of the matrix, sets the value of the inverse, 
## and gets the value of the inverse. The inverse is 
## calculated using the solve() function.

## All matrices are assumed to be invertible.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the matrix
## created using the above function. The cacheing is 
## implemented to check if the inverse has already been 
## calculated, and uses that value if so. If not, it calculates
## the inverse of the matrix and sets it in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}