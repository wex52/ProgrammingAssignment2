## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function builds a set of four functions and
## returns the functions within a list. Each function has its own environment.
## The function will also have two data objects, x and s.

makeCacheMatrix <- function(x = matrix()) { # initialize x, avoid call error
    s <- NULL                               # initialize s
    set <- function(y) { # mutator method
        x <<- y          # assign input y to x object in makeCacheMatrix parent
        s <<- NULL       # clears previous s, if x is reset
    }
    get <- function() x  # retrieves x from makeCacheMatrix parent
    setsolve <- function(solve) s <<- solve  # sets value of s in parent
    getsolve <- function() s                 # gets value of s from parent
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## The cacheSolve function returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
    s <- x$getsolve()                    # attempts to retrieve solution of x
    if(!is.null(s)) {                    # if s already exists, returns it
        message("getting cached data")
        return(s)
    }
    data <- x$get()        # s is NULL. Gets matrix.
    s <- solve(data, ...)  # solves matrix
    x$setsolve(s)          # sets solution
    s                      # returns solution
}
