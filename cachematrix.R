## This file contains two functions, first fuction caches a matrix and its inverse
## and the second function checks if inverse of the first matrix is already
## computed and computes if the inverse is not previously computed

## The first function , makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function()x
        setsolve <- function(solve) I <<- solve
        getsolve <- function() I
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##The following function calculates the inverse of the special "vector" created with the above function. 
##However, it first checks to see if the inveres matrix has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        I <- x$getsolve()
        if(!is.null(I)) {
                message("getting cached inverse matrix")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setsolve(I)
        I
} 

