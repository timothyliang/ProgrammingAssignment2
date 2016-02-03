## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
        invsmat <- NULL
        set <- function(y) {
                x <<- y
                invsmat <<- NULL
        }
        get <- function() x
        setinvs <- function(solve) invsmat <<- solve
        getinvs <- function() invsmat
        list(set = set, get = get,
             setinvs = setinvs,
             getinvs = getinvs)
}

## This function will calculate the inverse of the matrix captured above. 
## But it will determine whether the inverse has been calculated or not. 
## If the requested matrix is the same and inverse of it exists then it will
## return that inverse. Otherwise, it will go ahead and caculate the inverse. 
cacheinvs <- function(x, ...) {
        invsmat <- x$getinvs()
        if(!is.null(invsmat)) {
                message("getting cached data")
                return(invsmat)
        }
        data <- x$get()
        invsmat <- solve(data, ...)
        x$setinvs(invsmat)
        invsmat
}

## Here is a test run for the functions. 
t <- rnorm(10000)
x <- matrix(t, nrow = 100, ncol = 100)
tempmat <- makeCacheMatrix(x)
cacheinvs(tempmat)
