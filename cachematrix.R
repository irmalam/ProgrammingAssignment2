## makeCacheMatrix creates a special “matrix”, which is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvM <- function(invM) m <<- invM
    getinvM <- function() m
    list(set = set, get = get,
    setinvM = setinvM,
    getinvM = getinvM)
}


## cacheSolve computes the inverse of the special “matrix” created with the makeCacheMatrix
## function. It first checks to see if the inverse has already been computed. If so, it
## retrieves the inverse from the cache, and skips the computation. Otherwise, it computes
## the inverse of the matrix and sets the value of the inverse in the cache via the
## setinvM() function.

## For this assignment, assume that the matrix supplied is always invertible.
## No need to check
cacheSolve <- function(x, ...) {
    m <- x$getinvM()
	   data <- x$get()
       
       ## If the inverse has already been calculated (and the matrix has not changed),
       ## then the cacheSolve should retrieve the inverse from the cache
       if ((!is.null(m)) && (identical(x, data))) {
           message("getting cached data")
           return(m)
       }
       
       m <- solve(data, ...)
       x$setinvM(m)
       m
}
