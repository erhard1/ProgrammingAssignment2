## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMtrix creates a special "matrix",
## which is really a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<-  NULL
        }
        get <- function() x
        setinvs <- function(invert) invs <<- invert
        getinvs <- function() invs
        list(set = set, get = get,
             setinvs = setinvs,
             getinvs = getinvs)
}

## The following function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in
## the cache via the setinvs function.

cacheSolve <- function(x, ...) {
        invs <- x$getinvs()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinvs(invs)
        invs
}