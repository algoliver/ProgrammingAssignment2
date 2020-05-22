## This R Code creates two functions in order to create an special matrix object
## and calculate its inverted matrix and cache it. In this way, computation time can be saved

## makeCacheMatrix creates a special matrix object (a list) where the 
## inverted matrix will be saved and recovered later.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinv <- function(inver) mat <<- inver
        getinv <- function() mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve does three steps: 
## 1) recovers the inverted matrix saved
## 2) checks if this inverted matrix is not null
## 3) if it is null, computes an inverted matrix with the function solve
## 4) Saves the new inverted matrix in the list for later use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinv()
        if(!is.null(mat)) {
                message("getting inverse matrix in cache")
                return(mat)
        }
        dtx <- x$get()
        mat <- solve(dtx)
        x$setinv(mat)
        mat
}
