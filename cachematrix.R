##makeCacheMatrix: This function creates a special "matrix" object that can cache 
##                  its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##              makeCacheMatrix above. If the inverse has already been calculated 
##              (and the matrix has not changed), then cacheSolve should retrieve 
##              the inverse from the cache.


## This function returns a list of functions which sets and gets the passes in matrix 
## and caches, sets and gets its inverse matrix.
## Returns: list of functions defined on the passed in matrix.
## Throws Error if passed in matrix is not invertable.

makeCacheMatrix <- function(x = matrix()) {

    matrixInv <- NULL
    set <- function(y) {
        x <<- y
        matrixInv <<- NULL
    }
    get <- function() x
    setInv <- function(matrixInverse) matrixInv <<- matrixInverse
    getInv <- function() matrixInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function takes in a special matrix created by the makeCacheMatrix function
## and computes its inverse if it is not already computed. 
## Returns: inverse matrix

cacheSolve <- function(x, ...) {
    matrixInv <- x$getInv()
    if(!is.null(matrixInv)) {
        message("getting cached data")
        return(matrixInv)
    }
    data <- x$get()
    matrixInv <- solve(data, ...)
    x$setInv(matrixInv)
    matrixInv
}
