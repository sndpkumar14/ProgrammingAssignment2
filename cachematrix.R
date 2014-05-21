## This function creates a special "matrix" object that can cache its inverse.
## It returns matrix x

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- matrix(m) m <<- x
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}
## end of function makeCacheMatrix


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It takes in matrix X as the parameter and returns its inverse.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## It is assumed that inputs matrix is a square invertible matrix for this assignment.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve  retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        cachematrix <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- matrix(data, ...)
        x$setmatrix(m)
        m
}
