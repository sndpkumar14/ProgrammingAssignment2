## This function creates a special "matrix" object that can cache its inverse.
## It returns matrix x

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
               
               ## << indicates setting the global variables
               ## so that it can be accessed in other function also
               
                x <<- y
                m <<- NULL
        }
        
        
        ## get matrix dimensions, 1st element is number of rows, second element is number of colums
        rw <- dim(x)[1]
        cl <- dim(x)[2]
        
        ## set values for setmatrix
        
        get <- function() x
        setmatrix <- matrix(m,rw,cl) 
        
        ## set values for getmatrix
        m <<- x
        
       
        getmatrix <- function(m)
        
        ## now create the list which will contain set and get parameters and 2 functions
        
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

cacheSolve <- function(x) {
       
        cachematrix <- function(x) {
        m <- x$getmatrix()
        
        
        ## check and exclude NULL values
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## get matrix values set in data variable 
        
        data <- x$get()
        
        ## calling solve() with just one parameter will make it return inverse of matrix
        
        m <- solve(data)
        x$setmatrix(m)
       
        ## Return m ,which is a matrix that is the inverse of 'x'
        m
}

## end of function cachesolve
