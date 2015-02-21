## Programming Assognment 2

## The following function  takes an invertible matrix as an input and creates 
## a special "matrix" object that can cache its inverse. It contains list of functions to 
## (a) set the values of the matrix, (b) get the values of the matrix,
## (c) calculate the inverse of the matrix, (d) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        iX <- NULL ## iX is the name of the inverse matrix
        set <- function(y) {
                x <<- y
                iX <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) iX<<-inverse
        getinverse <- function() iX
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}


## To do test run (with NxN matrix with randomly generated elements) use the following commands
## by replacing N with any (not too big) integer number you want
## source("cachematrix.R")
## mymatrix <- matrix(rnorm(N*N),N,N)
## special_matrix <- makeCacheMatrix(mymatrix)
## inverse_of_mymatrix <- cacheSolve(special_matrix)

