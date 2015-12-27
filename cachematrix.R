## Two functions that can be used to create a matrix object that can be used to cache the inverse
## Usage: Create a matrix object and give it a matrix: x <- makeCacheMatrix(matrix(rnorm(9),3,3))
##        Get the inverse matrix by calling: cacheSolve(x)

##
# Function to create a matrix object in which the inverse can be stored
# Argument: x = matrix
##
makeCacheMatrix <- function(x = matrix()) {
    # Cached inverse
    inverse <- NULL
    
    ## Define getters and setters
    set <- function(y) {
        x <<- y
        # new matrix data so clear cached inverse
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


##
# Function to return the inverse of matrix 'x$get()'
# Arugment: x = makeCacheMatrix object
##
cacheSolve <- function(x, ...) {
    ## Check whether inverse matrix exists in cache
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        # Return cached inverse matrix
        message("returning cached data")
        return(inverse)
    }
    
    ## Get matrix and calculate inverse
    m <- x$get()
    inverse <- solve(m, ...)
    
    ## Cache result
    x$setinverse(inverse)
    inverse
}
