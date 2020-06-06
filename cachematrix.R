# A pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" 
# object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    ## Initialize the inverse property
    m <- NULL
    ## Method to set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Method the get the matrix
    get <- function() x
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) m <<- inverse
    ## Method to get the inverse of the matrix
    getInverse <- function() m
    
    # Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special "matrix" returned by the 
## makeCacheMatrix function above.If the inverse has already been 
## calculated(and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)  # Just return the inverse if its already set
    }
    data <- x$get() # Get the matrix from our object
    m <- solve(data, ...) #Calculate the inv using matrix multiplication
    
    x$setInverse(m) # Set the inverse to the object
    
    m # Return the matrix
}
