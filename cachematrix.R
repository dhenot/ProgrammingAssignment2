## These functions allow to store a matrix in an object able to cache
## its inverse matrix


## This function creates the special cache matrix object
## which holds the matrix and its cached inverse (if already computed)

makeCacheMatrix <- function(x = matrix()) {
    # Store the cached inverse matrix (NULL when not computed)
    s <- NULL
    
    # Set/get the matrix (reset the cached inverse matrix when storing a new matrix)
    set <- function(newMatrix) {
        x <<- newMatrix
        s <<- NULL
    }
    get <- function() x
    
    # Set/get the cached inverse matrix
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    # Construct the result object from the 4 previous functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function takes an object created by makeCacheMatrix and returns the
## inverse of its matrix. It is retrieved from cache if available, otherwise
## it is computed and stored in the cache

cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse matrix
    s <- x$getsolve()
    
    # Check if we have a valid cached value
    if(!is.null(s)) {
        message("getting cached data")
    } else {
        # Cache was empty : compute the inverse and store the result in cache
        s <- solve(x$get(), ...)
        x$setsolve(s)
    }
    
    # Return the inverse matrix
    s
}
