## The functions in this file offer a function for calculating the
## inverse of a matrix using caching to improve performance for
## cases in which the inverse of a specific matrix is calculated
## more than once. The functions are adapted from the examples 
## provided in the course instruction file on 
## https://github.com/rdpeng/ProgrammingAssignment2

## An example for using these functions is below. First, initialise
## an object capable of holding a cached inverse by calling it with
## a normal matrix object (see matrix() for more information):
##
##   my_matrix <- matrix(data = c(1, 2, 3, 4), nrow=2, ncol=2)
##   m <- makeCachematrix(my_matrix)
##
## Now, to get the inverse of this matrix, simply do:
##
##   cacheSolve(m)
##
## This will use the cached version, if available. To update
## the matrix contained in the object without instantiating a new
## one, you can do:
##
##   new_matrix <- matrix(data = c(5, 6, 7, 8), nrow=2, ncol=2)
##   m$set(new_matrix)

## The makeCacheMatrix function creates "matrix object" (actually a
## list) which is capable of storing a matrix as well as the inverse
## calculated for that matrix. The return value of this function
## can be specified as an argument to the cacheSolve function. In
## addition to this, the end user can call the 'set' and 'get' list
## items to update and retrieve the matrix stored in the "matrix
## object" (not entirely unlike using object orientation).

makeCacheMatrix <- function(x = matrix()) {

    # Initialise the label 'inverse' that will hold the cached inverse 
    # so that searches done with the '<<-' operator stop in this 
    # environment.
    cached_inverse <- NULL
    
    # Set the matrix; this also 'invalidates' any previously calculated
    # inverse.
    set <- function(new_x) {
        x <<- new_x
        cached_inverse <<- NULL
    }
    
    # Get the matrix contained in this object.
    get <- function() x
    
    # Store and retrieve any calculated inverse of this matrix.
    setinverse <- function(inverse) cached_inverse <<- inverse
    getinverse <- function() cached_inverse
    
    # Now return a list to the caller with all the functions we
    # have just set up.
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## The cacheSolve function returns the inverse of a matrix, just like
## the base solve function does, but it uses caching to improve
## performance on subsequent calls. Note that we do not check for
## variations in the parameters after the first one (exactly like
## in the example provided).

cacheSolve <- function(x, ...) {
    
        # Check to see whether we have an inverse in the cache already.
        inverse <- x$getinverse()
        if (is.null(inverse)) {
            
            # No inverse yet; we calculate it.
            matrix <- x$get()
            inverse <- solve(matrix, ...)
            
            # And store it in the cache for the next time.
            x$setinverse(inverse)
            
        } else {
            
            # Let the user know we're returning a cached value.
            message("using cached value for inverse")
            
        }
        
        # Return either the cached or freshly calculated inverse.
        inverse
            
}
