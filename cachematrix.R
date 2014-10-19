# ------
# Below functions allow matrices of a specific type to be created
# for which the inverse matrix can be stored in the cache.
#
# As a first step function makeCacheMatrix is called on an invertible matrix and
# the output assigned to a variable (e.g. "my_matrix <- makeCacheMatrix(<matrix>)").
# Then function cacheSolve is called on that variable (e.g. "cacheSolve(my_matrix)").
# This prints the inverse of my_matrix and stores it in the cache.
# The inverse matrix is available then via the getinverse-function ("my_matrix$getinverse()").
# When cacheSolve is called again on that matrix the inverse is taken from the cache.
#
# ------
# Please note that I took the example functions from the assignment description and 
# modified them as needed.
# ------


# Function makeCacheMatrix takes an invertible matrix as input.
# Its output is a list of four functions:
# - set: change the matrix
# - get: print the matrix
# - setinverse: store the inverse in the cache
# - getinverse: print the inverse if it is available in the cache - print NULL otherwise
makeCacheMatrix <- function(x = matrix()) {

        # Initialization
        inverted_matrix <- NULL

        # Define function set: change the matrix, make sure the obsolete inverse is removed from the cache
        set <- function(y) {
                x <<- y
                inverted_matrix <<- NULL
        }

        # Define function get: print the matrix
        get <- function() x

        # Define function setinverse: store the inverse in the cache
        setinverse <- function(inverse) inverted_matrix <<- inverse

        # Define function getinverse: print the inverse
        getinverse <- function() inverted_matrix

        # This is the result of the function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Function cacheSolve takes a list that was created with function makeCacheMatrix as input.
# Its output is the inverse of the contained matrix.
cacheSolve <- function(x, ...) {

        # Get inverse from the cache
        inverted_matrix <- x$getinverse()

        # If the inverse is not NULL anymore it means that it could be found in the cache
        if(!is.null(inverted_matrix)) {
                message("getting cached data")
                # Leave the function, the function-result is the inverse from the cache
                return(inverted_matrix)
        }

        # get the matrix
        data <- x$get()

        # determine inverse
        inverted_matrix <- solve(data, diag(nrow(data)), ...)

        # store inverse in the cache
        x$setinverse(inverted_matrix)

        # the inverse is the result of the function
        inverted_matrix
}

