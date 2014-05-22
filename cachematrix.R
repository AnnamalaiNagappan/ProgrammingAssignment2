# Example:
# > z <- matrix(rnorm(4), nrow = 2) // Create a matrix x
# > cz <- makeCacheMatrix(z) // Create our special matrix
# > cz$get() // Return the matrix
# > cacheSolve(cz) // Return the inverse
# > cacheSolve(cz) // Call the 2nd time, so return
# // the cached inverse


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

        # inve will store the cached inverse matrix
        inve <- NULL

        # Set the matrix
        set <- function(y) {
                x <<- y
                inve <<- NULL
        }

        # Get the matrix
        get <- function() x

        # Setter for the inverse
        setinverse <- function(inv) inve <<- inv

        # Getter for the inverse
        getinverse <- function() inve

        # Return the matrix with our newly defined functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve: Calculate the inverse of the matrix. If the inverse of the matrix is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
    invr <- x$getinverse()

    # If the inverse of matrix is already there, return it.
    if (!is.null(invr)) {
        print("Getting Cached Data")
        return(invr)
    }

     # Inverse is not available in the cache, so calculate it.
    data <- x$get()
    invr <- solve(data, ...)

    # Set the inverse and cache it.
    x$setinverse(invr)

    # Return the inverse
    invr
}