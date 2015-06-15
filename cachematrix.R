# makeCacheMatrix function
# Arguments:
#   x: a matrix
# Returns:
#   A function that stores the matrix and its inverse (when calculated) and has the aditional functions set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
        # Sets inv to null
        inv <- NULL
        # function that stores the initial value to the matrix stored and sets its inverse to null
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # function that returns the matrix stored
        get <- function() x
        # functions that sets the inverse of the matrix stored (calculated on the funcion cacheSolve)
        setinverse <- function(inverse) inv <<- inverse
        # Function that returns the inverse of the stored matrix
        getinverse <- function() inv
        # Adds the definition of the previsous symbols
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve function
# Arguments:
#   x: a matrix
#   and any other arguments to be passed to the solve function (the actual funcion that computes the inverse of a matrix)
# Returns:
#   A matrix that is the inverse of the matrix x in the argument. When the calculation is performed by the first time, the inverse matrix is stored and return all the other times the function is called
cacheSolve <- function(x, ...) {
        # Get the value stored for the inverse of the matrix
        inv <- x$getinverse()
        # check if it's null
        if(is.null(inv)) {
                # If it is null, display the message "Calculated data"
                message("Calculated data")
                # Get the matrix from x
                data <- x$get()
                # Compute the inverse of x and pass on any extra arguments provided
                inv <- solve(data, ...)
                # Store the inverse for later use
                x$setinverse(inv)
                # Return the computed matrix
                return(inv)
        }
        else {
                # If it is not null, display the message "Cached data"
                message("Cached data")
                # And return the stored matrix inv
                return(inv)
        }
}

# One of the possible ways to test this assignement. Please uncoment to use
#x <- makeCacheMatrix(matrix(rnorm(36), 6, 6))
#x$get()
#y <- cacheSolve(x)
#y
#cacheSolve(x)
#x$get() %*% y
