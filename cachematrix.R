## Here are 2 functions: makeCacheMatrix stores functions and creates a special
## "matrix" object that can cache its inverse. cacheSolve computes the inverse of the
##special "matrix" returned by makeCacheMatrix.

## makeCacheMatrix stores 4 functions (get, set, setinverseMatrix, getinverseMatrix).
## get returns the matrix x stored in the main function. set changes the matrix stored in
## the main function. setinverseMatrix and getinverseMatrix do the same than set and get by storing
## and return the value of the input in a variable m into the main function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverseMatrix <- function (solve) m <<- solve
        getinverseMatrix <- function() m
        list (set = set, get = get,
              setinverseMatrix = setinverseMatrix,
              getinverseMatrix = getinverseMatrix)
}


## cacheSolve verifies the value m, stored previously in getinverseMatrix, exists and is not NULL.
## If it is in memory it returns a message and the value m. Otherwise it gets the matrix stored with
## makeCacheMatrix, m calculate the inverse and stores it in the object generated and assigned with
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverseMatrix(m)
        m
}