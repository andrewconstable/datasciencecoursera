## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse
## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

## dynamic_matrix <- makeCacheMatrix(matrix(c(1, 0, 0, 1), 
##                   nrow = 2, ncol = 2))
## typeof(dynamic_matrix) "list"
## dynamic_matrix$get() returns matrix(c(1, 0, 0, 1), 
##                   nrow = 2, ncol = 2)
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## dynamic_matrix$getinverse() if NULL is returned:
## data <- dynamic_matrix$get() calls the original matrix
## i <- solve(data, ...) calculates the inverse
## dynamic_matrix$setinverse(i) sets the inverse
## now you can call dynamic_matrix$getinverse()
## subsequent calls to cacheSolve will return TRUE for the 
## 'if' statement printing the message and dynamic_matrix$getinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
