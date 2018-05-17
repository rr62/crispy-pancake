
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# The second function, cacheSolve, (a) tries to  get any cached solution, (b) if no cached solution exists, it solves for the inverse of the matrix
# There are unit tests included at the end of this source file.
# These tests can be executed with the following command:  source('~/cachematrix.R')

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

 v <- makeCacheMatrix(matrix(c(4,5,6,1), 2, 2))
v$get()
v$getinverse()
cacheSolve(v)
cacheSolve(v)
v$getinverse()
