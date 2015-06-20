## This function calculate inverse matrix by solve().
## And it will cache the calculation, and be called out by cacheSolve() if it is already calculated.

## foo <- makeCacheMatrix(yourmatrix)
## cacheSolve(foo)
## if you execute cacheSolve(foo) again, you will see message "getting cached data", since it is 
## already saved in the cached enviroment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Write a short comment describing this function

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
