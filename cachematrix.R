## Given a square matrix returns the inverse

## Prepare a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## Null the inverse matrix
    set <- function(y) { ## If a new matrix is set then set the new matrix and null the inverse
        message("Setting the matrix and NULLing the inverse")
        x <<- y
        inv <<- NULL
    }
    get <- function() x ## Return the matrix
    setinv <- function(solve) inv <<- solve ## Solve for the matrix and store in inv
    getinv <- function() inv ## Return the inverse matrix
    list(set = set, get = get, ## Define our function list 
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() ##Call getinv above with our cacheable matrix
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    message("No data cached, solving inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
