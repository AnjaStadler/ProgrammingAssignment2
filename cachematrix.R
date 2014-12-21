# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<- function(x = numeric()) {
    # initialize variable i containing inverse matrix value if already calculated, otherwise NULL
    i <- NULL
    # save the original matrix to variable x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # returns the original matrix
    get <- function() x
    # saves the inverse matrix to variable i
    setinverse <- function(inverse) i <<- inverse
    # returns the inverse matrix or NULL if not yet set
    getinverse <- function() i
    # constructing a list from the matrix
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    # assigns the cached inverse value to the variable m
    m <- x$getinverse()
    # check if the inverse has already been calculated
    if(!is.null(m)) {
        # inverse has already been calculated; return value and message
        message("getting cached data")
        return(m)
    }
    # inverse has not been calculated yet
    # get the original matrix
    data <- x$get()
    # calculate the inverse
    m <- solve(data, ...)
    # save inverse matrix to the cache matrix
    x$setinverse(m)
    # return inverse matrix
    m
}

