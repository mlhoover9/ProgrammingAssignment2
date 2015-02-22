## The following functions will calculate the inverse of a matrix, and if the matrix
## has been cached before, instead of repeatedly computing it, the inverse of the matrix
## will be returned from the cache.  

## The function makeCacheMatrix will create a matrix, which is a list containing
## a function that will set and get the value of the matrix, and set and get the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list (set = set, get = get,
              setinverse = setinverse, 
              getinverse = getinverse)
}


## the cacheSolve function will first check to see if the inverse of the matrix created
## with the above function has already been calculated, and if so it will skip the
## computation. If the inverse has not been calculated already, it will caclulate the
## inverse of the data and will set the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
