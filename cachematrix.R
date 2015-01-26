makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of the matrix's inverse
        setinverse <- function(solve) m <<- solve
        
        # get the value of the matrix's inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {

        # get the value of the matrix from the previous function
        m <- x$getinverse()
        
        # if that value is not null, they fetch it from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        # otherwise, calculate the inverse of the matrix...
        m <- solve(data, ...)
        
        # ...and set the value of the new matrix!
        x$setinverse(m)
        m
}
