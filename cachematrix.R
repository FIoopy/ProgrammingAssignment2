## makeCacheMatrix creates a list with the following functions:
## 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x 
        setinv <- function(inv) m <<- inv 
        getinv <- function() m 

        list(
                set = set, 
                get = get,
                setinv = setinv,
                getinv = getinv)       
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getinv() 
        if(!is.null(m)) { 
                message("getting cached data")
                return(m) 
        }
        data <- x$get() 
        m <- solve(data) 
        x$setinv(m) 
        m 
}
