## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.  The "matrix" object is a list that (1) sets the value of the vector, (2) gets the value of the vector, (3) sets the value of the inverse, and (4) gets the value of the inverse.  The function "cacheSolve" then computes the inverse of the matrix returned by "makeCacheMatrix" function.  If the inverse has not been computed, data gets the matrix from makeCacheMatrix, m calculates the inverse, and x$setinverse(m) stores it in the object m of makeCacheMatrix.

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.  

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


## The function "cacheSolve" computes the inverse of the special "matrix" returned by the function "makeCacheMatrix."  If the inverse has already been calculated and the matrix has not changed, the cacheSolve should skip the computation and retrieve the inverse from the cache.  If the inverse has not been computed, data gets the matrix from makeCacheMatrix, m calculates the inverse, and x$setinverse(m) stores it in the object m of makeCacheMatrix.

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

