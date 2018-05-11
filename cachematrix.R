## This is composed of two functions, the first part is the one making cache 
## to store values in a separate environment, and the second function solves
## the function and retrieves values from cache if it is available

## This function creates the list of cached inverse of matrices

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

## This function solves the value of the function, but if it was previously
## solved, it calls the stored value in cache

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



#Test
mymatrix <- matrix(rnorm(16),nrow = 4,ncol = 4)
mymatrix
cachmatrix1<-makeCacheMatrix(mymatrix)
cachmatrix1$get()
cacheSolve(cachmatrix1)

