## Caching the inverse of a given matrix
## Zhaofei in 2021 July

## An example runs like that
## aMatrix <- makeCacheMatrix(myMatrix)
## > aMatrix$set(myMatrix)
## > cacheSolve(aMatrix)

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

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
