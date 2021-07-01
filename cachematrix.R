## Caching the inverse of a given matrix
## Zhaofei in 2021 July

## An example runs like that
## aMatrix <- makeCacheMatrix(myMatrix)
## > aMatrix$set(myMatrix)
## > cacheSolve(aMatrix)

##  This function creates a special "matrix" object that can cache its inverse.

##define the argument with default mode of "matrix"
makeCacheMatrix <- function(x = matrix()) {
        ## initialize i as NULL; will hold value of matrix inverse 
        i <- NULL
        set <- function(matrix) { 
                ## value of matrix in parent environment
                m <<- matrix
                ## if there is a new matrix, reset inv to NULL
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
        ## get the inversed metrix from cach
        m <- x$getinverse()
        ## existing m will return the value but not do further calculation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## calculate the inverse of the matrix
        m <- solve(data, ...) 
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
        
}
