## these functions cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

## the funcion:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse)
                invs <<- inverse
        getinverse <- function() invs
        
        list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invs <- x$getinverse()
        
        if (!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        return (invs)
}
