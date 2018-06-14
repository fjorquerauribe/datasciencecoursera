## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setInverse <- function(inv){
        i <<- inv
    }
    
    getInverse <- function(){
        i
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the matrix returned by the 
## above function. If the inverse has already been calculated,
## then this function should return the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    i <- solve(data)
    
    x$setInverse(i)
    
    i
}
