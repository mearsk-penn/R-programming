## Caches the inverse of a matrix 

## Creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Retrieves the inverse of the matrix from the cache or computes the inverse 
## if it doesn't already exist 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- inverse(data, ...)
    x$setinverse(inv)
    inv
        
}