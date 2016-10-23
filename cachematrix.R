#Functions to cache the inverse of a matrix

#Create a "special matrix object" 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        
        list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
        
}


#Return the inverse of the matrix (given a special matrix object like above), either retrieving it from cache or computing it otherwise
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
