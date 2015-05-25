#Main function to read a matrix and create inverse matrix and match them to catch cache
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
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
#If Cache is found return from inverse rather than processing it
cacheSolve <- function(x, ...) {

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
 
        inv <- solve(data, ...)

        x$setinverse(inv)
   
         inv
 
}
