

# this function creates a matrix container with additional functions for setting and getting it's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)

}


#this function gets the inverse of the given matrix container
# if cached from the cache 
# if not it is calculated
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
