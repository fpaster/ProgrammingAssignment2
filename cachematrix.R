

# this function creates a matrix container with additional functions for setting and getting it's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){				#sets the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x				#returns the matrix
    setInverse <- function(inv) i <<- inv	#sets the inverse
    getInverse <- function() i			#returns the inverse
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)

}


#this function gets the inverse of the given matrix container
# if cached from the cache 
# if not it is calculated
cacheSolve <- function(x, ...) {
    i <- x$getInverse()				#gets the inverse from the matrix (possibly null if not available)
    if(!is.null(i)){				#if available returns it at function returns
        message("getting cached data")
        return(i)
    }
    data <- x$get()				#if not then the inverse is calculated and set new
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
