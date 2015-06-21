## Together, these two functions create a special object that 
#stores a matrix and caches its inverse. 

#makeCacheMatrix creates a list of functions that:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix

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


#The following function calculates the inverse of the matrix stored in the list 
#created with the above function, if it has not already been calculated. 
#If the inverse has already been calculated, it skips the computation, gets the 
#inverse matrix from the cache and sets the value of the inverse in the cache 
#via the setinverse() function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
