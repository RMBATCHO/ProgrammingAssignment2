## The makeCacheMatrix function, creates a matrix that cache the value of the 
## inverse so that when we need it again, it can be looked up in the cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        } ## Sets the value of the matrix.
        get <- function() x ## Gets the value of the matrix. 
        setinverse <- function(inverse) i <<- inverse ##Sets the value of the inverse.
        getinverse <- function() i ## Gets the value of the inverse.
        data <- data.frame(set, get, setinverse, getinverse)
        matrix(data, nrow = 1 ,  ncol = 4 )
}


## The following function, cacheSolve,  computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Then it returns a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        ## checks whether the inverse has been calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i) ## If previous calculation was done, the inverse is retrieved from the cache.
        }
        data <- x$get()
        i <- solve(data, ...) ## If no previous calculation was done, it calculates the inverse of the data.
        x$setinverse(i) ## it then sets the value of the inverse in the cache via the setinverse function.
        i
}
