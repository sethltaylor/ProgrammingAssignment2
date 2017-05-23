## Matrix inversion can be computationally costly so there could be a 
## benefit to caching the inverse of the matrix and retrieving it for future computations.
## The following to functions are used to invert and cache the inverted matrix.

## This function creates a list containing a function that 
## sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse matrix, and gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
                      x <<- y
                      m <<- NULL
              }
              get <- function() x
              setinverse <- function(solve) m <<- solve
              getinverse <- function() m
              list (set = set, get = get, 
                    setinverse = setinverse, getinverse = getinverse)
}


## This function first checks to see if the inverse of the matrix has been computed.
## If it has then it returns the cached matrix. If it hasn't then it computes the inverse
## and stores the value in the cache using the setinverse function. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("Returning cached matrix")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

#Testing
x <- matrix(2:5, nrow =2) #Creating a matrix to invert
cache <- makeCacheMatrix(x) #Creates the list of functions to cache inverted matrix
inverse <- cacheSolve(cache) #Inverts matrix because it hasn't been computed before, and caches it
inverse2 <- cacheSolve(cache) #Retrieves already cached inverted matrix
