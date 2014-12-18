## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(x = matrix()) {
        m <- NULL ## set initial inverse value to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL

        }

        get <- function(){ x}
        setInverse <- function(Inverse) {m <<- Inverse} #set the Inverse of the matrix
        getInverse <- function() {m} #get the Inverse of the matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve<- function(x ) {
        m <- x$getInverse()
        if(!is.null(m)) { #Check if any cache value available
                message("getting cached data")
                return(m)# return cached inverse of matrix
        }
        data <- x$get() # get the matrix and load into data
        m <- solve(data) # inverse the matrix and store in  m
        x$setInverse(m) #set Inverse of the matrix
        m # return Inverse value
}