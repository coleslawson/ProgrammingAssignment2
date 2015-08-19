## These functions calculate the inversion of a matrix and then cache it
## This is useful because then we can find the inversion of a matrix
## without actually wasting the time to calculate it again and again

## Creates 4 functions for our matrix and puts them into a list
## These functions are then the arguments for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        #sets the variable for the inverse to be null
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL 
        }
        #returns the matrix
        get <- function() {x}
        #stores inverse
        setinv <- function(solved) {inv <<- solved}
        #returns inverse
        getinv <- function() {inv}
        #list with these 4 functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks to see if the inverse of a matrix is already stored. If so it
## returns the inverse and ends.
## If not it solves for the inverse, passes the inverse to be stored,
## and then returns the inverse
## NOTE: Function assumes that matrix is always invertible 
cacheSolve <- function(x, ...) {
        #returns inverse of x
        #y <- as.data.frame(x)
        inv <- x$getinv() 
        #checks to see if inverse is already calculated
        if(!is.null(inv)) { 
                message("getting cached data")
                #if not, it returns the inverse and function ends
                return(inv) 
        }
        #gets matrix
        data <- x$get()
        #solves for matrix inverse
        inv <- solve(data, ...)
        #passes the inverse to be stored
        x$setinv(inv)
        #returns inverse
        #message("alive")
        x$getinv()
}
