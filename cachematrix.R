## makeCacheMatrix takes x, a square invertible matrix 
## and returns a special matrix; a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix
## <<- is used to assign the object value to a different environment

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(solve) i <<- solve
        geti <- function() i 
        list(set = set, get = get,
             seti = seti ,
             geti = geti )

}


## cacheSolve takes x, the output of makeCacheMatrix 
## and returns the inverse of the matrix originally input
## into the makeCacheMatrix function
## Before computing the if(!is.null) checks if inverse has
## been calculated already
## If it has it takes the inverse from the cache
## If not it computes it

cacheSolve <- function(x, ...) {

        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$seti(i)
        
	  return(i)
}
