## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve ## use the value of m from the invoking environment and calculate the inverse matrix
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve checks to see if the inverse of the matrix
## is in the list generated in makeCacheMatrix
## If it is, it retrieves the stored variable
## If not, it calculates and stores the variable back to the list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()  ## Get the Inverse
     if(!is.null(m)) {    ## Return inverse unless it is empty
          message("getting cached data")
          return(m)
     }
     data <- x$get()    ## retrieve orignal matrix
     m <- solve(data, ...)  ## calculate inverse matrix
     x$setInverse(m)    ## assign inverse matrix to original variable 
     m
}
