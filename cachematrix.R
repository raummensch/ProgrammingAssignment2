## Put comments here that give an overall description of what your
## functions do

# I am new to programming and it took me a while to understand all this.
# To my knowledge the 2 functions complement each other. The first function
# essentially holds the data matrix (x) and its inverse as a list that can
# be called upon. The second function computes, caches and prints the inverse,
# ONLY if the inverse is not already stored in the cache.

# The below comments are not nicely embedded next to the code. I found it
# easier to be understood when writing them as a summary before hand. The 
# instructions only requested comments, not a specific format.

## Write a short comment describing this function
# The following happens:
# 1. The result of makeCacheMatrix is a function with the inverse (inv) being
#    NULL.
# 2. The set function is needed to update the matrix to compute new matrix data.
# 3. get reports back the matrix.
# 4. setinv & getinv compute the inverse (setinv) and report it back (getinv)

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv)
}


## Write a short comment describing this function
# This happens:
# 1. The if-statement checks if a matrix inverse already exists. If so (meaning
#    "inv" is not empty), the "getting cached data" line AND the inverse (inv)
#    are printed.
#    However, to my understanding there is no check if the inverse is the correct inverse
#    of the data (x-matrix that we process). Hence, if we change the data (x-matrix)
#    we need to use the amatrix$set command first. Otherwise the output still prints
#    the "getting cached data" (as inv is not empty), BUT the result  is wrong.
# 2. If we have no inverse in the cache, this function aquires the data, calculates
#    the inverse, sets the new inverse and prints it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
