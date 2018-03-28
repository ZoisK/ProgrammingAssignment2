## ############################################################################
## A special matrix object that caches it's inverse so that the computationally 
## costly inverse is caclulated only once and returned from the cache 
## makeCacheMatrix - creates the matrix object 
##      - defining methods: get, set, getinv, setinv
## cacheSolve - is used to caclulate the inverse and store it in cache
##
## USAGE:
## Assuming some invertible matrix A 
## (1) Create the special matrix object my_A invoking makeCacheMatrix 
## > my_A <- makeCacheMatrix( A)
## (2) Caclulate the inverse  
## > cacheSolve( my_A)
## Note that repeated caclulation is simply returning the cached value
## (informational message is displayed)
## (3) get the matrix A
## > my_A$get()
## (4) get the inverse matrix of A
## > my_A$getinv()
##
## Further functions (set)
## - my_A$set(), my_A$setinv() are meant rather as "internal"
## however they are available as well. 
## ############################################################################


# #############################################################################
# makeCacheMatrix
# create a "special' matrix object that can cache it's inverse
# INPUTS: 
#   x - a matrix (conventional matrix data structure in R)
# RETURNS: 
#   the following list of methods for the matrix object: 
#   get - returns the matrix 
#   set - sets the matrix 
#   getinv - returns the inverse of the matrix
#   setinv - sets the inverse of the matrix (used by cacheSolve)


makeCacheMatrix <- function(x = matrix()) {
    # properties
    inv <- NULL
    
    # methods
    # set - set the matrix and reset the inverse to NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # get - returns the matrix    
    get <- function() x 
    
    # setinv - set the inverse matrix in the cache    
    setinv <- function(i) { 
        inv <<- i
    } 
    
    # getinv - returns the inverse    
    getinv <- function() inv
    
    # return the methods as a list
    list( set = set, get = get, setinv = setinv, getinv = getinv)
}

# #############################################################################
# cacheSolve
# return the inverse of the input matrix. Use the cache if it's available
# or calculate if not. The input matrix has to be created by makeCacheMatrix 
#
# INPUTS: 
#   x - a matrix created by makeCacheMatrix
# RETURNS: 
#   the inverse of the matrix x
#

cacheSolve <- function(x, ...) {
    
    # get the inverse matrix using getinv method
    inv <- x$getinv()
    
    # if the inverse matrix is not null it was already calculated 
    # No need to calculate again! 
    # So return the cached inverse matrix 
    # (also print a message for diagnostic purposes)
    if( !is.null( inv)) {
        
        message("using cached data")
        
        return( inv)
    }
    
    # if the inverse matrix is null => it has already calculated 
    # It has to be calculated!  
    # (also print a message for diagnostic purposes)
    
    # get the matrix, using the get method 
    mtx <- x$get()
    
    # calculate the inverse
    # Note: explicit assumption that the  matrix supplied is always invertible
    # thus no error handling, only suppress error messages for practical use
    #
    inv <- try( solve(mtx), silent = TRUE)
    
    # set the inverse matrix, using the setinv method
    x$setinv( inv)
    
    # return the calculated inverse matrix 
    inv
}
