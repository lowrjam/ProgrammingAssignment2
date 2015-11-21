## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
    #variable inv stores inverse of matrix x
    inv <- NULL
    ## function set replaces value of matrix x with matrix y
    ## and resets value of inv to NULL
    set <- function(y) {
        ## note us of <<- operator which changes values of
        ## x and inv outside the scope of the set function
        x <<- y
        inv <<- NULL
    }
    ## function get simply returns the value of x
    get <- function() x
    ## function setinv replaces inv with new value newinv
    ## note use of <<- operator which changes value of inv outside the scope of setinv function
    setinv <- function(newinv) inv <<- newinv
    ## function getinv simply returns the value of inv
    getinv <- function() inv
    ## create list of functions accessible to makeCacheMatrix function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by the function makeCacheMatrix

cacheSolve <- function(x = matrix(), ...){
    ## get the value of inv by calling the getinv function of object x
    inv <- x$getinv()
    ## check if inv is not null
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if inv is null calcualte the inv value
    ## first get object x's invertible matrix
    data <- x$get()
    ## then compute the inverse of the matrix using solve
    inv <- solve(data, ...)
    ## then set the value of inv with the new inverse matrix
    x$setinv(inv)
    ## return inv
    inv
}
