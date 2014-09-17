## makeCacheMatrix allows to define and manage matrices with the possibility to cache their inverse
##
## usage: m <- makeCacheMatrix(m1)
##  where m1 (optional) is a matrix
##   an error is returned if m1 is not a square matrix (since only square matrices can be inverted)
## this done, the following functions are available:
##  retrieve the contents of m: m$get()
##  change the contents of m to matrix m2: m$set(m2))
##   this causes the cached inverse to be reset
##  set the cached inverse of m to minv: m$setinverse(minv)
##   this function does not check whether the minv is indeed the proper inverse of m
##  get the cached inverse of m: m$getinverse()

makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix allows to define and manage matrices with the possibility to cache their inverse
                
        ## if a value is provided for x, check that it is a square matrix indeed
        if (!is.null(x)) {
                stopifnot(is.matrix(x),nrow(x)==ncol(x))
        }
        ## initialize inverse to null
        xinv <- NULL
        
        ## change the contents of the matrix
        set <- function(y) {
                        ## if a value is provided for y, check that it is a square matrix indeed
                if (!is.null(y)) {
                        stopifnot(is.matrix(y),nrow(y)==ncol(y))
                }
                ## change the present contents of the matrix
                x <<- y
                ## reset inverse to null
                xinv <<- NULL
        }
        
        ## retrieve the present contents of the matrix
        get <- function() x
        
        ## set the cached inverse matrix
        setinverse <- function(minv) {
                ## check that minv is a square matrix with dimensions identical to x
                stopifnot(is.matrix(minv),nrow(minv)==ncol(minv),nrow(minv)==nrow(x))
                ## set the cached inverse matrix
                xinv <<- minv
        }
        
        ## retrieve the cached inverse matrix
        getinverse <- function() xinv
        
        ## return the list of functions available for the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve allows to compute and cache the inverse of a matrix defined using makeCacheMatrix
##
## usage: cacheSolve(m)
##  this returns the inverse of m
##  relying on R to stop with various messages when m has not been created with makeCacheMatrix hence no parameter type checking
##  if m already has its inverse cached (i.e. m$getinverse() returns something) then the cached value is returned
##  if m doesn't have its inverse cached yet (i.e. m$getinverse() returns NULL) then the inverse is computed, cached and returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##  relying on R to stop with various messages when x has not been created with makeCacheMatrix 
        ##   hence no parameter type checking
                
        ## check if inverse has already been cached
        minv <- x$getinverse()
        
        ## if yes then just return the cached value
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        
        ## else retrieve the contents of x and compute its inverse
        ##  note that x is assumed to be invertible, hence no explicit error handling
        ##  any extra arguments '...' are passed through to the solve call
        data <- x$get()
        minv <- solve(data, ...)
        
        ## cache the computed inverse matrix
        x$setinverse(minv)
        
        ## and return it
        minv
}
