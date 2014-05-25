## This R script contains a pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It sets up an object that is a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## subfunction set will set the value to the matrix
        set <- function(y) {
                ## we will set the value to the matrix in the parental environment
                x <<- y
                m <<- NULL
        }
        
        ## subfunction get will get the value of the matrix
        get <- function() x
        
        ## subfunction setinverse will set the value of the inverse in the parental environment
        setinverse <- function(solve) m <<- solve
        
        ## subfunction getinverse will get the value of the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)      
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## check whether the inverse of the matrix has already been calculated
        ## if yes, retreve the value of the cached inverse matrix
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if inverse matrix has not yet been calculated, use the solve function
        ## assign the calculated inverse matrix in the parental environment by calling the setinverse subfunction
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

## Test the script

mat <- makeCacheMatrix()
## object mat is initialized

mat$set(matrix(c(2,3,2,4,5,5,78,65,22), 3, 3))
## the subfunction set is used to set the matrix stored in x

mat$get()
## the subfunction get is used to get the matrix stored in x

cacheSolve(mat)
## uses the cacheSolve function to get the inverse of the matrix for the first time

cacheSolve(mat)
## uses the already calculated and cached inverse matrix
