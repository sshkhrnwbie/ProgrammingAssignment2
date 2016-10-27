## Caching the inverse of a matrix

## MakeCacheMatrix creates a list of functions for given matrix which are:
## 1. set() = Set the value of the matrix
## 2. get() = Get the value of the matrix
## 3. setinv() = Set the value of the inverse
## 4. getinv() = Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv<<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## CacheSolve calculates inverse for the special matrix 
## created using above function. 
## IF INVERSE ALREADY CALCULATED -> (return inverse using getinv)
## ELSE -> (Calculate inverse and set it using setinv and return the same)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
          message("Getting cached inverse of matrix")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinv(inv)
        inv
}
