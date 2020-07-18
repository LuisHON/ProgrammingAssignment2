## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix: This function creates a special "matrix" 
   ## object that can cache its inverse.

makeCacheMatrix = function(m = matrix()) {
   
   # creates a Null object to store the Inverse Matrix
   InverseMatrix = NULL
   
   # method to set the matrix
   set = function(newmatrix) {
      m <<- newmatrix
      InverseMatrix <<-NULL
   }
   
   # method to get the matrix
   get = function() m
   
   # method to set the inverse matrix
   setInverse = function(inv) InverseMatrix <<- inv
   
   # method to get the inverse matrix
   getInverse = function() InverseMatrix
   
   # return a list of methods
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Write a short comment describing this function

## 2. cacheSolve: This function computes the inverse of the special "matrix"
   ## returned by makeCacheMatrix above. If the inverse has already
   ## been calculated (and the matrix has not changed), then the
   ## cachesolve should retrieve the inverse from the cache.

cacheSolve = function(x, ...) {
   
   # get the inverse matrix
   InverseMatrix = x$getInverse()
   
   # if martrix already have data, display message and
      # returns the existent inverse matrix
   if(!is.null(InverseMatrix)) {
      
      message("getting cached data")
      
      return(InverseMatrix)
   }
   
   # variable dat receive the matrix
   dat = x$get()
   
   # InverseMatrix receive the calculated inverse of the matrix
   InverseMatrix = solve(dat,...)
   
   # set the result to InverseMatrix
   x$setInverse(InverseMatrix)
   
   # Return the result Matrix
   InverseMatrix
}