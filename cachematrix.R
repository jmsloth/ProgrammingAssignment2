### makeCacheMatrix and cacheSolve allow you to create a matrix with a cached inverse.  Once solved once, the inverse
### need not be computed again until the matrix is changed.


## makeCacheMatrix takes a matrix as an argument and creates a list of 4 functions.
## it stores the matrix and its inverse but these are not part of the list,
## rather the first two elements of the list, set and get, allow you to read and replace the matrix
## getinverse and setinverse likewise allow you to set the inverse and get the inverse
## This is very much like object oriented programming where only defined functions can modify
## the data values of the object/list, the locations of the data x and z are hidden and cannot be directly modified
## by a process calling the object.  This is most noticable in the set function, which makes it impossible to change
## the matrix without resetting the inverse value, z.


makeCacheMatrix <- function(x = matrix()) {
      
      z <- NULL  #initializes the inverse z as NULL
      
      set <- function(y){
            x <<- y
            z <<- NULL # this is important.  if the set function is used to change the matrix, this ensures
                        # that the inverse is also set to NULL
      }
      
      get <- function() x  # the object is a list so the get function returns the matrix itself
      setinverse <- function(inverse) z <<- inverse
      
      getinverse <- function() z
      
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)
      
}


## cacheSolve calculates the matrix inverse but only if the inverse has not yet been calculated

cacheSolve <- function(x, ...) {
      
      z <- x$getinverse()
      if(!is.null(z)){
            message("getting cached data")
            return(z)
      }
      
      data <- x$get()
      z <- solve(data, ...)
      x$setinverse(z)
      z
      
      
}
