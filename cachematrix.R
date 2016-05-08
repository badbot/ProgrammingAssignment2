
## This function creates  a list of get and set functions for a value of a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  #This function set the value of the matrix 
  set <- function(y) {  
    x <<- y
    inv <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse matrix
  setInverseMatrix <- function(invMatrix) inv <<- invMatrix
  #get the value of the inverse matrix
  getInverseMatrix <- function() inv

  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)  
}


#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverseMatrix()
  if (!is.null(inv)) { 
    #If the inverse has already been calculated (and the matrix 
    #has not changed), then cacheSolve should retrieve the inverse from the cache
    message("getting cached inverse matrix")
    return(inv)
  }  
  data <- x$get()
  inv <- solve(data)
  x$setInverseMatrix(inv)
  inv
  
}
