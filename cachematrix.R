## 2 functions - makecacheMatrix and cacheSolve are defined in this Code
## 

## makeCacheMatrix takes a matrix vector and returns a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialze the inverse matrix
  m <- NULL 
  
  ## set function sets the initial matrix value and nullifies the inverse
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get function returns the incoming original matrix
  get <- function() x
  
  ## setinverse function takes the inverted matrix and set the variable defined initially
  setinverse <- function(invm) m <<- invm
  
  ##getinverse function returns the inverse matrix variable
  getinverse <- function() m
  
  ##finally returning the special matrix object a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated , it returns the cache.

cacheSolve <- function(x, ...) {
  
  ## step1 , get the inverse of incoming matrix..
  
  m <- x$getinverse()
  
  ## if its not null , that means its already run once and hence should be from the cache
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If not , get matrix and create the inverse
  data <- x$get()
  m <- solve(data,...)
  
  ## call the set function of the object to set its inverse in the cache
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
