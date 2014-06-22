## Below are two functions that are used to create a special matrix and cache
## its inverse. The purpose of these functions is to avoid repeating a potentially
## complex matrix inverse calculation.

## The following function makeCacheMatrix, creates a special type of matrix.
## It is also used to set the value of the matrix, get the value of the
## matrix, set the value of the inverse and get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function below computes the inverse of the matrix, created from the
## first function. The function below first checks if the inverse has already 
## been computed, if this is the case, we just retrieve the cached inverse.
## If not, we then go ahead and compute it. 

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() ##calling the get inverse method from our previous function
  
  if(!is.null(m)) {
    message("getting cached data..")
    return(m)
  }
  
  ##the inverse hasn't been previously computed so we do it below:
  
  data <- x$get()
  m <-solve(data)
  x$setinverse(m)
  m
}
