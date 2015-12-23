## The following functions provide the ability to make a matrix with 
# added feature of holding cache values to save time in computing an 
#an inverse of a matrix by caching results


## makeCacheMatrix is a wrapper function to create a matrix
# with added functionality of holding its inverse.
makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y){
    x <<- y;
    minverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) minverse <<- inv
  getinverse <- function() minverse
  
  list(set=set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve is a function, given a matrix created using 
#makeCacheMatrix it will calculate the inverse and set it back
#as matrix property if it does not already exist
cacheSolve <- function(x, ...) {
  minverse <- x$getinverse()
  if(!is.null(minverse)){
    message("getting cached matrix inverse data")
    return (minverse)
  }
  
  minverse <- solve(x$get())
  x$setinverse(minverse)
  ## Return a matrix that is the inverse of 'x'
  minverse
        
}
