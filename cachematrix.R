## Cache inverse of matrix


## Create a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function( inver ){
    inv <<- inver
  }
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Cache and use value of inverse of matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Cached data")
    return(inv)
  }
  mat <- x$get()
  inver <- solve(mat)
  x$setinverse(inver)
  inv
}
