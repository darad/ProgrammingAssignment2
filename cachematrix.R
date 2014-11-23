## Athor: DaraD

## Creates a special matrix with some methods and inverse attribute

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of a spcial matrix that is returned by
## makeCacheMatrix. It checks wether the inverse is already
## cached. If yes, it returns the cached version. Otherwise,
## it computes the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if (!is.null(inv)) {
     message("getting cached inverse")
     return (inv)
   }
   
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
}
