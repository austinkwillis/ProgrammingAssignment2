#The two methods below demonstrate in-memory caching
#of an inverse matrix. The initial computation will
#be cached, and the methods will determine and return
#this version if the computation has already taken place.

#make cache inverse takes an input parameter of a standard square matrix (it assumes the
#input matrix will have an inverse solvable by R's solve). makeCacheMatrix sets up
#methods to be used by cacheinverse to return the inverse either by calculation
#or a cache, based on whether the i variable is set)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheinverse checks the input makeCacheMatrix environment matrix for a solved inverse.
#if one has not been calculated, it does so and outputs "getting cached data" along with
#the inverse, otherwise on the inverse is returned.
cacheinverse <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}