## makeCacheMatrix will create an environment providing the ability to cache.
## More practically, it will return a list of functions allowing one to get and
## set a matrix and its inverse.

## cacheSolve makes use of the set of functions and the environment 
## created by makeCacheMatrix(), and uses that to intelligently 
## return either the cached inverse of a matrix, or, when a cache is not
## available to compute the inverse, set the cache, and return the inverse.

## The below function will return a list of functions
## that can be used to get and set a matrix and its inverse.

## NOTE: When I use the word "global" in the below, it more precisely means the 
## parent namespace, scope, or environment.

makeCacheMatrix <- function(x = matrix()) {
  
  # Guarantee internally scoped i is NULL
  i <- NULL
  
  # Set the global matrix x with passed-in y
  # When setting a new matrix, need to clear the inverse cache, i,
  # because it is invalidated.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Return the global matrix, x
  get <- function() x
  
  # Set the global inverse cache matrix, i
  setInverse <- function(inv) i <<- inv
  
  # Return the global inverse cache matrix, i
  getInverse <- function() i
  
  # Return a list of the above-defined functions 
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## The below function will be used to return the inverse of a matrix, x
## If the inverse already exists "in cache" i.e., the global environment,
## then the function will intelligently use that, instead of performing the 
## computation. Otherwise, the inverse matrix computation is performed on x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  # If we have a cache, return that and exit function immediately
  if(!is.null(i)){
    message("getting cached data")
    return(m)
  }
  
  # Get the original matrix
  data <- x$get()
  
  # Call solve, with no b arg, which returns the inverse of the first arg matrix
  i <- solve(data, ...)
  
  # Set the cache "global" inverse matrix that can be used next time cacheSolve is called
  # on x.
  x$setInverse(i)
  
  # Return the inverse
  i
}
