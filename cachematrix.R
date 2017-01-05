## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL # Member that holds the inverse matrix
  m_matrix <- x     # The matrix to invert
  
  # Inverse matrix getter
  getInverse <- function() {
    return(m_inverse)
  }
  
  # Inverse matrix setter
  setInverse <- function(invMat) {
    m_inverse <<- invMat
  }
  
  # Returns the original Matrix
  get <- function() {
    return(m_matrix)
  }
  
  # Setter of the input matrix
  # Clears the inversed matrix in cache if any
  set <- function(m) {
    m_matrix <<- m
    m_inverse <<- NULL
    
  }

  list(getInverse = getInverse,
       setInverse = setInverse,
       set = set,
       get = get)
}


## Computes the matrix invert if necessary and stores it in the cache object
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Returning cached matrix")
    return (inv)
  }
  # Computing the inverse
  inv <- solve(x$get())
  x$setInverse(inv)
  return(inv)
}
