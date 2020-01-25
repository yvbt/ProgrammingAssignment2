## Here is set of functions that illustrate caching of the inverse of a matrix. 
## Functions below demonstrate the concept of lexical scoping used for retrieve values from objects
## based on the way functions are nested when they were written

## makeCacheMatrix() creates an R object that stores a matrix and its inverse
## we have access to the specific functions in its list and have access to the entire environment defined by makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  ## define the default value of x as an empty matrix
  
  ## initialization of storage for caching value of solve matrix
  m_store <- NULL
  
  ## define function for setting value to an object in the parent environment
  set <- function(new_obj) {
    x <<- new_obj
    m_store <<- NULL
  }
  
  ## define function for retrieves matrix object from the parent environment of makeCacheMatrix()
  get <- function() x
  
  ## define function to assign value of inverse matrix in the parent environment
  setsolve <- function(solve_matrix) m_store <<- solve_matrix
  
  ## define function for retrieves cached value of the solve matrix
  getsolve <- function() m_store
  
  ## create the list elements is what allows us to use the $ form of the extract operator to access the functions by name
  ## eg. x$get() instead x[[2]]
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve is required to retrieve the inverse matrix from an object of type makeCacheMatrix()
## cacheSolve is able to calculate and store the matrix object for the input argument of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## try to retrieve a inverse matrix from the object passed in as the argument
  m_store <- x$getsolve()
  
  ## if the value here is not equal to NULL, we have a cached inverse matrix and return it to the parent environment
  if(!is.null(m_store)) {
    ## getting cached data
    return(m_store)
  }
  ## If the value above is FALSE, cacheSolve gets the matrix from the input object and calculates a solve()
  ## set the inverse matrix in the input object and then returns the value to the parent environment
  data <- x$get()
  m_store <- solve(data, ...)
  x$setsolve(m_store)
  m_store
}
