## makeCacheMatrix sets up 4 functions that get called by cacheSolve
## These functions handle storing and getting things from memory as needed.

makeCacheMatrix <- function(x = matrix()) {  # establish a function that takes a matrix as input
  iv <- NULL                                 # set this variable as NULL initially
  
  set <- function(y) {                       # store the incoming matrix as x and  
    x <<- y                                  # make sure iv is NULL
    iv <<- NULL
  }
  
  get <- function() x                        # this returns x when get is called
  
  setinvert <- function(solv) iv <<- solv    # takes in a matrix and sets iv equal to that matrix  
  
  getinvert <- function() iv                 # returns iv
  
  list(set = set, get = get,                 # returns a list with these values
       setinvert = setinvert,
       getinvert = getinvert)
}

## cacheSolve uses the memory handling functions established by makeCacheMatrix and 
## performs the actual calculation of the inverted matrix if it is not already cached.

cacheSolve <- function(cim, ...) {    # takes the list of 4 functions that makeCacheMatrix returns
  iv <- cim$getinvert()               # runs the getinvert function and sets iv equal to it 
  if(!is.null(iv)) {                  # Look for inverted matrix in cache
    message("getting cached data")    # if matrix is already cached, says so
    return(iv)                        # and returns the cached version
  }
  data <- cim$get()                   # runs the get function of makeCacheMatrix and stores it as data
  iv <- solve(data, ...)              # runs the built-in function solve on data
  
  cim$setinvert(iv)                   # runs the setinvert function of makeCacheMatrix on iv
  # (storing the inverted matrix as iv)
  
  iv                                  # returns iv
}
