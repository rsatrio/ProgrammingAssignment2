
# This function create a special Matrix that contain a variable to hold 
# its inverse
# Example of usage :
# m<-matrix(c(1:4), 2, 2)
# mcache<-makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
  
  ## Check whether the data supplied is matrix/not.
  ## If not a matrix, throw error
  if (!is.matrix(x)) {
    stop("The data supplied must be a matrix")
  }
  
  #Initialize the inverse variable
  i<-NULL
  
  #Set the matrix data
  set <- function(m1)  {
    
    # Check whether the data supplied is matrix/not
    # If not a matrix, throw error
    
    if (!is.matrix(m1)) {
      stop("The data supplied must be a matrix")
    }
    
    x <<- m1
    i <<- NULL
    
  }
  
  #Get the matrix data
  get <- function() x
  
  # Setting the value of the inversed Matrix
  setInverse <- function(inverseMatrix) i <<- inverseMatrix
  
  # Getting the value of the inversed Matrix
  getInverse <- function() i
  
  #Return the function as a list
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  #Finish
}




# This function takes variable of makeCacheMatrix 
# and calculate its inverse (if none exist), or
# show its inverse from the cache ( if it exist)
# Example of usage :
# m<-matrix(c(1:4), 2, 2)
# mcache<-makeCacheMatrix(m)
# cacheSolve(mcache)
# mcache$getInverse()


cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x' (if any)
  inverse <- x$getInverse()
  
  # Check the cache 
  # if cache is not exist , then calculate the inverse. 
  # If cache exist, printout the cache to screen
  
  
  if (is.null(inverse)) {
    print(" Calculate inverse")
    dataMatrix <- x$get()
    inverse <- solve(dataMatrix)
    x$setInverse(inverse)
    return(inverse)
  } else  {
    print("Printing from cache")
    
    return(inverse)
  }
  
  #Finish
}
