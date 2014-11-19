## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #In general, I followed the same procedure as in the example given in the description
  #of the assignment.
  
  invmatrix <- NULL #this is the inverse of the matrix that is kept in the
                    #environment defined by this function 
  
  #The set function is used to modify the matrix you want to get the inverse from
  setmatrix <- function(y) {
    x <<- y # this is where the previous matrix is replaced by the new one you provide (y)
    invmatrix <<- NULL #this cleans the inverse of the previous matrix
  }
  
  getmatrix <- function() x #this just prints the matrix you have in memory in this environment
  
  setinverse <- function(inverse) invmatrix <<- inverse #it sets the new inversem atrix
  
  
  getinverse <- function() invmatrix  #it prints the inverse of the matrix in  the environment

  #now, lets make the list with all the options as in the example
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$getmatrix()
  message("calculating inverse")
  invmatrix <- solve(data)
  x$setinverse(invmatrix)
  invmatrix
}
