## Assignment 2 
## In this file there are 2 functions that show the properties of lexical scoping in R


#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #In general, I followed the same procedure as in the example given in the description
  #of the assignment.
  
  #this is the inverse of the matrix that is kept in the environment defined by this function
  #when you call the makeCacheMatrix it sets it equat to NULL
  invmatrix <- NULL 
  
  
  #The set function is used to modify the matrix you want to get the inverse from
  set <- function(y) {
    x <<- y # this is where the previous matrix is replaced by the new one you provide (y)
    invmatrix <<- NULL #this cleans the inverse of the previous matrix
  }
  
  get <- function() x #this just prints the matrix you have in memory in this environment
  
  setinverse <- function(inverse) invmatrix <<- inverse #it sets the new inversem atrix
    
  getinverse <- function() invmatrix  #it prints the inverse of the matrix in  the environment

  #now, lets make the list with all the options as in the example
  #This list is the "special ""matrix"" object" asked for in the assignment. It is obviously NOT
  #a matrix, but since the assignment was vague about what exaclty the function was supposed to 
  #return I decided to keep it the same as in the example
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  
  invmatrix <- x$getinverse() #
  
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  data <- x$get()
  message("calculating inverse")
  invmatrix <- solve(data)
  x$setinverse(invmatrix)
  invmatrix
}
