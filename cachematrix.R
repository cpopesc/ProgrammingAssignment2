# R Programming - Assignment 2 - cacheSolve function
# by Cristian Popescu (August 6th, 2014)
#
#

#this function will create the basic operations to apply to a matrix object
#it will store in invMatrix the inverse matrix computed
makeCacheMatrix <- function(x=numeric(), numrow, numcol) {
  #we convert the vector into a matrix
  myMatrix <- matrix(data = x, nrow=numrow, ncol=numcol, byrow=FALSE, dimnames=NULL)
  invMatrix <- NULL
  
  #set the current matrix and resets the inverse matrix previously computed
  set <- function(y) {
    myMatrix <<- y
    invMatrix <<- NULL
  }
  
  #get current matrix
  get <- function() myMatrix
  
  #stores the inverse matrix
  setInverse <- function(myInvMatrix) invMatrix <<- myInvMatrix
  
  #retrieves the inverse matrix
  getInverse <- function() invMatrix
  

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#it generates the inverse matrix       
cacheSolve <- function(x, ...){
  myInv <- x$getInverse()

 #if inverse matrix was already computed, then returns it
  if(!is.null(myInv)) {
    message("getting cached data")
    return(myInv)
  }
  
  myData <- x$get()

  #computes the inverse matrix using solve
  myInv <- solve(myData)

  #stores the inverse matrix
  x$setInverse(myInv)

  print(x$getInverse())
}


mdat <- makeCacheMatrix(c(1,1,2,1,2,2,1,1,1),3,3)
cacheSolve(mdat)
