.

## two functions makeCachematrix and cacheSolve are used
## library(MASS) is used so that we can get inverse for both squared and non squared matrices both

## Function: makeCacheMatrix(x = matrix())
## Purpose: create a matrix with getter and setter functions and the 
## functionality to compute the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y        ##accessible outside
    inv<<-NULL   ##accessible outside
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse  #solve
  getinv<-function(){  #get the inverse 
    inver<-ginv(x)
    inver%*%x
    
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv )

}



## for getting cached data
## Function: cacheSolve
## Purpose: Store the inverse of the matrix in memory if it doesn't already
## exist, otherwise retrieve it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()   #try to get the object in memory
  if(!is.null(inv)){   #retrieve cached data 
    message("getting cached data!")
    return(inv)
  }
  
  data<-x$get()  #matrix to invert
  inv<-solve(data, ...)
  x$setinv(inv)  #set the inverse on the object
  inv
}
