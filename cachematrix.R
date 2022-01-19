## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## two functions makeCachematrix and cacheSolve are used
## library(MASS) is used so that we can get inverse for both squared and non squared matrices both

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x
    
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv )

}


## Write a short comment describing this function
## for getting cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){  message("getting cached data!")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}
