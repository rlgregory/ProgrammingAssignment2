## The makeCacheMatrix function will create a special matrix which is actually a list containing
## a function to set and get the value of the matrix and set and get the inverse matrix.
## The cacheSolve function will then get the cached inverse function and print, otherwise if
## it hasn't been previously stored, then it will determine the inverse itself.

## Create a function which begins with a 'null' matrix 

makeCacheMatrix <- function(x = matrix()) { 
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  } 
  get<-function() x
  setinverse<-function(solve) inv<<-solve
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The input of this function was created by makeCacheMatrix and if the inverse was
## already cached, will access it from there.  If not, it will then calculate the inverse.

cacheSolve <- function(x, ...) { 
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    ## Return a matrix that is the inverse of 'x' 
  } 
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}