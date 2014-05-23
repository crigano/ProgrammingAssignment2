## makeCacheMatrix<-function(x = matrix()) is a constructor for an object:
## e.g, xx<-makeCacheMatrix(x)
## This Object stores the private member data fields x, and m
## This Object's get and set method provide access to the data
## Sets the Data Fields
## Uses Superassignment <<- operator to maintain the state across function 
## invocations
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y   
    m<<-NULL
  }
  
  get<-function() x # get the orignal matrix
  setmatrix<-function(solve) m<<- solve # inverts the matrix
  getmatrix<-function() m
  
# Group functions into this object to get and set data fields
  list(set=set, get=get,
  setmatrix=setmatrix,
  getmatrix=getmatrix)
}

## Return a matrix that is the inverse of 'x' 
## Methods to determine if the data field in m is initialized inmakeCacheMatrix<-function .
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()# returns m from the makeCacheMatrix<-function Object
  if(!is.null(m)){
    message("return cached inverse")
    return(m)
  }
  matrix<-x$get() #If not stored in makeCacheMatrix<-function Object, Invert
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m 
}

