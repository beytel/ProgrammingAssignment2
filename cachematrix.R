##Assignment 2 Lexical scoping
##caching the inverse of a matrix

## This function creates a special "matrix objectthat will cache the inverse of the object

makeCacheMatrix<-function(x=matrix()){
  inverseMatrix<-NULL
  set<-function(y){
    x<<-y
    inverseMatrix<<-NULL
  }
  get<-function()x
  setInverseMatrix<-function(inverse)inverseMatrix<<-inverse
  getInverseMatrix<-function() inverseMatrix
  list(set=set,get=get,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)
}


## This functions provides the inverse of the above matrix object.  
## If the inverse was calculated prior and no changes to the matrix
## it returns the cache inversed, if not it calculates the inverse 

cacheSolve <- function(x,...){
  inverseMatrix<- x$getInverseMatrix()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  myMatrix<-x$get()
  inverseMatrix<-solve(myMatrix,...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
