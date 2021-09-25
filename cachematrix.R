makeCacheMatrix<-function(x=matrix()){
  invert<-NULL
  matrix_value<-function(y)
  {
    x<<-y
    invert<<-NULL
  }
  get<-function()
  {
    x
  }
  setInverse<-function(inverse){
    invert<<-inverse
    
  }
  getInverse<-function()
  {
    invert
  }
  list(matrix_value=matrix_value, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve<-function(x,...){
  invert<-x$getInverse()
  if(!is.null(invert)){
    message("Getting the Data")
    return(invert)
  }
  mat<-x$get()
  invert<-solve(mat,...)
  x$setInverse(invert)
  invert
}