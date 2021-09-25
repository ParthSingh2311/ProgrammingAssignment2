makeCacheMatrix<-function(x=matrix()){ #The main function that will be used to input the values
  invert<-NULL                         # setting the default value of the inverse of the matrix to null
  matrix_value<-function(y)            # function to set the value of the matrix
  {
    x<<-y
    invert<<-NULL
  }
  get<-function()                      # function to get the value of the matrix
  {
    x
  }
  setInverse<-function(inverse){       # function to set the value of the inverse of the matrix
    invert<<-inverse
    
  }
  getInverse<-function()               # function to fetch the value of the inverse of the matrix
  {
    invert
  }
  list(matrix_value=matrix_value, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve<-function(x,...){          # function to solve, i.e. give the inverse of the matrix. 
  invert<-x$getInverse()
  if(!is.null(invert)){               # condition to see if the value is already cached. If it is it will print the value along with a message. Otherwise, it would say NULL
    message("Getting the Data from Cache")
    return(invert)
  }
  mat<-x$get()
  invert<-solve(mat,...)             # function to get the inverse of the matrix stored in the variable, invert
  x$setInverse(invert)               # sending the value of the inverse to the cache
  invert
}
