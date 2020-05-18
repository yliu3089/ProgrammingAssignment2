## Overall, the purpose of this function is to cache
## the inverse of the matrix

## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  iv<-NULL
  set<-function(y){
    x<<-y
    iv<<-NULL
  }
   get<-function()x
   set.inverse<-function(inverse) iv<<-inverse
   get.inverse<-function() iv
   list(set=set, get=get,
        set.inverse=set.inverse,
        get.inverse=get.inverse)
}


##This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  iv<-x$get.inverse()
  if(!is.null(iv)){
    message("getting cached data")
    return(iv)
  }
  data<-x$get()
  iv<-solve(data,...)
  x$set.inverse(iv)
  iv
      ## Return a matrix that is the inverse of 'x'
}

