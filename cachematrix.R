## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix using four other functions (set,get (values of matrix) 
## setinverse, and getinverse (set/get inverse of matrix))

makeCacheMatrix <- function(x = matrix()) {

       m<-NULL
       set<-function(y){
              x<<-y
              m<<-NULL
       }
       get<-function() x
       setinverse<-function(solve) m<<-solve
       getinverse<-function() m
       list(set=set,get=get,
            setinverse=setinverse,
            getinverse=getinverse)
}


## cacheSolve checks to see if an inverse of a matrix has already
## been calculated previously and gets the result(inverse of the matrix)
## before any computation is made

cacheSolve <- function(x, ...) {
      m<-x$getinverse()
      if(!is.null(m)){
             message("getting cached data")
             return (m)
       }
      data<- x$get()
      m<-solve(data,...)
      x$setinverse(m)
      m
}


