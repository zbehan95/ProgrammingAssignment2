## Takes a matrix and creates a special matrix that contains the inverse in cache
## the cache is initialized NULL upon creation in makeCacheMatrix until cacheSolve is called
## after calling cacheSolve once, subsequent calls will return the cached value

## Creates an object of makeCacheMatrix, which consists of a matrix 
## and a cached inverse matrix which is initialized NULL

makeCacheMatrix <- function(x = matrix()){
      i <- NULL
      set <- function(aMatrix){
            x <<- aMatrix
            i <<- NULL
      }
      get <-function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}      



## Takes an object of makeCacheMatrix, and returns the cached inverse 
## or calculates, caches, and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)){ 
            message("returning cached inverse")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      i
}
