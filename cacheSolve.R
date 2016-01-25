Enter file contents here


#### The following function cache the inverse of matrix ####
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

#### The following function solve the inverse of special "matrix" created with the above function. ####
#### It first checks to see if the inverse of matrix has already been solved. #### 
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
     message("getting cached data")
     return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse
  m
}
