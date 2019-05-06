##cacheSolve: This function computes the inverse of the special "matrix" returned

 ## NULL set an empty inverse matrix in the parent env

  makeCacheMatrix <- function(m = matrix()) {   
  
  set <- function(y){
       x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list (set = set, get = get,  setsolve = setsolve,
        getsolve = getsolve)
}

cacheSolve <- function(x = matrix(), ...){
  
  m <- x$getsolve()
  
   ##if m already exists (the inverse is already calculated) 
  if(!is.null(m)){                                  
    message("Getting cached matrix")
    return(m)
  }
  
  inverse <- x$get()
  m <- solve(inverse, ...)
  x$setsolve(m)
  m
}
## Return a matrix that is the inverse of 'x'
