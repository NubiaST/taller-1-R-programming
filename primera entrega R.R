############1######################
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y){
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inversa <<- solveMatrix
  getInverse <- function() inversa
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#########2########################
cacheSolve <- function(x, ...) {
  inversa <- x$getInverse()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  datos <- x$get()
  inversa <- solve(datos)
  x$setInverse(inversa)
  inversa     
}