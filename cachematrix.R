## Se realizará una función que mantenga una función en la memoria caché

## Se elabora una función para la lectura de una matriz y se quede en memoria caché

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Se genera la función que genera la matriz inversa de la matriz que se tiene en memoria caché

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  dato1 <- x$get()
  m <- solve(dato1, ...)
  x$setInverse(m)
  m
}
