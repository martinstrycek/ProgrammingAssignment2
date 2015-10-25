## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list able to do 
# set a matrix to be inversed
# get a matrix to be inversed
# set a value of inverse matrix
# get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  if (!is.matrix(x)) {
    stop("X is not a matrix")
  }
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <-function() {
    x
  }
  setInverseMartix <- function(inverse){
    m <<- inverse
  } 
  getInverseMatrix <-function() {
    m
  } 
  list(set = set, get = get,
       setInverseMartix = setInverseMartix,
       getInverseMatrix = getInverseMatrix
  )
}


# cacheSolve returnsinverse of a matrix. 
# First is checks if the inverse has been calculated 
# if yes returns cached inverse
# if not calculates, save into cache inverse and returns

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("I have used cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverseMartix(m)
  m
}
