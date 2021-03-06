### By kuldeep Rajpoot
## Defining makeCacheMatrix 
# First creating a function that takes 'input matrix' as input
# after that stores the inverse of matrix in cache 

makeCacheMatrix <- function(input_matrix=matrix()){
  matri <- NULL
  set <- function(y){
    input_matrix <<- y
    matri <<- NULL
  }
  get <- function() input_matrix
  solveMat <- function(solve) matri<<- solve
  getMat <- function() matr
  list(set = set, get = get,
       solveMat = solveMat,
       getMat = getMat)
}

#A  function that would take input as the list output of earlier function "makeCacheMatrix"
#extract cached matrix from earlier function

cacheSolve <- function(input_matix, ...){
  matri <- input_matix$getMat()
  if(!is.null(matr)){
    message("Cached Inverse Available")
    return(matri)
  }
  data <-input_matix$get()
  matri<-solve(data)
  input_matix$solveMat(matr)
  matri
}

mat_one <- matrix(floor(rnorm(16, 5, 2)), 4,4)
mat_one
solve(mat_one)

mat_one1 <- makeCacheMatrix(mat_one)

cacheSolve(mat_one1)
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##
## Same here, changed "mean" to "solve" and "m" to "s"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}