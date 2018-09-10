## Put comments here that give an overall description of what your
## functions do
##Week 3 -R Programming Assignment cacheMatrix

## Write a short comment describing this function
##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
   getMatrix <- function()x
   setInverse <- function(inverse) invMatrix <<- inverse
   getInverse <- function() invMatrix
   list(setMatrix =setMatrix, getMatrix = getMatrix,
        setInverse =setInverse , getInverse = getInverse)
   
   

}


## Write a short comment describing this function
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting Cached Invertyible Matrix")
    return(invMatrix)
  }
  MatrixData <- x$getMatrix()
  invMatrix <- solve(MatrixData, ...)
  x$setInverse(invMatrix)
  return(invMatrix)
  
  TestMatrix <- matrix(1:4 ,2,2)
  > TestMatrix
  [,1] [,2]
  [1,]    1    3
  [2,]    2    4
  > CacheMatrix <- makeCacheMatrix(TestMatrix)
  > CacheMatrix$getMatrix()
  [,1] [,2]
  [1,]    1    3
  [2,]    2    4
  > CacheMatrix$getInverse()
  NULL
}
