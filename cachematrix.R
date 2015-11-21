## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is going to store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i_m <- NULL
  set <- function(y){
    i_m <<- NULL
    x <<- y
  }
  get <- function() x
  setmean <- function(inv_mat) i_m <<- inv_mat
  getmean <- function() i_m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## Write a short comment describing this function
## This function is going to look whether the inverse matrix is stored in the 
## previous function and, if it is, will return it. If it is not stored, it
## will calculate the inverse matrix and store it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i_m <- x$getmean()
  if(!is.null(i_m)){
    message("getting cached data")
    return(i_m)
  }
  data <- x$get()
  i_m <- solve(data, ...)
  x$setmean(i_m)
  i_m
}

## Now, I'm going to create 2 squared matrices and use the function "solve"
## just to check that my functions work right.

m <- matrix(1:4, 2, 2)
w <- matrix(4:7, 2, 2)
solve(m)
solve(w)

## Executing the functions using the 2 previously created matrices
f1m <- makeCacheMatrix(m)
f2m <- cacheSolve(f1m)
f2m

f1w <- makeCacheMatrix(w)
f2w <- cacheSolve(f1w)
f2w

## checking results
f2m - solve(m)
f2w - solve(w)
