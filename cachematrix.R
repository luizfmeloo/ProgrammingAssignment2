library(MASS)
makeCacheMatrix <- function(x = matrix()) { ##create a function to make the matrix
  i <- NULL #make a NULL variable that will be used in cacheSolve function
  set <- function(y){
    #make a assignment into the function
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse #create a function that set a inverse value
  getInverse <- function() {
    s_inverse <- ginv(x)
    s_inverse%*%x
  } #create a function that get the inverse value
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #create a function that get cache data 
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("It's getting some cached data")
    return(i)
  }
  data_cache <- x$get()
  i <- solve(data_cache, ...) #the inverse value will be calculated
  x$setInverse(i)
  i ##print the value into the console
}
