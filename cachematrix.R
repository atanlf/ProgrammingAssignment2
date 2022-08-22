makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    #Defining makeCacheMatrix as a function of x, a matrix. 
    #Defining set as a function of y
    #Defining the matrix x as y
    #Define m(the final answer to cacheSolve below) as staring out empty
    get <- function() x
    setinverse <- function(solve) m <<- solve(x)
    getinverse <- function() m
    #Define get as another function of x
    #Define setinverse as the inverse of m, equivalent to the inverse of x
    #By now m should equate to x
    #Define getinverse as a function of m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
#Stating the subsets that can be used once the matrix is cached

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    #Define cacheSolve as a function of x(the cached matrix from above)
    #Define m as the inverse of x
    #If there is already a value for m it will be loaded
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    #Define data as x(cached matrix)
    #Confirm m as the inverse of data, i.e. x
    #Present m
}
