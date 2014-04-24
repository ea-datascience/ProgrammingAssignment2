## Put comments here that give an overall description of what your
## functions do

## The function expects a matrix as an argument
## 1. It initializes the inverse to NULL
## 2. The set function sets the value of the input matrix and resets the inverse to NULL
## 3. The get function return the input matrix
## 4. setinv stores a value in the inv variable
## 5. getinv returns the value stored in the inv variable
## The function returns a list with 4 internal methods as named members.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function receives one argument, which is the output of the makeCacheMatrix, or the "special" matrix object
## 1. Reads the inverse value stored in the "special" matrix and stores it locally
## 2. Makes sure that the value is not empty
## 3. Returns the value stored locally that contains the inverse of the input "special" matrix object
## 4. If the inverse matrix gotten from the function parameter is null, then it calculates the inverse using solve
## 5. Function stores the inverse in the original matrix object, using the setinv method
## It returns the inverse calculated as well

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
