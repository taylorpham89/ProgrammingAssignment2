## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix recieves input to create an invertible matrix that is stored in x
## makeCacheMatrix has 4 function : 
## 1.function set() input value for x - during this function, x will be assigned a maxtirx value and inv will be restored to NULL
## 2.function get will return the value of x 
## 3. function setInv() with assign the inversed matrix of x to inv (this operation will be carried out in the cachesSolve function)
## 4. function getInv() will return the value of inv
## Finally, makeCacheMatrix will return a list of defined function (set, get, setInv and getInv) so that it can be called upon in cacheSolve function 

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(g) {
          x <<- g
          inv <<- NULL
        }
        get <- function () x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list( set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve will carry out 2 task: check whether there is an inverted matrix or not, if it exists, then the value inv will be return
## if the inverted matrix is not there yet, cacheSolve will retrieve the original matrix and turn it into the inverted one, and store it in the variable inv


cacheSolve <- function(f,...) {
    ## check whether the inverted matrix exists or not, if yes -> return inv, if not -> next step
    inv <- f$getInv()
    if (!is.null(inv)) {
      message ("getting cached data")
      return (inv)
    }
    ## retrieve the original matrix using function get(), since f is in the argument of cacheSOlve, we can make use of it to call the defined function previously using f$get()
    ## solve() function will do the operatuion to produce the inverted matrix and store it in the variable inv
    data <- f$get()
    inv <- solve(data)
    f$setInv (inv)
    inv
}

