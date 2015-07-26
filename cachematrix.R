## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this is the parent function that will define and output 4 different functions
## x is the input which should be a square invertible matrix
## y offers you the option of changing your matrix after you called the function
## getsolved 
##instructions on running the program:
##1.load the two functions into R after downloading them to your work dir
##>source("makecachematrix.R")


##2.define an invertible square matrix such as
##>mymatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 5, 0), 3, 3)

##3.call the parent function a<-makeCacheMatrix(mymatrix)
##4.you can experiment with a$get() to confirm that you have the right matrix in place
##5. call cacheSolve(a) to get the inverse of your initial matrix


makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolved <- function(solve) inv <<- solve
        getsolved <- function() inv
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
}


## Write a short comment describing this function

##briefly, this function starts by checking whether the calculation was already performed
##it looks at x$getsolved() to see whether there is an actual value stored
##if there is non-NULL data there, it will return it after telling the user 
##that it is providing data from cache
##if there is a null value in the x$getsolved, it will get the raw data with x$get()
##and it will solve(ie. invert) the matrix
##it will store the output in cache with x$setsolved
##and finally it will return the result

cacheSolve <- function(x, ...) {
        inv <- x$getsolved()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolved(inv)
        inv
}
