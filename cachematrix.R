## Coursera R-Programming Assignment 2 by Jieshu Qian 

## Computation of the inverse of a matrix can be time consuming, especially when it
## has to be computed repeatedly (e.g. in a loop). The following two functions 
## "makeCacheMatrix" and "cacheSolve" are defined to cache the inverse of a matrix.

## Function "makeCacheMatrix" is defined as a list containing functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    
    #set the initial value of cache null because the cache is empty
    initial <- NULL
    
    #store the matrix and assign the matrix a new value
    setmatrix <- function(new) {
        x <<- new
        initial <<- NULL
    }
    
    #return the stored matrix x
    getmatrix <- function() {
        x
    }
    
    #set the value of the inverse of the matrix
    setinverse <- function(inverse){
        initial <<- inverse
    }
    
    #get the value of the inverse of the matrix
    getinverse <- function(){
        initial
    }
    
    #return a list including all the functions defined above
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    # get the cached value
    inverse <- y$getinverse()
    
    # if a cached value exists, return it
    if (!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    } 
    
    # if a cached value does not exist, get the matrix, compute the inverse, and 
    # store the inverse in the cache
    data <- y$getmatrix()
    inverse<- solve(data)
    y$setinverse(inverse)
    
    # Return the inverse matrix
    inverse
}

## Run a test using the following example

## define a 2 by 2 matrix
x <- matrix(1:4, 2, 2) 

## run function "makeCacheMatrix"
y <- makeCacheMatrix(x)

## get the matrix
y$getmatrix()

## return the inverse of the matrix "x" defined above
cacheSolve(y)

