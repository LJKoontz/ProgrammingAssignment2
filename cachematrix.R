## Put comments here that give an overall description of what your
## functions do

## This Function creates a Special "matrix" object (it's technically
## a list) that can store a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) 
{
        inversedMatrix <- NULL
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(matrix) inversedMatrix <<- matrix
        getInverse <- function() inversedMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## this checking to see if there is a value in getInverse and if there
## is not then it is calculating the inverse and then caching it. 

cacheSolve <- function(x, ...) 
{
        ## if getInverse is Null then 
        inversedMatrix <- x$getInverse()
        
        if(is.null(inversedMatrix))
        {
                ##Call Solve to calculate the inverse of the matrix 
                ## that we got from get
                regularMatrix <- x$get()
                inversedMatrix <- solve(regularMatrix)
                
                ##store result in setInverse  
                x$setInverse(inversedMatrix)
        }
        inversedMatrix
}