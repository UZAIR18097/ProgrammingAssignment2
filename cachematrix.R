## Put comments here that give an overall description of what your
## functions do

## caching matrix for matrix argument.it returns vectors of getters and setters
##for matrix and holds place for matrix inverse in get and set methods.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


#solves inverse of square matrix or retrives previously calculated matrix
#or retrives a previous inverse from cache given list argument from makecacheMatrix
#function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null()) {
                message("getting cached data")
                return(m)
        }
        data <- x$getdata()
        m <- solve(data=data) %*% data
        x$setinverse(m)
        m
        
}
