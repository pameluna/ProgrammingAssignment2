## Week 3, Programming Assignment 2, Peer Review, Due Feb 22, 2015

## The two functions below creates and caches the inverse of a matrix.
## Function makeCacheMatrix first creates the matrix object and sets the
## parameters to build the inverse matrix.
## Function cacheSolve checks if the inverse matrix has been created and
## resides in the cache. If so, it retrieves it from cache; if not, it
## creates the inverse matrix and puts it in the cache.

# makeCacheMatrix function creates a matrix object used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){               #set the value of matrix object
                x<<-y
                m<<-NULL
        }
        
        get<-function()                 #get the value of matrix object
                
                setmatrix<-function(solve) m<<- solve #set value of inverse matrix
        getmatrix<-function() m         #get the value of inverse matrix
        
        list(set=set, get=get,          #pairlist, set tags
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


#cacheSolve Function calculates inverse of matrix created with makeCacheMatrix
#Checks for cached inverse matrix first, creates inverse matrix and
#sets to cache if doesn't exist

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()        #get inverse matrix object from makeCacheMatrix function
        
        #check if inverse matrix exists (is already in cache), if so, then retrieve
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        #if no inverse matrix already in cache, create inverse matrix and put in cache
        matrix<-x$get()         #get matrix created by makeCacheMatrix function
        m<-solve(matrix, ...)   #calculate inverse matrix
        x$setmatrix(m)          #set inverse matrix in cache
        m
}

