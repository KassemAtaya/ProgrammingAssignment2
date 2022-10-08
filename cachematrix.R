## The two functions in this code are: 
## makeCacheMatrix : This function consists of set,get,setinv, getinv
## cacheSolve : This function is used to get the cach data 
library(MASS)
## The mass library is used to calculate inverse for non squared and square Matrices
makeCacheMatrix <- function(x = matrix()) {                                                            #Function 1
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL

        }
        get<-function()x
        setinv<-function(inverse)inv<<-inverse
        getinv<-function(){
                inver<-ginv(x)
                inver%*%x

        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## Cach data retrieval 
cacheSolve <- function(x, ...) {                                                                      #function 2
        inv <-x$getinv()
        if(!is.null(inv)){
                message("Getting Cached Data.")
                return(inv)

        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)

        inv ## Return a matrix that is the inverse of 'x'
}
