##1 Create a function makeVector() to create a special "vector", which is really a 
##list containing a function to
##-set the value of the mactrix
##-get the value of the mactrix
##-set the value of the inverse
##-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(sovle) s <<- solve
        getsolve <- function()s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##2 Create a function to calculate the inverse of the special "vector" created 
##with the above function. However, it first checks to see if the mean has already
##been calculated. If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
##the cache via the setsolve function.

cacheslove <- function(x, ...){
        s <- x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
                data <- x$get()
                s <- solve(data,...)
                x$setsolve(s)
               s
}

