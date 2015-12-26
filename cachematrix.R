#makeCacheMatrix: This function creates a special "matrix" object that can cache 
#its inverse.


#cacheSolve: This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


#Similar the example, but with the solve() function. 

#makeCasheMatrix creates a cashe for a matrix, and allowes it to be stored, quired, and replaced
makeCasheMatrix <- function(x = matrix()){
        i <- NULL #sets i to null initially
        set <- function(y){#if we're making a new inv we want i to be null
                i <- NULL
                x <<- y
        }
        
        get <- function() x #gets the matrix
        
        setinv <- function(inv){
                i <- inv 
        }
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}
#casheSolve checks to see if there is a cashed function and takes that out, otherwise it inverses the matrix
casheSolve <- function(x, ...){
        
        i <- x$getinv()
        if(!is.null(i)){
                print("Getting the Inverse Matrix from cashe")
                return(inv)
        }
        data <- x$get()
        i <- solve(data, ...)#This inverses the matrix
        x$setinv(i)
        i
}




