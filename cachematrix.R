## Function is list that will
## 1: set then get the matrix
## 2: set then get the inverse

makeCacheMatrix <- function(x = matrix()) {   ## creating the function
    inv_mat <- NULL      ## naming currently unsolved inverse matrix
    set <- function(y) {   ## naming set matrix of our function
        x<<-y              ## saying we want to set the answer to our function
        inv_mat <- NULL    ## naming currently unsolved inverse matrix that will be set
    }
    get <- function() x    ## name matrix returned
    setinv <- function(solve) inv_mat <<- solve  ## set matrix to cache
    getinv <- function() inv_mat  ## get matrix from cache
    list(set=set,get=get,setinv=setinv,getinv=getinv) ## listing functions named
}


## Function solves inverse of above set matrix
## if the inverse has not be calculated.
## Then function puts inverse in cache.

cacheSolve <- function(x, ...) {   ## creating the function
    inv_mat<-x$getinv()       ## get cached matrix inverse
    if(!is.null(inv_mat)) {   ## if matrix inverse cached
    message("getting cached data") ## say this so user knows solution returned is cached version
    return(inv_mat)         ## return cached matrix
    }
    data <- x$get()       ## if null, get by solving
    inv_mat <- solve(x)   ## calculate inverse matrix
    x$setinv(inv_mat)     ## set inverse matrix to cache per makeCacheMatrix function
    inv_mat               ## print solution
}