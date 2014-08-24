## take a matrix x and set it in the cache with setMatrix function
## return matrix x with getMatrix function 
## take inverse matrix i (which is calculated by cacheSolve)
## and set it in the cache with setInverse function
## return inverse matrix i with getInverse function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(y) {
                ## setting the x variable in the parent environment of function 
                ## with the value of the matrix passed in
                x <<- y  
                ## setting up placeholder for inverse matrix (NULL for now)
                i <<- NULL
        }
        ## returning matrix x
        getMatrix <- function() x
        ## taking inverse matrix of x and setting/giving it to i variable
        ## in parent environment of function
        setInverse <- function(inverse) i <<- inverse
        ## returning inverse matrix i
        getInverse <- function() i
        ## returning list of matrix, inverse matrix values
        list (setMatrix = setMatrix, getMatrix = getMatrix, 
              setInverse = setInverse, getInverse = getInverse)
}


## take a matrix x and see if there is an inverse of it already cached
## if already cached, return the inverse matrix of x
## if not, calculate the inverse matrix of x and cache then return it

cacheSolve <- function(x, ...) {
        ## use getInverse function to pull whatever
        ## data is in placeholder inverse matrix x
        ## then store in i
        i <- getInverse()
        
        ## checks if there is already an inverse matrix
        ## being stored in i (i.e. i is not empty)
        ## if yes, just return what is being cached
        if (!is.null(i)){
                message("getting cached inverse matrix")
                return(i)
        }
        
        ## since there is no cached inverse matrix ...
        
        ## use the matrix that was just passed with getMatrix()
        data <- getMatrix()
        ## and use solve() function to calculate 
        ## inverse of matrix that's now in data
        i <- solve(data)
        ## cache the calculated inverse matrix 
        setInverse(i)
        ## return the calculated inverse matrix
        i
}
