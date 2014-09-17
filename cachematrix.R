## The makeCacheMatrix function will do FOUR things:
## 1. It sets the value of the matrix
## 2. It gets the value of the matrix
## 3. It calculates the inverse matrix
## 4. It gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## Set the inverse_matrix to NULL
        inverse_matrix <- NULL
        
        ## User enters the elements of the matrix and assign it to x
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        ## Create a function that picks up x
        get <- function() x
        
        ## Create a function that makes [setmatrix = inverse matrix] by using the solve()
        setmatrix <- function(solve) inverse_matrix <<- solve
        
        ## Create a function that picks up the inverse matrix
        getmatrix <- function() inverse_matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The cacheSolve function will do TWO things:
## 1. Check to see if the inverse_matrix is empty or not.
## 2. If it is NOT empty, then it will retrieve the inverse matrix from cached inverse_matrix,
##    If it is empty, then it will calculate the inverse matrix using the solve()

cacheSolve <- function(x, ...) {
        ## Using the input expression, call the getmatrix function and set equal to inverse_matrix
        inverse_matrix <- x$getmatrix()
        
        ## If the inverse_matrix is NOT empty, then just print out the inverse_matrix
        if(!is.null(inverse_matrix)) {
                message("The inverse matrix has already been calculated. Retriving from cache data now.")
                return(inverse_matrix)
        }
        
        ## Otherwise, get the argument x
        data <- x$get()
        
        ## And solve it
        inverse_matrix <- solve(data, ...)
        
        ## Set the argument x equals to the inverse matrix
        x$setmatrix(inverse_matrix)
        return(inverse_matrix)
}
