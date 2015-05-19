## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1. Prompt user for number of rows & columns
## 2. Create a square matrix using random numbers
myMatrix <- function(){
    #prompt user to enter matrix dimensions
    numPrompt <- readline(prompt="Enter number of rows (number of rows == number of columns: ")
    matrixDim <- (as.integer(numPrompt))
    
    #generate matrixDim* matrixDim random numbers between 1 and 10 for the matrix
    matrix(sample(1:10, matrixDim*matrixDim, replace=TRUE), matrixDim, matrixDim)  
}

#Inputs: a square matrix
#Returns: a list containing functions to 
#         1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## actually return the matrix being passed
    get <- function() x
    ## sets the function to calculate the matrix
    setInverse <- function(inverse) m <<- inverse
    ## actually return the inverse of the matrix m
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
#Inputs: the output of makeCacheMatrix()
#Returns: Inverse of the original created matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #get the data from the cache
    matrixInverse <- x$getInverse()
    #if it exisits, return that value
    if(!is.null(matrixInverse)) {
        message("getting cached inverse data")
        return(matrixInverse)
    }
    else{
        ## otherwise calculate the inverse
        data <- x$get()
        matrixInverse <- inverse(data)
        ## put this answer in the cache
        x$setInverse(matrixInverse)
        # return the matrix inverse
        matrixInverse
    }      
}

#Inputs: a matrix
#Outputs: the inverse of the matrix if it exisits 
inverse <- function (myMatrix){
    #if determinant is greater than zero calcuate inverse, else send error msg
    if (det(myMatrix) != 0){
        print(det(myMatrix))
        return(solve(myMatrix))
    }
    else{
        print('Determinant is zero, therefore there is no inverse')
        return("NaN")
    }
}


#test run
test <- function(){
    
    tempMatrix <- myMatrix()
    print(tempMatrix)
    temp <- makeCacheMatrix(tempMatrix)
    
    start.time = Sys.time()
    cacheSolve(tempMatrix)
    dur = Sys.time() - start.time
    print(dur)
    
    start.time = Sys.time()
    cacheSolve(tempMatrix)
    dur = Sys.time() - start.time
    print(dur)
}
