## This function creates a special "matrix" object that can cache its inverse

## For this assignment, assume that the matrix supplied is always invertible.
## 
## In order to complete this assignment, you must do the following:
##
## 1.Fork the GitHub repository containing the stub R files at https://github.com/rdpeng/ProgrammingAssignment2 to create a copy under your own account.
## 2.Clone your forked GitHub repository to your computer so that you can edit the files locally on your own machine.
## 3.Edit the R file contained in the git repository and place your solution in that file (please do not rename the file).
## 4.Commit your completed R file into YOUR git repository and push your git branch to the GitHub repository under your account.
## 5.Submit to Coursera the URL to your GitHub repository that contains the completed R code for the assignment.
## 
## In addition to submitting the URL for your GitHub repository, you will need to submit the 40 character SHA-1 hash (as string of numbers from 0-9 and letters from a-f) that identifies the repository commit that contains the version of the files you want to submit. You can do this in GitHub by doing the following
## 1.Going to your GitHub repository web page for this assignment
## 2.Click on the "?? commits" link where ?? is the number of commits you have in the repository. For example, if you made a total of 10 commits to this repository, the link should say "10 commits".
## 3.You will see a list of commits that you have made to this repository. The most recent commit is at the very top. If this represents the version of the files you want to submit, then just click the "copy to clipboard" button on the right hand side that should appear when you hover over the SHA-1 hash. Paste this SHA-1 hash into the course web site when you submit your assignment. If you don't want to use the most recent commit, then go down and find the commit you want and copy the SHA-1 hash.


makeCacheMatrix <- function(x = matrix()) {

    # Set myMatrix to NULL
    
    myMatrix <- NULL
    
    # Set x to y and set myMatrix to NULL
    set <- function(y) {
        x <<- y
        myMatrix <<- NULL
    }
    
    # Get x
    get <- function() {
        x
    }
    
    # set myMatrix to solve_myMatrix
    setInverse <- function(solve_myMatrix) {
        myMatrix <<- solve_myMatrix
    }
    
    # get myMatrix
    getInverse <- function() {
        myMatrix
    }
    
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    myMatrix <- x$getInverse()
    
    if (!is.null(myMatrix)) {
        message("Getting cached data...")
        return(myMatrix)
    }
    
    data <- x$get()
    
    # solves equation a %*% x = b for x, where b is a vector or matrix.
    myMatrix <- solve(data, ...)
    
    x$setInverse(myMatrix)
    
    ## Return myMatrix that is the inverse of 'x'
    myMatrix
    
}
