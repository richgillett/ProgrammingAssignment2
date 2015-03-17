setwd("C:/Users/Rich/Documents/coursera/ProgrammingAssignment2")

## The first function creates a list of four functions.    
## The functions do the following four things; 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

# create a variable called inverse - shortened to inv here.  Make it empty.
    
	inv <- NULL

# 1. create a function which 'sets' the value of the matrix
#    which will set x in the parent environment
#    and also make sure that inverse is set and available in the parent environment too 

        set <- function(y) 	{
							x <<- y
							inv <<- NULL
							}

# 2. 'get' is a function which will return the value of x - the value of the matrix, 
# so we can see it later using x$get.  I have used curly brackets to make it clearer that these are functions.
# and so it's more obvious to me as a novice what they are returning. 

        get <- function() { x }

# 3. sets the value of the inverse, using the solve function and makes inv available in the parent environment

        setinverse <- function(sent_replacement_inverse) { inv <<- sent_replacement_inverse }

# 4. gets the value of the inverse 

        getinverse <- function() { inv }

# The section of code here will be returned, in this case we'll create and return a four item list;

		list(	set = set, 					
				get = get,
				setinverse = setinverse,
				getinverse = getinverse)
}

## The second function is a Cached solver, which checks to see if an inverse has been created.
## If an inverse has already been calculated, it uses it.  Else;
## If it has not been calculated, this function creates it. It tells the first set of code what the inverse is.

cacheSolve <- function(x, ...) {
        
		## Return a matrix that is the inverse of 'x'
		## This function checks to see if the inverse has already been calculated
		
		inv <- x$getinverse()
		
		## if it has been calculated (i.e. it is not empty, bring it back)
		
		if(!is.null(inv)) 	{
		
							message("getting cached data")
							return(inv)
							
							}
		
		else
		
		## If the inverse has not been already calculated it we calculate it here.
		## We do that using the following steps below.
		## Step 1 - Specify the data 
        
		data <- x$get()
		
		## step 2 - assign a value to inv which will use the solve function 
        
		inv <- solve(data, ...)
		
		## Step 3 - we then pass the value of the mean back to the first function above
        
		x$setinverse(inv)
        
		# finally, return the value inverse
		
		inv		

}
