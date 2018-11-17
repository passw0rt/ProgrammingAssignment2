## The following pair of functions, makeCacheMatrix and cacheSolve, cache the inverse of a given invertible square matrix.

## The makeCacheMatrix function maps each invertible square matrix to a list of four functions, set, get, setInv, and getInv, for getting and setting the invertible square matrix stored within the parent environment of the functions as well as getting and setting the matrix identified as its inverse.  The default argument of makeCacheMatrix is presumed to be the 1x1 unit matrix when no invertible square matrix is specified. 

makeCacheMatrix <- function(thisMatrix = matrix(data=1,nrow=1,ncol=1)) 
{

	    	
	    	cachedInverseMatrix <- NULL
     
			set <- function(newMatrix) 
        	{
                thisMatrix <<- newMatrix
                cachedInverseMatrix <<- NULL
        	}
        
        	get <- function()
        	{ 
        		thisMatrix
        	}
        
        	setInv <- function(inverseMatrix)
        	{
        		cachedInverseMatrix <<- inverseMatrix
        		
        	}
        	
        	getInv <- function() 
        	{
        		cachedInverseMatrix
        	}	
        
        	return(list(set = set, get = get, setInv = setInv,getInv = getInv))

}


## The cacheSolve function returns the inverse matrix of the invertible square matrix stored in the parent environment of the list of functions returned by calling makeCacheMatrix. The cacheSolve function calculates the inverse matrix of the invertible square matrix and stores it in the parent environment of the functions only if no inverse matrix has been stored in the parent environment.

cacheSolve <- function(cacheMatrix, ...) 
{
		cachedInverseMatrix <- cacheMatrix$getInv() 
	
		if 		(	is.null(cachedInverseMatrix)	)
		{
				cachedInverseMatrix <- cacheMatrix$setInv(solve(cacheMatrix$get(), ...))	
		}
		
		cachedInverseMatrix	
}
