# Creates square invertable matrix that can cache its inverse
# Returns list containing functions that:
#       set matrix, set
#       get matrix, get
#       set inverse, setInv
#       get inverse, getInv
# Function list used as input for cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
        # Variable storing inversion result
        matInv <- NULL
        # Uses "<<-" to assign value to object in environment 
        # different from the current
        set <-  function(y) {
                x <<- y
                # Initializes matInv to null
                matInv <<- NULL    
        }       
        # Returns input matrix
        get = function() x
        # Sets inversed matrix
        setInv = function(inverse) matInv <<- inverse 
        # Returns inversed matrix
        getInv = function() matInv
        # List of functions
        list(set = set, get = get, setInv = setInv, getInv = getInv)   
}


# Returns inverse of original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
        # Gets inversed matrix
        mat_inv = x$getInv()
        # If inverse already calculated
        if(!is.null(mat_inv)) { 
                # Retrieves inverse from cache, skips calculation
                message("getting cached data")
                return(mat_inv) 
                # Returns previously calculated inverse
        }
        # If inverse not already calculated
        # Gets matrix object
        mat_data = x$get()
        # Calculates matrix inverse
        mat_inv = solve(mat_data, ...)
        # Sets inverse values in cache using setInv function
        x$setInv(mat_inv) 
        # Returns calculated inverse
        return(mat_inv) 
}