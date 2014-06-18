## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	cs <- NULL
	set <- function(y) {
		x <<- y
		cs <<- NULL
	}
	get <- function() x
	setcm <- function(cm) cs <- cm
	getcm <- function () cs
	list (set=set, get=get, setcm=setcm, getcm=getcm)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
            y <- makeCacheMatrix(x)
		cs <- y$getcm()
		if (!is.null(cs))
		{
		 print ("getting cached data")
		 return (cs)
		}
		data <- y$get()
            if (det(x) == 0)
            {
               print ("matrix not invertible")
            }
            else
            {
		  cs <- solve(data)
		  y$setcm(cs)
            }
		cs
}
