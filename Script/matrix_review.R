## Some matrix refresher

mat <- matrix(c(10,8,5,12), ncol =2, byrow = T)
B <- matrix(c(5,3,15,6), ncol =2, byrow = T)
dim(mat); dim(B)

## Matrix addition
 mat + B
 [,1] [,2]
 [1,]   15   11
 [2,]   20   18
 
 ## Matrix subtraction
 mat - B
 [,1] [,2]
 [1,]    5    5
 [2,]  -10    6
 
 ## Transpose
 t(mat)
 [,1] [,2]
 [1,]   10    5
 [2,]    8   12 
 
 ## Matrix multiplication
 # By a scalar (use the * or /)
 mat *2
 [,1] [,2]
 [1,]   20   16
 [2,]   10   24
 
 mat/2
 [,1] [,2]
 [1,]  5.0    4
 [2,]  2.5    6
 
 #Element-wise multiplication (both matrices of same dimensions)
 mat*B
 [,1] [,2]
[1,]   50   24
[2,]   75   72
 
 # Matricial multiplication (using %*% operator)
 mat%*%B # number of columns in mat  == no. rwos in B
 [,1] [,2]
 [1,]  170   78
 [2,]  205   87
 
 #Matrix crossproduct (calculate matricial product and transpose)
 t(mat)%*% B
 [,1] [,2]
 [1,]  125   60
 [2,]  220   96
 
 #More efficient to use `crossprod` or tcrossprod`
 crossprod(mat,B) ## equiv. to t(mat)%*%B
 [,1] [,2]
 [1,]  125   60
 [2,]  220   96
 
 tcrossprod(mat,B) ## equiv. to mat %*% t(B)
 [,1] [,2]
 [1,]   74  198
 [2,]   61  147
 
 ## Exterior Product (outer function)
 # Compute exterior product of 2 matrices using the `%o%`
 mat %o% B # equiv. to outer(mat,B,"*")
 [,1] [,2]
 [1,]   50   40
 [2,]   25   60
 
 [,1] [,2]
 [1,]  150  120
 [2,]   75  180
 
 [,1] [,2]
 [1,]   30   24
 [2,]   15   36
 
 [,1] [,2]
 [1,]   60   48
 [2,]   30   72
 
 ## Kronecker Product (using the %x% operator and prduces 
                    # (block matrix)
 mat %x% B
 [,1] [,2] [,3] [,4]
 [1,]   50   30   40   24
 [2,]  150   60  120   48
 [3,]   25   15   60   36
 [4,]   75   30  180   72
 
 ## Power of a matrix
 # No built in function in base R for matrix power
 #1. install.packages("expm"); library(expm) and the %^% operator
 mat %^% 2
 [,1] [,2]
 [1,]  140  176
 [2,]  110  184
 #2. install.packages("matrixcalc"); library(matrixcalc)
 matrix.power(mat,2)
 [,1] [,2]
 [1,]  140  176
 [2,]  110  184
 # Check that the power is correct by multiplying matrix by itself
 mat %*% mat
 [,1] [,2]
 [1,]  140  176   ## matrix must be square to calculate power, rows = cols
 [2,]  110  184
 
 # Element-wise Power
 mat^2
 [,1] [,2]
 [1,]  100   64
 [2,]   25  144
 
 ## Determinant of a matrix (scalar value encoding property of matrix)
 det(mat)
 [1] 80
 det(B)
 [1] -15
 
 ## Inverse of a matrix (use `solve` function)
 M <- solve(mat)
 [,1]   [,2]
 [1,]  0.1500 -0.100
 [2,] -0.0625  0.125
 ## A matrix multiplied by its inverse is the identity matrix
 ## Check:
 mat %*% solve(mat)
 [,1] [,2]
 [1,]    1    0
 [2,]    0    1
 # the `solve` function is used to solve system of equations
 #if you want to find solution to A%*%X = B:
 solve(mat,B)
 [,1]    [,2]
 [1,] -0.7500 -0.1500
 [2,]  1.5625  0.5625
 
 ## Rank of a matrix (min. no of cols/rows that are linearly independent)
  # No base function in R to calculate rank but can use `qr` function,
  # which in addition to calculating the QR decomposition, returns the
  # rank of the input matrix.
  qr(mat)
  $rank
  [1] 2
  
 ## Matrix diagonal (use the `diag` function in base R)
  diag(mat)
  [1] 10 12
  # Appying the `rev` function to the cols of matrix, you can also extract
  # elements of secondary diagonal
  # rev() - provides a reversed version of its argument. It is generic 
  # function with a default method for vectors and one for dendrograms.
  diag(apply(mat,2,rev))
  5 8
  
  ## Diagonal matrix
  ## You can also make a diagonal matrix, passing a vector as input
  diag(c(7,9,2,6))
  [,1] [,2] [,3] [,4]
  [1,]    7    0    0    0
  [2,]    0    9    0    0
  [3,]    0    0    2    0
  [4,]    0    0    0    6
  ## You can also create identity matrices
  diag(4)
  [,1] [,2] [,3] [,4]
  [1,]    1    0    0    0
  [2,]    0    1    0    0
  [3,]    0    0    1    0
  [4,]    0    0    0    1
  
  ## Eigen values and eigenvectors (using `eigen`)
  eigen(mat)
  eigen() decomposition
  $values
  [1] 17.403124  4.596876
  
  $vectors
  [,1]       [,2]
  [1,] -0.7339565 -0.8286986
  [2,] -0.6791964  0.5596952
  
  ## Singular, QR and Cholesky (`svd`,`qr`, `chol`)
  svd(mat)
  $d
  [1] 17.678275  4.525328
  
  $u
  [,1]       [,2]
  [1,] -0.7010275 -0.7131342
  [2,] -0.7131342  0.7010275
  
  $v
  [,1]       [,2]
  [1,] -0.5982454 -0.8013130
  [2,] -0.8013130  0.5982454
  # The function will return a list, where d is a vector containing
  # singular values in decresing order, u and v are matrices contaning
  # the left and right singular vectors.
  
  
  # QR decomposition
  # the first element will return a matrix of same dimension as original
  # upper triangle contains thr R of the decomp and the lower Q
  qr(mat)
  $qr
  [,1]       [,2]
  [1,] -11.1803399 -12.521981
  [2,]   0.4472136   7.155418
  
  $rank
  [1] 2
  
  $qraux
  [1] 1.894427 7.155418
  
  $pivot
  [1] 1 2
  
  attr(,"class")
  [1] "qr"
  
  # Cholesky factorization of a real symmetric positive-definite square matric
  
  chol(mat)     # Doesn't check for symmetry. Use `isSymmetric` to check
  [,1]     [,2]
  [1,] 3.162278 2.529822
  [2,] 0.000000 2.366432
  
  isSymmetric(mat)
  [1] FALSE