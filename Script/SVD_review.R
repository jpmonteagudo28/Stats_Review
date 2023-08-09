mat <- matrix(1:20,4,5)
tmat <-t(mat)
prodmat <- tmat%*%mat # V matirx
prodaat <- mat %*% tmat # U matrix
idenmat <- mat %*% (iden <- diag(5))
eigens <- eigen(prodmat)
eigensaat <- eigen(prodaat)
singval <- round(sqrt(eigens$values),3);singval[is.nan(singval)] <- 0
U <- round(eigensaat$vectors,3)
V <- round(eigens$vectors,3)
plot(eigens$values,lty = 1, lwd = 2, pch = 10, col = "orangered", type ="l")
svd(mat)
## Calculating the SVD consists of finding the eigenvalues and eigenvectors of 
## AA^t and A^tA. The eigenvectors of A^tA make up the columns of V , the eigenvectors
## of AA^t  make up the columns of U. Also, the singular values in S are square roots 
## of eigenvalues from AA^t or A^tA.  The singular values are the diagonal entries of 
## the S matrix and are arranged in descending order. The singular values are always 
## real numbers. If the matrix A is a real matrix, then U and V are also real.

## https://web.mit.edu/be.400/www/SVD/Singular_Value_Decomposition.htm#:~:text=Calculating%20the%20SVD%20consists%20of,up%20the%20columns%20of%20U.

## To address the varying values of the resulting matrices and eigenvalues
## The SVD is not unique. If we perturb the column vectors of both of the basis matrices 
## by the same sign matrix, we obtain another valid SVD for the same matrix. This is also 
## true for eigendecompositions of symmetric matrices. I didn't run your code, but what 
## almost undoubtedly happened was that each of your eigen calls ended up choosing a 
## different sign pattern for the basis vectors. There's no algorithmic error here

    ## Taken from https://stats.stackexchange.com/questions/49389/singular-value-decomposition-procedure-in-r
