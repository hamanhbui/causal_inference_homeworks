# Ha Bui
# hbui13@jhu.edu

library(MASS)

# Input:
#           df: a data frame with n rows.
# Output:   a data frame with n rows obtained from the input dataframe by sampling rows with replacement.
df_resample = function(df){
    shuffled_df = df[sample(1:nrow(df), replace=TRUE), ]
    return(shuffled_df)
}

# Input:
#           mat: a data matrix with n rows and k columns (rows are samples, columns are variables).
# Output:   a data frame with column (variable) names "x1" to "xk", and data from the matrix.
df_make = function(mat){
    df = as.data.frame(mat)

    for (i in 1:NCOL(mat)){
        names(df)[i] = paste("x", toString(i), sep="")
    }

    return(df)
}

# Input:
#           df: a data frame to be resampled
#           k: number of resampled datasets to generate.
#           f: a function of df giving the statistic of interest (e.g. function(df) { mean(df$x1) })
#           q: a real number 0 < q < 0.5 giving the lower quantile of the desired confident interval.
# Output:   a four element vector giving the statistic of interest (first element),
#           and lower and upper confidence intervals corresponding to
#           q and 1-q quantiles (second and third elements) of the empirical
#           bootstrap distribution, and the size of the confidence interval.
bootstrap_ci = function(df, k, f, q){
    estimator = f(df)

    diff_es = rep(0, k)

    for (i in 1:k){
        shuffled_df = df_resample(df)
        diff_es[i] = estimator - f(shuffled_df)
    }

    lower_q = quantile(diff_es, q)
    upper_q = quantile(diff_es, 1 - q)
    
    out = c(estimator, estimator + lower_q[[1]], estimator + upper_q[[1]], upper_q[[1]] - lower_q[[1]])
    return(out)
}


# Input:
#           X,y: a training dataset given as a set of feature rows, represented
#           by a n by k matrix X, and a set of corresponding
#           output predictions, represented by a n by 1 matrix y.
#           A: a function of the features x, used in estimating equations.
#           tol: tolerance used to exit the Newton-Raphson loop.
# Output:
#           A row vector of weights for a logistic regression model (with no intercept)
#           maximizing the likelihood of observing the data.
logisreg = function(X, y, A, tol = 0.01){
    W = rep(0, NCOL(X))
    while(TRUE){
        mu = 1 / (1 + exp(-X %*% W))
        grad = t(A(X)) %*% (y - mu)
        
        mu = as.vector(mu)
        hess = t(A(X)) %*% diag(mu * (1 - mu)) %*% X

        W_new = W + solve(hess) %*% grad
        
        if(norm(W_new - W) < tol){
            break
        }else{
            W = W_new
        }
    }
    return(t(W))
}

# Input:
#           none
# Output:
#           none
# Description:
#           Generates a 1000 sample data frame with 5 variables drawn from...
#           uses bootstrap_ci(.) and appropriately defined closures to generate 5%/95% confidence intervals
#           for the following statistics:
#           mean of x1
#           variance of x2
#           median of x3
#           covariance of x4 and x5
#           (use 1000 resamples)
#           print the output of each of the four calls to bootstrap_ci(.), each on a separate line.
main = function(){

    # set the seed for the pseudo-random number generator.
    set.seed(0)
    # set the tolerance for Newton-Raphson.
    tol <- 0.01
    
    # load the dataset
    dat <- read.csv("ihdp_subset.csv", header = TRUE)

    
    m <- as.matrix(dat)
    
    y <- m[,ncol(m), drop=FALSE]
    X <- m[,2:12]
    X <- cbind(rep(1, dim(X)[1]), X)

    A1 = function(x){
        x
    }

    w1 <- logisreg(X, y, A1, tol)

    print(w1)

    A2 = function(x){
        x^2
    }

    w2 <- logisreg(X, y, A2, tol)

    print(w2)

    k <- 3

    mu <- c(1, 2, 3)

    Sigma <- matrix(c(1, 1, 1, 1, 3, 1, 1, 1, 5), k, k)

    n <- 1000

    dat <- mvrnorm(n = n, mu, Sigma)

    df <- df_make(dat)

    mean_1 <- function(df) { mean(df$x1) }
    mean_2 <- function(df) { mean(df$x2) }

    k <- 1000

    print(bootstrap_ci(df, k, mean_1, 0.025))
    print(bootstrap_ci(df, k, mean_2, 0.025))
}

main()

