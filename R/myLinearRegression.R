#' Produce a ggpair plot Y vs less than 5 columns & output Coefficients and P-value
#'
#' This function produces a ggpair plot using GGally pakage for the column Y vs the columns
#' presented in in the matrix X. It will subset for the rows only presented on sub parameter.
#' It will also produce the coefficients and p-value for the linear model fit.
#'
#' @param X is a matrix.
#' @param Y a vector representing the response.
#' @param sub a vector representing the number of rows from the matrix. Number of sub should match Y.
#' @return ggpair plot,  \code{coef} and \code{p-value}.
#' @export
#' @examples
#' X = matrix(runif(100),nrow=20,ncol=4,byrow = TRUE)
#' myLinearRegression(X=X,Y=c(152,266,341,148,242,264,544,279,488,62),sub=c(2,3,4,15,12,9,7,18,19,20))

myLinearRegression <- function(X=X,Y=Y,sub=sub){
  if(dim(X)[2] < 5) {
    xy <- matrix(NA, nrow = dim(X)[1], ncol = dim(X)[2])
    for (i in sub) {
      xy[i,] <- X[i,]
    }
    xy[rowSums(is.na(xy)) != ncol(xy), ]
    xy_df <- na.omit(as.data.frame(xy))
    final_df<-cbind(Y,xy_df)
    modelfit_full <- lm( Y ~ . , data = final_df)
    coef<-summary(modelfit_full)$coefficients[,1]
    p_value<-summary(modelfit_full)$coefficients[,4]
    plot<-GGally::ggpairs(final_df, title="Correlogram with ggpairs()")
    return(list(plot,"coef"=coef,"p-value"=p_value))
  }else{
    warning('Too many variables to plot')
  }
}
