#' @title A constructor function for t tests
#'
#' TODO: Brief description
#'
#' @param x #vector of data
#' @param y #vector of data
#' @param alpha #the alpha, a value between 0-1
#'
#' @return A named list
#' @export
#'
#' @examples
#'
myconstr=function(x, y, alpha=0.05){

  #declaring vectors for data and the source "v". Code from the example.
  data <- vector(mode = "numeric", length = length(x) + length(y))
  v <- vector(mode = "list", length = length(data))

  #creating dataframe
  data <- c(x,y)
  v <- rep(c("x","y"), c(length(x),length(y))) # Creation of qual var
  df = data.frame("data" = data, "v" = v)

  #running ttest

  tt<-t.test(x,y,var.equal = TRUE, conf.level=1-alpha)


  #returning a list
  returnObject = list('data' = df, 'alpha' = alpha, 'ConfidenceInt' = 1-alpha, 'pvalue'=tt$p.value)
  class(returnObject) = "Rttest"
  returnObject
}










#testing myconstr

set.seed(21)
x <- rnorm(30,5,2)
set.seed(23)
y <- rnorm(30,3,2)
alpha <- 0.05

obj <- myconstr(x=x, y=y, alpha=alpha)
class(obj)
print(obj)






