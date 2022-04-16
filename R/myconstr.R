#' @title A constructor function for t tests
#'
#' @description This function takes in two vectors of data and an alpha value, and
#' it provides the confidence interval and p-value from a standard t-test.
#' NOTE: as the function is now, it assumes the variances of the two vectors of data are the same.
#'
#' @param x #vector of data
#' @param y #vector of data
#' @param alpha #the alpha, a value between 0-1
#'
#' @return A named list with a dataframe of the data, confidence interval, and p-value
#' @export
#'
#' @importFrom stats  t.test
#'
#' @examples
#'x <- rnorm(30,5,2); y <- rnorm(30,3,2)
myconstr=function(x, y, alpha=0.05){

  ###conditionals to check arguments
  #confirming x is a numeric vector
  xClass = class(x)
  if(xClass != "numeric") {
    stop(paste0("The input x is of class ", xClass, " but must be a vector!!"))
  }
  #confirming y is a numeric vector
  yClass = class(y)
  if(yClass != "numeric") {
    stop(paste0("The input y is of class ", xClass, " but must be a vector!!"))
  }
  #confirming alpha is between 0-1
  if(alpha < 0 || alpha > 1) {
    stop(paste0("The input alpha is out of bounds. Input a value for alpha between 0 and 1"))
  }


  #declaring vectors for data and the source "v". Code from the example.
  data <- vector(mode = "numeric", length = length(x) + length(y))
  v <- vector(mode = "list", length = length(data))

  #creating dataframe
  data <- c(x,y)
  v <- rep(c("x","y"), c(length(x),length(y)))
  df = data.frame("data" = data, "v" = v)

  #running ttest

  tt<-t.test(x,y,var.equal = TRUE, conf.level=1-alpha)


  #returning a list
  returnObject = list('data' = df, 'alpha' = alpha, 'ConfidenceInt' = 1-alpha, 'pvalue'=tt$p.value)
  class(returnObject) = "Rttest"
  returnObject
}

#' @title A print method for Rttest objects
#'
#' @description This function prints a Rttest object.
#'
#' @param x #An object of class Rttest. This is a list with data, confidence interval, and pvalue
#' @param ... #allows the function to work if more than one argument is submitted.
#'
#' @return A formatted table of the input data with a note at the bottom with the confidence interval and the p-value
#'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling %>% footnote
#' @export
#'
#'
print.Rttest <- function(x, ...) {

  #though it shouldn't ever happen, the method confirms that an Rttest object is given as the argument
  stopifnot(class(x) == "Rttest")

  #generating table kable styled
  printTable <- kable(x$data, caption = "Input Data") %>%
    kable_styling("hover", full_width = F) %>%
    footnote(general = paste("Confidence Interval: ", x$ConfidenceInt, '\n',
                             "p-value: ", x$pvalue))
  printTable

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

