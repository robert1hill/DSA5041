#' @title A constructor function for t tests
#'
#' @description This function takes in two vectors of data, true/false of whether the data is paired, and an alpha value for the acceptable error
#' It provides the confidence interval and p-value from a t-test.
#'
#' @param x #vector of data
#' @param y #vector of data
#' @param paired #a boolean that shows whether the supplied data is paired. This value defaults to false.
#' @param alpha #the alpha, a value between 0-1
#'
#' @return A named list with a dataframe of the data, confidence interval, and p-value
#' @export myttest
#'
#' @export
#'
#' @importFrom stats  t.test
#' @importFrom rlang call2
#' @importFrom stats var.test
#'
#' @examples
#'x <- rnorm(30,5,2); y <- rnorm(30,3,2)
#'obj <- myttest(x=x, y=y, paired=FALSE, alpha=0.05)
myttest=function(x, y, paired=FALSE, alpha=0.05){

  ###conditionals to check arguments
  #confirming x is a numeric vector
  xClass = class(x)
  if(xClass != "numeric") {
    stop(paste0("The input x is of class ", xClass, " but must be a vector!!"))
  }
  #confirming y is a numeric vector
  yClass = class(y)
  if(yClass != "numeric") {
    stop(paste0("The input y is of class ", yClass, " but must be a vector!!"))
  }
  #confirming paired is a boolean
  pClass = class(paired)
  if(pClass != "logical") {
    stop(paste0("The input paired is of class ", pClass, " but must be either TRUE or FALSE!!"))
  }
  #confirming alpha is between 0-1
  if(alpha < 0 || alpha > 1) {
    stop(paste0("The input alpha is out of bounds. Input a value for alpha between 0 and 1"))
  }


  ####Building out the dataframe
  #declaring vectors for data and the source "v". Code from the example.
  data <- vector(mode = "numeric", length = length(x) + length(y))
  v <- vector(mode = "list", length = length(data))

  #creating dataframe
  data <- c(x,y)
  v <- rep(c("x","y"), c(length(x),length(y)))
  df = data.frame("data" = data, "v" = v)


  ####Checking sample varience to determine which type of ttest to run
  #Once checked, the ttest is run.

  tTestType <- ''
  tt <- ''

  if(paired) {
    tTestType <- "Paired"
    tt<-t.test(x,y,mu=0, paired = TRUE)
  }
  else {

    #testing the variances. If the pvalue is > than alpha, then we assume they are equal.
    varTest <- var.test(x,y)
    varEqual <- varTest$p.value > alpha

    #if var is equal
    if(varEqual){
      tTestType <- "T-test"
      tt<-t.test(x,y,var.equal = TRUE)#,conf.level=1-alpha)
    }

    #if var is not equal
    else {
      tTestType <- "Welch"
      tt<-t.test(x,y,var.equal = FALSE)#,conf.level=1-alpha)
    }
  }


  #based on the ttest p-value, we accept or reject the Null
  result = 'N'
  if(tt$p.value < alpha) {
    result = 'Y'
  }

  #returning a list
  returnObject = list('testType' = tTestType,
                      'result' = result,
                      'statistics' = tt,
                      'data' = df,
                      'pvalue'=tt$p.value)
  class(returnObject) = "Rttest"
  returnObject
}

#' @title A print method for Rttest objects
#'
#' @description This function prints the confidence interval for an Rttest object.
#'
#' @param x #An object of class Rttest. This is a list with data, confidence interval, and pvalue
#' @param ... #allows the function to work if more than one argument is submitted.
#'
#' @return A string that has the confidence interval for the provided alpha.
#'
#' @export print.Rttest
#' @export
#'
#'
print.Rttest <- function(x, ...) {

  #though it shouldn't ever happen, the method confirms that an Rttest object is given as the argument
  stopifnot(class(x) == "Rttest")

  #generating table kable styled
  output <- paste("The confidence interval for the difference between the sample means is: ",
        x$statistics['conf.int'])
  output

}


#' @title A plot method for Rttest objects
#'
#' @description This function plots the data that was supplied when creating a Rttest object, if the data is non-paired. The plot is of two boxplots. However, if the data is paired, one boxplot of the difference of data is shown.
#'
#'
#' @param x #An object of class Rttest. This is a list.
#' @param ... #any additional arguments to the plot function.
#'
#' @return Side-by-side boxplots that show the data distribution of the supplied populations or a single boxplot of the difference of the data, depending on whether the data is paired.
#'
#' @importFrom ggplot2 ggplot geom_boxplot aes labs geom_errorbar ggtitle
#' @export plot.Rttest
#'
#' @export
#'
#' @example
#' \dontrun{
#' plot(obj)}
#'
plot.Rttest <- function(x, ...) {

  #though it shouldn't ever happen, the method confirms that an Rttest object is given as the argument
  stopifnot(class(x) == "Rttest")

  plotIt <- ''

  #checking to see if the ttest was paired
  if(x$testType == "Paired") {

    #since it's paired, we take the difference between the samples
    dataDiff <- x$data[x$data[,2]=='x',1] - x$data[x$data[,2]=='y',1]

    #and plot it as a boxplot
    plotIt <- ggplot(x$data[x$data[,2]=='x',], aes(x=rep("x",length(dataDiff)),y=dataDiff)) +
      geom_boxplot() +
      geom_errorbar(aes(ymin=x$statistics$conf.int[1],ymax=x$statistics$conf.int[2]), color="yellow", size=1) +
      labs(x="Values", y="Difference between paired samples") +
      ggtitle(paste(x$testType, " with a p-value of ", round(x$pvalue,3)))
  }
  else {

    #generating boxplots
    plotIt <- ggplot(x$data, aes(x=x$data[,2],y=x$data[,1],fill=x$data[,2])) +
      geom_boxplot() +
      labs(x="Population", y="Values") +
      ggtitle(paste(x$testType, " with a p-value of ", round(x$pvalue,3)))
  }

  plotIt

}




