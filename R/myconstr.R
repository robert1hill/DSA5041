#' Square function
#'
#' TODO: Brief description
#'
#' @param x #comment detailing what x is
#' @param y #comment detailing what y is
#' @param alpha #comment detailing what alpha is
#'
#' @return A named list
#' @export
#'
#' @examples
#'
myconstr=function(x, y, alpha){ #Do I want to put in any default values?

  #creating dataframe

  #TODO create code that checks the vector lengths and appends NA to the end of the shorter
  data = data.frame(x, y)

  #calculating confidence interval

  #calculating pvalue

  #returning a list
  returnObject = list('data' = data, 'alpha' = alpha, 'CONFIDENCE INTERVAL' = 'CONFIDENCE INTERVAL', 'pvalue'='pvalue')
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





