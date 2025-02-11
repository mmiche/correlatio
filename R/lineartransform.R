#' Linearly transform one scale into another scale.
#
#' @description Apply the default random forest model to the data.
#
#' @param futureRange Vector that shows the range of the new scale, e.g., c(1, 5).
#
#' @param vec A vector which contains the values that shall be transformed to the new scale.
#
#' @param digits A single integer that shows the number of digits, which the transformed values shall get rounded to.
#
#' @return a vector with the linearly transformed new values, rounded to how many digits the user has set the function argument 'digits'.
#
#' @author Marcel Miché
#
#' @importFrom stats lm coefficients
#
#' @examples
#' someValues <- stats::rnorm(n=10)
#' # Linearly transform to values between 1 and 5, rounded to zero digits.
#' lineartransform(futureRange = c(1, 5), vec = someValues, digits = 0)
#
#' @references
#'
#' [lm; linear model command from the stats package](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/lm.html)
#
#' @export
#
lineartransform <- function(futureRange=c(1, 5), vec = NULL, digits=NULL) {
    
    # Error handling. Start.
    
    # futureRange must be a numeric vector with a minimum (first) and maximum (second) value.
    if((length(futureRange)!=2 | any(is.na(futureRange)) | any(!is.numeric(futureRange))) || any(diff(futureRange)<0)) {
        stop("The function argument 'futureRange' must be a numeric vector, with two values, first, the minimum of the range, seconde, the maximum of the range, e.g., c(1, 5).")
    }
    
    # A single vector (vec) of numeric values to be linearly transformed.
    if((is.null(vec) | !(length(vec)>=3) | any(!is.numeric(vec))) || var(vec)==0) {
        stop("The function argument 'vec' must be a numeric vector containing at least three values.")
    }
    
    # digits must be a single integer value >= 0.
    if((any(is.na(digits)) | any(!is.numeric(digits)) | any(digits%%1 != 0L)) || any(digits < 0)) {
        stop("The function argument 'digits' must be a single integer value, which shows to how many digits you want to round the linearly transformed values.")
    }
    
    # Error handling. Stop.
    
    rng <- range(vec)
    lmy <- seq(futureRange[1], futureRange[2], length.out = length(vec))
    lmx <- seq(min(vec), max(vec), length.out = length(vec))
    modCoefs <- coefficients(stats::lm(lmy ~ lmx))
    if(is.null(digits)) {
        return(modCoefs[1] + modCoefs[2] * vec)
    } else {
        return(round(modCoefs[1] + modCoefs[2] * vec, digits = digits))
    }
}
