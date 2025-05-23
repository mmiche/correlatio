#' Visualize the correlation coefficient geometrically.
#
#' @description Visualize the correlation coefficient geometrically, i.e., use the angle between the linear vector that represents the predictor and the linear vector that represents the outcome, show where the dropping of the perpendicular lands on the linear vector that represents the predictor in the two-dimensional linear space, finally read b regression weight from the simple linear regression between predictor and outcome; or read the beta regression weight, in case the predictor and outcome have been scaled (mean = zero, standard deviation = one).
#
#' @param data A data.frame with two columns, which shall be correlated by Pearson's product-moment method.
#
#' @param visualize A single boolean value (default: TRUE), which determines whether the data shall be visualized in two plots.
#
#' @param x A single character, i.e., the column name of the data.frame which shall be the predictor (independent variable) in the simple linear regression.
#
#' @param y A single character, i.e., the column name of the data.frame which shall be the outcome (dependent variable) in the simple linear regression.
#
#' @details Any textbook on linear algebra and/or analytic geometry usually contains at least one numeric example and a geometric visualization of a correlation between two continuous variables. I want to express my gratitude to Dr. Johannes Andres (who taught statistics as well as multivariate statistics to psychology students, of which I was one).
#
#' @return a list with results (name: res), and one graph as elements (name: anglePlot).
#' res is a list with 14 objects:
#' \enumerate{
#' \item covMat Covariance matrix of predictor and outcome.
#' \item covPredMat Covariance matrix of predictor and the predicted outcome, based on the simple linear regression estimates.
#' \item corMat Correlation matrix of predictor and outcome.
#' \item spreadMat Square root of the variance of the predictor and the variance of the outcome. If the angle is greater than 90 degrees, the spread of the predictor is multiplied by minus one.
#' \item angle The angle between predictor and outcome: In R, compute: acos(cor(predictor,outcome))*180/pi.
#' \item rsquared Explained variance of the outcome variable.
#' \item errorVariance Difference between the variance of the outcome and the explained variance of the (predicted) outcome variable.
#' \item errorSpread Square root of the error variance.
#' \item observedSpread Square root of the variance of the outcome variable.
#' \item yhatSpread Square root of the difference between outcome variance and variance of the predicted outcome. If the angle is greater than 90 degrees, yhatSpread is multiplied by minus one.
#' \item bWeight The regression weight (slope) of the simple linear regression of the predictor and the outcome variable.
#' \item betaWeight Same as bWeight, if the predictor and the outcome have both been scaled (mean = 0, standard deviation = 1).
#' \item anglePlot Visualization of the regression weight.
#' \item plotDf tibble with coordinates for anglePlot.
#' }
#
#' @author Marcel Miché
#
#' @importFrom ggplot2 ggplot aes_string geom_path geom_vline geom_hline unit theme element_blank element_text element_line
#' @importFrom tibble is_tibble
#' @importFrom stats sd var cov cor lm
#
#' @examples
#' positiveCorDat <- data.frame(x1=c(5,9,3,6,2,9,3,7,2,8),
#'                       x2=c(2,6,7,8,3,5,5,8,3,9))
#' negativeCorDat <- data.frame(x1=c(5,9,3,6,2,9,3,7,2,8),
#'                       x2=c(5,7,9,2,5,5,8,1,9,8))
#' # Run corvisualize with positiveCorDat.
#' corvisualize(data=positiveCorDat, x="x1", y="x2", visualize=TRUE)
#' # Run corvisualize with negativeCorDat.
#' corvisualize(data=negativeCorDat, x="x1", y="x2", visualize=TRUE)
#
#' @references
#'
#' \insertRef{boyer1949invention}{correlatio}
#'
#' \insertRef{gniazdowski2013geometric}{correlatio}
#'
#' \insertRef{graffelman2013linear}{correlatio}
#'
#' \insertRef{graffelman2023improved}{correlatio}
#
#' @export
#
corvisualize <- function(data=NULL, x="x1", y="x2", visualize=TRUE) {

    # Error handling. Start.

    # data must be a data.frame with exactly two columns.
    if(is.null(data) | !(is.data.frame(x=data) | is.matrix(x=data)) || ncol(x=data)!=2) {
        stop("The function argument 'data' must be of class data.frame. It must contain exactly two columns, which shall be analyzed by the Pearson product-moment correlation method.")
    }

    # Both columns of the data.frame must be numeric.
    bothNumeric <- unlist(lapply(data, FUN=class))
    if(!all(bothNumeric == "numeric")) {
        stop("Both columns of the data frame must be of class 'numeric'. Both columns are assumed to be on a continuous scale, e.g., centimeter or inch.")
    }

    # visualize must either be TRUE or FALSE.
    if(length(visualize)!=1 || is.na(x=visualize) || is.numeric(x=visualize) || !any(c(visualize==TRUE, visualize==FALSE))) {
        stop("The function argument 'visualize' must be either TRUE or FALSE.")
    }

    # If column names of data are not proper, e.g., empty character.
    if(!is.character(x) || nchar(x) == 0) {
        stop("The function argument 'x' must be a column name consisting of at least one character.")
    }

    if(!is.character(y) || nchar(y) == 0) {
        stop("The function argument 'y' must be a column name consisting of at least one character.")
    }

    if(!all(c(x, y) %in% colnames(data))) {
        stop("At least one of the column names (see function arguments 'x' and 'y') is wrong.")
    }

    # Error handling. Stop.

    if(tibble::is_tibble(data)) {
        data <- data.frame(data)
    }

    # ---------------------------------------

    data0 <- data
    idxOrder.x <- which(colnames(data0) == x)
    idxOrder.y <- which(colnames(data0) == y)
    colnames(data0)[idxOrder.x] <- "x"
    colnames(data0)[idxOrder.y] <- "y"

    covOut <- cov(data0)
    corOut <- cor(data0)
    spreadOut <- sqrt(diag(covOut))

    angleOut <- acos(x=corOut[2,1]) * 180/pi
    if(angleOut > 90) {
        spreadOut[["x"]] <- -spreadOut[["x"]]
    }
    lmRes <- lm(y ~ x, data = data0)
    covPredOut <- cov(data.frame(x=data0[,"x"], y=lmRes$fitted.values))
    rsquared <- covPredOut[2,2]/covOut[2,2]
    errorVar <- covOut[2,2] - covPredOut[2,2]
    errorSpread <- sqrt(errorVar)
    # ---------------------------
    # Spread of the outcome = length of outcome vector..
    # ---------------------------
    observedSpread <- spreadOut["y"]
    # ---------------------------
    predictionSpread <- sqrt(covOut[2,2] - errorVar)
    # ---------------------------
    # Spread of predictor = length of predictor vector on x-axis..
    # ---------------------------
    predictorSpread <- spreadOut["x"]
    # ---------------------------

    bWeightOut <- predictionSpread/predictorSpread
    betaWeightOut <- predictionSpread/observedSpread

    if(angleOut > 90) {
        betaWeightOut <- -betaWeightOut

    }

    if(angleOut > 90) {
        predictionSpread <- -predictionSpread
    }

    # res: result
    res <- list(
        covMat=covOut,
        covPredMat=covPredOut,
        corMat=corOut,
        spreadMat=spreadOut,
        angle=angleOut,
        rsquared=rsquared,
        errorVariance=errorVar,
        errorSpread=errorSpread,
        observedSpread=observedSpread,
        yhatSpread=predictionSpread,
        bWeight=bWeightOut,
        betaWeight=betaWeightOut)

    if(visualize) {

        dfOut <- tibble::tibble(x=c(0, predictorSpread, 0, predictionSpread),
                                y=c(0, 0, 0, errorSpread),
                                grp=c("x", "x", "y", "y"))
        anglePlot <-
            ggplot2::ggplot(data=dfOut, aes_string(x="x", y="y", group="grp")) +
            geom_path() +
            geom_vline(xintercept = res$yhatSpread, linetype = "dashed") +
            geom_hline(yintercept = res$errorSpread, linetype = "dashed") +
            theme(plot.margin = unit(c(1,1,1,1), "mm"),
                  panel.background = element_blank(),
                  panel.grid.major = element_line(
                      linewidth = 0.25, linetype = 'solid',
                      colour = "grey"),
                  panel.grid.minor = element_line(
                      linewidth = 0.25, linetype = 'solid',
                      colour = "grey"),
                  axis.text.x=element_text(size=16),
                  axis.title.x=element_text(size=16),
                  axis.text.y=element_text(size=16),
                  axis.title.y = element_text(size=16))

        res[["anglePlot"]] <- anglePlot
        res[["plotDf"]] <- dfOut
    }
    return(res)
}
