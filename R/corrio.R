#' Apply and visualize Pearson's product-moment correlation.
#
#' @description Apply the logistic regression model to the data.
#
#' @param data A data.frame with two columns, which shall be correlated by Pearson's product-moment method.
#
#' @param visualize A single boolean value (default: TRUE), which determines whether the data shall be visualized in two plots.
#
#' @return a list with a data.frame (name: dat), a list (name: details), and two graphs as elements (plot1 and plot2).
#' dat contains these five columns:
#' \enumerate{
#' \item x Values of the first variable (= x).
#' \item y Values of the second variable (= y).
#' \item x-mean(x) Difference between x and the mean of x.
#' \item y-mean(y) Difference between y and the mean of y.
#' \item covVec Product of x-mean(x) and y-mean(y).
#' }
#'
#' details is a list with 12 objects, each of which contains an explanation as attribute:
#' \enumerate{
#' \item Mean of variable 1 (variable 1 = x).
#' \item Mean of variable 2 (variable 2 = y).
#' \item Sum of all negative products (negSum): (x-mean(x)) * (y-mean(y)).
#' \item Sum of all positive products (posSum): (x-mean(x)) * (y-mean(y)).
#' \item Numerator of covariance formula: Sum of negSum and posSum.
#' \item Denominator of covariance formula: n - 1.
#' \item Covariance: numeratorCov/denominatorCov.
#' \item Standard deviation of variable 1 (i.e., x): R command sd().
#' \item Standard deviation of variable 2 (i.e., y): R command sd().
#' \item Product of standard deviations (prodSD) of variables 1 and 2 (i.e., x and y).
#' \item Correlation: Covariance/prodSD.
#' \item Percentages of pairwise directions of s, c, n (s = same, c = contrary, n = no)
#' }
#' plot1 and plot2 are two ways of visualizing the connection between the individual values and their respective mean value.
#
#' @author Marcel Miché
#
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab scale_colour_manual theme element_blank element_text element_rect geom_segment
#' @importFrom tibble tibble
#' @importFrom stats sd var
#
#' @examples
#' simData <- simcor(obs=100, rhos = .6)
#' corrio(data=simData[[1]], visualize = TRUE)
#
#' @references
#'
#' \insertRef{curran2010explorations}{correlatio}
#'
#' \insertRef{wickham2016programming}{correlatio}
#
#' @export
#
corrio <- function(data=NULL, visualize=TRUE) {
    
    # Error handling. Start.
    
    # data must be a data.frame with exactly two columns.
    if(is.null(data) | !(is.data.frame(x=data) | is.matrix(x=data)) || ncol(x=data)!=2) {
        stop("The function argument 'data' must be of class data.frame. It must contain exactly two columns, which shall be analyzed by the Pearson product-moment correlation method.")
    }
    
    if(!is.data.frame(x=data)) {
        data <- data.frame(data)
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
    
    # Error handling. Stop.

    # ---------------------------------------
    variable1 <- data[,1]
    variable2 <- data[,2]
    
    mean1 <- mean(variable1)
    mean2 <- mean(variable2)
    
    val1Mean1 <- variable1 - mean1
    val2Mean2 <- variable2 - mean2
    covVec <- val1Mean1*val2Mean2
    covDf <- data.frame(x=variable1, y=variable2, val1Mean1, val2Mean2, covVec)
    colnames(covDf)[c(3,4)] <- c("x-mean(x)", "y-mean(y)")
    
    attr(mean1, "Explanation") <- "Mean of variable 1 (variable 1 = x)."
    attr(mean2, "Explanation") <- "Mean of variable 2 (variable 2 = y)."
    negSum <- sum(covDf$covVec[covDf$covVec <= 0])
    attr(negSum, "Explanation") <- "Sum of all negative products (negSum): (x-mean(x)) * (y-mean(y))."
    posSum <- sum(covDf$covVec[covDf$covVec >= 0])
    attr(posSum, "Explanation") <- "Sum of all positive products (posSum): (x-mean(x)) * (y-mean(y))."
    numeratorCov <- sum(c(negSum, posSum))
    attr(numeratorCov, "Explanation") <- "Numerator of covariance formula: Sum of negSum and posSum."
    denominatorCov <- nrow(covDf)-1
    attr(denominatorCov, "Explanation") <- "Denominator of covariance formula: n - 1."
    covariance <- numeratorCov/denominatorCov
    attr(covariance, "Explanation") <- "Covariance: numeratorCov/denominatorCov."
    sd1 <- sd(variable1)
    attr(sd1, "Explanation") <- "Standard deviation of variable 1 (i.e., x): R command sd()."
    sd2 <- sd(variable2)
    attr(sd2, "Explanation") <- "Standard deviation of variable 2 (i.e., y): R command sd()."
    prodSD <- sd1 * sd2
    attr(prodSD, "Explanation") <- "Product of standard deviations (prodSD) of variables 1 and 2 (i.e., x and y)."
    correlation <- covariance/prodSD
    attr(correlation, "Explanation") <- "Correlation: Covariance/prodSD."

    # scn = same direction, contrary direction, no direction.
    # Direction from mean values of both respective variables.
    # No direction, if any of these 3 conditions are met:
    # 1. Each of the pair of values of both variables are exactly equal to their mean.
    # 2. The first of the pair of values is exactly equal to its mean.
    # 3. The second of the pair of values is exactly equal to its mean.
    scnCheck <- c(
        "s"=length(which(covDf[,"covVec"]>0)),
        "c"=length(which(covDf[,"covVec"]<0)),
        "n"=length(which(covDf[,"covVec"]==0L)))
    scn <- rep(NA, times=nrow(covDf))
    if(scnCheck[1]>0) {
        scn[covDf[,"covVec"]>0] <- "s"
    }
    if(scnCheck[2]>0) {
        scn[covDf[,"covVec"]<0] <- "c"
    }
    if(scnCheck[3]>0) {
        scn[covDf[,"covVec"]==0L] <- "n"
    }
    
    # Theoretically possible, if not c, s, and n exist:
    # cs, cn, or sn.
    csnTheoretical <- c("sc", "cn", "sn")
    
    if(length(unique(scn))==2) {
        scnCheck <- scnCheck[scnCheck!=0]
        scnSet <- which(csnTheoretical %in% paste0(names(scnCheck), collapse = ""))
        scn <- factor(scn, levels = names(scnCheck))
    } else {
        scn <- factor(scn, levels = c("c", "s", "n"))
    }
    
    vals1 <- c(variable1, rep(mean1, times=nrow(data)))
    vals2 <- c(variable2, rep(mean2, times=nrow(data)))
    vars1 <- as.factor(rep(1:2, each=nrow(data)))
    vars2 <- as.factor(rep(3:4, each=nrow(data)))
    pairs <- as.factor(rep(1:nrow(data), times=2))
    
    pairs1 <- as.factor(rep(1:nrow(data), times=2))
    pairs1Range <- range(as.numeric(as.character(pairs1)))
    nxt <- nrow(data) + 1
    nxtLast <- nxt + (nrow(data)-1)
    pairs2 <- as.factor(rep(nxt:nxtLast, times=2))
    pairs2Range <- range(as.numeric(as.character(pairs2)))
    
    if(length(unique(scn))==2) {
        clr <- as.factor(rep(scn, times=2))
        if(scnSet == 1) {
            useColor <- c("c"= "#F8766D", "s" = "#00BFC4")
        } else if(scnSet == 2) {
            useColor <- c("c"= "#F8766D", "n" = "#C77CFF")
        } else if(scnSet == 3) {
            useColor <- c("s" = "#00BFC4", "n" = "#C77CFF")
        }
    } else {
        clr <- factor(rep(scn, times=2), c("c", "s", "n"))
        useColor <- c("c"= "#F8766D", "s" = "#00BFC4", "n" = "#C77CFF")
    }
    
    prop_scn <- as.numeric(prop.table(table(scn))*100)
    names(prop_scn) <- levels(clr)
    attr(prop_scn, "Explanation") <- "Percentages of pairwise directions of s, c, n (s = same, c = contrary, n = no)"
    
    detailsLs <- list(mean1, mean2, negSum, posSum, numeratorCov, denominatorCov, covariance,
                      sd1, sd2, prodSD, correlation, prop_scn)
    
    if(visualize) {
        plotData <- tibble::tibble(vals1, vals2, vars1, vars2,
                                   pairs1, pairs2, pairs, clr)
        
        plot1 <-
            ggplot2::ggplot(data=plotData, aes(x=vars1, y=vals1, group=1, color=clr)) +
            geom_line(aes(group=pairs)) +
            geom_line(aes(x=vars2, y=vals2, group=pairs)) +
            xlab(label="Pair of variables") + ylab(label="Values") +
            scale_colour_manual(values = useColor) +
            theme(
                panel.background = element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x=element_text(size=16),
                axis.text.y=element_text(size=16),
                axis.title.y = element_text(size=16),
                panel.border = element_rect(color="grey", fill=NA),
                legend.text=element_text(size=16),
                legend.title = element_blank(),
                legend.position = "top")
        
        plot2 <-
            ggplot2::ggplot(data=plotData, aes(x=pairs1, y=vals1, group=pairs1, color=clr)) +
            geom_line(aes(group=pairs1), linewidth=1) +
            geom_line(aes(x=pairs2, y=vals2, group=pairs2), linewidth=1) +
            geom_segment(aes(x=pairs1Range[1], y=mean1, xend=pairs1Range[2], yend=mean1), color="black") +
            geom_segment(aes(x=pairs2Range[1], y=mean2, xend=pairs2Range[2], yend=mean2), color="black") +
            xlab(label="Pair of variables") + ylab(label="Values") +
            scale_colour_manual(values = useColor) +
            theme(
                panel.background = element_blank(),
                axis.ticks.x = element_blank(),
                axis.text.x=element_blank(),
                axis.title.x=element_text(size=16),
                axis.text.y=element_text(size=16),
                axis.title.y = element_text(size=16),
                panel.border = element_rect(color="grey", fill=NA),
                legend.text=element_text(size=16),
                legend.title = element_blank(),
                legend.position = "top")
        
        return(list(dat=covDf, details=detailsLs, plot1=plot1, plot2=plot2))
    } else {
        return(list(dat=covDf, details=detailsLs, plot="Set function argument 'visualize' to TRUE to see plots 1 and 2."))
    }
}
