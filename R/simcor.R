#' Simulate two correlated variables.
#
#' @description Simulate pairs of variables with a predefined correlation between them.
#
#' @param obs A single integer that determines the number of simulated observations in each of the pair of variables.
#
#' @param rhos A vector with at least one value that shows the theoretical correlation between the simulated pair of variables.
#
#' @return a list with as many data.frames (each consisting of two columns) as there are values passed to the function argument 'rhos'.
#
#' @author Marcel Miché
#
#' @importFrom stats rnorm cor
#
#' @examples
#' # Simulate a list with two data.frames. The first one contains variables that are correlated
#' # around -.8, the second one around .7. Both data.frames contain 200 observations.
#' simcor(obs = 200, rhos = c(-.8, .7))
#
#' @references
#'
#' \href{https://stirlingcodingclub.github.io/simulating_data/index.pdf}{pdf, see headline: Simulating data with known correlations}
#
#' @export
#
simcor <- function(obs=100, rhos=c(-.5, .5)) {
    
    # Error handling. Start.

    # obs must be a single integer value, greater than 2
    if(length(obs)>1 | any(is.na(obs)) | any(!is.numeric(obs)) | any(obs%%1 != 0L)) {
        obs <- 100
        message("Message: The function argument 'obs' must be a single integer value. It has been set to its default of 100 (observations).")
    }
    
    # rhos must be a vector with a least one numeric value. All values must be between -1 and 1.
    if((length(rhos)==0L | any(is.na(rhos)) | any(!is.numeric(rhos))) || any(rhos < -1 | rhos > 1)) {
        stop("The function argument 'rhos' must be a vector with a least one numeric value, which must lie within -1 and 1.")
    }
    
    # Error handling. Stop.
    
    df <- data.frame(row.names = 1:obs)
    pairsLsOut <- list()
    for(i in 1:length(rhos)) {
        rho <- rhos[i]
        # Source for how to simulate variables x1 and x2:
        # https://stirlingcodingclub.github.io/simulating_data/index.pdf
        # In that pdf, see headline: Simulating data with known correlations
        x1 <- rnorm(n = obs, mean = 0, sd = 1)
        x2 <- (rho * x1) + sqrt(1 - rho*rho) * rnorm(n = obs, mean = 0, sd = 1)
        res <- round(cor(x1, x2), digits=5)
        pairsLsOut[[paste0("Correlation_", as.character(res))]] <- data.frame(x1, x2)
    }
    return(pairsLsOut)
}
