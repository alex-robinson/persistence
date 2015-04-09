
# Load useful functions 
library(myr)



trend <- function(y,x,L)
{
    n  = length(y) 
    ii = which(is.na(y))

    # Only perform detrending if at least 50% of points exist
    if (length(ii)/n < 0.5) {

        # Fill in missing points with linear approximation
        if (length(ii)>0) y[ii] = approx(x,y,xout=x[ii])$y 

        # Calculate the smooth fit
        fit = loess(y~x,span=(L*2+1)/n)
        yfit = predict(fit)
    } else {
        yfit = y*NA 
    }

    return(yfit)
}



## Load and preprocess data ##
if (TRUE) {

    # load("data/HadGHCND_TX_data3D.day1-365.1950-2011.RData")
    # dat = data3D 
    # rm(data3D)

    # Detrend each time series individually
    dat$yfit = dat$data*NA 
    for (i in 1:length(dat$grid$ID)) {
        for (j in 1:length(dat$days)) {
            dat$yfit[i,j,] = trend(dat$data[i,j,],x=dat$years,L=5)
        }
    }
            

}




