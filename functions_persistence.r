

calc_runmean = function(y2D,nday)
{   # y2D[nday,nyear]
    
    y          = as.vector(y2D)
    trend      = y*NA 
    if (sum(is.na(y))<length(y)) {
        tmp = approx(c(1:length(y)),y,xout=c(1:length(y)),rule=2)$y 
        trend = runmed(tmp,k=nday)
    }
    dim(trend) = dim(y2D)

    trend[which(is.na(y))] = NA 

    return(trend)
}

calc_runmean_year = function(ysm2D,nyr)
{   # Input time running median series from a point ysm[day,year],
    # Calculate the running mean given a window of nyrs and ndays 
    # around each point in time

    trend = ysm2D*NA 

    # for ()

    return(trend)
}

per_write = function(filename)
{


}





# ## To get the myr library, which is hosted on github perform the following:
# if (FALSE) {
#     # Step 1: install devtools
#     install.package("devtools")

#     # Step 2: load devtools and install myr
#     library(devtools)
#     install_github("alex-robinson/myr")

#     # Step 3: load the library as usual
#     library(myr)
# }
