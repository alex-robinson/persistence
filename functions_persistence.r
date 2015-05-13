

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
    nday  = dim(ysm2D)[1] 
    nyear = dim(ysm2D)[2]

    for (k in 1:nday) {
        y = ysm2D[k,]
        if (sum(is.na(y))<nyear) {
            tmp = approx(c(1:length(y)),y,xout=c(1:length(y)),rule=2)$y 
            trend[k,] = runmed(tmp,k=nyr)
        }
    }

    return(trend)
}

calc_runmean_2D = function(y2D,nday,nyr)
{   # Input 2D array: y[day,year],
    # Calculate the running mean given a window of nyrs and ndays 
    # around each point in time

    # Wrap data so that there is a buffer on all sides 
    nbuff1 = (nday-1)/2 
    nbuff2 = (nyr-1)/2 

    dims0 = dim(y2D)
    dims  = dim(y2D)
    dims[1] = dims[1]+2*nbuff1
    dims[2] = dims[2]+2*nbuff2
    
    # Fill in buffered array 
    y2Dex = array(NA,dim=dims)
    y2Dex[1:nbuff1,(nbuff2+1):(dims[2]-nbuff2)] = y2D[(dims0[1]-nbuff1+1):dims0[1],]
    y2Dex[(nbuff1+1):(dims[1]-nbuff1),(nbuff2+1):(dims[2]-nbuff2)] = y2D[,]
    y2Dex[(dims[1]-nbuff1+1):dims[1],(nbuff2+1):(dims[2]-nbuff2)] = y2D[1:nbuff1,]

    y2Dex[,1:nbuff2] = y2Dex[,((dims[2]-nbuff2+1):dims[2])-nbuff2]
    y2Dex[,(dims[2]-nbuff2+1):dims[2]] = y2Dex[,(1:nbuff2)+nbuff2]


    # Get indices of points describing our window

    i0 = floor((nday-1)/2)
    ii = c(-i0:i0)
    k0 = floor((nyr-1)/2)
    kk = c(-k0:k0)
    
    trendex = y2Dex*NA 

    for (i in i0:(dim(y2Dex)[1]-i0)) {
        for (k in k0:(dim(y2Dex)[2]-k0)) {
            trendex[i,k] = mean(y2Dex[i+ii,k+kk],na.rm=TRUE)
        }
    }

    # Cut off buffer 
    trend = trendex[(nbuff1+1):(dims[1]-nbuff1),(nbuff2+1):(dims[2]-nbuff2)]

    return(trend)
}

runmean <- function(x,n=31)
{
    n0 = floor((n-1)/2)
    inds = c(-n0:n0)
    xsm  = x*NA
    for ( i in (n0+1):(length(x)-n0) ) xsm[i] = mean(x[i+inds],na.rm=TRUE)

    return(xsm)
}

calc_persistence = function(y,time,trend=NULL,nday=91,nyr=5)
{   # Given 2D inputs of y[nday,nyr] and the smooth trend[nday,nyr],
    # calculate the persistence index (-1,0,1) and the persistence
    # vector per[nevents,nyr] and the event_day[nevents,nyr]
    # time should be a decimal-based vector 

    if (length(y) != length(time)) {
        cat("calc_persistence:: error: length of time vector does not match length of y.\n")
        return(NULL)
    }

    # Calculate trend 
    if (is.null(trend)) trend = calc_runmean_2D(y,nday=nday,nyr=nyr)

    # Calculate persistence vector
    per_ind = y*0 
    per_ind[y >= trend]     =  1
    per_ind[y <  trend]     = -1 
    per_ind[is.na(per_ind)] = 0              # To avoid NA values

    # Go through vector and calculate persistent events 
    # Perform this step on a 1D vector to avoid artificial cutoffs 
    # at day 1 and day 365 of the year 
    per_ind1D = as.vector(per_ind) 
    
    per_pos  = rep(NA,length(time))   # [nevents,nyr] => max number of events in a year is 365
    per_neg  = rep(NA,length(time))
    mday_pos = rep(NA,length(time))
    mday_neg = rep(NA,length(time))

    pval  = per_ind1D[1]
    nper  = 0
    e_neg = 0 
    e_pos = 0 

    for (k in 1:length(time)) {

        if (per_ind1D[k] == pval) {
            nper = nper+1 
        } else {
            if (pval < 0) {
                e_neg = e_neg + 1 
                per_neg[e_neg]  = nper 
                mday_neg[e_neg] = mean(time[(k-nper):k])
            } else if (pval > 0) {
                e_pos = e_pos + 1 
                per_pos[e_pos]  = nper 
                mday_pos[e_pos] = mean(time[(k-nper):k])
            }

            nper = 0 
            pval = per_ind1D[k]
        }

    }

    # Remove excess NA values from persistence event vectors 
    ## TO DO ## 


    return(list(trend=trend,ind=per_ind,perp=per_pos,dayp=mday_pos,pern=per_neg,dayn=mday_neg))

}

bin_persistence = function(days,per,bins)
{
    mids       = bins[1:(length(bins)-1)] + diff(bins)/2
    per_binned = mids*NA 

    for (k in 1:length(mids)) {
        ij = which(days >= bins[k] & days < bins[k+1])
        if (length(ij)>0) per_binned[k] = mean(per[ij],na.rm=TRUE)
    }

    return(list(mids=mids,per=per_binned))
}

per_write = function(filename)
{   # Write a netcdf file with persistence information 

    ## TO DO ##
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
