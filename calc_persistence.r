
# Load useful functions 
library(myr)                # Only needed for plotting: myfigure function
library(RNetCDF)
source("functions_persistence.r")

## Load data ##
if (TRUE) {

    # Load the dataset 
    filename = "data/HadGHCND_TX_data3D.day1-365.1950-2011.nc"

    nc = open.nc(filename)
    dat = list()     
    dat$day  = var.get.nc(nc,"day")
    dat$year = var.get.nc(nc,"year")
    dat$ID   = var.get.nc(nc,"ID")
    dat$lon   = var.get.nc(nc,"lon")
    dat$lat   = var.get.nc(nc,"lat")
    dat$tas   = var.get.nc(nc,"tas")
    close.nc(nc)

    # Add additional time dimension (days are decimal points dt=1/365)
    dat$time = c(1:(length(dat$day)*length(dat$year)))/365 - 0.5*1/365 + min(dat$year)

}

if (TRUE) {

    ## User parameters 
    smooth_nday = 91     # Must be odd 
    smooth_nyr  =  1     # Must be odd 

    # Calculate the running medians 
    tas_sm = dat$tas*NA 
    for (q in 1:length(dat$ID)) {
        tas_sm[q,,] = calc_runmean(dat$tas[q,,],nday=smooth_nday)

    }

    # Calculate persistence vector
    per = dat$tas*0 
    per[dat$tas >= tas_sm] =  1
    per[dat$tas <  tas_sm] = -1 
    per[is.na(per)] = 0              # To avoid NA values for cumsum function

}

fldr  = "plots"
ptype = "pdf" 

if (TRUE) {

    # Make a simple test plot for a given point and year 
    ip = 1 
    k  = 60 

    xlim = range(dat$day)
    ylim1 = c(-12,18)
    ylim2 = c(-1,1)
    ylim3 = c(-30,30)

    myfigure(fldr,"per_test",asp=1.0,pointsize=16)
    par(mfrow=c(3,1))
    par(plt=c(0.1,0.95,0.18,0.95))

    plot(xlim,ylim1,type="n",xlab="",ylab="Temperature anomaly (°C)")
    abline(h=0,col="grey40")
    points(dat$day,dat$tas[ip,,k],col=1)
    lines(dat$day,tas_sm[ip,,k],lwd=2,col=1)

    plot(xlim,ylim2,type="n",xlab="",ylab="Tendency")
    abline(h=0,col="grey40")
    lines(dat$day,per[ip,,k],type="l",lwd=2,col=4,xlab="",ylab="Tendency")

    plot(xlim,ylim3,type="n",xlab="",ylab="Persistence")
    abline(h=0,col="grey40")
    lines(dat$day,cumsum(per[ip,,k]),lwd=2,col=4)

    mtext(side=1,line=1.5,cex=0.7,"Day")

    graphics.off()
}

