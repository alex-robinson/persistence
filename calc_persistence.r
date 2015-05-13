
# Load useful functions 
#library(myr)                # Only needed for plotting: myfigure function
library(RNetCDF)
source("functions_persistence.r")

mdays = c(31,28,31,30,31,30,31,31,30,31,30,31)
mfrac = cumsum(mdays) / sum(mdays)

## Load data ##
if (TRUE) {

    # Load the dataset 
    filename = "data/HadGHCND_TX_data3D.day1-365.1950-2011.nc"

    nc  = open.nc(filename)
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
    nday = 91     # Must be odd 
    nyr  =  5     # Must be odd 

    # ntot = 1 
    ntot = length(dat$ID)

    # Calculate persistence information
    cat("Calculating persistence... ")

    per = list(trend=dat$tas*NA,ind=dat$tas*NA,dayp=dat$tas*NA,perp=dat$tas*NA,
               dayn=dat$tas*NA,pern=dat$tas*NA)

    # Check Western Europe 
    qq = which(dat$lat >= 37 & dat$lat <= 55 & dat$lon >= -10 & dat$lon <= 10)

    for (q in qq) {
    # for (q in 1:ntot) {
        tmp = calc_persistence(dat$tas[q,,],time=dat$time,nday=nday,nyr=nyr)
        per$trend[q,,] = tmp$trend 
        per$ind[q,,]   = tmp$ind 
        per$dayp[q,,]  = tmp$dayp 
        per$perp[q,,]  = tmp$perp 
        per$dayn[q,,]  = tmp$dayn 
        per$pern[q,,]  = tmp$pern 
    }

    cat("done.\n")

    # Get binned persistence 
    ss = list(c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))
    bins = seq(1949+0.9166667,2013,by=0.25)

    tmpp = bin_persistence(per$dayp[qq,,],per$perp[qq,,],bins=bins)
    tmpn = bin_persistence(per$dayn[qq,,],per$pern[qq,,],bins=bins)

}

fldr  = "plots"
ptype = "pdf" 


# Plot persistence information 
if (FALSE) {

    # Check Western Europe 
    qq = which(dat$lat >= 37 & dat$lat <= 55 & dat$lon >= -10 & dat$lon <= 10)

    dayp = as.vector(per$dayp[qq,,])
    perp = as.vector(per$perp[qq,,])
    dayn = as.vector(per$dayn[qq,,])
    pern = as.vector(per$pern[qq,,])

    xlim = c(1949,2013)
    ylim = range(perp,pern,na.rm=TRUE)

    # myfigure(fldr,"per_WEUROPE",asp=1.0,pointsize=14)
    par(mfrow=c(2,1))

    par(plt=c(0.1,0.95,0.1,0.95))
    plot(xlim,ylim,type='n',ann=FALSE)
    points(dayp,perp,cex=0.8)
    lines(tmpp$mids,tmpp$per,col=4,lwd=2)
    abline(v=2003.5,col=2,lwd=1)
    mtext(side=2,line=1.8,las=0,"Positive persistence (days)")
    
    par(new=TRUE,plt=c(0.22,0.35,0.7,0.9))
    plot(c(-15,15),c(30,60),type="n",ann=FALSE,axes=FALSE)
    map(add=TRUE,col="grey50")
    points(dat$lon[qq],dat$lat[qq],pch=20,cex=0.5,col=2)

    par(plt=c(0.1,0.95,0.1,0.95))
    plot(xlim,ylim,type='n',ann=FALSE)
    points(dayn,pern,cex=0.8)
    lines(tmpn$mids,tmpn$per,col=4,lwd=2)
    abline(v=2003.5,col=2,lwd=1)
    mtext(side=2,line=1.8,las=0,"Negative persistence (days)")

    # graphics.off()
}




if (FALSE) {

    # Make a simple test plot
    ip = qq[3] 
    
    xlim  = c(2001,2004)

    ylim1 = c(-12,18)
    ylim2 = c(-1,1)
    ylim3 = c(-100,30)

    myfigure(fldr,"per_test",asp=1.0,pointsize=16)
    par(mfrow=c(3,1))
    par(plt=c(0.1,0.95,0.18,0.95))

    plot(xlim,ylim1,type="n",xlab="",ylab="Temperature anomaly (°C)")
    abline(h=0,col="grey40")
    points(dat$time,dat$tas[ip,,],col=1)
    lines(dat$time,per$trend[ip,,],lwd=3,col=2)

    plot(xlim,ylim2,type="n",xlab="",ylab="Tendency")
    abline(h=0,col="grey40")
    lines(dat$time,per$ind[ip,,],type="l",lwd=2,col=4,xlab="",ylab="Tendency")

    plot(xlim,ylim3,type="n",xlab="",ylab="Persistence")
    abline(h=0,col="grey40")
    lines(dat$time,cumsum(per$ind[ip,,]),lwd=2,col=4)

    # mtext(side=1,line=1.5,cex=0.7,"Year")

    graphics.off()
}

