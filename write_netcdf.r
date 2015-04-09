
library(RNetCDF)

# Load Rdata dataset
if (FALSE) {
    load("data/HadGHCND_TX_data3D.day1-365.1950-2011.RData")

    # data3D:
    # List of 4
    # $ grid :'data.frame':  1319 obs. of  3 variables:
    #  ..$ ID  : num [1:1319(1d)] 1 2 3 4 5 6 7 8 9 10 ...
    #  ..$ lons: num [1:1319(1d)] -180 -176 -172 -161 -161 ...
    #  ..$ lats: num [1:1319(1d)] 67.5 67.5 67.5 67.5 65 62.5 60 70 67.5 65 ...
    # $ days : num [1:365] 1 2 3 4 5 6 7 8 9 10 ...
    # $ years: num [1:62(1d)] 1950 1951 1952 1953 1954 ...
    #  ..- attr(*, "dimnames")=List of 1
    #  .. ..$ : NULL
    # $ data : num [1:1319, 1:365, 1:62] 6.7 4.6 3 -1.1 0.7 2.7 4.6 -3.5 -1.7 -0.5 ...

}


# Create NetCDF file and write the dimensions and data
if (TRUE) {

    filename = "data/HadGHCND_TX_data3D.day1-365.1950-2011.nc"

    nc = create.nc(filename)

    dim.def.nc(nc, "day",  dimlength=length(data3D$days),  unlim=FALSE)
    dim.def.nc(nc, "year", dimlength=length(data3D$years), unlim=FALSE)
    dim.def.nc(nc, "ID",   dimlength=length(data3D$grid$ID), unlim=FALSE)

    var.def.nc(nc, "day", "NC_INT", "day")
    var.def.nc(nc, "year", "NC_INT", "year")
    var.def.nc(nc, "ID", "NC_INT", "ID")

    var.def.nc(nc, "lon", "NC_FLOAT", "ID")
    var.def.nc(nc, "lat", "NC_FLOAT", "ID")

    var.def.nc(nc, "tas", "NC_FLOAT", c("ID","day","year"))
    att.put.nc(nc, "tas", "long_name", "NC_CHAR", "Near-surface air temperature anomaly")
    att.put.nc(nc, "tas", "units", "NC_CHAR", "degrees Celcius")
    att.put.nc(nc, "tas", "missing_value", "NC_FLOAT", -9999.0)

    var.put.nc(nc, "day",  data3D$days)
    var.put.nc(nc, "year", data3D$years)
    var.put.nc(nc, "ID",   data3D$grid$ID)
    var.put.nc(nc, "lon",  data3D$grid$lon)
    var.put.nc(nc, "lat",  data3D$grid$lat)

    var.put.nc(nc, "tas",  data3D$data)

    close.nc(nc)

}