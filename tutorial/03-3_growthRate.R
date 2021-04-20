# read time series data for confirmed cases
TS.data <- covid19.data("ts-confirmed")

# compute changes and growth rates per location for all the countries
growth.rate(TS.data)

# compute changes and growth rates per location for 'Italy'
growth.rate(TS.data,geo.loc="Italy")

# compute changes and growth rates per location for 'Italy' and 'Germany'
growth.rate(TS.data,geo.loc=c("Italy","Germany"))

#####

# Combining multiple geographical locations:

# obtain Time Series data
TSconfirmed <- covid19.data("ts-confirmed")

# explore different combinations of regions/cities/countries
# when combining different locations, heatmaps will also be generated comparing the trends among these locations
growth.rate(TSconfirmed,geo.loc=c("Italy","Canada","Ontario","Quebec","Uruguay"))

growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","United States","Canada","Ontario","Quebec","Uruguay"))

growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","US","Canada","Ontario","Quebec","Uruguay"))

# turn off static plots and activate interactive figures
growth.rate(TSconfirmed,geo.loc=c("Brazil","Canada","Ontario","US"), staticPlt=FALSE, interactiveFig=TRUE)

# static and interactive figures
growth.rate(TSconfirmed,geo.loc=c("Brazil","Italy","India","US"), staticPlt=TRUE, interactiveFig=TRUE)
