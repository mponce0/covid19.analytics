# obtain all the records combined for "confirmed", "deaths" and "recovered" cases
# for the global (worldwide) *aggregated* data
 covid19.data.ALLcases <- covid19.data()

# obtain time series data for global "confirmed" cases
 covid19.confirmed.cases <- covid19.data("ts-confirmed")

# reads all possible datasets, returning a list
 covid19.all.datasets <- covid19.data("ALL")

# reads the latest aggregated data of the global cases
 covid19.ALL.agg.cases <- covid19.data("aggregated")

# reads time series data for global casualties
 covid19.TS.deaths <- covid19.data("ts-deaths")

# read "Time Series" data for the city of Toronto
 Toronto.TS.data <- covid19.data("ts-Toronto")

# this can be also done using the covid19.Toronto.data() fn
 Tor.TS.data <- covid19.Toronto.data() 

# or get the original data as reported by the City of Toronto
 Tor.DF.data <- covid19.Toronto.data(data.fmt="ORIG")

# retrieve US time series data of confirmed cases
 US.confirmed.cases <- covid19.data("ts-confirmed-US")

# retrieve US time series data of death cases
 US.deaths.cases <- covid19.data("ts-deaths-US")

# or both cases combined
 US.cases <- covid19.US.data()
