# retrieve time series data
TS.data <- covid19.data("ts-ALL")

# static and interactive plot 
totals.plt(TS.data)
# totals for Ontario and Canada, without displaying totals and one plot per page
totals.plt(TS.data, c("Canada","Ontario"), with.totals=FALSE,one.plt.per.page=TRUE)

# totals for Ontario, Canada, Italy and Uruguay; including global totals with the linear and semi-log plots arranged one next to the other
totals.plt(TS.data, c("Canada","Ontario","Italy","Uruguay"), with.totals=TRUE,one.plt.per.page=FALSE)

# totals for all the locations reported on the dataset, interactive plot will be saved as "totals-all.html"
totals.plt(TS.data, "ALL", fileName="totals-all")
# retrieve aggregated data
data <- covid19.data("aggregated")

# interactive map of aggregated cases -- with more spatial resolution
live.map(data)

# or
live.map()

# interactive map of the time series data of the confirmed cases with less spatial resolution, ie. aggregated by country
live.map(covid19.data("ts-confirmed"))
