# totals for confirmed cases for "Ontario"
tots.per.location(covid19.confirmed.cases,geo.loc="Ontario")

# total for confirmed cases for "Canada"
tots.per.location(covid19.confirmed.cases,geo.loc="Canada")

# total nbr of confirmed cases in Hubei including a confidence band based on moving average
tots.per.location(covid19.confirmed.cases,geo.loc="Hubei", confBnd=TRUE)

# total nbr of deaths for "Mainland China"
tots.per.location(covid19.TS.deaths,geo.loc="China")

###

# read the time series data for all the cases
all.data <- covid19.data('ts-ALL')

# run on all the cases
tots.per.location(all.data,"Japan")

###

# total for death cases for "ALL" the regions
tots.per.location(covid19.TS.deaths)

# or just
tots.per.location(covid19.data("ts-confirmed"))
