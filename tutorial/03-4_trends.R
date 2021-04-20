# single location trend, in this case using data from the City of Toronto
tor.data <- covid19.Toronto.data()
single.trend(tor.data[tor.data$status=="Active Cases",])

# or data from the province of Ontario
ts.data <- covid19.data("ts-confirmed")
ont.data <- ts.data[ ts.data$Province.State == "Ontario",]
single.trend(ont.data)

# or from Italy
single.trend(ts.data[ ts.data$Country.Region=="Italy",])


# multiple locations
ts.data <- covid19.data("ts-confirmed")
mtrends(ts.data, geo.loc=c("Canada","Ontario","Uruguay","Italy"))

# multiple cases
mtrends(tor.data)


# interactive plot of trends
# for all locations and all type of cases
itrends(covid19.data("ts-ALL"),geo.loc="ALL")

# or just for confirmed cases and some specific locations, saving the result in an HTML file named "itrends_ex.html"
itrends(covid19.data("ts-confirmed"), geo.loc=c("Uruguay","Argentina","Ontario","US","Italy","Hubei"), fileName="itrends_ex")

# interactive trend for Toronto cases
itrends(tor.data[,-ncol(tor.data)])
