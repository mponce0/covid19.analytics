# select the data of interest
mydata <- covid19.ALL.agg.cases

# identify unique countries
unique(mydata[,"Country_Region"])

# identify unique cities
unique(mydata[,"Province_State"])

