# a quick function to overview top cases per region for time series and aggregated records
report.summary()
  

# save the tables into a text file named 'covid19-SummaryReport_CURRENTDATE.txt'
# where *CURRRENTDATE* is the actual date
report.summary(saveReport=TRUE)

# summary report for an specific location with default number of entries
report.summary(geo.loc="Canada")

# summary report for an specific location with top 5
report.summary(Nentries=5, geo.loc="Canada")

# it can combine several locations
report.summary(Nentries=30, geo.loc=c("Canada","US","Italy","Uruguay","Argentina"))
