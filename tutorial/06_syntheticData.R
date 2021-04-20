# TS data structure:
#       "Province.State" "Country.Region" "Lat" "Long"  dates . . .

# First let's create a 'fake' location
fake.locn <- c(NA,NA,NA,NA)

# names for these columns
names(fake.locn) <- c("Province.State","Country.Region","Lat","Long")

# let's set the dates
dates.vec <- seq(as.Date("2020/1/1"), as.Date("2020/4/09"), "days")

# data.vecX would be the actual values/cases
data.vec1 <- rpois(length(dates.vec),lambda=25)
# can also add more cases
data.vec2 <- abs(rnorm(length(dates.vec),mean=135,sd=15))
data.vec3 <- abs(rnorm(length(dates.vec),mean=35,sd=5))

# this will names the columns as your dates
names(data.vec1) <- dates.vec
names(data.vec2) <- dates.vec
names(data.vec3) <- dates.vec

# merge them into a data frame with multiple entries
synthetic.data <- as.data.frame(rbind(
			rbind(c(fake.locn,data.vec1)),
			rbind(c(fake.locn,data.vec2)),
			rbind(c(fake.locn,data.vec3))
			))

# finally set you locn to somethign unqiue, so you can use it in the generate.SIR.model fn
synthetic.data$Country.Region <- "myLocn"

# one could even add "status"
synthetic.data$status <- c("confirmed","death","recovered")

# OR just one case per locn
synthetic.data2 <- synthetic.data[,-ncol(synthetic.data)]
synthetic.data2$Country.Region <- c("myLocn","myLocn2","myLocn3")

# now we can use this 'synthetic' dataset with any of the TS functions
# data checks
integrity.check(synthetic.data)
consistency.check(synthetic.data)
data.checks(synthetic.data)

# quantitative indicators
tots.per.location(synthetic.data)
growth.rate(synthetic.data)
single.trend(synthetic.data2[3,])
mtrends(synthetic.data)

# SIR models
synthSIR <- generate.SIR.model(synthetic.data2,geo.loc="myLocn")
plt.SIR.model(synthSIR, interactiveFig=TRUE)
sweep.SIR.models(synthetic.data2,geo.loc="MyLocn")
