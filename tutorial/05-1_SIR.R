# read time series data for confirmed cases
data <- covid19.data("ts-confirmed")

# run a SIR model for a given geographical location
generate.SIR.model(data,"Hubei", t0=1,t1=15)
generate.SIR.model(data,"Germany",tot.population=83149300)
generate.SIR.model(data,"Uruguay", tot.population=3500000)
generate.SIR.model(data,"Ontario",tot.population=14570000, add.extras=TRUE)

# the function will aggregate data for a geographical location, like a country with multiple entries
generate.SIR.model(data,"Canada",tot.population=37590000, add.extras=TRUE)

#####

# modelling the spread for the whole world, storing the model and generating an interactive visualization
world.SIR.model <- generate.SIR.model(data,"ALL", t0=1,t1=15, tot.population=7.8e9, staticPlt=FALSE)
# plotting and visualizing the model
plt.SIR.model(world.SIR.model,"World",interactiveFig=TRUE,fileName="world.SIR.model", add.extras=TRUE)

