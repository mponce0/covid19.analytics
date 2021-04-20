# read TimeSeries data
TS.data <- covid19.data("TS-confirmed")

# select a location of interest, eg. France
# France has many entries, just pick "France"
FR.data <- TS.data[ (TS.data$Country.Region == "France") & (TS.data$Province.State == ""),]

# sweep values of R0 based on range of dates to consider for the model
ranges <- 15:25
deltaT <- 35
params_sweep <- sweep.SIR.models(data=FR.data,geo.loc="France", t0_range=ranges, deltaT=deltaT)

# the parameters --beta,gamma,R0-- are returned in a "matrix" "array" object
print(params_sweep)
#       [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]    
# beta  0.5231031 0.5250777 0.5323438 0.5217565 0.5355503 0.5473388 0.559132
# gamma 0.4768969 0.4749223 0.4676562 0.4782435 0.4644497 0.4526611 0.440868
# R0    1.096889  1.105608  1.138323  1.090985  1.153086  1.209158  1.268253
#       [,8]      [,9]      [,10]     [,11]   
# beta  0.5668948 0.5753911 0.5835743 0.592407
# gamma 0.4331052 0.4246089 0.4164257 0.407593
# R0    1.308908  1.355108  1.401389  1.453428

# obtain the R0 values from the parameters
R0s <- unlist(params_sweep["R0",])
# nbr of infected cases
FR.infs<- preProcessingData(FR.data,"France")

# average per range
# define ranges
lst.ranges <- lapply(ranges, function(x) x:(x+deltaT))

# compute averages
avg.FR.infs <- lapply(lst.ranges, function(x) mean(FR.infs[x]))

# plots
plot(R0s, type='b')
# plot vs average number of infected cases
plot(avg.FR.infs, R0s, type='b')

