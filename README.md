# COVID19

## Introduction
The covid19 package allows users to obtain live data from the
novel Coronavirus COVID-19 as published by the JHU CCSE repository [1].
The goal of this pakcage is to make the latest data quickly available
for reserachers and the scientific community.


### Data Accesibility
The `covid19()` function can obtain data from the JHU's CCSE repository,
aggregating the data in different categories "confirmed"/"deaths"/"recovered"
cases reported daily per country/region/city.
By using this function, the data will be pulled directly from the JHU repository
with the latest updates.


### Analitcal Estimates
In addition to the data availability we procide some basics functions to estimate
totals per regions/country/cities, growth rates and daily changes in the reported
number of cases.

 
## Installation
For using the "covi19" package, first you will need to install it.

The stable version can be downloaded from the CRAN repository:
```
install.packages("covid19")
```

To obtain the development version you can get it from the github repository, i.e.
```
# need devtools for installing from the github repo
install.packages("devtools")

# install bioC.logs
devtools::install_github("mponce0/covid19")
```

For using the package, either the stable or developmemnt version, just load it using the library function:
```
# load covid19
library(covid19)
```


## Examples

### Reading data
```
# obtain all the records combined for "confirmed", "deaths" and "recovered" cases
 covid19.data.ALLcases <- covid19()

# obtain records combined for "confirmed" cases
 covid19.confirmed.cases <- covid19("confirmed")

# obtain records combined for "deaths" cases
 covid19.deaths.cases <- covid19("deaths")

# obtain records combined for "recovered" cases
 covid19.recovered.cases <- covid19("recovered")
```

## References
[1] 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
https://github.com/CSSEGISandData/COVID-19
