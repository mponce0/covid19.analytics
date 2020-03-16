# COVID19

## Introduction
The covid19 package allows users to obtain live data from the
novel Coronavirus COVID-19 as published by the JHU CCSE repository [1].

The goal of this package is to make the latest data quickly available
for researchers and the scientific community.

<object data="man/figures/livemap.html" width="105%" height="525"></object>

### Data Accesibility
The `covid19.data()` function can obtain data from the JHU's CCSE repository,
aggregating the data in different categories "confirmed"/"deaths"/"recovered"
cases reported daily per country/region/city.
By using this function, the data will be pulled directly from the JHU repository
with the latest updates.


### Analitcal & Graphical Indicators
In addition to the data availability the package includes some basics functions
to estimate totals per regions/country/cities, growth rates and daily changes in
the reported number of cases.


Function  | description
---	 | ---
 `tots.per.location`  |  compute totals per region and plot time series for that specific region/country
 `growth.rate`  |  compute changes and growth rates per region and plot time series for that specific region/country
 `totals.plt`   |  plots in a static and interactive plot total number of cases per day
 `live.map`     |  interactive map displaying cases around the world



### Further Features
We will continue working on adding and developing new features to the package,
in particular modelling and predictive capabilities.


 
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
 covid19.data.ALLcases <- covid19.data()

# obtain records combined for "confirmed" cases
 covid19.confirmed.cases <- covid19.data("confirmed")

# obtain records combined for "deaths" cases
 covid19.deaths.cases <- covid19.data("deaths")

# obtain records combined for "recovered" cases
 covid19.recovered.cases <- covid19.data("recovered")
```

### Some basic analysis
#### Totals per Country/Region/Province
```
# totals for confirmed cases for "Ontario"
tots.per.location(covid19.confirmed.cases,geo.loc="Ontario")

# total for confirmed cases for "Canada"
tots.per.location(covid19.confirmed.cases,geo.loc="Canada")

# total nbr of deaths for "Mainland China"
tots.per.location(covid19.deaths.cases,geo.loc="China")

# total for death cases for "ALL" the regions
tots.per.location(covid19.deaths.cases)
```

<p>
  <img src="man/figures/Hubei_totals.png" width="40%" />
  <img src="man/figures/Italy_totals.png" width="40%" />
  <img src="man/figures/Germany_totals.png" width="40%" />
  <img src="man/figures/Ontario_totals.png" width="40%" />
</p>

The figures show the total number of cases for different cities (provinces/regions) and countries:
one the upper plot in log-scale with a linear fit to an exponential law and in linear scale in the bottom panel.
Details about the models are included in the plot, in particular the *growth rate* which in several cases appears to be around 1.2+ as predicted by some models.
Notice that in the case of Hubei, the values is closer to 1, as the dispersion of the virus has reached its logistic asymptope while in other cases (e.g. Germany and Italy --for the presented dates--) is still well above 1, indicating its exponential growth.

**DISCLAIMER** Please notice that the modelling function implements a *simple (naive)* and straight-forward linear regression model, which is **not** optimal for exponential fits.
The reason is that the errors for large values of the dependant variable weight much more than those for small values when apply the exponential function to go back to the original model.
Nevertheless for the sake of a quick interpretation is OK, but one should bare in mind the implications of this simplification.



#### Growth Rate
```
# read data for confirmed cases
data <- covid19.data("confirmed")

# compute changes and growth rates per location for all the countries
growth.rate(data)

# compute changes and growth rates per location for 'Italy'
growth.rate(data,geo.loc="Italy")

# compute changes and growth rates per location for 'Italy' and 'Germany'
growth.rate(data,geo.loc=c("Italy","Germany"))
```

#### Graphical Output
```
# static and interactive plot 
totals.plt(data)
```
<object data="man/figures/totals.html" width="80%" height="525">
</object>

```
# interactive map of cases
live.map(data)
```
<p>
Interactive examples can be seen at
    <a href="https://mponce0.github.io/covid19/">https://mponce0.github.io/covid19/</a>
</p>


## Further Resources
* https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov
* https://www.repidemicsconsortium.org/
* https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/
* https://www.youtube.com/watch?v=Kas0tIxDvrg


## References
[1] 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
https://github.com/CSSEGISandData/COVID-19
