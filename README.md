# COVID19.Analytics

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
[![CRAN checks](https://cranchecks.info/badges/worst/covid19.analytics)](https://cranchecks.info/pkgs/covid19.analytics)
[![Downloads](https://cranlogs.r-pkg.org/badges/covid19.analytics)](https://cran.r-project.org/package=covid19.analytics)
<!-- badges: end -->


## Introduction
The "covid19.analytics" R package allows users to obtain live\* worldwide data from the
*novel CoronaVirus Disease* originally reported in 2019, CoViD-19, as published by the
JHU CCSE repository [1], as well as, provide basic analysis tools and functions to
investigate these datasets.

The goal of this package is to make the latest data promptly available
to researchers and the scientific community.

<object data="man/figures/livemap.html" width="105%" height="525"></object>
<!--
.
<object data="https://raw.githubusercontent.com/mponce0/covid19.reports/master/reports/livemap.html" width="105%" height="525"></object>
.
<object data="https://github.com/mponce0/covid19.reports/blob/master/reports/livemap.html" width="105%" height="525">
</object>
-->


### Data Accessibility
The `covid19.data()` function allows users to obtain realtime data about the CoViD19 reported cases
from the JHU's CCSE repository, in the following modalities:
* "aggregated" data for the latest day, with a great 'granularity' of geographical regions (ie. cities, provinces, states, countries)
* "time series" data for larger accumulated geographical regions (provinces/countries)

* "deprecated": we also include the original data style in which these datasets were reported initially.

The datasets also include information about the different categories (status) "confirmed"/"deaths"/"recovered"
of the cases reported daily per country/region/city.

This data-acquisition function, will first attempt to retrieve the data directly
from the JHU repository with the latest updates.
If for what ever reason this fails (eg. problems with the connection) the package
will load a preserved "image" of the data which is **not** the latest one but it
will still allow the user to explore this older dataset.
In this way, the package offers a more robust and resilient approach to the quite
dynamical situation with respect to data availability and integrity.


#### Data retrieval options
<!--
 |    argument    |   description  |
 |----------------|----------------|
 | `aggregated`   |  latest number of cases *aggregated* by country |
 **Time Series data**
 | `ts-confirmed` |  time series data of confirmed cases |
 | `ts-deaths`    |  time series data of fatal cases |
 | `ts-recovered` |  time series data of recovered cases |
 | `ts-ALL`       |  all time series data combined |
 | `ts-confirmed-us` |  time series data of confirmed cases for the US detailed per state |
 | `ts-deaths-us`    |  time series data of fatal cases for the US detailed per state |
**Deprecated data formats**
 | `ts-dep-confirmed` | time series data of confirmed cases as originally reported (deprecated) |
 | `ts-dep-deaths`    | time series data of deaths as originally reported (deprecated) |
 | `ts-dep-recovered` | time series data of recovered cases as originally reported (deprecated)|
 **Combined**
 | `ALL`          | all of the above |
-->

<!------- TABLE ------>
<table style="width:100%">
  <tr>
    <th>argument</th>
    <th>description</th> 
  </tr>
  <tr>
    <td> <code>aggregated</code> </td>
    <td> latest number of cases <i>aggregated</i> by country </td>
  </tr>
  <tr>
    <th colspan="2"> <b>Time Series data</b> </th>
  </tr>
  <tr>
    <td> <code>ts-confirmed</code> </td>
    <td> time series data of confirmed cases </td> 
  </tr>
  <tr>
    <td> <code>ts-deaths</code> </td>
    <td> time series data of fatal cases </td> 
  </tr>
  <tr>
    <td> <code>ts-recovered</code> </td>
    <td> time series data of recovered cases </td>
  </tr>
  <tr>
    <td> <code>ts-ALL</code> </td>
    <td> all time series data combined </td>
  </tr>
  <tr>
     <th colspan="2"> <b>Deprecated data formats</b> </th>
  </tr>
  <tr>
   <td> <code>ts-dep-confirmed</code> </td>
   <td> time series data of confirmed cases as originally reported (deprecated) </td>
  </tr>
  <tr>
   <td> <code>ts-dep-deaths</code> </td>
   <td> time series data of deaths as originally reported (deprecated) </td>
 </tr>
 <tr>
   <td> <code>ts-dep-recovered</code> </td>
   <td> time series data of recovered cases as originally reported (deprecated) </td>
 </tr>
 <tr>
  <th colspan="2"> <b>Combined</b> </th>
 </tr>
 <tr>
  <td> <code>ALL</code> </td>
  <td> all of the above </td>
 </tr>
   <tr>
    <th colspan="2"> Time Series data for <i>specific locations</i> </th>
  </tr>
   <tr>
    <td> <code>ts-Toronto</code> </td>
    <td> time series data of confirmed cases for the city of Toronto, ON - Canada </td> 
  </tr>
  <tr>
    <td> <code>ts-confirmed-US</code> </td>
    <td> time series data of confirmed cases for the US detailed per state </td> 
  </tr>
  <tr>
    <td> <code>ts-deaths-US</code> </td>
    <td> time series data of fatal cases for the US detailed per state </td> 
  </tr>
</table>
<!------- TABLE ------>


### Data Structure
The *TimeSeries* data is organized in an specific manner with a given set of fields or columns,
which resembles the following structure:

<table>
 <tr>
  <td>"Province.State"</td>  <td>"Country.Region"</td>  <td>"Lat"</td>  <td>"Long"</td>  <td>...</td><td>seq of dates</td><td>...</td> 
 </tr>
</table>


#### Using your own data and/or importing new data sets
If you have data structured in a data.frame organized as described above, then most of the functions provided by the "covid19.analytics" package for analyzing *TimeSeries* data will work with your data.
In this way it is possible to add new data sets to the ones that can be loaded using the repositories predefined in this package and extend the analysis capabilities to these new datasets.

Be sure also to check the compatibility of these datasets using the `Data Integrity and Consistency Checks` functions described in the following section.


### Data Integrity and Consistency Checks
Due to the ongoing and rapid changing situation with the CoViD-19 pandemic, sometimes the reported data has been detected to change its internal format or even show some "anomalies" or "inconsistencies" (see https://github.com/CSSEGISandData/COVID-19/issues/).

For instance, in some cumulative quantities reported in time series datasets, it has been observed that these quantities instead of continuously increase sometimes they decrease their values which is something that should not happen, (see for instance, https://github.com/CSSEGISandData/COVID-19/issues/2165). We refer to this as inconsistency of **"type II"**.

Some negative values have been reported as well in the data, which also is not possible or valid; we call this inconsistency of **"type I"**.

When this occurs, it happens at the level of the origin of the dataset, in our case, the one obtained from the JHU/CCESGIS repository [1].
In order to make the user aware of this, we implemented two consistency and integrity checking functions:

* `consistency.check()`, this function attempts to determine whether there are consistency issues within the data, such as, negative reported value (inconsistency of "type I") or anomalies in the cumulative quantities of the data (inconsistency of "type II")

* `integrity.check()`, this determines whether there are integrity issues within the datasets or changes to the structure of the data

Alternatively we provide a `data.checks()` function that will run both functions on an specified dataset.

#### Data Integrity
It is highly unlikely that you would face a situation where the internal structure of the data, or its actual integrity may be compromised but if you think that this is the case or the `integrity.check()` function reports this, please we urge you to contact the developer of this package (https://github.com/mponce0/covid19.analytics/issues).

#### Data Consistency
Data consistency issues and/or anomalies in the data have been reported several times, see https://github.com/CSSEGISandData/COVID-19/issues/.
These are claimed, in most of the cases, to be missreported data and usually are just an insignificant number of the total cases.
Having said that, we believe that the user should be aware of these situations and we recommend using the `consistency.check()` function to verify the dataset you will be working with.


### covid19-Sequencing data
The `covid19.genomic.data()` allows users to obtain the covid19's genomic sequencing data from NCBI [3].


### Analytical & Graphical Indicators
In addition to the access and retrieval of the data, the package includes some
basics functions to estimate totals per regions/country/cities, growth rates
and daily changes in the reported number of cases.


### Overview of the Main Functions from the "covid19.analytics" Package
<!--
 | Function  | description |
 | --------	 | ----------- |
 **Data Acquisition**
 | `covid19.data` |  obtain live\* worldwide data for covid19 virus, from the JHU's CCSE repository [1]
 **Analysis**
 | `report.summary`  |  summarize the current situation, will download the latest data and summarize different quantities
 | `tots.per.location`  |  compute totals per region and plot time series for that specific region/country
 | `growth.rate`  |  compute changes and growth rates per region and plot time series for that specific region/country
 **Graphics**
 | `totals.plt`   |  plots in a static and interactive plot total number of cases per day
 | `live.map`     |  generates an interactive map displaying cases around the world
 **Modelling**
 | `generate.SIR.model`  |  generates a SIR (Susceptible-Infected-Recovered) model
-->

<!------- TABLE ------>
<table style="width:100%">
  <tr>
    <th> Function </th>
    <th> Description </th>
    <th> Main Type of Output</th>
  </tr>
  <tr>
   <th colspan="3"> <b>Data Acquisition</b> </th>
  </tr>
  <tr>
    <td> <code>covid19.data</code> </td>
    <td> obtain live* worldwide data for covid19 virus, from the JHU's CCSE repository [1] </td>
    <td> return dataframes/list with the collected data </td>
  </tr>
   <tr>
    <td> <code>covid19.Toronto.data</code> </td>
    <td> obtain live* data for covid19 cases in the city of Toronto, ON Canada, from the City of Toronto reports [2] </td>
    <td> return dataframe/list with the collected data </td>
  </tr>
   <tr>
    <td> <code>covid19.US.data</code> </td>
    <td> obtain live* US specific data for covid19 virus, from the JHU's CCSE repository [1] </td>
    <td> return dataframe with the collected data </td>
  </tr>

<tr>
  <td> <code>covid19.genomic.data</code> </td>
  <td> obtain covid19's genomic sequencing data from NCBI [3] </td>
  <td> list, with the RNA seq data in the <code>"$NC_045512.2"</code> entry </td>
 </tr>
   <tr>
   <th colspan="3"> <b>Data Quality Assessment</b> </th>
  </tr>
  <tr>
    <td> <code>data.checks</code> </td>
    <td> run integrity and consistency checks on a given dataset </td>
    <td> diagnostics about the dataset integrity and consistency </td>
  </tr>
  <tr>
    <td> <code>consistency.check</code> </td>
    <td> run consistency checks on a given dataset </td>
    <td> diagnostics about the dataset consistency </td>
  </tr>
  <tr>
    <td> <code>integrity.check</code> </td>
    <td> run integrity checks on a given dataset </td>
    <td> diagnostics about the dataset integrity </td>
  </tr>
 <tr>
   <th colspan="3"> <b>Analysis</b> </th>
  </tr>
  <tr>
    <td> <code>report.summary</code> </td>
    <td> summarize the current situation, will download the latest data and summarize different quantities </td>
    <td> on screen table and static plots (pie and bar plots) with reported information, can also output the tables into a text file</td>
  </tr>
  <tr>
   <td> <code>tots.per.location</code> </td>
   <td> compute totals per region and plot time series for that specific region/country </td>
   <td> static plots: data + models (exp/linear, Poisson, Gamma), mosaic and histograms when more than one location are selected </td>
  </tr>
  <tr>
   <td> <code>growth.rate</code> </td>
   <td> compute changes and growth rates per region and plot time series for that specific region/country </td>
   <td> static plots: data + models (linear,Poisson,Exp), mosaic and histograms when more than one location are selected </td>
  </tr>
  <tr>
   <td> <code>single.trend</code> <br> <code>mtrends</code> </td>
   <td> visualize different indicators of the "trends" in daily changes for a single or mutliple locations </td>
   <td> compose of static plots: total number of cases vs time, daily changes vs total changes in different representations</td>
  </tr>
 <tr>
   <th colspan="3">Graphics and Visualization</th>
 </tr>
  <tr>
   <td> <code>total.plts</code> </td>
   <td> plots in a static and interactive plot total number of cases per day, the user can specify multiple locations or global totoals </td>
   <td> static and interactive plot </td>
 </tr>
   <tr>
   <td> <code>itrends</code> </td>
   <td> generates an interactive plot of daily changes vs total changes in a log-log plot, for the indicated regions </td>
   <td> interactive plot </td>
 </tr>
  <tr>
   <td> <code>live.map</code> </td>
   <td> generates an interactive map displaying cases around the world </td>
   <td> static and interactive plot </td>
  </tr>
  <tr>
   <th colspan="3">Modelling</th>
 </tr>
  <tr>
   <td> <code>generate.SIR.model</code> </td>
   <td> generates a SIR (Susceptible-Infected-Recovered) model </td>
   <td> list containing the fits for the SIR model </td>
 </tr>
  <tr>
   <td> <code>plt.SIR.model</code> </td>
   <td> plot the results from the SIR model </td>
   <td> static and interactive plots </td>
 </tr>
</table>
<!------- TABLE ------>



### Details and Specifications of the Analytical & Visualization Functions

#### Reports
The `report.summary()` generates an overall report summarizing the different datasets.
It can summarize the "Time Series" data (`cases.to.process="TS"`), the "aggregated" data (`cases.to.process="AGG"`) or both (`cases.to.process="ALL"`).
It will display the top 10 entries in each category, or the number indicated in the `Nentries` argument, for displaying all the records set `Nentries=0`.

The function can also target specific geographical location(s) using the `geo.loc` argument.
When a geographical location is indicated, the report will include an additional "Rel.Perc" column for the confirmed cases indicating the *relative* percentage among the locations indicated.
Similarly the totals displayed at the end of the report will be for the selected locations.

In each case ("TS" or/and "AGG") will present tables ordered by the different cases included, i.e.
confirmed infected, deaths, recovered and active cases.

The dates when the report is generated and the date of the recorded data will be included at the beginning of each table.

It will also compute the totals, averages, standard deviations and percentages of various quantities:
* it will determine the number of *unique* locations processed within the dataset
* it will compute the total number of cases per case

* Percentages: percentages are computed as follow:
  - for the "Confirmed" cases, as the ratio between the corresponding number of cases and the total number of cases, i.e. a sort of *"global percentage"* indicating the percentage of infected cases wrt the rest of the world
  - for "Confirmed" cases, when geographical locations are specified, a *"Relative percentage"* is given as the ratio of the confirmed cases over the total of the selected locations
  
  - for the other categories, "Deaths"/"Recovered"/"Active", the percentage of a given category is computed as the ratio between the number of cases in the corresponding category divided by the "Confirmed" number of cases, i.e. a *relative percentage* with respect to the number of confirmed infected cases in the given region

* For "Time Series" data:
  - it will show the *delta* (change or variation) in the last day, daily changes day before that (t-2), three days ago (t-3), a week ago (t-7), two weeks ago (t-14) and a month ago (t-30)
  - when possible, it will also display the percentage of "Recovered" and "Deaths" with respect to the "Confirmed" number of cases
  - The column "GlobalPerc" is computed as the ratio between the number of cases for a given country over the total of cases reported
  - The *"Global Perc. Average (SD: standard deviation)"* is computed as the average (standard deviation) of the number of cases among all the records in the data
  - The *"Global Perc. Average (SD: standard deviation) in top X"* is computed as the average (standard deviation) of the number of cases among the top *X* records


Typical structure of a `summary.report()` output for the Time Series data:
```
################################################################################ 
  ##### TS-CONFIRMED Cases  -- Data dated:  2020-04-12  ::  2020-04-13 12:02:27 
################################################################################ 
  Number of Countries/Regions reported:  185 
  Number of Cities/Provinces reported:  83 
  Unique number of geographical locations combined: 264 
-------------------------------------------------------------------------------- 
  Worldwide  ts-confirmed  Totals: 1846679 
-------------------------------------------------------------------------------- 
   Country.Region Province.State Totals GlobalPerc LastDayChange   t-2   t-3   t-7  t-14 t-30
1              US                555313      30.07         28917 29861 35098 29595 20922  548
2           Spain                166831       9.03          3804  4754  5051  5029  7846 1159
3           Italy                156363       8.47          4092  4694  3951  3599  4050 3497
4          France                132591       7.18          2937  4785  7120  5171  4376  808
5         Germany                127854       6.92          2946  2737  3990  3251  4790  910
.
.
.
-------------------------------------------------------------------------------- 
  Global Perc. Average:  0.38 (sd: 2.13) 
  Global Perc. Average in top  10 :  7.85 (sd: 8.18) 
-------------------------------------------------------------------------------- 

******************************************************************************** 
********************************  OVERALL SUMMARY******************************** 
******************************************************************************** 
  ****  Time Series TOTS **** 
  	 ts-confirmed	 ts-deaths	 ts-recovered 
  	 1846679	      114091	    421722 
               			6.18% 		   22.84% 
  ****  Time Series AVGS **** 
  	 ts-confirmed	 ts-deaths	 ts-recovered 
  	 6995	         432.16	    1686.89 
  			             6.18% 		   24.12% 
  ****  Time Series SDS **** 
  	 ts-confirmed	 ts-deaths	 ts-recovered 
  	 39320.05	     2399.5	    8088.55 
  			             6.1% 		    20.57% 

 * Statistical estimators computed considering 250 independent reported entries 
******************************************************************************** 
```

Typical structure of a `summary.report()` output for the *Aggregated* data:
```
################################################################################################################################# 
  ##### AGGREGATED Data  -- ORDERED BY  CONFIRMED Cases  -- Data dated:  2020-04-12  ::  2020-04-13 12:02:29 
################################################################################################################################# 
  Number of Countries/Regions reported: 185 
  Number of Cities/Provinces reported: 138 
  Unique number of geographical locations combined: 2989 
--------------------------------------------------------------------------------------------------------------------------------- 
                      Location Confirmed Perc.Confirmed Deaths Perc.Deaths Recovered Perc.Recovered Active Perc.Active
1                        Spain    166831           9.03  17209       10.32     62391          37.40  87231       52.29
2                        Italy    156363           8.47  19899       12.73     34211          21.88 102253       65.39
3                       France    132591           7.18  14393       10.86     27186          20.50  91012       68.64
4                      Germany    127854           6.92   3022        2.36     60300          47.16  64532       50.47
5  New York City, New York, US    103208           5.59   6898        6.68         0           0.00  96310       93.32
.
.
.
=================================================================================================================================
  	 Confirmed	 Deaths	  Recovered 	Active 
  Totals 
  	 1846680  	 114090	  421722    	1310868 
  Average 
  	 617.83	    38.17.  	141.09    	438.56 
  Standard Deviation 
  	 6426.31	   613.69	  2381.22 	  4272.19 
  
 * Statistical estimators computed considering 2989 independent reported entries
```

In both cases an overall summary of the reported cases is presented by the end, displaying totals, average and standard devitation of the computed quantities.

A full example of this report for today can be seen 
 <a href="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/covid19-SummaryReport.txt" target="_blank">here</a>
<!--<a href="https://github.com/mponce0/covid19.reports/blob/master/reports/covid19-SummaryReport.txt" target="_blank">here</a>-->
(updated twice a day, daily).

In addition to this, the function will also generate some graphical outputs, including pie and bar charts representing the top regions in each category.


#### Totals per Location & Growth Rate
It is possible to dive deeper into a particular location by using the `tots.per.location()` and `growth.rate()` functions.
Theses functions are capable of processing different types of data, as far as these are "Time Series" data.
It can either focus in one category (eg. "TS-confirmed","TS-recovered","TS-deaths",) or all ("TS-all").
When these functions detect different type of categories, each category will be processed separatedly.
Similarly the functions can take multiple locations, ie. just one, several ones or even "all" the locations within the data.
The locations can either be countries, regions, provinces or cities. If an specified location includes multiple entries, eg. a country that has several cities reported, the functions will group them and process all these regions as the location requested.


##### Totals per Location
This function will plot the number of cases as a function of time for the given locations and type of categories, in two plots: a log-scale scatter one a linear scale bar plot one.

When the function is run with multiple locations or all the locations, the figures will be adjusted to display multiple plots in one figure in a mosaic type layout.

Additionally, the function will attempt to generate different fits to match the data:
* an exponential model using a Linear Regression method
* a Poisson model using a General Linear Regression method
* a Gamma model using a General Linear Regression method
The function will plot and add the values of the coefficients for the models to the plots and display a summary of the results in screen.

It is possible to instruct the function to draw a "confidence band" based on a *moving average*, so that the trend is also displayed including a region of higher confidence based on the mean value and standard deviation computed considering a time interval set to equally dividing the total range of time over 10 equally spaced intervals.

The function will return a list combining the results for the totals for the different locations as a function of time.


##### Growth Rate
The `growth.rate()` function allows to compute *daily changes* and the *growth rate* defined as the ratio of the daily changes between two consecutive dates.

The `growth.rate()` shares all the features of the `tots.per.location()` function, i.e. can process the different types of cases and multiple locations.

The graphical output will display two plots per location:
* a scatter plot with the number of changes between consecutive dates as a function of time, both in linear scale (left vertical axis) and log-scale (right vertical axis) combined
* a bar plot displaying the growth rate for the particular region as a function of time.

When the function is run with multiple locations or all the locations, the figures will be adjusted to display multiple plots in one figure in a mosaic type layout.
In addition to that, when there is more than one location the function will also generate two different styles of heatmaps comparing the changes per day and growth rate among the different locations (vertical axis) and time (horizontal axis).

The function will return a list combining the results for the "changes per day" and the "growth rate" as a function of time.


#### Trends in Daily Changes
We provide three different functions to visualize the *trends* in daily changes of reported cases from time series data.

* <code>single.trend</code>, allows to inspect one single location, this could be used with the worldwide data sliced by the corresponding location, the Toronto data or the user's own data formatted as "Time Series" data.

* <code>mtrends</code>, similar to single.trend function, but accepts multiple or single locations generating one plot per location requested

* <code>itrends</code>, function to generate an interactive plot of the trend in daily changes representing changes in number of cases vs total number of cases in log-scale using **splines** techniques to smooth the abrupt variations in the data


The first two functions will generate "static" plots in a compose with different insets:
- the main plot represents daily changes as a function of time
- the inset figures in the top, from left to right:
   - total number of cases (in linear and semi-log scales),
   - changes in number of cases vs total number of cases
   - changes in number of cases vs total number of cases in log-scale
- the second row of insets, represent the "growth rate" (as defined above) and the "normalized" growth rate defined as the growth rate divided by the maximum growth rate reported for this location



#### Plotting Totals
The function `totals.plt()` will generate plots of the total number of cases as a function of time.
It can be used for the total data or for an specific or multiple locations.
The function can generate static plots and/or interactive ones, as well, as linear and/or semi-log plots.


#### Plotting Cases in the World
The function `live.map()` will display the different cases in each corresponding location all around the world in an interactive map of the world.
It can be used with time series data or aggregated data, aggregated data offers a much more detailed information about the geographical distribution.


### Experimental: Modelling the evolution of the Virus spread
We are working in the development of *modelling* capabilities.
A preliminary prototype has been included and can be accessed using the `generate.SIR.model` function, which implements a simple SIR (*Susceptible-Infected-Recovered*) ODE model using the actual data of the virus.

This function will try to identify the data points where the onset of the epidemy began and consider the following data points to generate a proper guess for the two parameters describing the SIR ODE system.
After that, it will solve the different equations and provide details about the solutions as well as plot them in a static and interactive plot.


### Further Features
We will continue working on adding and developing new features to the package,
in particular modelling and predictive capabilities.



## Installation
For using the "covi19.analytics" package, first you will need to install it.

The stable version can be downloaded from the CRAN repository:
```R
install.packages("covid19.analytics")
```

To obtain the development version you can get it from the github repository, i.e.
```R
# need devtools for installing from the github repo
install.packages("devtools")

# install bioC.logs
devtools::install_github("mponce0/covid19.analytics")
```

For using the package, either the stable or development version, just load it using the library function:
```R
# load "covid19.analytics"
library(covid19.analytics)
```


## Examples

### Reading data
```R
# obtain all the records combined for "confirmed", "deaths" and "recovered" cases -- *aggregated* data
 covid19.data.ALLcases <- covid19.data()

# obtain time series data for "confirmed" cases
 covid19.confirmed.cases <- covid19.data("ts-confirmed")

# reads all possible datasets, returning a list
 covid19.all.datasets <- covid19.data("ALL")

# reads the latest aggregated data
 covid19.ALL.agg.cases <- covid19.data("aggregated")

# reads time series data for casualties
 covid19.TS.deaths <- covid19.data("ts-deaths")
```

Read covid19's genomic data 
```R
# obtain covid19's genomic data
 covid19.gen.seq <- covid19.genomic.data()

# display the actual RNA seq
 covid19.gen.seq$NC_045512.2
```


### Some basic analysis
#### Summary Report
```R
# a quick function to overview top cases per region for time series and aggregated records
report.summary()
```

<p>
  <img src="man/figures/report-summ-agg.pdf" width="45%" />
  <img src="man/figures/report-summ-TSconfirmed.pdf" width="45%" />
</p>


```R
# save the tables into a text file named 'covid19-SummaryReport_CURRENTDATE.txt'
# where *CURRRENTDATE* is the actual date
report.summary(saveReport=TRUE)
```

<object data="man/figures/covid19-SummaryReport.pdf" type="application/pdf" width="100%" height="500px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/covid19-SummaryReport.pdf">
 <p> 
  E.g. today's report is available <a href="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/covid19-SummaryReport.txt">here</a> 
 </p>
 </embed>
</object>

```R
# summary report for an specific location with default number of entries
report.summary(geo.loc="Canada")

# summary report for an specific location with top 5
report.summary(Nentries=5, geo.loc="Canada")

# it can combine several locations
report.summary(Nentries=30, geo.loc=c("Canada","US","Italy","Uruguay","Argentina"))
```



#### Totals per Country/Region/Province
```R
# totals for confirmed cases for "Ontario"
tots.per.location(covid19.confirmed.cases,geo.loc="Ontario")

# total for confirmed cases for "Canada"
tots.per.location(covid19.confirmed.cases,geo.loc="Canada")

# total nbr of deaths for "Mainland China"
tots.per.location(covid19.TS.deaths,geo.loc="China")

# total nbr of confirmed cases in Hubei including a confidence band based on moving average
tots.per.location(covid19.confirmed.cases,geo.loc="Hubei", confBnd=TRUE)
```


<object data="man/figures/Hubei_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Hubei_totals.pdf">
 </embed>
</object>
<object data="man/figures/Italy_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Italy_totals.pdf"> 
 </embed>
</object>
<object data="man/figures/Germany_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Germany_totals.pdf">
 </embed>
</object>
<object data="man/figures/Ontario_totals.pdf" type="application/pdf" width="450px">
 <embed src="https://github.com/mponce0/covid19.analytics/blob/master/man/figures/Ontario_totals.pdf">
<!--
<p>
  <img src="man/figures/Hubei_totals.png" width="24%" >
  <img src="man/figures/Italy_totals.png" width="24%" >
  <img src="man/figures/Germany_totals.png" width="24%" >
  <img src="man/figures/Ontario_totals.png" width="24%" >
</p>
-->
  <p>
  Images available <a href="https://github.com/mponce0/covid19.analytics/tree/master/man/figures/">here</a> 
 </p>
 </embed>
</object>

<!--
<p>
  <img src="man/figures/Hubei_totals.pdf" width="24%" />
  <img src="man/figures/Italy_totals.pdf" width="24%" />
  <img src="man/figures/Germany_totals.pdf" width="24%" />
  <img src="man/figures/Ontario_totals.pdf" width="24%" />
</p>
-->


The figures show the total number of cases for different cities (provinces/regions) and countries:
one the upper plot in log-scale with a linear fit to an exponential law and in linear scale in the bottom panel.
Details about the models are included in the plot, in particular the *growth rate* which in several cases appears to be around 1.2+ as predicted by some models.
Notice that in the case of Hubei, the values is closer to 1, as the dispersion of the virus has reached its logistic asymptote while in other cases (e.g. Germany and Italy --for the presented dates--) is still well above 1, indicating its exponential growth.


**IMPORTANT** Please notice that the "linear exponential" modelling function implements a *simple (naive)* and straight-forward linear regression model, which is **not** optimal for exponential fits.
The reason is that the errors for large values of the dependent variable weight much more than those for small values when apply the exponential function to go back to the original model.
Nevertheless for the sake of a quick interpretation is OK, but one should bare in mind the implications of this simplification.


We also provide two additional models, as shown in the figures above, using the Generalized Linear Model `glm()` function, using a *Poisson* and *Gamma* family function.
In particular, the `tots.per.location` function will determine when is possible to automatically generate each model and display the information in the plot as well as details of the models in the console.


```R
# read the time series data for all the cases
all.data <- covid19.data('ts-ALL')

# run on all the cases
tots.per.location(all.data,"Japan")
```
<p>
  <img src="man/figures/Japan_confirmed.pdf" width="32.5%" />
  <img src="man/figures/Japan_recovered.pdf" width="32.5%" />
  <img src="man/figures/Japan_deaths.pdf" width="32.5%" />
</p>


It is also possible to run the `tots.per.location` (and `growth.rate`) functions,
on the whole data set, for which a quite large but complete mosaic figure will
be generated, e.g.
```R
# total for death cases for "ALL" the regions
tots.per.location(covid19.TS.deaths)

# or just
tots.per.location(covid19.data("ts-confirmed"))
```



#### Growth Rate
```R
# read time series data for confirmed cases
TS.data <- covid19.data("ts-confirmed")

# compute changes and growth rates per location for all the countries
growth.rate(TS.data)

# compute changes and growth rates per location for 'Italy'
growth.rate(TS.data,geo.loc="Italy")

# compute changes and growth rates per location for 'Italy' and 'Germany'
growth.rate(TS.data,geo.loc=c("Italy","Germany"))
```

<p>
  <img src="man/figures/gr-changes_Hubei.pdf" width="24%" />
  <img src="man/figures/gr-changes_Italy.pdf" width="24%" />
  <img src="man/figures/gr-changes_Germany.pdf" width="24%" />
  <img src="man/figures/gr-changes_Canada.pdf" width="24%" />
</p>

The previous figures show on the upper panel the number of changes on a daily basis in linear scale (thin line, left y-axis) and log scale (thicker line, right y-axis), while the bottom panel displays the growth rate for the given country/region/city.


Combining multiple geographical locations:
```R
# obtain Time Series data
TSconfirmed <- covid19.data("ts-confirmed")

# explore different combinations of regions/cities/countries
# when combining different locations, heatmaps will also be generated comparing the trends among these locations
growth.rate(TSconfirmed,geo.loc=c("Italy","Canada","Ontario","Quebec","Uruguay"))

growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","United States","Canada","Ontario","Quebec","Uruguay"))

growth.rate(TSconfirmed,geo.loc=c("Hubei","Italy","Spain","US","Canada","Ontario","Quebec","Uruguay")
```

<p>
  <img src="man/figures/changes-per-day-1.pdf" width="22.5%" />
  <img src="man/figures/changes-per-day-2.pdf" width="22.5%" />
  <img src="man/figures/heatmap-changes.pdf" width="22.5%" />
  <img src="man/figures/heatmap-growthRate.pdf" width="22.5%" />
</p>


#### Trends
```R
# single location trend, in this case using data from the City of Tornto
tor.data <- covid19.Toronto.data()
single.trend(tor.data) 

# or data from the province of Ontario
ts.data <- covid19.data("ts-confirmed")
ont.data <- ts.data[ ts.data$Province.State == "Ontario",]
single.trend(ont.data)

# or from Italy
single.trend(ts.data[ ts.data$Country.Region=="Italy",])


# multiple locations
ts.data <- covid19.data("ts-confirmed")
mtrends(ts.data, geo.loc=c("Canada","Ontario","Uruguay","Italy")

# interactive plot of trends
# for all locations and all type of cases
itrends(covid19.data("ts-ALL"),geo.loc="ALL")

# or just for confirmed cases and some specific locations, saving the result in an HTML file named "itrends_ex.html"
itrends(covid19.data("ts-confirmed"), geo.loc=c("Uruguay","Argentina","Ontario","US","Italy","Hubei"), fileName="itrends_ex")
```

<p>
  <img src="man/figures/trendTor.pdf" width="40%" />
  <object data="man/figures/itrends_ex.html" width="58.5%" height="525"></object>
</p>



#### Visualization Tools
```R
# retrieve time series data
TS.data <- covid19.data("ts-ALL")

# static and interactive plot 
totals.plt(TS.data)
```
<object data="man/figures/totals.html" width="80%" height="525">
</object>

```R
# totals for Ontario and Canada, without displaying totals and one plot per page
totals.plt(TS.data, c("Canada","Ontario"), with.totals=FALSE,one.plt.per.page=TRUE)

# totals for Ontario, Canada, Italy and Uruguay; including global totals with the linear and semi-log plots arranged one next to the other
totals.plt(TS.data, c("Canada","Ontario","Italy","Uruguay"), with.totals=TRUE,one.plt.per.page=FALSE)

# totals for all the locations reported on the dataset, interactive plot will be saved as "totals-all.html"
totals.plt(TS.data, "ALL", fileName="totals-all")
```

<object data="man/figures/totals-all.html" width="100%" height="600">
</object>


```R
# retrieve aggregated data
data <- covid19.data("aggregated")

# interactive map of aggregated cases -- with more spatial resolution
live.map(data)

# or
live.map()

# interactive map of the time series data of the confirmed cases with less spatial resolution, ie. aggregated by country
live.map(covid19.data("ts-confirmed"))

```
<p>
Interactive examples can be seen at
    <a href="https://mponce0.github.io/covid19.analytics/">https://mponce0.github.io/covid19.analytics/</a>
</p>


#### Simulating the Virus spread
```R
# read time series data for confirmed cases
data <- covid19.data("ts-confirmed")

# run a SIR model for a given geographical location
generate.SIR.model(data,"Hubei", t0=1,t1=15)
generate.SIR.model(data,"Germany",tot.population=83149300)
generate.SIR.model(data,"Uruguay", tot.population=3500000)
generate.SIR.model(data,"Ontario",tot.population=14570000)

# the function will aggregate data for a geographical location, like a country with multiple entries
generate.SIR.model(data,"Canada",tot.population=37590000)
```

<p>
  <img src="man/figures/SIR-model-Ontario.pdf" width="32.5%" />
  <img src="man/figures/SIR-model-Canada.pdf" width="32.5%" />
  <img src="man/figures/SIR-model-Germany.pdf" width="32.5%" />
</p>


```R
# modelling the spread for the whole world, storing the model and generating an interactive visualization
world.SIR.model <- generate.SIR.model(data,"ALL", t0=1,t1=15, tot.population=7.8e9, staticPlt=FALSE)
# plotting and visualizing the model
plt.SIR.model(world.SIR.model,"World",interactiveFig=TRUE,fileName="world.SIR.model")
```

<object data="man/figures/world.SIR.model.html" width="105%" height="525"></object>


## References
(\*) Data can be upto 24 hs delayed wrt the latest updates.

[1] 2019 Novel CoronaVirus CoViD-19 (2019-nCoV) Data Repository by
Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)
https://github.com/CSSEGISandData/COVID-19

[2] COVID-19: Status of Cases in Toronto -- City of Toronto
https://www.toronto.ca/home/covid-19/covid-19-latest-city-of-toronto-news/covid-19-status-of-cases-in-toronto/

[3] Severe acute respiratory syndrome coronavirus 2 isolate Wuhan-Hu-1, complete genome
NCBI Reference Sequence: NC_045512.2
https://www.ncbi.nlm.nih.gov/nuccore/NC_045512.2

* Delamater PL, Street EJ, Leslie TF, Yang Y, Jacobsen KH.
Complexity of the Basic Reproduction Number (R0).
Emerg Infect Dis. 2019;25(1):1-4.
https://dx.doi.org/10.3201/eid2501.171901
https://wwwnc.cdc.gov/eid/article/25/1/17-1901_article

### How to Cite this Package
```R
> citation("covid19.analytics")

To cite package ‘covid19.analytics’ in publications use:

  Marcelo Ponce (2020). covid19.analytics: Load and Analyze Live Data
  from the CoViD-19 Pandemic. R package version 1.1.
  https://CRAN.R-project.org/package=covid19.analytics

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {covid19.analytics: Load and Analyze Live Data from the CoViD-19 Pandemic},
    author = {Marcelo Ponce},
    year = {2020},
    note = {R package version 1.1},
    url = {https://CRAN.R-project.org/package=covid19.analytics},
  }
```


## Further Resources
<p aling="center">
 <img src="https://phil.cdc.gov//PHIL_Images/2871/2871_lores.jpg" width="75%">
 <br>
 <a href="https://phil.cdc.gov/Details.aspx?pid=2871" target="_blank">Source-Credit: CDC/ Alissa Eckert, MS; Dan Higgins, MAMS</a>
</p>


### More R Resources
* The R Epidemics Consortium (RECON): https://www.repidemicsconsortium.org/
* SIR model: https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov
* EpiModel: https://rviews.rstudio.com/2020/03/19/simulating-covid-19-interventions-with-r/
* https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

<!--
### Explanatory Videos
* Exponential Growth and Epidemics, by 3Blue1Brown:  https://www.youtube.com/watch?v=Kas0tIxDvrg
* Simulating an epidemic, by 3Blue1Brown:  https://www.youtube.com/watch?v=gxAaO2rsdIs
-->

### Dashboards
* https://www.covidgraph.com
<!-- * https://ici.radio-canada.ca/info/2020/coronavirus-covid-19-pandemie-cas-carte-maladie-symptomes-propagation/ -->
* https://ici.radio-canada.ca/info/2020/coronavirus-covid-19-pandemie-cas-carte-maladie-symptomes-propagation/index-en.html
* https://resources-covid19canada.hub.arcgis.com/
* https://aatishb.com/covidtrends/
* https://nextstrain.org/ncov
* http://gabgoh.github.io/COVID/index.html
* https://coronavirus.jhu.edu/map.html
* https://coronavirus.jhu.edu/data/new-cases
<!-- * https://schulich.yorku.ca/covid-19-dynamics/ -->

