# read a data set
data <- covid19.data("TS-confirmed")

# look at the structure and column names
str(data)
names(data)

# find 'Country' column
country.col <- pmatch("Country",names(data))
# slice the countries
countries <- data[,country.col]
# list of countries
print(unique(countries))
# sorted table of countries, may include multiple entries
print(sort(table(countries)))

# find 'Province' column
prov.col <- pmatch("Province",names(data))
# slice the Provinces
provinces <- data[,prov.col]

# list of provinces
print(unique(provinces))
# sorted table of provinces, may include multiple entries
print(sort(table(provinces)))

