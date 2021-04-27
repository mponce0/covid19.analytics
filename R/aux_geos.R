# Geographical locations utilities
# covi19.analytics
# M.Ponce

geographicalRegions <- function(cont=NULL) {
#'
#' function to define continents and its constituent countries
#'
#' @param  cont  optional argumetn, to specify a particular continent; if no argument is given then it returns all the continents and countries for each
#'
#' @return  list with the composition of continents
#'
#' @export
#'

	processCtryLst <- function(lst) {

		return(unlist(strsplit(lst,"\n",fixed=TRUE))[-1])
	}


        #### Define some regions to aid the user choice
        #SouthAmerica <- c(read.csv("southamerica.csv"))[[1]]
                #c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", V"enezuela", "French Guiana", "Falkland Islands", "Trinidad and Tobago")

        #NorthAmerica <- c("Canada","US","Mexico")

        #CentralAmerica <- c("Panama", "Costa Rica", "Nicaragua", "Honduras", "El Salvador", "Guatemala", "Belize", "Cuba")

        #America <- c(SouthAmerica,CentralAmerica,NorthAmerica)

        #Europe <- c(read.csv("europe.csv", header=FALSE))[[1]]


        #Asia <- c(read.csv("asia.csv", header=FALSE))[[1]]

        #Africa <- c(read.csv("africa.csv", header=FALSE))[[1]]

        #Oceania <- c(read.csv("oceania.csv", header=FALSE))[[1]]



	### SOURCE:	https://www.worldatlas.com/cntycont.htm

	SouthAmerica <- processCtryLst("
Argentina
Bolivia
Brazil
Chile
Colombia
Ecuador
Guyana
Paraguay
Peru
Suriname
Uruguay
Venezuela")

	NorthAmerica <- processCtryLst("
Canada
US
Mexico")

	CentralAmerica <- processCtryLst("
Antigua and Barbuda
Bahamas
Barbados
Belize
Costa Rica
Cuba
Dominica
Dominican Republic
El Salvador
Grenada
Guatemala
Haiti
Honduras
Jamaica
Nicaragua
Panama
Saint Kitts and Nevis
Saint Lucia
Saint Vincent and the Grenadines
Trinidad and Tobago")

	America <- c(SouthAmerica,CentralAmerica,NorthAmerica)


	Europe <- processCtryLst("
Albania
Andorra
Armenia
Austria
Azerbaijan
Belarus
Belgium
Bosnia and Herzegovina
Bulgaria
Croatia
Cyprus
Czech Republic
Denmark
Estonia
Finland
France
Georgia
Germany
Greece
Hungary
Iceland
Ireland
Italy
Latvia
Liechtenstein
Lithuania
Luxembourg
Macedonia
Malta
Moldova
Monaco
Montenegro
Netherlands
Norway
Poland
Portugal
Romania
San Marino
Serbia
Slovakia
Slovenia
Spain
Sweden
Switzerland
Ukraine
United Kingdom
Vatican City")


	Africa <- processCtryLst("
Algeria
Angola
Benin
Botswana
Burkina Faso
Burundi
Cameroon
Cabo Verde
Central African Republic
Chad
Comoros
Congo
Democratic Republic of Congo
Djibouti
Egypt
Equatorial Guinea
Eritrea
Ethiopia
Gabon
Gambia
Ghana
Guinea
Guinea-Bissau
Ivory Coast
Kenya
Lesotho
Liberia
Libya
Madagascar
Malawi
Mali
Mauritania
Mauritius
Morocco
Mozambique
Namibia
Niger
Nigeria
Rwanda
Sao Tome and Principe
Senegal
Seychelles
Sierra Leone
Somalia
South Africa
South Sudan
Sudan
Swaziland
Tanzania
Togo
Tunisia
Uganda
Zambia
Zimbabwe")


	Asia <- processCtryLst("
Afghanistan
Bahrain
Bangladesh
Bhutan
Brunei
Burma (Myanmar)
Cambodia
China
East Timor
India
Indonesia
Iran
Iraq
Israel
Japan
Jordan
Kazakhstan
North Korea
South Korea
Kuwait
Kyrgyzstan
Laos
Lebanon
Malaysia
Maldives
Mongolia
Nepal
Oman
Pakistan
Philippines
Qatar
Russian Federation
Saudi Arabia
Singapore
Sri Lanka
Syria
Tajikistan
Thailand
Turkey
Turkmenistan
United Arab Emirates
Uzbekistan
Vietnam
Yemen")


	Oceania <- processCtryLst("
Australia
Fiji
Kiribati
Marshall Islands
Micronesia
Nauru
New Zealand
Palau
Papua New Guinea
Samoa
Solomon Islands
Tonga
Tuvalu
Vanuatu")


	#########


        regions <- list(SOUTHAMERICA=SouthAmerica, NORTHAMERICA=NorthAmerica, CENTRALAMERICA=CentralAmerica, AMERICA=America,
			AFRICA=Africa, EUROPE=Europe, ASIA=Asia, OCEANIA=Oceania)

	if (is.null(cont)) {
        	return(regions)
	} else {
		kcont <- gsub(" ","",toupper(cont))
		if (kcont %in% names(regions)) {
			return(regions[kcont])
		} else {
			warning("Continent ",cont," not found!",'\n',
				"Possible options are: ", paste(names(regions),collapse=' ') )
		}
	}
}
