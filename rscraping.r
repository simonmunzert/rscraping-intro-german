### ----------------------------------------------------------
### Automated Data Collection with R
### Simon Munzert
### Kurzeinführung zu Web Scraping mit R
### ----------------------------------------------------------

# wichtige Pakete laden
library(RCurl)
library(XML)
library(stringr)
library(plyr)
library(ggplot2)



### Datensatz zu Psychologen erstellen -----------------------

# Wikipedia-Links sammeln
psychwiki <- htmlParse("http://en.wikipedia.org/wiki/List_of_psychologists")

psychlinks <- xpathSApply(psychwiki, "//li/a[1]", xmlGetAttr, "href")
psychlinks <- psychlinks[79:460]
psychlinks <- psychlinks[str_detect(psychlinks, "/wiki/")]
psychurls <- str_c("http://en.wikipedia.org", psychlinks)

# Webseiten herunterladen
# Funktion dlPages
dlPages <- function(pageurl, folder ,handle) {
	dir.create(folder, showWarnings = FALSE)
	page_name <- str_c(basename(pageurl), ".html")
	if (!file.exists(str_c(folder, "/", page_name))) {
		content <- try(getURL(pageurl, curl = handle))
		write(content, str_c(folder, "/", page_name))
		Sys.sleep(1)
	}
}
# Download
handle <- getCurlHandle()
l_ply(as.list(psychurls), dlPages, folder = "psychologists", handle = handle)
length(list.files("psychologists"))

# Files importieren
pages_parsed <- lapply(str_c("psychologists/", list.files("psychologists")), htmlParse)

# Informationen extrahieren
name <- sapply(pages_parsed, xpathSApply, '//*[(@id = "firstHeading")]//span', xmlValue) # Name
name[1:10]

infos <- file.info(str_c("psychologists/", list.files("psychologists")))
size <- infos$size # Dateigröße (~ Bedeutung?)
size[1:10]

categories <- sapply(pages_parsed, xpathSApply, '//*[(@id = "mw-normal-catlinks")]//a', xmlValue) # Kategorien (ganz unten)
categories[1:3]

birthslist <- lapply(categories, str_extract, "\\d{4} births")
birthslist <- sapply(birthslist, na.exclude)
birthslist <- sapply(birthslist, str_extract, "\\d{4} births")
births <- as.numeric(unlist(ifelse(sapply(birthslist, str_detect, "\\d{4} births") != T, NA, sapply(birthslist, str_extract, "\\d{4}")))) # Geburtsjahr

deathslist <- lapply(categories, str_extract, "\\d{4} deaths")
deathslist <- sapply(deathslist, na.exclude)
deathslist <- sapply(deathslist, str_extract, "\\d{4} deaths")
deaths <- as.numeric(unlist(ifelse(sapply(deathslist, str_detect, "\\d{4} deaths") != T, NA, sapply(deathslist, str_extract, "\\d{4}")))) # Todesjahr

# Datensatz erstellen
psychdf <- data.frame(name = name, size = size, birth = births, death = deaths, stringsAsFactors = FALSE)
psychdf$age <- ifelse(is.na(psychdf$death), 2014 - psychdf$birth, psychdf$death - psychdf$birth)
psychdf$wikirank <- order(psychdf$size, decreasing = TRUE)

# Wer hat den besten "Wiki-Rang"?
psychdf[order(psychdf$size, decreasing = TRUE),][1:10,]
psychdf[name=="Ulf-Dietrich Reips",]

# Wie alt werden Psychologen?
gg <- ggplot(psychdf[!is.na(psychdf$death),], aes(age))
gg + geom_histogram(aes(y = ..density..)) + stat_density(geom="line",color="red", size=2)


### Namenskarten mit Telefonbuchdaten erstellen ------------------
# Ziel: Erstellung von Namenskarten auf Basis von Telefonbuchdaten
# Quelle: http://www.dastelefonbuch.de/

# vorbereitete Funktionen laden
source("rfunctions/scraping-function.r")
source("rfunctions/extraction-function.r")
source("rfunctions/mapping-function.r")

# Daten beziehen und grafisch aufbereiten
# mithilfe eigens geschriebener Funktionen
namesScrape("Reips", update.file = FALSE)
phonebook <- namesParse("Reips")
namesPlot(phonebook, "Reips", show.map = TRUE, save.pdf = TRUE, minsize.cities = 450000, add.cities = "", print.names = FALSE)



### API-Nutzung: Wettervorhersage mit R erhalten ----------------
# Nutzung der Yahoo-Weather-API
# https://developer.yahoo.com/weather/

load("yahooid.RData") # API-ID laden (Registrierung bei Yahoo nötig!)

getWeather <- function(place = "New York", ask = "current", temp = "c") {
	if (!ask %in% c("current","forecast")) {
		stop("Wrong ask parameter. Choose either 'current' or 'forecast'.")
	}
	if (!temp %in%  c("c", "f")) {
		stop("Wrong temp parameter. Choose either 'c' for Celsius or 'f' for Fahrenheit.")
	}	
## get woeid
	base_url <- "http://where.yahooapis.com/v1/places.q('%s')"
	woeid_url <- sprintf(base_url, URLencode(place))
	parsed_woeid <- xmlParse((getForm(woeid_url, appid = yahooid)))
	woeid <- xpathSApply(parsed_woeid, "//*[local-name()='locality1']", xmlAttrs)[2,]
## get weather feed
	feed_url <- "http://weather.yahooapis.com/forecastrss"
	parsed_feed <- xmlParse(getForm(feed_url, .params = list(w = woeid, u = temp)))
## get current conditions
	if (ask == "current") {
		xpath <- "//yweather:location|//yweather:condition"
		conds <- data.frame(t(unlist(xpathSApply(parsed_feed, xpath, xmlAttrs))))
		message(sprintf("The weather in %s, %s, %s is %s. Current temperature is %s degrees %s.", conds$city, conds$region, conds$country, tolower(conds$text), conds$temp, toupper(temp)))
	}
## get forecast	
	if (ask == "forecast") {
		location <- data.frame(t(xpathSApply(parsed_feed, "//yweather:location", xmlAttrs)))
		forecasts <- data.frame(t(xpathSApply(parsed_feed, "//yweather:forecast", xmlAttrs)))
		message(sprintf("Weather forecast for %s, %s, %s:", location$city, location$region, location$country))
		return(forecasts)
	}
}

getWeather("Konstanz")
getWeather("Konstanz", ask = "forecast")




### Crash-Kurs Web Scraping mit R -------------------------------

# wichtige Pakete
library(RCurl) # R als HTTP-Client
library(XML) # R als HTML/XML-Parser
library(stringr) # R zur String-Manipulation und Verwendung regulärer Ausdrücke

# nützliche Tools
http://selectorgadget.com/ # XPath-Ausdrücke per Point-and-Click konstruieren lassen


# wichtige Funktionen
websiteInhalt <- getURL("Ziel-URL")

geparstesObjekt <- htmlParse("Ziel-URL")

extrahierteInfo <- xpathSApply(geparstesObjekt, 'XPath-Ausdruck', xmlValue)

bereinigteInfo <- str_extract(extrahierteInfo, "regulärer Ausdruck zum Aufräumen")

htmlTabellen <- readHTMLTable("Ziel-URL")

websiteLinks <- getHTMLLinks("Ziel-URL")




### Übung -------------------------------------------------------

### 1. Aktuelle Schlagzeilen von Spiegel Online scrapen
	# a) Besuchen Sie folgende Webseite:	http://www.spiegel.de/schlagzeilen/
	# b) Parsen Sie die Webseite mit htmlParse(). 
	# c) Verwenden Sie SelectorGadget, um einen XPath-Ausdruck zu finden, mit dem sich die Schlagzeilen von der Seite scrapen lassen!
	# d) Wenden Sie den Ausdruckn mit R und der xpathSApply()-Funktion an, um die Schlagzeilen aus dem geparsten Dokument zu extrahieren!
	


### 2. Emailadressen scrapen
	# a) Besuchen Sie folgende Webseite: http://www.psychlogie.uni-konstanz.de/forschung/ags/
	# b) Parsen Sie die Webseite mit htmlParse().
	# c) Verwenden Sie SelectorGadget, um einen XPath-Ausdruck zu finden, mit dem sich die Emailadressen von der Seite scrapen lassen!
	# d) Verwenden Sie SelectorGadget, um einen XPath-Ausdruck zu finden, um die Namen der Arbeitsgruppeninhaber von der Seite zu scrapen!
	# e) Wenden Sie die Ausdrücke mit R und der xpathSApply()-Funktion an, um die Informationen aus dem geparsten Dokument zu extrahieren!

# Parsen
parsed <- htmlParse(...)

# Emaildressen extrahieren
email <- xpathSApply(parsed, 'XPath-Ausdruck', xmlValue)

# Namen der Arbeitsgruppeninhaber extrahieren
name <- xpathSApply(parsed, 'XPath-Ausdruck', xmlValue) 



### 3. Vornamen von Babygalerien sammeln
	# a) Besuchen Sie folgende Webseite: http://www.babygalerie24.de/konstanz
	# b) Versuchen Sie mit den kennengelernten Mitteln zunächst die Namen der weiblichen, dann die Namen der männlichen Babies von der Webseite zu scrapen, die im Juni 2014 geboren wurden!


