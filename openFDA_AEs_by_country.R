###### openFDA_AEs_by_country.R
###### Updated by Hannah K. 5/21/17
###### Mine OpenFDA database for information about adverse events
###### Check if adverse event reporting is different across different countries
###### Deep dive into most commonly reported AEs in countries reporting the most AEs overall

#install.packages(c("httr", "jsonlite", "lubridate", "plyr"))
library("httr")
library("jsonlite")
library("plyr")
library("ggplot2")

options(stringsAsFactors = FALSE)

#get sample sizes to look at
num_countries <- "25"
num_aes <- "10"

#get a list of the top countries, to sample size, in the data with the highest levels of AE reporting in 2016
countries_url <- paste("https://api.fda.gov/drug/event.json?api_key=J2mscqLvHW3cz0tiMOuB5HD4yO8I9gaeu3MhRlcc&search=receivedate:[20160101+TO+20161231]&count=occurcountry.exact&limit=", num_countries,sep="")
countries.raw <- GET(countries_url)

#check that query was successful
if(countries.raw$status_code != 200) {stop("Unsuccessful query")}

#convert raw results to character fields and isolate results
countries.char <- fromJSON(rawToChar(countries.raw$content))
countries.results <- as.data.frame(countries.char$results)

#qc to ensure correct sample size of countries was captured
if(length(countries.results$term) != as.integer(num_countries)) {stop("Code error - check country sample")}

#get a list of the most common AEs in 2016, to sample size, in the data
aes_url <- paste("https://api.fda.gov/drug/event.json?api_key=J2mscqLvHW3cz0tiMOuB5HD4yO8I9gaeu3MhRlcc&search=receivedate:[20160101+TO+20161231]&count=patient.reaction.reactionmeddrapt.exact&limit=", num_aes,sep="")
aes.raw <- GET(aes_url)

#check that query was successful
if(aes.raw$status_code != 200) {stop("Unsuccessful query")}

#convert raw results to character fields and isolate results
aes.char <- fromJSON(rawToChar(aes.raw$content))
aes.results <- as.data.frame(aes.char$results)

#qc to ensure correct sample size of AEs was captured
if(length(aes.results$term) != as.integer(num_aes)) {stop("Code error - check event sample")}

#create full universe of AEs to collect
countries <- as.data.frame(unique(countries.results$term))
events <- as.data.frame(unique(aes.results$term))
countries$id <- 1
events$id <- 1
countries.AEs.all <- merge(countries, events, by="id")
names(countries.AEs.all)[names(countries.AEs.all) == "unique(countries.results$term)"] <- "country"
names(countries.AEs.all)[names(countries.AEs.all) == "unique(aes.results$term)"] <- "term"

#qc that row count is correct
if(nrow(countries.AEs.all) != as.integer(num_countries)*as.integer(num_aes)) {stop("Code error - check full universe creation")}

#create new empty dataframe to store AEs from each country from queries
countries.AEs <- data.frame()

#for each of the top countries, get the top 1000 reported AEs in 2016
for (i in 1:length(countries.results$term))
{
  #query the openFDA database for the top 1000 reported AEs in 2016
  url <- paste("https://api.fda.gov/drug/event.json?api_key=J2mscqLvHW3cz0tiMOuB5HD4yO8I9gaeu3MhRlcc&search=receivedate:[20160101+TO+20161231]+AND+occurcountry:", 
               countries.results$term[i], "&count=patient.reaction.reactionmeddrapt.exact&limit=1000",
               sep="")
  aes.raw <- GET(url)
  
  #check that query was successful
  if (aes.raw$status_code != 200) {stop("Unsuccessful query")}
  
  #convert raw results to characters
  aes.char <- fromJSON(rawToChar(aes.raw$content))
  aes.tmp <- aes.char$results
  
  #add columns with the country queried and the total AEs for that country
  aes.tmp$country <- countries.results$term[i]
  aes.tmp$total <- countries.results$count[i]
  
  #save the results to the dataframe
  rbind(countries.AEs, aes.tmp) -> countries.AEs
}

#filter for AEs of interest (in top sample size)
countries.AEs.top <- merge(x=countries.AEs, y=aes.results, by="term")

#merge to full desired universe to check that all AEs of interest were collected for all countries and if not, fill in gaps
countries.AEs.f <- merge(x=countries.AEs.all, y=countries.AEs.top, by=c("country", "term"), all.x=TRUE)

#if there are AEs of interest not queried in top 1000 from the country, query specifically to fill those gaps
if (nrow(countries.AEs.top) < as.integer(num_countries)*as.integer(num_aes)) {
  for(i in 1:nrow(countries.AEs.f)) {
    if (is.na(countries.AEs.f$count.x[i])) {
      #make version of missing term in format required for query
      queryterm <- gsub(" ", "+", countries.AEs.f$term[i])
      
      #query missing country/event combination
      url <- paste("https://api.fda.gov/drug/event.json?api_key=J2mscqLvHW3cz0tiMOuB5HD4yO8I9gaeu3MhRlcc&search=receivedate:[20160101+TO+20161231]+AND+occurcountry:", 
                   countries.AEs.f$country[i], "+AND+patient.reaction.reactionmeddrapt:\"", queryterm,
                   "\"&count=patient.reaction.reactionmeddrapt.exact&limit=1000",
                   sep="")
      data.raw <- GET(url)
      
      #check that query was successful
      if (data.raw$status_code != 200) {stop("Unsuccessful query")}
      
      #convert raw results to characters
      data.char <- fromJSON(rawToChar(data.raw$content))
      data.tmp <- data.char$results
      
      #fill in missing data with new query results
      countries.AEs.f$count.x[i] <- data.tmp[which(data.tmp$term == countries.AEs.f$term[i]), "count"]
      
      #fill in total row for country
      countries.AEs.f$total[i] <- countries.results[which(countries.results$term==countries.AEs.f$country[i]), "count"]
    }
  }
}

#make AEs proportional to total AEs to account for differing population sizes and overall reporting rates
countries.AEs.f$index_aes <- countries.AEs.f$count.x/countries.AEs.f$total

#qc to make sure values are between 0 and 1
if(min(countries.AEs.f$index_aes) < 0 | min(countries.AEs.f$index_aes) > 1) {stop("Code error - min AE index out of bounds")}
if(max(countries.AEs.f$index_aes) <0 | max(countries.AEs.f$index_aes) > 1) {stop("Code error - max AE index out of bounds")}

#calculate basic stats by AE
countries.AEs.stats <- ddply(countries.AEs.f,~term,summarise,min=min(index_aes), max=max(index_aes), mean=mean(index_aes),sd=sd(index_aes), total_reports=sum(count.x))
countries.AEs.stats$range <- countries.AEs.stats$max - countries.AEs.stats$min

#print result stats sorted by biggest range across countries for outliers
countries.AEs.stats[order(-countries.AEs.stats$range), ]

#label countries with full names vs abbreviations
#get mapping of abbreviation to name
abbr.raw <- GET("http://data.okfn.org/data/core/country-list/r/data.json")

#check that list grab was successful
abbr.raw$status_code

#convert raw results to character fields and isolate results
abbr.char <- as.data.frame(fromJSON(rawToChar(abbr.raw$content)))

#merge full names with dataset
countries.AEs.cln <- merge(x=countries.AEs.f, y=abbr.char, by.x="country", by.y="Code", all.x=TRUE)

#update some names for clarity
countries.AEs.cln$Name <- gsub("Taiwan, Province of China", "Taiwan", countries.AEs.cln$Name)
countries.AEs.cln$Name  <- gsub("Korea, Republic of", "South Korea", countries.AEs.cln$Name)

#get the AE with highest range across countries
ae_highest_range <- countries.AEs.stats[which(countries.AEs.stats$range == max(countries.AEs.stats$range)), "term"]

#get the data for the AE with highest range across countries
countries.highest.range.ae <- subset(countries.AEs.cln, term==ae_highest_range)

#qc row count
if(nrow(countries.highest.range.ae) != as.integer(num_countries)) {stop("Code error - check subset for AE with highest range")}

#get the countries in order of the highest ranged AE
countries.order <- countries.highest.range.ae[order(-countries.highest.range.ae$index_aes), "Name"]

#sort data by highest range AE for plotting
countries.AEs.cln$Name <- factor(countries.AEs.cln$Name, levels=countries.order)
countries.AEs.cln.srt <- countries.AEs.cln[with(countries.AEs.cln,order(Name, term)),] 

#plot AEs by country in a stacked bar to visualize differences
ggplot(countries.AEs.cln.srt, aes(x=Name, y=index_aes, fill=term)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(size=5), legend.text=element_text(size=5), legend.title=element_text(size=7))+
  labs(x="Country", y="Events Proportional to Total", fill="Event")
  
#make heat map of proportional AEs for the AE with the highest range
#get the baseline world map as background
world <- map_data("world")

#name updates for merging with AE data
world$region <- gsub("USA", "United States", world$region)
world$region <- gsub("UK", "United Kingdom", world$region)

#merge with AE data for key regions and re-sort
world.aes <- merge(x=world, y=countries.highest.range.ae,by.x="region", by.y="Name", all.x=TRUE)
world.aes <- world.aes[order(world.aes$order), ]

#plot heatmap of AE with highest range across countries by country
ggplot(world.aes, aes(long, lat)) + 
  geom_polygon(aes(fill=index_aes, group=group),colour="black",size=0.1) +
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude', fill= paste(ae_highest_range, "Proportional to Total", sep=" ")) +
  theme_bw() + theme(legend.title=element_text(size=7))

#save data for shiny
#save(countries.AEs.cln, file="C:/Users/kstr270/Box Sync/MigratedMyDocs/01 Administrative/openfda_example.rds")

#look at quick clustering of countries by different AE rates
#transpose data from long to wide
countries.AEs.trans <- reshape(countries.AEs.cln[, c("Name", "term", "index_aes")], idvar = "Name", timevar = "term", direction = "wide")

#change country from first column to rowname for easy processing
rownames(countries.AEs.trans) <- countries.AEs.trans$Name
countries.AEs.trans.num <- countries.AEs.trans[, 2:ncol(countries.AEs.trans)]

#scale data
countries.AEs.trans.scale <- scale(countries.AEs.trans.num)

#check variable correlations
cor(countries.AEs.trans.scale)

#dizziness and fatigue, and dizziness and headache are highly correlated so remove dizziness from analysis
countries.AEs.trans.scale <- countries.AEs.trans.scale[ , !(colnames(countries.AEs.trans.scale) %in% 'index_aes.DIZZINESS')]

#look at options for number of clusters
set.seed(2017)
wss <- NULL
for (i in 2:10) wss[i] <- sum(kmeans(countries.AEs.trans.scale, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#try a 5 cluster solution
fit <- kmeans(countries.AEs.trans.scale, 5)
aggregate(countries.AEs.trans.scale,by=list(fit$cluster),FUN=mean)
countries.AEs.trans.clus5 <- data.frame(countries.AEs.trans.scale, fit$cluster)
countries.AEs.trans.clus5$region <- rownames(countries.AEs.trans.scale)

#merge with world data and resort
world.aes.clus5 <- merge(x=world, y=countries.AEs.trans.clus5, by="region", all.x=TRUE)
world.aes.clus5 <- world.aes.clus5[order(world.aes.clus5$order), ]

#plot clusters on map
ggplot(world.aes.clus5, aes(long, lat)) + 
  geom_polygon(aes(fill=fit.cluster, group=group),colour="black",size=0.1) +
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_gradient(breaks=seq(0, 5, 1), low = "#8b0000", high = "#FFFF00") +
  labs(x='Longitude', y='Latitude', fill= "Cluster") +
  theme_bw() + theme(legend.title=element_text(size=7))

