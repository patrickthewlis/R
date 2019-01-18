#######################
Clear the Console
#######################

cat("\014")

###############################
Specify Path in normalizePath
###############################

# Allowing us to run our script
# Defines the library trees where are packages are looked for
# All of the packages are stored in one folder

.libPaths("S:/R/win-library/3.4")


#######################
Set Working Directory
#######################

setwd("~/Documents/R/Tutorials/Text_Mining/Digital_Diplomacy")


################
Libraries
################ 

# Install at once using the 'c' array function

lapply(c("XML", "rvest", "dplyr", "tidytext", "tm", "stringr", "ggplot2", "ggthemes"),
       library, character.only = TRUE)


################
Loading Texts
################

txt <- DirSource(system.file("texts", "txt", package = "tm"))
texts <- file.path("~", "Documents", "External_Projects", "Digital_Diplomacy", "Twitter_Tracking", "Text_Files", "Israel_MFA_Yearly_Track")    # Construct path to file
texts
dir(texts)  # Check that the corpus has loaded

####################
Load Texts into R
####################

# Using the "tm" package
# Structure for managing documents in tm = CORPUS
# Command: "VCorpus"

docs <- VCorpus(DirSource(texts, encoding = "UTF-8"))     # Identify the source of the directory
# i.e. document within file directory
summary(docs)                         # Summarise the corpus




######################################################################
#                           PRE-PROCESSING

#     FOR ALL PRE-PROCESSING, NEED TO USE "content_transformer()"
######################################################################


# YEAR: 2015

# Inspect the Specific Document
inspect(docs[8])

# Read the documents into the terminal (1-7 for the years 2012 to 2018)
writeLines(as.character(docs[8])) # 2015


#####################
Removal
#####################


# Remove Punctuation
docs <- tm_map(docs, removePunctuation)

# Remove URLs
# define function to remove URL
# Substitute all lines starting with "http" with blankspace
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

# argument = function (x)
# Therefore, when we are working with "docs", x = docs
# Similar to when f(x)=x2, when f(5)=25
# Therefore, wherever x is in the function, it is replaced by "docs"
# Thus, gsub("http[^[:space:]]*", "", x) = gsub("http[^[:space:]]*", "", docs)
# Therefore removeURL(docs) is equal to gsub("http[^[:space:]]*", "", docs)
# docs <- tm_map(docs, (gsub"http[^[:space:]]*", "", docs)))


# Remove Other Numbers/Punctuation
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeNumPunct))



# define function
# Keep if alpha (i.e. letters) and space
# Everything else: Remove

# Remove Unnecessary Whitespace
docs <- tm_map(docs, stripWhitespace)

# Remove Punctutation (with the exception of the hashtag)
#removeMostPunctuation<-
#  function (x, preserve_intra_word_dashes = FALSE) 
#  {
#    rmpunct <- function(x) {
#      x <- gsub("#", "\002", x)
#      x <- gsub("[[:punct:]]+", "", x)
#      gsub("\002", "#", x, fixed = TRUE)
#    }
#    if (preserve_intra_word_dashes) { 
#      x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
#      x <- rmpunct(x)
#      gsub("\001", "-", x, fixed = TRUE)
#    } else {
#      rmpunct(x)
#    }
#  }

docs <- tm_map(docs, content_transformer(removeMostPunctuation),
               preserve_intra_word_dashes = TRUE)

# Remove Capitalization (Convert all words to lowercase)
docs <- tm_map(docs, content_transformer(tolower))

# Stemming (Highlight words that have a common root)
docs <- tm_map(docs, content_transformer(gsub), pattern = "irans", replacement = "iran")
docs <- tm_map(docs, content_transformer(gsub), pattern = "attacks", replacement = "attack")
docs <- tm_map(docs, content_transformer(gsub), pattern = "terrorist", replacement = "terrorism")

# Remove Stopwords (Pre-programmed and Custom stopwords)
myStopwords <- c(stopwords('english'), "rt", "mon", "tue", "wed", "thu",
                 "fri", "sat", "sun", "sep", "oct", "nov", "dec",
                 "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "we",
                 "th", "w", "yrs", "year", "years", "on", "my", "mr", "met", "next",
                 "it", "htt ", "ht ", "he", "i", "in", "as", "ago", "a", "also",
                 "can", "th", "way", "via", "st", "see", "one", "old", "now", "must", "never",
                 "new", "like", "around", "th", "de", "la", "day", "today", "last", "thank",
                 "israel", "pm", "netanyahu", "israeli", "israels", "fm", "win", "peres",
                 "perers", "israelipm", "presidentruvi", "osraeli", "minister", "shimon",
                 "rivlin", "prme", "benjamin", "prime", "will", "report", "president", "meet",
                 "intl", "amb", "coi", "human", "law", "people", "meets", "weekend", "visit",
                 "live", "watch", "since", "meeting", "tells", "addresses", "summary", "editorials",
                 "ambassador")

docs <- tm_map(docs, content_transformer(removeWords), myStopwords)


####################################################################
#                   Tokenizing and Searching
####################################################################

# Check that pre-processing has worked
writeLines(as.character(docs[8]))

# Create a copy of the original
docs_2015_copy <- docs[8]

# Create Term-Document Matrix
tdm <- TermDocumentMatrix(docs[8], control = list(wordLengths = c(1, Inf))) # No restrictions on wordlength
# "inf" denotes infinity

# Inspect Term-Document Matrix
tdm

# Inspect Frequency of Words (Command: 'findFreqTerms(tdm)')
(freq.terms <- findFreqTerms(tdm, lowfreq = 50)) 

# Find frequency for all words in the corpus
# Treat Term-Document Matrix as a matrix i.e. as it already is
term.freq <- rowSums(as.matrix(tdm))

# Create/return subset of word vectors which meet set conditions
# Conditions: Must appear 750 or more times in the corpus
term.freq_25 <- subset(term.freq, term.freq >=25)
term.freq_50 <- subset(term.freq, term.freq >=50)
term.freq_75 <- subset(term.freq, term.freq >=75)
term.freq_100 <- subset(term.freq, term.freq >=100)
term.freq_125 <- subset(term.freq, term.freq >=125)
term.freq_150 <- subset(term.freq, term.freq >=150)

# Store the subset in a dataframe for 'tidy' text analysis
df_25 <- data.frame(term = names(term.freq_25), freq = term.freq_25)
df_50 <- data.frame(term = names(term.freq_50), freq = term.freq_50)
df_75 <- data.frame(term = names(term.freq_75), freq = term.freq_75)
df_100 <- data.frame(term = names(term.freq_100), freq = term.freq_100)
df_125 <- data.frame(term = names(term.freq_125), freq = term.freq_125)
df_150 <- data.frame(term = names(term.freq_150), freq = term.freq_150)






####################################################################
#                   Visualising Word Distributions
####################################################################

# Package: ggplot2
# Bar Graph: Frequency Distribution
# Count Model
# Visual Preferences: Fill in Red with Black Border
# Count intervals are set to 50 words i.e. 'scale_y_continuous

# UNORDERED FREQUENCY PLOTS

# DataFrame 1: Frequency of >=25
ggplot(df_25, aes(x = df_25$term, y = df_25$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  scale_y_continuous(breaks = round(seq(min(df_25$freq), max(df_25$freq), by = 5)))

# DataFrame 2: Frequency of >=50
ggplot(df_50, aes(x = df_50$term, y = df_50$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  scale_y_continuous(breaks = round(seq(min(df_50$freq), max(df_50$freq), by = 5)))

# DataFrame 3: Frequency of >=75
ggplot(df_75, aes(x = df_75$term, y = df_75$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  scale_y_continuous(breaks = round(seq(min(df_75$freq), max(df_75$freq), by = 5)))

# DataFrame 4: Frequency of >=100
ggplot(df_100, aes(x = df_100$term, y = df_100$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  scale_y_continuous(breaks = round(seq(min(df_100$freq), max(df_100$freq), by = 10))) +
  ggtitle("Word Frequency Plot: Words Cited More than 100 Times") +
  theme_economist() 

# DataFrame 5: Frequency of >=125
ggplot(df_125, aes(x = df_125$term, y = df_125$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  scale_y_continuous(breaks = round(seq(min(df_125$freq), max(df_125$freq), by = 25)))





# ORDERED FREQUENCY PLOTS

# Refresh Plot Pane
plot.new()

# DataFrame 1: Frequency of >=25
ggplot(df_25, aes(x = reorder(df_25$term, -df_25$freq), y = df_25$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  scale_y_continuous(breaks = round(seq(min(df_25$freq), max(df_25$freq), by = 5)))

# DataFrame 2: Frequency of >=50
ggplot(df_50, aes(x = reorder(df_50$term, -df_50$freq), y = df_50$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  ggtitle("Word Frequency Plot (2015): Words Cited More than 50 Times") +
  theme_solarized() 

# DataFrame 3: Frequency of >=75
ggplot(df_75, aes(x = reorder(df_75$term, -df_75$freq), y = df_75$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  ggtitle("Word Frequency Plot (2015): Words Cited More than 75 Times") +
  theme_solarized() 

# DataFrame 4: Frequency of >=100
ggplot(df_100, aes(x = reorder(df_100$term, -df_100$freq), y = df_100$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  ggtitle("Word Frequency Plot (2015): Words Cited More than 100 Times") +
  theme_solarized() 

# DataFrame 5: Frequency of >=125
ggplot(df_125, aes(x = reorder(df_125$term, -df_125$freq), y = df_125$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  ggtitle("Word Frequency Plot (2015): Words Cited More than 125 Times") +
  theme_solarized() 

# DataFrame 6: Frequency of >=150
ggplot(df_150, aes(x = reorder(df_150$term, -df_150$freq), y = df_150$freq)) + geom_bar(stat = "identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  ggtitle("Word Frequency Plot (2015): Words Cited More than 150 Times") +
  theme_solarized() 




# Alternative Method: Ordered Frequency Bar for Top 40 Words

term.freq_top40 <- subset(term.freq, term.freq >=33)
df_top40 <- data.frame(term = names(term.freq_top40), freq = term.freq_top40)
df_top40$term <- factor(df_top40$term, levels = df_top40$term[order(-df_top40$freq)])

# Rename (Include Capitalization)
levels(df_top40$term)[levels(df_top40$term)=="unga"] <- "UNGA"
levels(df_top40$term)[levels(df_top40$term)=="mfa"] <- "MFA"
levels(df_top40$term)[levels(df_top40$term)=="ltcpeterlerner"] <- "LTC Peter Lerner"
levels(df_top40$term)[levels(df_top40$term)=="israelmfa"] <- "@IsraelMFA"
levels(df_top40$term)[levels(df_top40$term)=="idf"] <- "IDF"
levels(df_top40$term)[levels(df_top40$term)=="un"] <- "UN"
levels(df_top40$term)[levels(df_top40$term)=="us"] <- "US"
levels(df_top40$term)[levels(df_top40$term)=="eu"] <- "EU"
levels(df_top40$term)[levels(df_top40$term)=="gaza"] <- "Gaza"
levels(df_top40$term)[levels(df_top40$term)=="hamas"] <- "Hamas"
levels(df_top40$term)[levels(df_top40$term)=="nepal"] <- "Nepal"
levels(df_top40$term)[levels(df_top40$term)=="danielocarmon"] <- "Daniel O'Carmon"
levels(df_top40$term)[levels(df_top40$term)=="jerusalem"] <- "Jerusalem"
levels(df_top40$term)[levels(df_top40$term)=="jews"] <- "Jews"
levels(df_top40$term)[levels(df_top40$term)=="jewish"] <- "Jewish"
levels(df_top40$term)[levels(df_top40$term)=="irandeal"] <- "Iran Deal"
levels(df_top40$term)[levels(df_top40$term)=="paris"] <- "Paris"
levels(df_top40$term)[levels(df_top40$term)=="shabbatshalom"] <- "#shabbatshalom"
levels(df_top40$term)[levels(df_top40$term)=="israelinun"] <- "@IsraelinUN"
levels(df_top40$term)[levels(df_top40$term)=="israelinnepal"] <- "@IsraelinNepal"
levels(df_top40$term)[levels(df_top40$term)=="gfca"] <- "GFCA"
levels(df_top40$term)[levels(df_top40$term)=="abbas"] <- "Abbas"
levels(df_top40$term)[levels(df_top40$term)=="iran"] <- "Iran"
levels(df_top40$term)[levels(df_top40$term)=="hebrew"] <- "Hebrew"
levels(df_top40$term)[levels(df_top40$term)=="israelis"] <- "Israelis"

ggplot(df_top40, aes(df_top40$term, df_top40$freq)) +
  geom_bar(stat="identity", fill="#FF9999", colour="black") +
  xlab("Words") + ylab("Count") + coord_flip() +
  ggtitle("Top 40 Words Cited in Israel MFA Tweets & Retweets during 2015") +
  theme_solarized() 




