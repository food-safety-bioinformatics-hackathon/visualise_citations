install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("bibliometrix")
install.packages("easyPubMed")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("readr")
library("stringr")
library(purrr)
library(bibliometrix)
library(easyPubMed)




####Easypubmed

dami_query <- "7789705[PMID]"
dami_on_pubmed <- get_pubmed_ids(dami_query)
dami_abstracts_xml <- fetch_pubmed_data(dami_on_pubmed)
dami_abstracts_list <- articles_to_list(dami_abstracts_xml)
article_to_df(pubmedArticle = dami_abstracts_list[[1]], autofill = FALSE)
article_to_df(pubmedArticle = dami_abstracts_list[[2]], autofill = TRUE, max_chars = 300)[1:2,]




text <- readLines("D:/Dropbox/Woolhouse_Lab/docs/Meetings/201900602_QuadramHackathon/R/data/Webofscience/ARGannot_2014_all275.txt")

####parse species names#######
speciesnames <- read_rds("D:/Dropbox/Woolhouse_Lab/docs/Meetings/201900602_QuadramHackathon/R/taxid/bacteria.rds")
#speciesnames <- read.csv("D:/Dropbox/Woolhouse_Lab/docs/Meetings/201900602_QuadramHackathon/R/taxid")

tbl<-str_split(speciesnames$spname, " ", n = 3) %>% 
map(., function(x) paste(x[1],x[2], sep = " ")) %>% 
  unlist() %>% 
  unique()

tbl2<-str_split(tbl, " ") %>% 
  unlist() %>% 
  unique()

tbl2 <- tolower(tbl2)

################################
  
#####
#WORD CLOUD code

docs <- Corpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))


docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("university", "reistance", "doi", "department", "genes", "usa", "genome", "information", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming


##CODE TO BUILD WORD FREQUENCY TABLE

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d <- subset(d, d$freq > 5)
#d <- merge(d$word, speciesnames)
d <- subset(d, d$word %in%  tbl2)
#write.csv(d, file="PMID24145532_ArgANNOT2014.csv")

#Plots word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=300, random.order=FALSE, rot.per=0, fixed.asp=TRUE, 
          colors=brewer.pal(8, "Dark2"))
