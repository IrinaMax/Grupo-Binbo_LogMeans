-----------
library(readr)
library(tm)
library(SnowballC)
library(skmeans)

products <- read_csv('producto_tabla.csv')
products
names(products)<- c('pid', 'product_name')
names(products)
## function to split product name and cut the last to section we do not need
strsplitextract_shortname <- function(product_f) {
  # Split the name
  tokens <- strsplit(product_f, " ")[[1]]
  
  # Delete ID
  tokens <- head(tokens, length(tokens) - 1)
  
  # Delete Brands (name till the last token with digit)
  digit_indeces <- grep("[0-9]", tokens)
  
  # Product names without digits
  digit_index <- ifelse(length(digit_indeces) == 0, 1,
                        max(digit_indeces))
  paste(tokens[1:digit_index], collapse = " ")
}

# Delete product with no name
products <- products[2:nrow(products),]
products

products$product_shortname <- unlist(lapply(products$product_name, extract_shortname))

# Short Names Preprocessing
CorpusShort <- Corpus(VectorSource(products$product_shortname))
CorpusShort <- tm_map(CorpusShort, tolower)
CorpusShort <- tm_map(CorpusShort, PlainTextDocument)

Corpus(VectorSource(products$product_shortname))
# Remove Punctuation
CorpusShort <- tm_map(CorpusShort, removePunctuation)
CorpusShort
# Remove Stopwords
CorpusShort <- tm_map(CorpusShort, removeWords, stopwords("es"))

# Stemming
CorpusShort <- tm_map(CorpusShort, stemDocument, language="es")

# Create DTM
CorpusShort <- Corpus(VectorSource(CorpusShort))
dtmShort <- DocumentTermMatrix(CorpusShort)
dimShort
# Delete Sparse Terms (all the words now)
sparseShort <- removeSparseTerms(dtmShort, 0.9999)
ShortWords <- as.data.frame(as.matrix(sparseShort))

# Create valid names
colnames(ShortWords) <- make.names(colnames(ShortWords))

# Spherical k-means for product clustering (30 clusters at the moment)
set.seed(123)
mod <- skmeans(as.matrix(ShortWords), 15, method = "genetic")
summary(mod)
mod
products$cluster <- mod$cluster
plot(products$pid, mod$cluster)
plot(products$pid, mod$size)

# Fig 03
with(products$cluster, pairs(products$cluster, col=c(1:3)[mod$cluster])) 

# Example for one of the clusters
#write_csv(products[mod$cluster == 1,], 'my_GB_cluster.csv')
#Sade as a file
write_csv(products[mod$cluster == 1,], 'my_GB_cluster.csv')



library(plyr)
n_products <- arrange(products, products$cluster, pid )
n_products$product_name<- NULL
#Save as a file
write_csv(n_products, 'my_GB_clusters.csv')
nf <- read.csv("my_GB_clusters.csv", header = T, stringsAsFactors = F)
nf
n_products$product_shortname <- factor(n_products$product_shortname)
library(cluster)
library(fpc)
cl_prod <- kmeans(n_products$pid, centers = 10)
cl_prod

plotcluster(n_products$pid, cl_prod$cluster)
plotcluster(cl_prod$cluster, cl_prod$size)
#Save as a file
write_csv(n_products, 'my_GB_clusters.csv')


# More complex
clusplot(product, cl_prod$cluster, color=TRUE, shade=F,
         labels=2, lines=0)

clusplot(cl_prod, clus$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
# Fig 03
with(n_products, pairs(n_products, col=c(1:3)[cl_prod$cluster])) 

library(Amelia)
missmap(products, main= "Missing values vs observed")

# Example for one of the clusters
write_csv(products[mod$cluster == 1,], 'my_GB_cluster.csv')
