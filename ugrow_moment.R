# read in all "moment" csv files and combine all baby data into one dataframe
filepath <- "C:/Data/uGrow/uGrow csv data/"
filelist.moment <- list.files(path=filepath, pattern="MomentData.csv$", full.names=TRUE)
data.moment <- data.frame()

for(i in 1:length(filelist.moment)){
  data <- read.csv(filelist.moment[i], header=FALSE, sep=",")
  data <- data[-1,]
  data <- cbind(BabyID = filelist.moment[i], data)
  data.moment <- rbind.fill(data.moment, data)
}

## need to manually process some lines and reload it

names(data.moment) <- c('BabyID','Date','Time','Momenttype','Note')
data.moment[is.na(data.moment)] <- ""

# combine columns of free text into one column, concatenating by space
data.moment$Note <- do.call(paste, c(data.moment[,5:7], sep = " "))
data.moment$BabyID <- as.numeric(unlist(str_extract_all(data.moment$BabyID,"\\d+")))
data.moment <- data.moment[,1:5]

## text analysis
library(tm)
stop_words <- stopwords("SMART")
note <- data.moment.english$English
note <- gsub("'", "", note)  # remove apostrophes
note <- gsub("[[:punct:]]", " ", note)  # replace punctuation with space
note <- gsub("[[:cntrl:]]", " ", note)  # replace control characters with space
note <- gsub("^[[:space:]]+", "", note) # remove whitespace at beginning of documents
note <- gsub("[[:space:]]+$", "", note) # remove whitespace at end of documents
note <- tolower(note)  # force to lowercase
doc.list <- strsplit(note, "[[:space:]]+")
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
D <- length(documents)
W <- length(vocab)
doc.length <- sapply(documents, function(x) sum(x[2, ]))
N <- sum(doc.length)
term.frequency <- as.integer(term.table)
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Build text mining model: Latent Dirichlet Allocation
library(lda)
library(LDAvis)
set.seed(0)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1
theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
note.model <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

json <- createJSON(phi = note.model$phi, 
                   theta = note.model$theta, 
                   doc.length = note.model$doc.length, 
                   vocab = note.model$vocab, 
                   term.frequency = note.model$term.frequency)

# visualize text clusters
serVis(json, out.dir = 'vis', open.browser = TRUE)



