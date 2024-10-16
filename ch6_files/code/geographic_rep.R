##################################################
# COMPUTE ENC ON FIRST CHOICE VOTES BY WARD
##################################################

library(readxl)

# d35 <- read.csv("⁨../../ch7_files/pr_cincy_2018-10-10/data/1936referendum_1935election.csv", stringsAsFactors=F)

wd1 <- read_xlsx('../data/worcester_cea_first-choice-votes-by-ward_1953-9.xlsx', sheet=1)
wd2 <- read_xlsx('../data/worcester_cea_first-choice-votes-by-ward_1953-9.xlsx', sheet=2)
wd3 <- read_xlsx('../data/worcester_cea_first-choice-votes-by-ward_1953-9.xlsx', sheet=3)
wd4 <- read_xlsx('../data/worcester_cea_first-choice-votes-by-ward_1953-9.xlsx', sheet=4)

wd <- list(wd1, wd2, wd3, wd4)

wd2 <- lapply(wd, as.data.frame)


### most preferred by ward

justvotes <- lapply(wd2, function(x) x[,2:ncol(x)])

getTopCand <- function(slot){
	cidx <- apply(slot, 1, function(x) which.max(x))
	out <- names(slot)[cidx]
	return(out)
}

mostpref1 <- lapply(justvotes, getTopCand)

### most preferred by ward, minus ticket leader

justvotes <- lapply(wd2, function(x) x[,2:ncol(x)])

getTopCand2 <- function(slot){
	drop.idx <- which.max(colSums(slot)) # drop top candidate
	newslot <- slot[,-drop.idx]
	cidx <- apply(newslot, 1, function(x) which.max(x))
	out <- names(newslot)[cidx]
	return(out)
}

mostpref2 <- lapply(justvotes, getTopCand2)

### vote shares by ward

justvotes <- lapply(wd2, function(x) x[,2:ncol(x)])

voteprops <- lapply(justvotes, function(x) x/sum(x))

wardprops <- lapply(voteprops, function(x) rowSums(x))

wps <- do.call(rbind, wardprops)

pcs <- lapply(voteprops, prcomp)

barplot(t(wps), beside=T)

### ENC stuff

getWardENC <- function(slot, dropTop=F){
	votes <- slot[,2:ncol(slot)]
	if (dropTop==T){
		drop.idx <- which.max(colSums(votes))
		votes <- votes[,-drop.idx]
	}
	voteShares <- apply(votes, 1, function(x) x/sum(x))
	wardENC <- apply(voteShares, 2, function(x) 1/sum(x^2))
	return(wardENC)
}

encs <- lapply(wd2, getWardENC)

encs2 <- lapply(wd2, getWardENC, dropTop=T)

encdf <- as.data.frame(do.call(rbind, encs))

names(encdf) <- paste0("ward", seq(1:10))
rownames(encdf) <- seq(1953, 1959, 2)

tencdf <- t(encdf)