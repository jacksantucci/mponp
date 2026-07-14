library(pscl)

load("../data/rcs.cincy.Rdata")
'%!in%' <- function(x,y)!('%in%'(x,y)) # negate the %in% operator

# rcs.cincy <- lapply(rcs.cincy, function(x) dropUnanimous(x, lop=3)) # drop where minority is lop or less


##########

# rcs <- rcs.cincy[[4]]

# maj.parties <- c("C-d", "C-r", "P")

getRollRate <- function(rcs, maj.parties){
	maj.idx <- which(rcs$legis.data$party %in% maj.parties)
	min.idx <- which(rcs$legis.data$party %!in% maj.parties)
	
	# vote <- rcs.worc[[2]]$votes[,102]
	
	roll.list <- list(NA)
	for (i in 1:ncol(rcs$votes)){
			
		vote <- rcs$votes[,i]
		# vote <- rcs$votes[,31]
		
		# abstain or not equal to majority position (omit notInLegis)
		vote[is.na(vote)] <- "abstain"
		vote[vote==9] <- NA
		
		# majority-of-majority position
		maj.pos <- names(which.max(table(vote[maj.idx]))) #, exclude=F)))
		
		# majority-of-minority position
		min.pos <- names(which.max(table(vote[min.idx]))) #, exclude=F)))
		
		# vote division
		vote.div <- table(vote)#, exclude=F)
		
		# is roll?
		maj.roll <- (names(which.max(vote.div)) != maj.pos)
		min.roll <- (names(which.max(vote.div)) != min.pos)
		
		vote.out <- c(maj.roll, min.roll)
		names(vote.out) <- c("maj.roll", "min.roll")
		
		roll.list[[i]] <- vote.out
	}
	rolls.by.vote <- do.call(rbind, roll.list)
	rolls.summary <- c(colSums(rolls.by.vote), "nvotes"=nrow(rolls.by.vote))
	out <- list("summary"=rolls.summary, "by.vote"=rolls.by.vote)
	return(out)
}

# foo <- getRollRate(rcs.worc[[6]], c("CEA-r", "CEA-d"))

# worc.rolls <- lapply(rcs.worc, function(x) getRollRate(x, c("CEA-r", "CEA-d")))

majorities <- list(
	c("C-d", "C-r"), # 1931
	c("C-r", "C-d"), # 1933
	c("C-d", "C-r"), # 1935
	c("C-d", "C-r", "P"), # 1937
	c("R", "P"), # 1939
	c("R", "C-d", "C-r"), # 1941 -- Include "C-d", "C-r"? (Kolesar 1995, p. 183; Straetz 1958, pp. 85-6)
	c("R"), # 1943
	c("R"), # 1945
	c("R"), # 1947
	c("C-d", "C-r"), # 1949
	c("C-r", "C-d"), # 1951
	c("R"), # 1953
	c("C-d", "C-r"), # 1955
	c("C-d", "C-r") # 1957
)

cincy.rolls <- list(NA)
for (i in 1:length(rcs.cincy)){
	cincy.rolls[[i]] <- getRollRate(rcs.cincy[[i]], majorities[[i]])
}

summaries <- lapply(cincy.rolls, '[[', 'summary')

summaries <- as.data.frame(do.call(rbind, summaries))

summaries$maj.prop <- summaries$maj.roll/summaries$nvotes
summaries$min.prop <- summaries$min.roll/summaries$nvotes

yrs <- seq(1931, 1957, 2)


# plot(yrs, summaries$maj.prop, type='l', ylim=c(0,max(summaries[,c("maj.prop","min.prop")])), axes=F, xlab="Final year of term", ylab="Proportion of all votes", main="Legislative rolls in Cincinnati City Council")
# for (i in 1:length(majorities)){
	# text(yrs[i], summaries$maj.prop[i], majorities[[i]])
# }
# lines(yrs, summaries$min.prop, lty=2)
# axis(1, tick=F, at=yrs)
# axis(2, tick=F, las=2)
# # text(1951, 0.01, "Majority rolls", pos=4)
# # text(1953, 0.07, "Minority rolls")
# legend("topleft", lty=c(1,2), legend=c("Majority rolls", "Minority rolls"))

pdf("../graphics/fig7.2_cincinnati_majority_rolls.pdf", width=6, height=4)
plot(yrs, 100*summaries$maj.prop, type='l', axes=F, xlab="", ylab="Percent of observed votes", main="Majority rolls and party control\nin Cincinnati City Council", ylim=c(0, 18))
mtext("Final year of term", 1, 4)
axis(1, tick=F, at=yrs, las=2)
axis(2, tick=F, las=2)
majs <- unlist(lapply(majorities, function(x) paste(x, collapse='\n')))
text(x=yrs, y=100*summaries$maj.prop, labels=majs, pos=3, cex=2/3)
legend("topleft", legend=c("C-d: regular Democrat", "C-r: reform Republican", "R: Republican", "P: Prog. Democrat", "Failed repeal", "Successful repeal"), pch=c(rep(NA, 4), 25, 25), pt.bg=c(rep(NA, 4), "white", "black"), ncol=2, bty=T, cex=2/3) # title="Majority composition",
points(x=c(1936, 1939, 1947, 1954, 1957), y=rep(0, 5), pch=rep(25, 5), bg=c(rep("white", 4), "black"))
dev.off()

### examine bills

idx1935 <- which(cincy.rolls[[3]]$by.vote[,"maj.roll"]==1)
dimnames(rcs.cincy[[3]]$votes[,idx1935])[[2]]
for (i in 1:ncol(rcs.cincy[[3]]$votes[,idx1935])){
	print(table(rcs.cincy[[3]]$votes[,idx1935[i]], rcs.cincy[[3]]$legis.data$party))
}


idx1939 <- which(cincy.rolls[[5]]$by.vote[,"maj.roll"]==1)
dimnames(rcs.cincy[[5]]$votes[,idx1939])[[2]]
rcs.cincy[[5]]$votes[,idx1939]
for (i in 1:ncol(rcs.cincy[[5]]$votes[,idx1939])){
	print(table(rcs.cincy[[5]]$votes[,idx1939[i]], rcs.cincy[[5]]$legis.data$party))
}

idx1949 <- which(cincy.rolls[[10]]$by.vote[,"maj.roll"]==1)
dimnames(rcs.cincy[[10]]$votes[,idx1949])[[2]]
for (i in 1:ncol(rcs.cincy[[10]]$votes[,idx1949])){
	print(table(rcs.cincy[[10]]$votes[,idx1949[i]], rcs.cincy[[10]]$legis.data$party))
}
rcs.cincy[[10]]$votes[,idx1949]

idx1951 <- which(cincy.rolls[[11]]$by.vote[,"maj.roll"]==1)
dimnames(rcs.cincy[[11]]$votes[,idx1951])[[2]]
for (i in 1:ncol(rcs.cincy[[11]]$votes[,idx1951])){
	print(table(rcs.cincy[[11]]$votes[,idx1951[i]], rcs.cincy[[11]]$legis.data$party))
}
rcs.cincy[[11]]$votes[,idx1951]

idx1955 <- which(cincy.rolls[[13]]$by.vote[,"maj.roll"]==1)
dimnames(rcs.cincy[[13]]$votes[,idx1955])[[2]]
rcs.cincy[[13]]$votes[,idx1955[33]]

idx1957 <- which(cincy.rolls[[14]]$by.vote[,"maj.roll"]==1)
dimnames(rcs.cincy[[14]]$votes[,idx1957])[[2]]