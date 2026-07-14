library(pscl)

load("../data/rcs.worc.Rdata")

# rcs.worc <- lapply(rcs.worc, function(x) dropUnanimous(x, lop=0)) # drop where minority is lop or less

'%!in%' <- function(x,y)!('%in%'(x,y)) # negate the %in% operator

##########

rcs <- rcs.worc[[1]]

# maj.parties <- c("CEA-d", "CEA-r") # might add "d" here after 1955? and grand coalitions?

getRollRate <- function(rcs, maj.parties){
	maj.idx <- which(rcs$legis.data$party %in% maj.parties)
	min.idx <- which(rcs$legis.data$party %!in% maj.parties)
	
	# vote <- rcs.worc[[2]]$votes[,102]
	
	roll.list <- list(NA)
	for (i in 1:ncol(rcs$votes)){
			
		vote <- rcs$votes[,i]
		
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

# worc.rolls <- lapply(rcs.worc, function(x) getRollRate(x, c("CEA-r", "CEA-d")))

majorities <- list(
	c("CEA-r", "CEA-d"), # 1951
	c("CEA-r", "CEA-d", "CEA-u"), # 1953
	c("CEA-d", "CEA-r", "CEA-u", "D"), # 1955
	c("CEA-d", "CEA-r", "D"), # 1957 # include Democrats, grand coalition?
	c("CEA-d", "CEA-r"), # 1959
	c("CEA-d", "CEA-r") # 1960
	)

worc.rolls <- list(NA)
for (i in 1:length(rcs.worc)){
	worc.rolls[[i]] <- getRollRate(rcs.worc[[i]], majorities[[i]])
}

summaries <- lapply(worc.rolls, '[[', 'summary')

summaries <- as.data.frame(do.call(rbind, summaries))

summaries$maj.prop <- summaries$maj.roll/summaries$nvotes
summaries$min.prop <- summaries$min.roll/summaries$nvotes

yrs <- seq(1951, 1961, 2)
yrs[yrs==1961] <- 1960

# plot(yrs, summaries$maj.prop, type='l', ylim=c(0, max(summaries[,c("maj.prop", "min.prop")])), axes=F, xlab="Final year of term", ylab="Proportion of all votes", main="Legislative rolls in Worcester City Council")
# for (i in 1:length(majorities)){
	# text(yrs[i], summaries$maj.prop[i], majorities[[i]])
# }
# lines(yrs, summaries$min.prop, lty=2)
# axis(1, tick=F, at=yrs)
# axis(2, tick=F, las=2)
# text(1951, 0.01, "Majority rolls", pos=4)
# text(1953, 0.07, "Minority rolls")

pdf("../graphics/fig7.8_worcester_majority_rolls.pdf", width=6, height=4)
plot(yrs, 100*summaries$maj.prop, type='l', axes=F, xlab="", ylab="Percent of observed votes", main="Majority rolls and party control\nin Worcester City Council", ylim=c(0, 15))
mtext("Final year of term", 1, 4)
axis(1, tick=F, at=yrs, las=2)
axis(2, tick=F, las=2)
majs <- unlist(lapply(majorities, function(x) paste(x, collapse='\n')))
text(x=yrs, y=100*summaries$maj.prop, labels=majs, pos=3, cex=2/3)
legend("topleft", legend=c("CEA-d: reform Democrat", "CEA-r: regular Republican", "CEA-u: reform unaffiliated", "D: regular Democrat", "Failed repeal", "Successful repeal"), pch=c(rep(NA, 4), 25, 25), pt.bg=c(rep(NA, 4), "white", "black"), ncol=1, bty=T, cex=2/3) # title="Majority composition",
points(x=c(1959, 1960), y=c(0, 0), pch=c(25, 25), bg=c("white", "black"))
# legend("topright", legend=c("Failed repeal", "Successful repeal"), pch=c(25, 25), pt.bg=c("white", "black"))
dev.off()


### examine bills

idx1959 <- which(worc.rolls[[5]]$by.vote[,"maj.roll"]==1)
dimnames(rcs.worc[[5]]$votes[,idx1959])[[2]]

### divisions on bills

vote.divs <- list(NA)
for (i in idx1959){
	vote.divs[[i]] <- table(rcs.worc[[5]]$votes[,i], rcs.worc[[5]]$legis.data[,"party"])
}

unlist(vote.divs, recursive=F)

## oc just the rolls

rcs.rolls <- rcs.worc[[5]]
new <- rollcall(rcs.rolls$votes[,idx1959], legis.data=rcs.rolls$legis.data)


oc.out <- oc(new, minvotes=2, polarity=c(3, 3))

############### NOT RUN

### OC

addCutline <- function(x, dims=c(1,2), billNo, lineWeight){
	cutlineData <- cbind.data.frame("normVec1"=x$rollcalls[, paste("normVector", dims[1], "D", sep = "")], "normVec2"=x$rollcalls[, paste("normVector", dims[2], "D", sep = "")], "midpt"=x$rollcalls[, "midpoints"])
    cutlineData <- na.omit(cutlineData[billNo,])
	cutlineData$xcut <- cutlineData$midpt*cutlineData$normVec1
	cutlineData$ycut <- cutlineData$midpt*cutlineData$normVec2
	attach(cutlineData)
	arrows(xcut+normVec2, ycut-normVec1, xcut-normVec2, ycut+normVec1, length=0, lwd=lineWeight, col="black")
	detach(cutlineData)
}

library(oc)

foo <- oc(rcs.worc[[5]], polarity=c("Holmstrom", "Wells"))

rolls.to.plot <- which(worc.rolls[[5]]$by.vote[,"maj.roll"]==T)

plot.OCcoords(foo)
for (i in 1:length(rolls.to.plot)){
	addCutline(foo, billNo=rolls.to.plot[i], lineWeight=1/3)
}