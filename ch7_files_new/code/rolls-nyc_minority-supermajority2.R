library(pscl)

load("../data/rcs.nyc.Rdata")
'%!in%' <- function(x,y)!('%in%'(x,y)) # negate the %in% operator

# rcs.nyc <- lapply(rcs.nyc, function(x) dropUnanimous(x, lop=1)) # drop where minority is lop or less

# maj.parties <- "D"

# rcs <- rcs.nyc[[2]]

##########

getRollRate <- function(rcs, maj.parties, vote.idx=1:ncol(rcs$votes), supermajority.threshold=1/3){
	maj.idx <- which(rcs$legis.data$party %in% maj.parties) # party or Agent
	min.idx <- which(rcs$legis.data$party %!in% maj.parties) # party or Agent
	
	maj.min <- ifelse(rcs$legis.data$party %in% maj.parties, "maj", "min") # 2018-10-04 new
	
	# vote <- rcs.nyc[[2]]$votes[,1]
	
	roll.list <- list(NA)
	
	votes <- rcs$votes[,vote.idx]
	
	for (i in 1:ncol(votes)){
			
		vote <- votes[,i]
		# vote <- rcs$votes[,31]
		
		# abstain or not equal to majority position (omit notInLegis)
		# vote[is.na(vote)] <- "abstain" # commenting this to see if it changes the rates
		vote[vote==9] <- NA
		
		### 2018-10-04 new code 2/3 rolls
		
		# vote division
		vote.div <- table(vote)#, exclude=F)
		prop.vote.div <- prop.table(vote.div)
		
		# majority-of-majority position
		maj.pos <- names(which.max(table(vote[maj.idx]))) #, exclude=F)))
		
		# majority-of-minority position
		min.pos <- names(which.max(table(vote[min.idx]))) #, exclude=F)))
		
		# did those in minority with majority-of-minority position amount to 1/3?
		min.tab <- prop.table(table(vote, maj.min))
		min.super.roll <- min.tab[min.pos, "min"]<supermajority.threshold #1/3
		
		# is roll?
		maj.roll <- (names(which.max(vote.div)) != maj.pos)
		min.roll <- (names(which.max(vote.div)) != min.pos)
		
		# is majority super roll? (majority position on vote gets less than 2/3)
		maj.super.roll <- ifelse(prop.vote.div[paste(maj.pos)]<(1-supermajority.threshold), T, F)
		
		vote.out <- c(maj.roll, min.roll, min.super.roll, maj.super.roll)
		
		roll.list[[i]] <- vote.out
	}
	rolls.by.vote <- do.call(rbind, roll.list)
	dimnames(rolls.by.vote)[[2]] <- c("maj.roll", "min.roll", "min.super.roll", "maj.super.roll")
	rolls.summary <- c(colSums(rolls.by.vote), "nvotes"=nrow(rolls.by.vote))
	out <- list("summary"=rolls.summary, "by.vote"=rolls.by.vote)
	return(out)
}


# majorities <- list(
	# c("CarrolWA", "CashmoreJ", "DeeringJA", "DiGiovannaAJ", "KeeganCE", "KinsleyJE", "McCarthyWM", "NugentJP", "QuinnH", "SchanzerAD", "SchickF", "SharkeyJT", "SpellmanHH"),
	# c("BurkeJA", "CarrollWA", "CashmoreJ", "CohenL", "ConradWN", "DiGiovannaAJ", "HartWR", "KeeganCE", "KinsleyJE", "McCarthyWM", "NugentJP", "QuinnHQ", "SchickF", "SharkeyJT"),
	# c("CarrollWA", "CaseyR", "CohenL", "ConradWN", "DiFalcoSS", "DiGiovannaAJ", "DonovanGE", "HartWR", "KinsleyJE", "McCarthyWM", "NugentJP", "PhillipsJA", "QuinnH", "SchickF", "SharkeyJT", "VogelE"),
	# c("CarrollWA", "CohenL", "DiFalcoSS", "DiGiovannaAJ", "HartWR", "NugentJP", "PhillipsAJ", "QuinnH", "SchickF", "SharkeyJT", "VogelE"),
	# c("CarrollWA", "CunninghamEA", "DiFalcoSS", "DiGiovannaAJ", "DowningME", "HartWR", "KeeganCE", "McCarthyWM", "MirabileTJ", "RagerE", "SchickF", "SchwartzB", "SharkeyJT", "VogelE")
# )

vote.indices.twothirds <- lapply(rcs.nyc, function(x) grep("A LOCAL LAW", dimnames(x$votes)[[2]], ignore.case=F))

vote.indices.threefourths <- lapply(rcs.nyc, function(x) grep("capital budget", dimnames(x$votes)[[2]], ignore.case=T))

majorities <- list(
	# c('AL', 'F', 'I', 'R'),
	c('D'),
	c('D'),
	c('D'),
	c('D'),
	c('D', 'AL.D')
)

# majorities <- rep("D", 5)

nyc.rolls.locallaw <- list(NA)
for (i in 1:length(rcs.nyc)){
	nyc.rolls.locallaw[[i]] <- getRollRate(rcs.nyc[[i]], majorities[[i]], vote.idx=vote.indices.twothirds[[i]], supermajority.threshold=1/3)
}

summaries <- lapply(nyc.rolls.locallaw, '[[', 'summary')

summaries <- as.data.frame(do.call(rbind, summaries))

summaries$maj.prop <- summaries$maj.roll/summaries$nvotes
summaries$min.prop <- summaries$min.roll/summaries$nvotes
summaries$min.super.prop <- summaries$min.super.roll/summaries$nvotes
summaries$maj.super.prop <- summaries$maj.super.roll/summaries$nvotes

nyc.rolls.budget <- list(NA)
for (i in 1:length(rcs.nyc)){
	nyc.rolls.budget[[i]] <- getRollRate(rcs.nyc[[i]], majorities[[i]], vote.idx=vote.indices.threefourths[[i]], supermajority.threshold=1/4)
}

summaries2 <- lapply(nyc.rolls.budget, '[[', 'summary')

summaries2 <- as.data.frame(do.call(rbind, summaries2))

summaries2$maj.prop <- summaries2$maj.roll/summaries2$nvotes
summaries2$min.prop <- summaries2$min.roll/summaries2$nvotes
summaries2$min.super.prop <- summaries2$min.super.roll/summaries2$nvotes
summaries2$maj.super.prop <- summaries2$maj.super.roll/summaries2$nvotes

yrs <- seq(1939, 1947, 2)

pdf("../graphics/fig7.5_nyc_minority_rolls.pdf", width=6, height=4)
plot(yrs, 100*summaries$min.super.prop, type='l', axes=F, ylim=c(0, 70), xlab="Final year of term", ylab="Percent of observed votes", main="Minority frustration in New York City Council", lty=2, xlim=c(1938, 1947))
lines(yrs, 100*summaries2$min.super.prop, lty=1)
axis(1, tick=F, at=yrs)
axis(2, tick=F, las=2)
text(1947, 45, "Local laws", pos=2)
text(1947, 13, "Capital budget", pos=2)
points(x=c(1938, 1940, 1947), y=rep(0, 3), pch=rep(25, 3), bg=c(rep("white", 2), "black"))
legend("topleft", legend=c("Failed repeal", "Successful repeal"), pch=c(25, 25), pt.bg=c("white", "black"))
dev.off()

# ### look at bills

## 1946-7

budget47rolls <- which(nyc.rolls.budget[[5]]$by.vote[,"min.super.roll"]==T)

votes.to.pull <- vote.indices.threefourths[[5]][budget47rolls]

dimnames(rcs.nyc[[5]]$votes)[[2]][votes.to.pull]

rcs.nyc[[5]]$votes[,votes.to.pull]

out1947 <- list(NA)
for (i in 1:ncol(rcs.nyc[[5]]$votes[,votes.to.pull])){
	out1947[[i]] <- table(rcs.nyc[[5]]$votes[,votes.to.pull[i]], rcs.nyc[[5]]$legis.data$party)
}

## 1942-3

budget43rolls <- which(nyc.rolls.budget[[3]]$by.vote[,"min.super.roll"]==T)

votes.to.pull <- vote.indices.threefourths[[3]][budget43rolls]

dimnames(rcs.nyc[[3]]$votes)[[2]][votes.to.pull]

rcs.nyc[[3]]$votes[,votes.to.pull]

## for scaling

the.rolls <- rollcall(rcs.nyc[[5]]$votes[,votes.to.pull], legis.data=rcs.nyc[[5]]$legis.data)

budget.votes <- rollcall(rcs.nyc[[5]]$votes[,vote.indices.threefourths[[5]]], legis.data=rcs.nyc[[5]]$legis.data, legis.names=rcs.nyc[[5]]$legis.data$Agent)

# i <- 3
# j <- 2 # 2 if minority rolls, 1 if majority rolls
# nyc1 <- which(nyc.rolls[[i]]$by.vote[,j]==T)
# dimnames(rcs.nyc[[i]]$votes[,nyc1])[[2]]

dimnames(rcs.nyc[[5]])

# ### oc plots

# plotOC <- function(oc.out){
	# legs <- oc.out$legislators
	# plot(c(-1,1), c(-1,1), pch=NA, axes=F)
	# text(legs$coord1D, legs$coord2D, paste0(legs$Agent, ' (', legs$party, ')'), cex=0.5)
# }

# library(oc)
# library(wnominate)

# foo <- oc(rcs.nyc[[3]], polarity=c("SharkeyJT", "EarleGB"))
# foo2 <- wnominate(rcs.nyc[[3]], polarity=c("SharkeyJT", "EarleGB"))

# plot.OCcoords(foo, cutline=NULL)
# plot.coords(foo2)