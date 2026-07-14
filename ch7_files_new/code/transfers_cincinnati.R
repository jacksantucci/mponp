### consequential transfer leakage (i.e., to eventual winners) -- may need to focus on leakage from winners -- OR: need to say "what percent of Charter's winners' vote came from Republicans"

source("functions_transfers.R")

load("../data/ctrans.Rdata")

years <- seq(1925, 1955, 2)

cands <- lapply(ctrans, function(x) paste(x$Candidate, x$Party))

names(cands) <- years

getLeakage <- function(transfers.in){
	
	transfers.in$Party[transfers.in$Party %in% c("C-r", "C-d")] <- "C"
	
	rounds <- transferReport(transfers.in, 6)
	
	winners <- lapply(rounds, function(x) x[x$is.winner==T,])
	
	# ## coding only leakage from winners here
	
	# sender.winner <- lapply(winners, function(x) x$Candidate.send %in% x$Candidate)
	
	# sums <- unlist(lapply(sender.winner, sum))
	
	# winners <- winners[sums>0]
	
	# ## end
	
	by.party <- lapply(winners, function(x) aggregate(rawChange ~ Party + Party.send, data=x, "sum"))
	
	by.party.single <- do.call(rbind, by.party)
	
	# by.party.single$Party[by.party.single$Party %in% c("C-r", "C-d")] <- "C"
	# by.party.single$Party.send[by.party.single$Party.send %in% c("C-r", "C-d")] <- "C"
	
	by.party.agg <- aggregate(rawChange ~ Party + Party.send, data=by.party.single, FUN="sum")
	
	by.sender <- split(by.party.agg, by.party.agg$Party.send)
	
	for (i in 1:length(by.sender)){
		by.sender[[i]]$percent <- by.sender[[i]]$rawChange/sum(by.sender[[i]]$rawChange)
	}
	
	leakage <- lapply(by.sender, function(x) x[x$Party != x$Party.send,])
	
	# next two lines compute leakage as percentage of party (or group's) total first-round vote
	
	r1.totals <- aggregate(as.numeric(r1) ~ Party, data=transfers.in, sum)
	
	for (i in 1:length(leakage)){
		leakage[[i]]$pct.tot.vote <- leakage[[i]]$rawChange/r1.totals[which(r1.totals$Party==leakage[[i]]$Party.send),2]
	}
	
	# leakage2 <- lapply(leakage, function(x) aggregate(percent ~ Party.send, data=x, FUN="sum"))
	
	# leakage3 <- do.call(rbind, leakage2)
	
	return(leakage)
}

foo <- lapply(ctrans, getLeakage)

RtoC <- lapply(foo, function(x) x$R)
RtoC <- lapply(RtoC, function(x) x[1,])

RCdf <- do.call(rbind, RtoC)
RCdf$year <- seq(1925, 1955, 2)

# plot(RCdf$year, 100*RCdf$percent, type='l', main="Share of Republican transfers\ngoing to Charter winners", ylab="Percent", xlab="Election", axes=F)
# axis(1, tick=F, at=RCdf$year, las=2)
# axis(2, tick=F, las=2)

CtoR <- lapply(foo, function(x) x$C)
CtoR <- lapply(CtoR, function(x) x[which(x$Party=="R"),])

CRdf <- do.call(rbind, CtoR)
CRdf$year <- seq(1925, 1955, 2)

# plot(CRdf$year, 100*CRdf$pct.tot.vote, type='l', main="Share of Charter vote flowing to non-Charter winners", ylab="Percent", xlab="Election", axes=F, ylim=c(0, 10))
# lines(CRdf$year, 100*CRdf$pct.tot.vote, lty=2)
# axis(1, tick=F, at=RCdf$year, las=2)
# axis(2, tick=F, las=2)

pdf("../graphics/fig7.3_leakage_cincinnati_wide.pdf", width=10, height=7)
plot(CRdf$year, 100*CRdf$percent, type='l', main="Transfer leakage to winners in opposing coalition:\nCincinnati", ylab="Percent of observed transfers", xlab="Election", axes=F, ylim=c(0, 50), xlim=c(1925, 1957))
points(CRdf$year, 100*CRdf$percent, pch=16)
lines(RCdf$year, 100*RCdf$percent, lty=2)
points(RCdf$year, 100*RCdf$percent, pch=1)
axis(1, tick=F, at=c(RCdf$year, 1957), las=2)
axis(2, tick=F, las=2)
text(1927, 100*CRdf$percent[2], "Charter to Republican", pos=4)
text(1927, 100*RCdf$percent[2], "Republican to Charter", pos=4)
points(x=c(1936, 1939, 1947, 1954, 1957), y=rep(0, 5), pch=rep(25, 5), bg=c(rep("white", 4), "black"))
legend(1945, 50, legend=c("Failed repeal", "Successful repeal"), pch=c(25, 25), pt.bg=c("white", "black"))
dev.off()