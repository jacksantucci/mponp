### consequential transfer leakage (i.e., to eventual winners) -- may need to focus on leakage from winners -- OR: need to say "what percent of Charter's winners' vote came from Republicans"

source("functions_transfers.R")

'%!in%' <- function(x,y)!('%in%'(x,y))

load("../data/bronxtrans.Rdata")

### for seeing what parties are in the data
unique(unlist(lapply(bronxtrans, function(x) x$Party)))

getLeakage <- function(transfers.in){
	
	nondominant.parties <- c("A", "F", "C", "L", "R", "R-F", "R-CNP", "R-F-CNP", "F-CNP", "F-CNP", "F-C-NP", "D-A", "A-C-NP", "S")
	
	# nondominant.parties <- c("R", "R-F", "R-CNP", "R-F-CNP")
	
	transfers.in$Party[transfers.in$Party %in% nondominant.parties] <- "nD"
	
	rounds <- transferReport(transfers.in, 6)
	
	winners <- lapply(rounds, function(x) x[x$is.winner==T,])
	
	# ## coding only leakage from winners here
	
	# sender.winner <- lapply(winners, function(x) x$Candidate.send %in% x$Candidate)
	
	# sums <- unlist(lapply(sender.winner, sum))
	
	# winners <- winners[sums>0]
	
	# ## end
	
	by.party <- lapply(winners, function(x) aggregate(rawChange ~ Party + Party.send, data=x, "sum"))
	
	by.party.single <- do.call(rbind, by.party)
	
	# grep("^A|^C|^F|^L|^R", bar)
	
	# nondominant.parties <- c("A", "F", "C", "L", "R", "R-F", "R-CNP", "R-F-CNP", "F-CNP", "F-CNP", "F-C-NP", "D-A", "A-C-NP", "S")
	
	# nondominant.parties <- c("A", "C", "D-A", "A-C-NP")
	
	# by.party.single$Party[by.party.single$Party %in% nondominant.parties] <- "nD"
	# by.party.single$Party.send[by.party.single$Party.send %in% nondominant.parties] <- "nD"
	
	# by.party.single$Party[by.party.single$Party != "D"] <- "nD"
	# by.party.single$Party.send[by.party.single$Party.send != "D"] <- "nD"
	
	by.party.agg <- aggregate(rawChange ~ Party + Party.send, data=by.party.single, FUN="sum")
	
	by.sender <- split(by.party.agg, by.party.agg$Party.send)
	
	for (i in 1:length(by.sender)){
		by.sender[[i]]$percent <- by.sender[[i]]$rawChange/sum(by.sender[[i]]$rawChange)
	}
	
	# leakage <- lapply(by.sender, function(x) x[x$Party != x$Party.send,])
	
	leakage <- by.sender
	
	# next two lines compute leakage as percentage of party (or group's) total first-round vote
	
	r1.totals <- aggregate(as.numeric(r1) ~ Party, data=transfers.in, sum)
	
	for (i in 1:length(leakage)){
		leakage[[i]]$pct.tot.vote <- leakage[[i]]$rawChange/r1.totals[which(r1.totals$Party==leakage[[i]]$Party.send),2]
	}
	
	# leakage2 <- lapply(leakage, function(x) aggregate(percent ~ Party.send, data=x, FUN="sum"))
	
	# leakage3 <- do.call(rbind, leakage2)
	
	return(leakage)
}

foo <- lapply(bronxtrans, getLeakage)

DtoND <- lapply(foo, function(x) x$D)

NDtoD <- lapply(foo, function(x) x$nD)

lapply(foo, function(x) x$R)


# CDdf <- do.call(rbind, CtoD)

# CDdf$year <- seq(1949, 1959, 2)

# plot(CDdf$year, 100*CDdf$percent, type='l', main="Share of CEA transfers\ngoing to Democratic winners", ylab="Percent", xlab="Election", axes=F)
# axis(1, tick=F, at=CDdf$year, las=2)
# axis(2, tick=F, las=2)