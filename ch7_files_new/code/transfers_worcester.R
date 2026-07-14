### consequential transfer leakage (i.e., to eventual winners) -- may need to focus on leakage from winners -- OR: need to say "what percent of Charter's winners' vote came from Republicans"

source("functions_transfers.R")

load("../data/wtrans.Rdata")

# ## only top 9 non-CEA dems

# top9dem <- function(slot){
	# ranking <- order(slot$r1, decreasing=T)
	# orig.index <- seq_along(slot$r1)
	# rank.df <- cbind.data.frame("fcvotes"=slot$r1, "Party"=slot$Party, orig.index)
	# rank.df.D <- rank.df[rank.df$Party=="D",]
	# rank.d <- order(rank.df.D$fcvotes, decreasing=T)
	# rankdfd2 <- cbind.data.frame(rank.d, rank.df.D)
	# idx.to.chg <- rankdfd2$orig.index[rankdfd2$rank.d<10]
	# slot$Party[idx.to.chg] <- "D9"
	# return(slot)
# }

wtrans <- lapply(wtrans, top9dem)

getLeakage <- function(transfers.in){
	
	rounds <- transferReport(transfers.in, 6)
	
	winners <- lapply(rounds, function(x) x[x$is.winner==T,])
	
	# ## coding only leakage from winners here
	
	# sender.winner <- lapply(winners, function(x) x$Candidate.send %in% x$Candidate)
	
	# sums <- unlist(lapply(sender.winner, sum))
	
	# winners <- winners[sums>0]
	
	# ## end
	
	by.party <- lapply(winners, function(x) aggregate(rawChange ~ Party + Party.send, data=x, "sum"))
	
	by.party.single <- do.call(rbind, by.party)
	
	by.party.single$Party[by.party.single$Party %in% c("CEA-r", "CEA-d", "CEA-u", "CEA-?")] <- "C"
	by.party.single$Party.send[by.party.single$Party.send %in% c("CEA-r", "CEA-d", "CEA-u", "CEA-?")] <- "C"
	
	by.party.agg <- aggregate(rawChange ~ Party + Party.send, data=by.party.single, FUN="sum")
	
	by.sender <- split(by.party.agg, by.party.agg$Party.send)
	
	for (i in 1:length(by.sender)){
		by.sender[[i]]$percent <- by.sender[[i]]$rawChange/sum(by.sender[[i]]$rawChange)
	}
	
	leakage <- lapply(by.sender, function(x) x[x$Party != x$Party.send,])
	
	# leakage2 <- lapply(leakage, function(x) aggregate(percent ~ Party.send, data=x, FUN="sum"))
	
	# leakage3 <- do.call(rbind, leakage2)
	
	return(leakage)
}

foo <- lapply(wtrans, getLeakage)

DtoC <- lapply(foo, function(x) x$D)
DCdf <- do.call(rbind, DtoC)

DCdf$year <- seq(1949, 1959, 2)

CtoD <- lapply(foo, function(x) x$C)
CDdf <- do.call(rbind, CtoD)

CDdf$year <- seq(1949, 1959, 2)

pdf("../graphics/fig7.9_leakage_worcester.pdf")
plot(CDdf$year, 100*CDdf$percent, type='l', main="Transfer leakage to winners in opposing coalition:\nWorcester", ylab="Percent of observed transfers", xlab="Election", axes=F, xlim=c(1949, 1960), ylim=c(0, 100))
points(CDdf$year, 100*CDdf$percent, pch=16)
lines(DCdf$year, 100*DCdf$percent, lty=2)
points(DCdf$year, 100*DCdf$percent, pch=1)
axis(1, tick=F, at=c(CDdf$year, 1960), las=2)
axis(2, tick=F, las=2)
text(1949, 100*DCdf$percent[1], "Non-CEA Democrats to CEA", pos=4)
text(1949, 100*CDdf$percent[1], "CEA to non-CEA Democrats", pos=4)
points(x=c(1959, 1960), y=c(0, 0), pch=c(25, 25), bg=c("white", "black"))
legend("topright", legend=c("Failed repeal", "Successful repeal"), pch=c(25, 25), pt.bg=c("white", "black"))
dev.off()

#### for comparing winners

transferReport(wtrans[[6]], 3)

tslot <- wtrans[[5]]

idxwin <- which(tslot[,ncol(tslot)]>0)

tslot[idxwin,]