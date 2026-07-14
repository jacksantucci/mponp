### aggregate ND to D transfers across boroughs

source("transfers_bronx.R")

brx <- NDtoD

source("transfers_brooklyn.R")

brk <- NDtoD

source("transfers_manhattan.R")

man <- NDtoD

source("transfers_queens.R")

que <- NDtoD

#### citywide

citywide <- list(NA)
for (i in 1:length(brx)){
	citywide[[i]] <- rbind(brx[[i]][brx[[i]]$Party=="D",],
	brk[[i]][brk[[i]]$Party=="D",],
	man[[i]][man[[i]]$Party=="D",],
	que[[i]][que[[i]]$Party=="D",]
	)
}

citywide.nd <- list(NA)
for (i in 1:length(brx)){
	citywide.nd[[i]] <- rbind(brx[[i]][brx[[i]]$Party=="nD",],
	brk[[i]][brk[[i]]$Party=="nD",],
	man[[i]][man[[i]]$Party=="nD",],
	que[[i]][que[[i]]$Party=="nD",]
	)
}

nd.sums <- unlist(lapply(citywide.nd, function(x) sum(x$rawChange)))

d.sums <- unlist(lapply(citywide, function(x) sum(x$rawChange)))

nd.leakage <- d.sums/(d.sums+nd.sums)

d.leakage <- nd.sums/(d.sums+nd.sums)

yrs <- seq(1937, 1945, 2)

pdf('../graphics/fig7.6_leakage_nyc.pdf')
plot(yrs, 100*nd.leakage, type='l', axes=F, ylab="Percent of observed transfers", xlab="Election", main="Transfer leakage to winners in opposing coalition:\nNew York City", xlim=c(1937, 1947), ylim=c(0, 80), sub="Note: no STV election in 1947; terms extended to four years.")
points(yrs, 100*nd.leakage, pch=16)
lines(yrs, 100*d.leakage, lty=2)
points(yrs, 100*d.leakage, pch=1)
axis(1, tick=F, las=2, at=c(yrs, 1947))
axis(2, tick=F, las=2)
text(1937.5, 100*nd.leakage[1], "Non-Democrats to Democrats", pos=4)
text(1937.5, 100*d.leakage[1], "Democrats to non-Democrats", pos=4)
points(x=c(1938, 1940, 1947), y=rep(0, 3), pch=rep(25, 3), bg=c(rep("white", 2), "black"))
legend("topright", legend=c("Failed repeal", "Successful repeal"), pch=c(25, 25), pt.bg=c("white", "black"))
dev.off()

# ########## NOW, of total votes, for the non-Democats

# slot <- citywide.nd[[5]]

# total.nd.vote <- lapply(citywide, function(x) sum(x$rawChange/x$pct.tot.vote))

# leak.tot.vote <- d.sums/unlist(total.nd.vote)

# plot(seq(1937, 1945, 2), 100*leak.tot.vote, type='l', axes=F, xlab="Election")
# axis(1, tick=F, las=2, at=seq(1937, 1945, 2))
# axis(2, tick=F, las=2)