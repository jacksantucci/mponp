### Figure 7: Worcester scatterplot (STV support by Dem. Aldermanic primary fractionalization)

source('worcester.R')

frac <- function(vector){ #Rae's fractionalization (F) measure
	raes.frac <- 1-sum((vector[is.na(vector)==F])^2)
	return(raes.frac)
}

prim47 <- read.csv('../data/worcPrimary.csv', sep=';')
names(prim47)[1] <- "Party"

# delete Blank
prim47$Blank <- NULL

# calculate F for 1947
freq47 <- prim47[,c(5:ncol(prim47))]
freq47.2 <- sweep(freq47, 1, rowSums(freq47, na.rm=T), "/")
prim47$f <- apply(freq47.2, 1, frac)

# fix f==1 to 0 because there is no frac in those cases
prim47$f[prim47$f==1] <- 0

primWo <- prim47[prim47$Party=="Dem" & prim47$Office=="Alderman",]
primWo <- subset(primWo, select=c("Ward", "Precinct", "f"))
allWo <- merge(dtaWo, primWo, by=c("Ward","Precinct"))

allWo$yesPR <- 100*allWo$yes.pr.pct

allWo$plotChr <- rep(NA, nrow(allWo))
allWo$plotChr[allWo$Ward %in% c(1, 10)] <- 15 # Republican wards
allWo$plotChr[allWo$Ward %in% c(8)] <- 3 # Academic wards
allWo$plotChr[allWo$Ward %in% c(5)] <- 17 # Labor wards
allWo$plotChr[allWo$Ward %in% c(3, 4, 6, 7, 9)] <- 1 # Manufacturing wards
allWo$plotChr[allWo$Ward %in% c(2, 9)] <- 2 # not named

outWoF <- lm(yesPR ~ f, data=allWo[which(allWo$f>0),])

outWoF2 <- lm(yesPR ~ f + I(f^2), data=allWo[which(allWo$f>0),])

pred <- predict(outWoF2)
ix <- sort(allWo$f[which(allWo$f>0)], index.return=T)$ix

pdf('../graphics/WorcesterRaeF.pdf')
plot(allWo$f, allWo$yesPR, pch=allWo$plotChr, axes=F, xlim=c(0, 0.8),  ylim=c(20, 100), xlab="Fractionalization of D aldermanic primary vote, Oct. 1947", ylab='Percent for charter, Nov. 1947')
# text(allWo$f, allWo$yesPR, allWo$Ward, cex=2/3)
legend(0.1, 100, legend=c('Republican (1, 10)', 'Plurality Irish-Catholic (3, 4, 6, 7)', 'Industrial (5)', 'Academic (8)', 'Party-competitive (2, 9)'), pch=c(15, 1, 17, 3, 2), bty='n', title="Binstock (1960) description of ward:", title.adj=0)
# abline(outWoF)
lines(allWo$f[which(allWo$f>0)][ix], pred[ix])
axis(1, tick=F)
axis(2, tick=F, las=2)
dev.off()