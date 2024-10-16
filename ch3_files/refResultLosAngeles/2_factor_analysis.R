newNames <- c(
	"Municipal ownership of utilities (PCC)",
	"Regulate private utilities (PCC)",
	"LA River bed - regulate use and condemnatons in (PCC)",
	"Apply state home-rule law to LA (PCC)",
	"Exec. agencies responsible to council (PCC)",
	"Set city-council salaries (PCC)",
	"Remove incumbent government (PCC)",
	"List-proportional voting (PCC)",
	"Appointed harbor commission, council sets salaries (Admin)",
	"Fix bonds of city officers (Admin)",
	"Sale of land along aqueduct (CC)",
	"Pension and insurance - firemen (CC)",
	"Pension and insurance - police (CC)",
	"Prohibit municipally-owned newspaper (CC)",
	"Private biz in city parks, commission fixes salaries (CC)",
	"Elected harbor commission, railroad, fixed salaries (CC)",
	"Districts plus at-large (CC)",
	"Raise electeds' salaries (CC)",
	"Regulate the dance halls (Other)"
)

load("props.Rdata")
load("la.ref.data.Rdata")

oldNames <- names(props)

##### create new props from la.ref.data

d <- la.ref.data

yesses <- d[,grep("yes", dimnames(d)[[2]])]
noes <- d[,grep("no", dimnames(d)[[2]])]
totals <- yesses+noes

names(yesses) <- paste0('q', seq(1, 19), 'yes')
names(noes) <- paste0('q', seq(1, 19), 'no')
names(blanks) <- paste0('q', seq(1, 19), 'blank')

props <- yesses/totals # overwrite original props (from props.Rdata) with blank votes included

names(props) <- newNames

##### principal components and biplot

pca.out <- prcomp(props, scale.=T)

pdf("biplot_losAngeles.pdf", width=15, height=15)
biplot(pca.out)
dev.off()

screeplot(pca.out)

fa1 <- factanal(props, factors=1)
fa2 <- factanal(props, factors=2)
fa3 <- factanal(props, factors=3)

### factor loadings 2 dimensions

plot.pch <- rep(NA, length(dimnames(fa2$loadings)[[1]]))
plot.pch[grep("\\(PCC)", dimnames(fa2$loadings)[[1]])] <- "P"
plot.pch[grep("\\(CC)", dimnames(fa2$loadings)[[1]])] <- "C"
plot.pch[grep("\\(Admin)", dimnames(fa2$loadings)[[1]])] <- "A"
plot.pch[grep("\\(Other)", dimnames(fa2$loadings)[[1]])] <- "O"

pdf("factorLoadings2d.pdf")
plot(fa2$loadings, pch=plot.pch, ylim=c(-1,1), xlim=c(-1, 1), col="darkgray", axes=F, main='2D factor loadings, precinct "yes" shares:\nLos Angeles, March 1913', xlab="Factor 1: PCC support", ylab="Factor 2: Establishment support")
text(fa2$loadings, labels=oldNames, cex=2/3, pos=1)
abline(v=c(-0.5, 0.5), lty=3)
abline(h=c(-0.5, 0.5), lty=3)
legend("bottom", bty='n', legend=c("Citizen's Commitee of 1,000", "People's Charter Conference", "Sitting administration", "Other"), pch=c("C", "P", "A", "O"), col="darkgray", cex=1, title="Proposal came from:", title.adj=0)
axis(1, tick=F, at=seq(-1, 1, 0.5))
axis(2, tick=F, las=2, at=seq(-1, 1, 0.5))
dev.off()


### factor loadings one dimension

# par(mar=c(5.1, 17.1, 4.1, 2.1))
forbp1 <- sort(fa1$loadings[,1])

xpos <- ifelse(forbp1>0, 0, forbp1)

pdf("factorLoadingsOneDimension.pdf")
bp1 <- barplot(forbp1, horiz=T, names.arg=NA, axes=F, xlim=c(-2.5,1), main="One-dimensional politics, 19 simultaneous ballot measures:\nLos Angeles, 24 March 1913")
axis(1, tick=F, at=seq(-1, 1, 0.5))
text(names(forbp1), x=xpos, y=bp1, pos=2, cex=2/3)
mtext("Factor loading", side=1, line=2, at=0)
mtext("Originating parties: PCC = People's Charter Conference;\nCC = Citizens' Committee of One Thousand; Admin = sitting administration.", side=1, line=4, at=0, cex=2/3)
dev.off()
# mtext(names(forbp1), side=2, at=bp1, las=2, line=1, cex=2/3)

##### table of proportions

library(xtable)

d <- la.ref.data

yesses <- d[,grep("yes", dimnames(d)[[2]])]
noes <- d[,grep("no", dimnames(d)[[2]])]
totals <- yesses+noes
max.tots <- apply(totals, 1, max)
blanks <- as.data.frame(apply(totals, 2, function(x) max.tots-x))

y.sums <- colSums(yesses)
n.sums <- colSums(noes)
b.sums <- colSums(blanks)

yes.pcts <- y.sums/(y.sums+n.sums) #+b.sums) # drop blank vote
yes.pcts <- 100*round(yes.pcts, 3)
rawTable <- cbind(newNames, yes.pcts)
rawTable <- rawTable[order(rawTable[,2], decreasing=T),]
dimnames(rawTable)[[2]] <- c("Proposal (initiating party)", "Yes %")
theTable <- xtable(rawTable, caption='Yes vote for each charter amendment, Los Angeles 1913', label='yesvote')
print.xtable(theTable, include.rownames=F)
