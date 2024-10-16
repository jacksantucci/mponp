### Figure 6: Worcester scatterplot (STV support by mayoral voting)

dtaWo <- read.csv('../data/worcesterGen.csv', sep=';')
attach(dtaWo)
dtaWo$mayor.total <- (Mayor.Dem+Mayor.Rep+Mayor.None)
dtaWo$dem.mayor.pct <- Mayor.Dem/(Mayor.Dem+Mayor.Rep+Mayor.None)
dtaWo$rep.mayor.pct <- Mayor.Rep/(Mayor.Dem+Mayor.Rep+Mayor.None)
dtaWo$none.mayor.pct <- Mayor.None/(Mayor.Dem+Mayor.Rep+Mayor.None)
dtaWo$yes.pr.pct <- PlanE.Yes/(PlanE.Yes+PlanE.No+PlanE.None)
dtaWo$no.pr.pct <- PlanE.No/(PlanE.Yes+PlanE.No+PlanE.None)
dtaWo$none.pr.pct <- PlanE.None/(PlanE.Yes+PlanE.No+PlanE.None)
dtaWo$pr.total <- (PlanE.Yes+PlanE.No+PlanE.None)
detach(dtaWo)

dtaWo$rPercent <- 100*dtaWo$rep.mayor.pct
dtaWo$yesPR <- 100*dtaWo$yes.pr.pct

dtaWo$plotChr <- rep(NA, nrow(dtaWo))
dtaWo$plotChr[dtaWo$Ward %in% c(1, 10)] <- 15 # Republican wards
dtaWo$plotChr[dtaWo$Ward %in% c(8)] <- 3 # Academic wards
dtaWo$plotChr[dtaWo$Ward %in% c(5)] <- 17 # Labor wards
dtaWo$plotChr[dtaWo$Ward %in% c(3, 4, 6, 7)] <- 1 # Manufacturing wards
dtaWo$plotChr[dtaWo$Ward %in% c(2, 9)] <- 2 # not named

pdf('../graphics/worcesterref.pdf')
plot(dtaWo$rPercent, dtaWo$yesPR, xlim=c(0, 100), ylim=c(0, 100), axes=F, pch=dtaWo$plotChr, xlab="Percent for Winslow (R)", ylab="Percent for charter")
abline(0,1)
axis(1, tick=F)
axis(2, tick=F, las=2)
legend("bottomright", legend=c('Republican (1, 10)', 'Plurality Irish-Catholic (3, 4, 6, 7)', 'Industrial (5)', 'Academic (8)', 'Party-competitive (2, 9)'), pch=c(15, 1, 17, 3, 2), bty='n', title="Binstock (1960) description of ward:", title.adj=0)
dev.off()