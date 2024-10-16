### Figure 5: Waterbury scatterplot

dtaW <- read.csv('../data/waterbury.csv')
attach(dtaW)
dtaW$pctFor <- PR.yes/(PR.yes+PR.no)
dtaW$pctRepMayor <- Foster.R.Mayor/(Foster.R.Mayor+Scully.D.Mayor+Ring.S.Mayor+Scully.ID.Mayor)
dtaW$pctDemMayor <- Scully.D.Mayor/(Foster.R.Mayor+Scully.D.Mayor+Ring.S.Mayor+Scully.ID.Mayor)
dtaW$pctIndDemMayor <- Scully.ID.Mayor/(Foster.R.Mayor+Scully.D.Mayor+Ring.S.Mayor+Scully.ID.Mayor)
dtaW$pctSocMayor <- Ring.S.Mayor/(Foster.R.Mayor+Scully.D.Mayor+Ring.S.Mayor+Scully.ID.Mayor)
dtaW$pctAllDemMayor <- (Scully.ID.Mayor+Scully.D.Mayor)/(Foster.R.Mayor+Scully.D.Mayor+Ring.S.Mayor+Scully.ID.Mayor)
detach(dtaW)

dtaW$pct.r <- 100*dtaW$pctRepMayor
dtaW$pctPR <- 100*dtaW$pctFor

pdf('../graphics/waterburyref.pdf')
plot(dtaW$pct.r, dtaW$pctPR, xlim=c(20, 80), ylim=c(20, 80), axes=F, pch=16, xlab="Percent for Foster (R), Oct. 1939", ylab="Percent for charter, Nov. 1939")
abline(0,1)
axis(1, tick=F)
axis(2, tick=F, las=2)
dev.off()