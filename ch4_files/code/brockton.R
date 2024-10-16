
### Figure 4: Brockton scatterplot

dtaB <- read.csv('../data/brockton.csv')


dtaB$pctFor <- 100*dtaB$Yes/(dtaB$Yes+dtaB$No) #+dtaB$Blank
dtaB$pctRepMayor <- 100*dtaB$Peterson.R/(dtaB$Peterson.R+dtaB$Lucey.D)

pdf('../graphics/brocktonref.pdf')
plot(dtaB$pctRepMayor, dtaB$pctFor, xlim=c(20,80), ylim=c(20,80), axes=F, pch=16, xlab="Percent for Peterson (R)", ylab="Percent for charter")
abline(0,1)
axis(1, tick=F)
axis(2, tick=F, las=2)
dev.off()

# out.lm.brock <- lm(pctFor ~ pctRepMayor, data=dtaB)

# plot(dtaB$pctRepMayor, dtaB$pctFor, pch=16, main="Brockton 1955: Support for at-large plurality charter\nby support for Republican mayoral candidate", xlab="Proportion for Peterson (R)", ylab="Proportion for charter", bty="n", sub=paste0("B=", round(coef(summary(out.lm.brock))[2, "Estimate"], 2), " (",round(coef(summary(out.lm.brock))[2, "Std. Error"], 2),"), R2=", round(summary(out.lm.brock)$r.squared, 2)))

# abline(h=0.5, lty=3)
# abline(out.lm.brock, lty=1)
# text(x=0.38, y=0.51, "Win threshold", lty=2)

# dev.off()
