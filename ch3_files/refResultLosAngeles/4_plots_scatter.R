load("props.Rdata")

newNames <- c(
	"Municipal ownership\nof utilities (PCC)",
	"Regulate private utilities\n(PCC)",
	"LA River bed - regulate use\nand condemnatons in (PCC)",
	"Apply state home-rule law\nto LA (PCC)",
	"Exec. agencies responsible\nto council (PCC)",
	"Set city-council\nsalaries (PCC)",
	"Remove incumbent government (PCC)",
	"Proportional representation\n(PCC)",
	"Appointed harbor commission,\ncouncil sets salaries (Admin)",
	"Fix bonds of city officers (Admin)",
	"Sale of land along aqueduct (CC)",
	"Pension and insurance - firemen (CC)",
	"Pension and insurance - police (CC)",
	"Prohibit municipally-owned newspaper (CC)",
	"Private biz in city parks,\ncommission fixes salaries (CC)",
	"Elected harbor commission,\nrailroad, fixed salaries (CC)",
	"Districts plus at-large (CC)",
	"Raise electeds' salaries (CC)",
	"Regulate the dance halls (Other)"
)

omit.idx <- which(names(props)!="terminate.admin")[1:7] # just the PCC amendments

# out.lm<- list(NA)
# for (i in 1:length(omit.idx)){
	# out.lm[[i]] <- lm(props[,omit.idx[i]] ~ props$terminate.admin)
# }
# out.coefs <- do.call(rbind, lapply(out.lm, coef))

# omit.idx <- omit.idx[order(out.cor, decreasing=T)]

pdf("scatterPlots.pdf", width=9, height=5)
par(mfrow=c(2,4))
for (i in 1:length(omit.idx)){
	plot(props$terminate.admin, props[,omit.idx[i]], ylim=c(0,1), xlim=c(0,1), main=newNames[omit.idx[i]], axes=F, xlab="", ylab="", pch=".")
	abline(0,1)
}
dev.off()