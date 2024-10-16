### run EI

library(eiPack)

load("la.ref.data.Rdata")

attach(la.ref.data)
d <- cbind.data.frame(q8yes, q8no, q7yes, q7no)
detach(la.ref.data)


########## OLD

### run EI

library(eiPack)

load("la.ref.data.Rdata")

attach(la.ref.data)

d1 <- cbind(q8yes, q8no, q8blank, q1yes, q1no, q1blank)

d2 <- cbind(q8yes, q8no, q8blank, q2yes, q2no, q2blank)

d3 <- cbind(q8yes, q8no, q8blank, q3yes, q3no, q3blank)

d4 <- cbind(q8yes, q8no, q8blank, q4yes, q4no, q4blank)

d5 <- cbind(q8yes, q8no, q8blank, q5yes, q5no, q5blank)

d6 <- cbind(q8yes, q8no, q8blank, q6yes, q6no, q6blank)

d7 <- cbind(q8yes, q8no, q8blank, q7yes, q7no, q7blank)

d9 <- cbind(q8yes, q8no, q8blank, q9yes, q9no, q9blank)

d10 <- cbind(q8yes, q8no, q8blank, q10yes, q10no, q10blank)

d11 <- cbind(q8yes, q8no, q8blank, q11yes, q11no, q11blank)

d12 <- cbind(q8yes, q8no, q8blank, q12yes, q12no, q12blank)

d13 <- cbind(q8yes, q8no, q8blank, q13yes, q13no, q13blank)

d14 <- cbind(q8yes, q8no, q8blank, q14yes, q14no, q14blank)

d15 <- cbind(q8yes, q8no, q8blank, q15yes, q15no, q15blank)

d16 <- cbind(q8yes, q8no, q8blank, q16yes, q16no, q16blank)

d17 <- cbind(q8yes, q8no, q8blank, q17yes, q17no, q17blank)

d18 <- cbind(q8yes, q8no, q8blank, q18yes, q18no, q18blank)

d19 <- cbind(q8yes, q8no, q8blank, q19yes, q19no, q19blank)

dlist <- list(d1, d2, d3, d4, d5, d6, d7, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19)

detach(la.ref.data)

tunes <- list(NA)
for (i in 1:length(dlist)){
	tunes[[i]] <- tuneMD(cbind(dlist[[i]][,1:3], dlist[[i]][,4:6]), data=dlist[[i]], ntunes=1, totaldraws=10)
}


tune.nocov <- tuneMD(cbind(q8yes, q8no, q8blank) ~ cbind(q1yes, q1no, q1blank, q2yes, q2no, q2blank, q3yes, q3no, q3blank, q4yes, q4no, q4blank, q5yes, q5no, q5blank, q6yes, q6no, q6blank, q7yes, q7no, q7blank, q9yes, q9no, q9blank, q10yes, q10no, q10blank, q11yes, q11no, q11blank, q12yes, q12no, q12blank, q13yes, q13no, q13blank, q14yes, q14no, q14blank, q15yes, q15no, q15blank, q16yes, q16no, q16blank, q17yes, q17no, q17blank, q18yes, q18no, q18blank, q19yes, q19no, q19blank), data=la.ref.data, ntunes=1, totaldraws=10)

# out.nocov <- ei.MD.bayes(cbind(q5yes, q5no, q5blank) ~ cbind(clinton, trump, johnson, stein, other, BLANK), covariate=NULL, data=maine.data, tune.list = tune.nocov, ret.beta="s", ret.mcmc=T)

out.nocov <- ei.MD.bayes(cbind(proportional.yes, proportional.no, proportional.blank) ~ cbind(muni.own.yes, muni.reg.yes, la.river.yes, home.rule.yes, responsible.boards.yes, meetings.salaries.yes, primaries.terminate.yes, harbor.district.yes, bonds.yes, sell.property.yes, firemen.insure.yes, police.insure.yes, no.muni.newspaper.yes, parks.salaries.yes, harbor.district.2.yes, districts.yes, all.salaries.yes, dance.halls.yes, muni.own.no, muni.reg.no, la.river.no, home.rule.no, responsible.boards.no, meetings.salaries.no, primaries.terminate.no, harbor.district.no, bonds.no, sell.property.no, firemen.insure.no, police.insure.no, no.muni.newspaper.no, parks.salaries.no, harbor.district.2.no, districts.no, all.salaries.no, dance.halls.no, muni.own.blank, muni.reg.blank, la.river.blank, home.rule.blank, responsible.boards.blank, meetings.salaries.blank, primaries.terminate.blank, harbor.district.blank, bonds.blank, sell.property.blank, firemen.insure.blank, police.insure.blank, no.muni.newspaper.blank, parks.salaries.blank, harbor.district.2.blank, districts.blank, all.salaries.blank, dance.halls.blank), covariate=NULL, data=la.ref.data, tune.list = tune.nocov, ret.beta="d", ret.mcmc=T, sample=50000, thin=100, burnin=20000)

save(tune.nocov, out.nocov, file="losAngelesResultEI.Rdata")