library(pscl)
library(oc)
# library(wnominate)

source("scaling_functions.R")

# load('../repeal_stv_june2018/data/rcs.cincy.Rdata')
load('../data/rcs.cincy.Rdata')

rcs.cincy2 <- list(NA)
for (i in 1:length(rcs.cincy)){
	rownames(rcs.cincy[[i]]$votes) <- paste0(rownames(rcs.cincy[[i]]$votes), ' (', rcs.cincy[[i]]$legis.data$party, ')')
}

seq(1931, 1957, 2)[grep('Taft', lapply(rcs.cincy, function(x) rownames(x$votes)))]

## if member appears in data more than once, number them in their rowname

rownames(rcs.cincy[[2]]$votes)[rownames(rcs.cincy[[2]]$votes)=="Replaces John Druffel\nDunlap (C-d)"] <- "Dunlap (C-d)"
rownames(rcs.cincy[[4]]$votes)[rownames(rcs.cincy[[4]]$votes)=="Replaces James A. Wilson at some point\nClark (C-d)"] <- "Clark (C-d)"
rownames(rcs.cincy[[6]]$votes)[rownames(rcs.cincy[[6]]$votes)=="Bigelow (P)"] <- "Bigelow2 (P)"
rownames(rcs.cincy[[10]]$votes)[rownames(rcs.cincy[[10]]$votes)=="Taft (C-r)"] <- "Taft2 (C-r)"
rownames(rcs.cincy[[11]]$votes)[rownames(rcs.cincy[[11]]$votes)=="Taft (C-r)"] <- "Taft2 (C-r)"
rownames(rcs.cincy[[14]]$votes)[rownames(rcs.cincy[[14]]$votes)=="Taft (C-r)"] <- "Taft3 (C-r)"

all.cincy <- combineRCs(rcs.cincy, forDynIRT=F)

# ### fix "Replaces..." in rownames
# rownames(all.cincy) <- substr(rownames(all.cincy), regexpr('\\n', rownames(all.cincy))+1, nchar(rownames(all.cincy)))

all.cincy.firstyr.idx <- apply(all.cincy, 1, function(x) which(x %in% c("1", "0"))[[1]])
all.cincy.firstyr <- unlist(lapply(all.cincy.firstyr.idx, function(x) substr(dimnames(all.cincy)[[2]][[x]], 1, 4)))

all.cincy.party <- substr(rownames(all.cincy), regexpr('\\(', rownames(all.cincy))+1, regexpr('\\)', rownames(all.cincy))-1)

all.cincy.rcs <- rollcall(all.cincy, legis.names=rownames(all.cincy))
all.cincy.rcs$legis.data$party <- all.cincy.party
all.cincy.rcs$legis.data$first.year <- as.numeric(all.cincy.firstyr)


set.seed(1776)

oc.cincy <- oc(all.cincy.rcs, polarity=c("StewartP (R)", "WilsonR (C-r)"), minvotes=20)

plot.OCcoords(oc.cincy)

### compute decade average cutting angles

oc.cincy$legislators$font <- NA
# oc.cincy$legislators$font[grep("193", oc.cincy$legislators$first.year)] <- 1
# oc.cincy$legislators$font[grep("194", oc.cincy$legislators$first.year)] <- 2
# oc.cincy$legislators$font[grep("195", oc.cincy$legislators$first.year)] <- 3
# oc.cincy$legislators$font[oc.cincy$legislators$first.year<=1951] <- 1
oc.cincy$legislators$font[oc.cincy$legislators$first.year>1936] <- 3
oc.cincy$legislators$font[oc.cincy$legislators$first.year>1951] <- 2

pdf('../graphics/ideal_points_cincy_NAMES.pdf')
plot(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, pch=NA, main="Cincinnati, 1929-57", xlab="1st dimension", ylab="2nd dimension", axes=F, ylim=c(-1, 1), xlim=c(-1.1, 1))
text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=rownames(oc.cincy$legislators), cex=2/3, font=oc.cincy$legislators$font)
# text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=oc.cincy$legislators$party, cex=1, font=oc.cincy$legislators$font)
axis(1, tick=F)
axis(2, tick=F, las=2)
mtext("Italics = first served 1937 or later\nBold = first served 1951 or later", line=-1)
dev.off()

pdf('../graphics/fig7.1_ideal_points_cincy.pdf')

plot(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, pch=NA, main="Cincinnati, 1929-57", xlab="1st dimension", ylab="2nd dimension", axes=F, ylim=c(-1, 1), xlim=c(-1.1, 1))
#text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=rownames(oc.cincy$legislators), cex=2/3, font=oc.cincy$legislators$font)
text(oc.cincy$legislators$coord1D, oc.cincy$legislators$coord2D, labels=oc.cincy$legislators$party, cex=2/3, font=oc.cincy$legislators$font)
axis(1, tick=F)
axis(2, tick=F, las=2)
mtext("Italics = first served 1938 or later\nBold = first served 1952 or later", line=-1)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Jordan (C-d)"], x0=-1, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Jordan (C-d)"], y0=-0.4, length=0, lty=2)
text(-1, -0.425, "Jordan", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Berry (C-d)"], x0=-0.7, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Berry (C-d)"], y0=-0.7, length=0, lty=2)
text(-0.7, -0.725, "Berry II", cex=2/3, font=3)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Berry (C-r)"], x0=-0.7, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Berry (C-r)"], y0=0.1, length=0, lty=2)
text(-0.7, 0.125, "Berry I", cex=2/3, font=3)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Dolbey (C-r)"], x0=-0.5, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Dolbey (C-r)"], y0=-0.8, length=0, lty=2)
text(-0.5, -0.825, "Dolbey", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Taft3 (C-r)"], x0=-0.35, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Taft3 (C-r)"], y0=-0.9, length=0, lty=2)
text(-0.35, -0.925, "Taft III", cex=2/3, font=2)

arrows(x1=oc.cincy$legislators$coord1D[rownames(oc.cincy$legislators)=="Rich (R)"], x0=0.6, y1=oc.cincy$legislators$coord2D[rownames(oc.cincy$legislators)=="Rich (R)"], y0=0.1, length=0, lty=2)
text(0.55, 0.1, "Rich", cex=2/3, font=3)

dev.off()