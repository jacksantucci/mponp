d <- read.csv('../data/refs.csv', sep='\t')

d$winner <- ifelse(d$for.pct>0.5, 1, 0)

d$for.pct2 <- 100*d$for.pct

pdf('../graphics/stv_yes_distribution.pdf')
hist(d$for.pct2, axes=F, breaks=10*2:10, col="white", xlab='Percent supporting STV charter', main="")
axis(1, tick=F, at=c(20, 30, 40, 50, 60, 70, 80, 90, 100))
axis(2, tick=F, las=2)
abline(v=50, lwd=3)
text(70, 11, "Winning (N=26)")
text(30, 11, "Losing (N=24)")
dev.off()

summary(d$for.pct2[d$winner==1])

sd(d$for.pct2[d$winner==1], na.rm=T)

summary(d$for.pct2[d$winner==0])

sd(d$for.pct2[d$winner==0], na.rm=T)

t.test(d$for.pct2[d$winner==1], d$for.pct2[d$winner==0], alternative="greater")

## inspect winners

foo <- d[d$winner==1, c(5,6,9)]

foo <- na.omit(foo)

table(foo$city)