library(xtable)

d <- read.csv('../data/slate_data.csv', stringsAsFactors=F)

print(xtable(d[,1:ncol(d)-1], caption='PR cities with and without pre-election "good-government" slates', label='tab:theslates'), include.rownames=F)

### average duration with and without good-government -- added 6 July 2023

# d$Repeal[d$City=="Cambridge"] <- 1962

d$duration <- d$Repeal-d$Adopt

d$slate.dummy <- ifelse(d$Slate=="None", 0, 1)

d$slate.dummy[d$City=="Cleveland"] <- 0 # because it adopted slates late, then only used them to endorse already-existing candidates

d$slate.dummy[d$City=="New York"] <- 0 # because it didn't have the same kind of slate

aggregate(duration ~ slate.dummy, data=d, FUN="mean")

aggregate(duration ~ slate.dummy, data=d, FUN="max")
