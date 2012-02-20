library(Amelia)
data(africa)

a.out <- amelia(x = africa, cs = "country", ts = "year",
                logs = "gdp_pc")
a.out2 <- transform(a.out, lgdppc = log(gdp_pc), newinfl = infl*100)
a.out2 <- transform(a.out2, newclib = civlib *100, newtrade =
                    trade/100)

summary(a.out2)
summary(transform(a.out, lgdppc = log(gdp_pc), newinfl = infl*100))
a.out3 <- amelia(a.out2)
a.out4 <- amelia(a.out)


africa <- transform(africa, ivar = gdp_pc * trade)
a.out <- amelia(x = africa, cs = "country", ts = "year",
                logs = "gdp_pc")
a.out2 <- transform(a.out, ivar = gdp_pc *trade, lgdppc = log(gdp_pc))
summary(a.out2)
a.out3 <- amelia(a.out2)

compare.density
compare.density(a.out2, "lgdppc")
