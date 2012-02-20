library(Amelia)
data(africa)
a.out <- amelia(x = africa, cs = "country", ts = "year",
                logs = "gdp_pc")
a.out2 <- transform(a.out, lgdppc = log(gdp_pc), newinfl = infl*100)
identical(a.out2$transform.vars, c("lgdppc", "newinfl"))
a.out2 <- transform(a.out2, newclib = civlib *100, newtrade =
                    trade/100)
identical(a.out2$transform.vars, c("lgdppc", "newinfl", "newclib", "newtrade"))

summary(a.out2)
summary(transform(a.out, lgdppc = log(gdp_pc), newinfl = infl*100))
a.out3 <- amelia(a.out2)
a.out4 <- amelia(a.out)