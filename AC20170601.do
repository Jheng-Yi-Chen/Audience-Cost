
set more off

*cwmid = dependence variable*
tab cwmid
summarize cwmid

*iec, liec, eiec = independence variable*
tab liec1
tab liec2
recode liec1(0 -999=.a)
recode liec2(0 -999=.a)
tab eiec1
tab eiec2
recode eiec1(0 -999=.a)
recode eiec2(0 -999=.a)
tab iec1
tab iec2
recode iec1(-1998 -998=.a)
recode iec2(-1998 -998=.a)
gen iec = min(iec1, iec2)
tab iec
gen liec = min(liec1, liec2)
gen eiec = min(eiec1, eiec2)
tab liec
tab eiec
summarize liec
summarize eiec
gen liecsquare = liec*liec
gen eiecsquare = eiec*eiec
tab liecsquare
tab eiecsquare
summarize liecsquare
summarize eiecsquare

*majow = control variable*
tab majpow1
tab majpow2
gen majpow = (majpow1+majpow2)
tab majpow

*cap = control variable*
gen cap = min(cap_1, cap_2)/max(cap_1, cap_2)
summarize cap

*alliance = control variable*
*defense=1, neutrality=2, entente=3, no agreement=4*
tab alliance
recode alliance (1 2 3 = 1) (4 = 2)
tab alliance

*polity or system = control variable*
tab polity21
tab polity22
gen polity = min(polity21, polity22)
tab polity
summarize polity

*recode polity to democracy and authoritarianism*
recode polity21 (-10/-6 = 1) (-5/5 = 2) (6/10 = 3), gen (democracy1)
recode polity22 (-10/-6 = 1) (-5/5 = 2) (6/10 = 3), gen (democracy2)
tab democracy1
tab democracy2
tab democracy1 polity21
tab democracy2 polity22
gen politydyad = 0
*both authoritarianism*
replace politydyad = 1 if (democracy1 == 1 & democracy2 == 1)
*authoritarianism and democracy*
replace politydyad = 2 if (democracy1 == 3 & democracy2 == 1) | (democracy1 == 1 & democracy2 == 3)
*both democracy*
replace politydyad = 3 if (democracy1 == 3 & democracy2 == 3)
*authoritarianism and non*
replace politydyad = 4 if (democracy1 == 2 & democracy2 == 1) | (democracy1 == 1 & democracy2 == 2)
*non and democracy*
replace politydyad = 5 if (democracy1 == 3 & democracy2 == 2) | (democracy1 == 2 & democracy2 == 3)
*both non*
replace politydyad = 6 if (democracy1 == 2 & democracy2 == 2)
tab politydyad
tab democracy1 democracy2
tab politydyad democracy1
tab politydyad democracy2

*distance = control variable*
gen lndistance = ln(distance)
summarize lndistance

*rgdpnapop = control variable*
summarize gdp1
summarize gdp2
*gen gdp = min(gdp1, gdp2)
*gen lngdp = ln(gdp)
summarize lngdp

*maj*
tab maj1
tab maj2
gen minmaj = min(maj1, maj2)
tab minmaj
summarize minmaj

*frac*
tab frac1
tab frac2
gen frac = max(frac1, frac2)
tab frac
recode frac (1=.a)
summarize frac

*sample distribution*
histogram liec
twoway histogram liec
twoway kdensity liec
histogram liec, percent
twoway (histogram liec, frequency) (kdensity liec, area(466054))
histogram eiec, percent
twoway (histogram frac, percent) (kdensity frac, area(433517))
twoway kdensity frac
histogram frac, percent
histogram minmaj, percent

count if cwmid == 1 & majpow == 0
count if cwmid == 1& majpow== 1
count if cwmid == 1& majpow== 2
count if cwmid == 0 & majpow== 0
count if cwmid == 0 & majpow== 1
count if cwmid == 0 & majpow== 2
table cwmid majpow
tabulate cwmid majpow

*recode liec1(0 -999=8)
*recode liec2(0 -999=8)
*recode eiec1(0 -999=8)
*recode eiec2(0 -999=8)
*tab eiec1 eiec2
*tab liec1 liec2

*corr*
cor iec liec eiec polity politydyad
pwcorr cwmid liec1 liec2 eiec1 eiec2 iec liec eiec polity21 polity22 polity

****************************************************************************************************

*time-series cross-dyads model*
*gee model*
gen dyadyear = (ccode1*10000000)+(ccode2*10000)+year
xtset dyadyear

*drop politydyad
drop if politydyad == 0
drop if politydyad == 4
drop if politydyad == 5
drop if politydyad == 6
*drop if politydyad == 1
*drop if politydyad == 2
*drop if politydyad == 3

*minmaj*
xtgee cwmid minmaj i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid minmaj i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit) nolog
estimates store m10, title(model 10)
xtgee cwmid minmaj eiec i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid minmaj eiec i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit) nolog
estimates store m7, title(model 7)

*frac*
xtgee cwmid frac i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid frac i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit) nolog
estimates store m2, title(model 2)
xtgee cwmid frac eiec i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid frac eiec i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit) nolog
estimates store m8, title(model 8)

*iec*
xtgee cwmid iec i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid iec i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit) nolog
estimates store m1, title(model 1)

*liec and eiec*
xtgee cwmid liec eiec i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid liec eiec i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit) nolog
estimates store m9, title(model 9)

*eiec*
xtgee cwmid eiec i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid eiec i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit)
estimates store m6, title(model 6)
xtgee cwmid eiec i.majpow cap ib2.alliance lndistance lngdp, family(binomial) link(logit)
estimates store m4, title(model 4)

esttab m8 using AC5.csv, replace se(3) b(3) star(* 0.05 ** 0.01 *** 0.001)

*liec*
xtgee cwmid liec i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog
xtgee cwmid liec i.majpow cap ib2.alliance ib2.politydyad lndistance lngdp, family(binomial) link(logit)
estimates store m5, title(model 5)
xtgee cwmid liec i.majpow cap ib2.alliance lndistance lngdp, family(binomial) link(logit)
estimates store m7, title(model 7)

esttab m7 using AC9.csv, replace se(3) b(3) star(* 0.05 ** 0.01 *** 0.001)
esttab m10 m7 m2 m8 m1 m9 m6 m5 using AC10.csv, replace se(3) b(3) star(* 0.05 ** 0.01 *** 0.001)

margins, dydx(*)

*relogit*
help relogit

relogit cwmid minmaj cap polity lndistance lngdp
relogit cwmid frac cap polity lndistance lngdp
relogit cwmid liec eiec cap polity lndistance lngdp

xtgee cwmid eiec cap polity lndistance lngdp, family(binomial) link(logit) nolog
estimates store m7, title(model 7)
relogit cwmid eiec cap polity lndistance lngdp
estimates store m6, title(model 6)
esttab m7 m6 using AC3.csv, replace se(3) b(3) star(* 0.05 ** 0.01 *** 0.001)

relogit cwmid minmaj i.majpow cap ib2.alliance polity lndistance lngdp
relogit cwmid frac i.majpow cap ib2.alliance polity lndistance lngdp
relogit cwmid liec eiec i.majpow cap ib2.alliance polity lndistance lngdp
relogit cwmid eiec i.majpow cap ib2.alliance polity lndistance lngdp

*square*
xtgee cwmid liec liecsquare eiec eiecsquare i.majpow cap ib2.alliance polity lndistance lngdp, family(binomial) link(logit) nolog

*graph*
mgen, atmeans at(eiec=(1(0.5)7)majpow=2 alliance=2)stub(QQ7)predlab(行政權)
mgen, atmeans at(liec=(1(0.5)7)majpow=2 alliance=2)stub(QQ8)predlab(立法權)
graph twoway connected QQ7pr QQ7eiec

*without alliance*
xtgee cwmid liec eiec i.majpow cap polity lndistance lngdp, family(binomial) link(logit) nolog

mgen, atmeans at(eiec=(1(0.5)7)majpow=2)stub(QQ7)predlab(行政權)
mgen, atmeans at(liec=(1(0.5)7)majpow=2)stub(QQ8)predlab(立法權)
graph twoway connected QQ7pr QQ8pr QQ7eiec

*do not use*
*mgen, atmeans at(eiec=(1(0.5)7)alliance=2 majpow=0)stub(QQ1)predlab(皆不是強權)
*mgen, atmeans at(eiec=(1(0.5)7)alliance=2 majpow=1)stub(QQ2)predlab(一國是強權)
*mgen, atmeans at(eiec=(1(0.5)7)alliance=2 majpow=2)stub(QQ3)predlab(兩國皆強權)
*graph twoway connected QQ1pr QQ2pr QQ3pr QQ1eiec
*mgen, atmeans at(liec=(1(0.5)7)alliance=2 majpow=0)stub(QQ4)predlab(皆不是強權)
*mgen, atmeans at(liec=(1(0.5)7)alliance=2 majpow=1)stub(QQ5)predlab(一國是強權)
*mgen, atmeans at(liec=(1(0.5)7)alliance=2 majpow=2)stub(QQ6)predlab(兩國皆強權)
*graph twoway connected QQ4pr QQ5pr QQ6pr QQ4liec

*mgen, atmeans at(eiec=(1(0.5)7)majpow=2 alliance=2 ccode1=2 ccode2==710)stub(QQ9)predlab(行政權)
*mgen, atmeans at(liec=(1(0.5)7)majpow=2 alliance=2 ccode1=2 ccode2==710)stub(QQ10)predlab(立法權)
*graph twoway connected QQ7pr QQ8pr QQ7eiec
*graph scatter (cwmid)ccode==2 ccode==710

*mtable, at(eiec=(1 2 3 4 5 6 7)) atmeans estname(pr_cwmid)
*mtable, at(liec=(1 2 3 4 5 6 7)) atmeans estname(pr_cwmid)

*margins i.majpow, at(liec=(1(0.5)7)) atmeans
*marginsplot
*margins i.majpow, at(eiec=(1(0.5)7)) atmeans
*marginsplot

*xtgee cwmid iec i.majpow cap i.alliance lndistance gdp, family(binomial) link(logit) nolog
*xtgee cwmid liec eiec i.majpow cap i.alliance lndistance gdp, family(binomial) link(logit) nolog

*logistic model*
gen dyadyear = (ccode1*10000000)+(ccode2*10000)+year
xtset dyadyear

xtlogit cwmid iec i.majpow cap i.alliance parreg contig rgdpnapop, nolog
xtlogit cwmid liec eiec i.majpow cap i.alliance parreg contig rgdpnapop, nolog

