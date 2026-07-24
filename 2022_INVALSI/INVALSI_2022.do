

		
******************************+********************
******************************+********************
********************** ICCS 2016 ******************
***************************************************
***************************************************
	
use "/Users/daniel/Dropbox (MIDE)/GEMR 2019/Data/Merge/ICCS_School_Teachers_Students_Europe_LA.dta", clear
tab idcntry, nol


tab1 IS3G31I IS3G31B IS3G30J IS3G30K
tab1 IS3G31I IS3G31B IS3G30J IS3G30K, nol

recode IS3G30C (3 4=0) (1 2=1), gen(marcha)
recode IS3G30K (3 4=0) (1 2=1), gen(ocupa)
recode IS3G30J (3 4=0) (1 2=1), gen(trafico)
recode IS3G30I (3 4=0) (1 2=1), gen(rayar)
*twoway lfitci IS3G30K IS3G30J 

table marcha if idcntry==152, c(mean ocupa sd ocupa)

** Index **
revrs  IS3G31I IS3G30J IS3G30K
egen radical=rowmean(revIS3G31I revIS3G30J revIS3G30K)
sum radical
sum radical, det

revrs  IS3G31B IS3G30C IS3G30D
egen pacific=rowmean(revIS3G31B revIS3G30C revIS3G30D)
sum pacific
sum pacific, det


** GINI index
gen gini=.
replace gini=25.3 if idcntry==956 // Belgium
replace gini=38.7 if idcntry==100 // Bulgaria
replace gini=45.8 if idcntry==152 // Chile
replace gini=47.7 if idcntry==170 // Colombia
replace gini=25.7 if idcntry==208 // Dinamarca
replace gini=28.8 if idcntry==276001 //Alemania
replace gini=43.8 if idcntry==214 //Dominicana
replace gini=30.8 if idcntry==233 // Estonia
replace gini=25.7 if idcntry==246 // Finlandia
replace gini=44.2 if idcntry==344 // Hong Knoing
replace gini=31.7 if idcntry==191 // Croacia
replace gini=35.3 if idcntry==380 // Italia
replace gini=29.9 if idcntry==410 // Korea
replace gini=35.5 if idcntry==440 // Lituania
replace gini=36.6 if idcntry==428 // Latvia
replace gini=43.9 if idcntry==484 // Mexico
replace gini=27 if idcntry==470 // Malta
replace gini=27.8 if idcntry==528 // Holoanda
replace gini=25.9 if idcntry==578 // Noruega
replace gini=44.7 if idcntry==604 // Përu
replace gini=35 if idcntry==643 // Rusia
replace gini=25.2 if idcntry==705 // SVN
replace gini=27.2 if idcntry==752 // Sweeden
replace gini=30 if idcntry==158 // Taiwan

** Variables
tab S_GENDER
tab S_GENDER, nol
tab S_HISCED, nol

recode S_HISCED (0 1 2 3=0) (4=1), gen(University)

gen internet= IS3G14G

gquantiles pct_know = PV1CIV, xtile nquantiles(100) by(idcntry)

gen study=2016

gen year_study=2016 + idcntry

*keep radical marcha ocupa trafico rayar gini PV1CIV pct_know S_GENDER S_HISCED University internet idcntry idschool study year_study totwgts jkzones

*save "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/subset_ICCS_2016.dta", replace

*** Estimation of everage by country
table idcntry [aw=totwgts], c(mean marcha mean rayar mean trafico mean ocupa)

svyset [pweight = totwgts], strata(jkzones) psu(jkreps)
svydes
svy: mean marcha ocupa trafico rayar radical pacific

svy: mean marcha, over(idcntry)
svy: mean rayar, over(idcntry)
svy: mean trafico, over(idcntry)
svy: mean ocupa, over(idcntry)
svy: mean radical, over(idcntry)
svy: mean pacific, over(idcntry)


graph hbar marcha [aw=totwgts], over(idcntry, gap (*.2) sort(marcha) label(nolabels)) ///			
				ytitle(" " "Disposition to march 2016") /// 
				intensity(40) ///
				yline(0.624) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/marching16.png", replace
		

graph hbar ocupa [aw=totwgts], over(idcntry, gap (*.2) sort(ocupa) label(nolabels)) ///			
				ytitle(" " "Disposition to Occupying buldings 2016") /// 
				intensity(40) ///
				yline(0.241) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ocupying16.png", replace
		
				
graph hbar trafico [aw=totwgts], over(idcntry, gap (*.2) sort(trafico) label(nolabels)) ///			
				ytitle(" " "Disposition to Blocking the traffic 2016") /// 
				intensity(40) ///
				yline(0.255) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/blocking16.png", replace
		
				
graph hbar rayar [aw=totwgts], over(idcntry, gap (*.2) sort(rayar) label(nolabels)) ///			
				ytitle(" " "Disposition to Spray walls 2016") /// 
				intensity(40) ///
				yline(0.297) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/spraying16.png", replace
			
graph hbar radical [aw=totwgts], over(idcntry, gap (*.2) sort(radical) label(nolabels)) ///			
				ytitle(" " "Mean's disposition to radical actions 2016") /// 
				intensity(40) ///
				yline(2.277) ///
				ylabel(0(1)4, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/radical16.png", replace

graph hbar pacific [aw=totwgts], over(idcntry, gap (*.2) sort(pacific) label(nolabels)) ///			
				ytitle(" " "Mean's disposition to pacific actions 2016") /// 
				intensity(40) ///
				yline(2.947) ///
				ylabel(0(1)4, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/pacific16.png", replace
	
				



bysort idcntry: asgen marcha_mean = marcha, w(totwgts)
bysort idcntry: asgen ocupa_mean= ocupa, w(totwgts)
bysort idcntry: asgen trafico_mean= trafico, w(totwgts)
bysort idcntry: asgen rayar_mean= rayar, w(totwgts)
bysort idcntry: asgen radical_mean= radical, w(totwgts)
bysort idcntry: asgen pacific_mean= pacific, w(totwgts)


**Correlaciones
preserve
collapse (mean) marcha ocupa trafico rayar radical pacific gini, by(idcntry)
pwcorr gini marcha ocupa trafic rayar radical pacific
restore


*** Gini y protestas
twoway (scatter marcha_mean gini, mlabel(country)) (lfitci marcha_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to peaceful marches 2016)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.73")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_marcha16.png", replace
		
twoway (scatter trafico_mean gini, mlabel(country)) (lfitci trafico_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to blocking the traffic 2016)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.78")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_trafico16.png", replace
		
twoway (scatter ocupa_mean gini, mlabel(country)) (lfitci ocupa_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to occupy building 2016)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.74")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_ocupa16.png", replace
		
twoway (scatter rayar_mean gini, mlabel(country)) (lfitci rayar_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to spray walls 2016)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.73")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_rayar16.png", replace

twoway (scatter radical_mean gini, mlabel(country)) (lfitci radical_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to radical actions 2016)") ///
		xtitle("Gini Index") ///
		ylabel(1(0.5)4) note("Correlación de Pearson= 0.719")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_radical16.png", replace

		
twoway (scatter pacific_mean gini, mlabel(country)) (lfitci pacific_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to pacific actions 2016)") ///
		xtitle("Gini Index") ///
		ylabel(1(0.5)4) note("Correlación de Pearson= 0.667")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_pacific16.png", replace


****  Models: Disposition to radical actions ***
xtmixed radical ||idcntry: ||idschool:
estat icc

xtmixed radical gini ||idcntry: ||idschool:

margins , at(gini=(25 45))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2016)") ///
		xtitle("Gini Index") ///
		xlabel(25 "Low Gini 0.25" 45 "High Gini 0.45") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ginieffect_radical16.png", replace
		
		
xtmixed radical gini University  ||idcntry: ||idschool:

margins , at(University=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2016)") ///
		xtitle("Highest parent's education") ///
		xlabel(0 "Non" 1 "University") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/edu_radical16.png", replace
		
		
xtmixed radical gini University PV1CIV ||idcntry: ||idschool:
xtmixed radical gini University PV1CIV S_GENDER ||idcntry: ||idschool:

		
margins , at(PV1CIV=(300 400 500 600 700))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2016)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(300 "-2SD" 400 "-1SD" 500"Average" 600"+2SD" 700 "+2SD") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/know_radical16.png", replace
		
margins , at(S_GENDER=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2016)") ///
		xtitle("Sex of the student") ///
		xlabel(0 "Boys" 1 "Girls") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gender_radical16.png", replace

**** For ISPP		
xtmixed radical gini University PV1CIV S_GENDER internet||idcntry: ||idschool:

margins , at(internet=(1 2 3 4))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2016)") ///
		xtitle("Freq of internet information") ///
		xlabel(1 "never" 2 "Montly" 3"Weekly" 4"Daily") ///
		ylabel(1(0.5)4)
graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/internet_pacific16.png", replace

****  Models: Disposition to marching in peaceful rallys ***
xtmixed pacific ||idcntry: ||idschool:
estat icc

xtmixed pacific gini ||idcntry: ||idschool:

margins , at(gini=(25 45))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2016)") ///
		xtitle("Gini Index") ///
		xlabel(25 "Low Gini 0.25" 45 "High Gini 0.45") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ginieffect_pacific16.png", replace
		
		
xtmixed pacific gini University  ||idcntry: ||idschool:

margins , at(University=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2016)") ///
		xtitle("Highest parent's education") ///
		xlabel(0 "Non" 1 "University") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/edu_pacific16.png", replace
		
		
xtmixed pacific gini University PV1CIV  ||idcntry: ||idschool:
xtmixed pacific gini University PV1CIV S_GENDER ||idcntry: ||idschool:

		
margins , at(PV1CIV=(80 100 120))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2016)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(80 "Low" 100"Average" 120 "High") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/know_pacific16.png", replace
		
margins , at(S_GENDER=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2016)") ///
		xtitle("Sex of the students") ///
		xlabel(0 "Boys" 1 "Girls") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gender_pacific16.png", replace

		

*** MODELS to report **
xtmixed radical ||idcntry: ||idschool:
eststo model00
xtmixed radical gini ||idcntry: ||idschool:
eststo model01
xtmixed radical gini University  ||idcntry: ||idschool:
eststo model02
xtmixed radical gini University PV1CIV ||idcntry: ||idschool:
eststo model03
xtmixed radical gini University PV1CIV S_GENDER ||idcntry: ||idschool:
eststo model04
xtmixed radical gini University PV1CIV S_GENDER internet ||idcntry: ||idschool:
eststo model05

*xtmixed pacific ||idcntry: ||idschool:
*eststo model06
*xtmixed pacific gini ||idcntry: ||idschool:
*eststo model07
*xtmixed pacific gini University  ||idcntry: ||idschool:
*eststo model08
*xtmixed pacific gini University PV1CIV  ||idcntry: ||idschool:
*eststo model09
*xtmixed pacific gini University PV1CIV S_GENDER ||idcntry: ||idschool:
*eststo model10
*xtmixed pacific gini University PV1CIV S_GENDER internet ||idcntry: ||idschool:
*eststo model11


esttab // check
esttab using  "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/table16.rtf", replace nogaps abs star(* 0.05 ** 0.01) transform(ln*: exp(2*@) exp(2*@)) b(2) /// 
				stats(ll df_m, fmt(3 1) label(-2LL df))
eststo clear

******************************+********************
******************************+********************
********************** ICCS 2009 ******************
***************************************************
***************************************************
	
use "/Users/daniel/Dropbox (MIDE)/Zfiles/Trento/all.dta", clear
tab idcntry, nol

tab1 is2p31d is2p31g is2p31h is2p31i
tab1 is2p31d is2p31g is2p31h is2p31i, nol


recode is2p31d (1 2=1) (3 4=0), gen(marcha)
recode is2p31g (1 2=1) (3 4=0), gen(rayar)
recode is2p31h (1 2=1) (3 4=0), gen(trafico)
recode is2p31i (1 2=1) (3 4=0), gen(ocupa)
tab is2p31g rayar 

** Index **
revrs is2p31g is2p31h is2p31i
egen radical=rowmean(revis2p31g revis2p31h revis2p31i)
sum radical
sum radical, det

revrs is2p31c is2p31d is2p31e
egen pacific=rowmean(revis2p31c revis2p31d revis2p31e)
sum pacific
sum pacific, det

** GINI index
gen gini=.
replace gini= 28.9 if idcntry== 40
replace gini= 35.2 if idcntry== 100
replace gini= 48.5 if idcntry== 152
replace gini= 31.8 if idcntry== 158
replace gini= 52.7 if idcntry== 170
replace gini= 28.9 if idcntry== 196
replace gini= 24.3 if idcntry== 203
replace gini= 24.3 if idcntry== 208
replace gini= 47.7 if idcntry== 214
replace gini= 31.9 if idcntry== 233
replace gini= 24.4 if idcntry== 246
replace gini= 32.7 if idcntry== 300
replace gini= 47 if idcntry== 320
replace gini= 42.8 if idcntry== 344
replace gini= 46.8 if idcntry== 360
replace gini= 30.1 if idcntry== 372
replace gini= 33 if idcntry== 380
replace gini= 31.7 if idcntry== 410
replace gini= 36.6 if idcntry== 428
replace gini= . if idcntry== 438
replace gini= 34.8 if idcntry== 440
replace gini= 28.3 if idcntry== 442
replace gini= 27.2 if idcntry== 470
replace gini= 44.8 if idcntry== 484
replace gini= 25.8 if idcntry== 528
replace gini= 31.9 if idcntry== 554
replace gini= 24.5 if idcntry== 578
replace gini= 49.5 if idcntry== 600
replace gini= 30.7 if idcntry== 616
replace gini= 35.6 if idcntry== 643
replace gini= 25.8 if idcntry== 703
replace gini= 25.5 if idcntry== 705
replace gini= 32.4 if idcntry== 724
replace gini= 26.9 if idcntry== 752
replace gini= 29.7 if idcntry== 756
replace gini= 40.7 if idcntry== 764
replace gini= 33.7 if idcntry== 926
replace gini= 26.8 if idcntry== 956



*** Homologar variables ***		
		
tab sgender, nol
gen S_GENDER= sgender
tab hisced, nol
recode hisced (0 1= 1), gen(S_HISCED)
tab S_HISCED
recode S_HISCED (5=1) (1 2 3 4=0), gen(University)

gen internet= is2g13e

gquantiles pct_know = PV1CIV, xtile nquantiles(100) by(idcntry)

gen study=2009

gen year_study=2009 + idcntry

keep radical marcha ocupa trafico rayar gini PV1CIV pct_know S_GENDER S_HISCED University internet idcntry idschool study year_study totwgts

save "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/subset_ICCS_2009.dta", replace

*** Estimation of everage by country
table idcntry [aw=totwgts], c(mean marcha mean rayar mean trafico mean ocupa)

svyset [pweight = totwgts], strata(jkzones) psu(jkreps)
svydes

svy: mean marcha ocupa trafico rayar radical pacific

svy: mean marcha, over(idcntry)
svy: mean rayar, over(idcntry)
svy: mean trafico, over(idcntry)
svy: mean ocupa, over(idcntry)


graph hbar marcha [aw=totwgts], over(idcntry, gap (*.2) sort(marcha) label(nolabels)) ///			
				ytitle(" " "Disposition to march 2009") /// 
				intensity(40) ///
				yline(0.61) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/marching09.png", replace
		

graph hbar ocupa [aw=totwgts], over(idcntry, gap (*.2) sort(ocupa) label(nolabels)) ///			
				ytitle(" " "Disposition to Occupying buldings 2009") /// 
				intensity(40) ///
				yline(0.24) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ocupying09.png", replace
		
				
graph hbar trafico [aw=totwgts], over(idcntry, gap (*.2) sort(trafico) label(nolabels)) ///			
				ytitle(" " "Disposition to Blocking the traffic 2009") /// 
				intensity(40) ///
				yline(0.21) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/blocking09.png", replace
		
				
graph hbar rayar [aw=totwgts], over(idcntry, gap (*.2) sort(rayar) label(nolabels)) ///			
				ytitle(" " "Disposition to Spray walls 2009") /// 
				intensity(40) ///
				yline(0.29) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/spraying09.png", replace
		
graph hbar radical [aw=totwgt], over(idcntry, gap (*.2) sort(radical) label(nolabels)) ///			
				ytitle(" " "Mean's disposition to radical actions 2009") /// 
				intensity(40) ///
				yline(1.93) ///
				ylabel(0(1)4, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/radical09.png", replace

graph hbar pacific [aw=totwgt], over(idcntry, gap (*.2) sort(pacific) label(nolabels)) ///			
				ytitle(" " "Mean's disposition to pacific actions 2009") /// 
				intensity(40) ///
				yline(2.58) ///
				ylabel(0(1)4, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/pacific09.png", replace
		 
			
bysort idcntry: asgen marcha_mean = marcha, w(totwgts)
bysort idcntry: asgen ocupa_mean= ocupa, w(totwgts)
bysort idcntry: asgen trafico_mean= trafico, w(totwgts)
bysort idcntry: asgen rayar_mean= rayar, w(totwgts)
bysort idcntry: asgen radical_mean= radical, w(totwgts)
bysort idcntry: asgen pacific_mean= pacific, w(totwgts)


**Correlaciones
preserve
collapse (mean) marcha ocupa trafico rayar radical pacific gini, by(idcntry)
pwcorr gini marcha ocupa trafic rayar radical pacific
restore


*** Gini y protestas
twoway (scatter marcha_mean gini, mlabel(country)) (lfitci marcha_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to peaceful marches 2009)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.71")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_marcha09.png", replace
		
twoway (scatter trafico_mean gini, mlabel(country)) (lfitci trafico_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to blocking the traffic 2009)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.43")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_trafico09.png", replace
		
twoway (scatter ocupa_mean gini, mlabel(country)) (lfitci ocupa_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to occupy building 2009)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.35")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_ocupa09.png", replace
		
twoway (scatter rayar_mean gini, mlabel(country)) (lfitci rayar_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to spray walls 2009)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.41")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_rayar09.png", replace

twoway (scatter radical_mean gini, mlabel(country)) (lfitci radical_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to radical actions 2009)") ///
		xtitle("Gini Index") ///
		ylabel(1(0.5)4) note("Correlación de Pearson= 0.376")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_radical09.png", replace

		
twoway (scatter pacific_mean gini, mlabel(country)) (lfitci pacific_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to pacific actions 2009)") ///
		xtitle("Gini Index") ///
		ylabel(1(0.5)4) note("Correlación de Pearson= 0.767")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_pacific09.png", replace


	

****  Models: Disposition to blocking the traffic ***
xtmixed radical ||idcntry: ||idschool:
estat icc

xtmixed radical gini ||idcntry: ||idschool:

margins , at(gini=(25 45))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2009)") ///
		xtitle("Gini Index") ///
		xlabel(25 "Low Gini 0.25" 45 "High Gini 0.45") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ginieffect_radical09.png", replace
		
		
xtmixed radical gini University  ||idcntry: ||idschool:

margins , at(University=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2009)") ///
		xtitle("Highest parent's education") ///
		xlabel(0 "Non" 1 "University") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/edu_radical09.png", replace
		
		
xtmixed radical gini University PV1CIV ||idcntry: ||idschool:
xtmixed radical gini University PV1CIV S_GENDER ||idcntry: ||idschool:
		
margins , at(PV1CIV=(300 400 500 600 700))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2009)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(300 "-2SD" 400 "-1SD" 500"Average" 600"+2SD" 700 "+2SD") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/know_radical09.png", replace
		
margins , at(S_GENDER=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2009)") ///
		xtitle("Sex of the student") ///
		xlabel(0 "Boys" 1 "Girls") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gender_radical09.png", replace


**** For ISPP		
xtmixed radical gini University PV1CIV S_GENDER internet||idcntry: ||idschool:

margins , at(internet=(1 2 3 4))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2009)") ///
		xtitle("Freq of internet information") ///
		xlabel(1 "never" 2 "Montly" 3"Weekly" 4"Daily") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/internet_radical09.png", replace


****  Models: Disposition to marching in peaceful rallys ***
xtmixed pacific ||idcntry: ||idschool:
estat icc

xtmixed pacific gini ||idcntry: ||idschool:

margins , at(gini=(25 45))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2009)") ///
		xtitle("Gini Index") ///
		xlabel(25 "Low Gini 0.25" 45 "High Gini 0.45") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ginieffect_pacific09.png", replace
		
		
xtmixed pacific gini University  ||idcntry: ||idschool:

margins , at(University=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2009)") ///
		xtitle("Highest parent's education") ///
		xlabel(0 "Non" 1 "University") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/edu_pacific09.png", replace
		
		
xtmixed pacific gini University PV1CIV  ||idcntry: ||idschool:
xtmixed pacific gini University PV1CIV S_GENDER ||idcntry: ||idschool:

		
margins , at(PV1CIV=(80 100 120))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2009)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(80 "Low" 100"Average" 120 "High") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/know_pacific09.png", replace
		
margins , at(S_GENDER=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 2009)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(0 "Boys" 1 "Girls") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gender_pacific09.png", replace

		
*** MODELS to report **
xtmixed radical ||idcntry: ||idschool:
eststo model00
xtmixed radical gini ||idcntry: ||idschool:
eststo model01
xtmixed radical gini University  ||idcntry: ||idschool:
eststo model02
xtmixed radical gini University PV1CIV ||idcntry: ||idschool:
eststo model03
xtmixed radical gini University PV1CIV S_GENDER ||idcntry: ||idschool:
eststo model04
xtmixed radical gini University PV1CIV S_GENDER internet ||idcntry: ||idschool:
eststo model05

*xtmixed pacific ||idcntry: ||idschool:
*eststo model05
*xtmixed pacific gini ||idcntry: ||idschool:
*eststo model06
*xtmixed pacific gini University  ||idcntry: ||idschool:
*eststo model07
*xtmixed pacific gini University PV1CIV  ||idcntry: ||idschool:
*eststo model08
*xtmixed pacific gini University PV1CIV S_GENDER ||idcntry: ||idschool:
*eststo model09


esttab // check
esttab using  "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/table09.rtf", replace nogaps abs star(* 0.05 ** 0.01) transform(ln*: exp(2*@) exp(2*@)) b(2) /// 
				stats(ll df_m, fmt(3 1) label(-2LL df))
eststo clear


******************************+********************
******************************+********************
********************** CCIVED *********************
***************************************************
***************************************************

use "/Users/daniel/Dropbox (MIDE)/MIDEUC/1.publicaciones/Corruption and trust/1_Bases_de_Datos/0_Respaldo/1_CIVED1999.dta", clear

tab1 bs5m9 bs5m10 bs5m11 bs5m12
tab1 bs5m9 bs5m10 bs5m11 bs5m12, nol
recode bs5m9 (3 4=1) (1 2=0) (0 8=.), gen(marcha)
recode bs5m10 (3 4=1) (1 2=0) (0 8=.), gen(rayar)
recode bs5m11 (3 4=1) (1 2=0) (0 8=.), gen(trafico)
recode bs5m12  (3 4=1) (1 2=0) (0 8=.), gen(ocupa)
tab rayar bs5m10



** Index **
recode bs5m10 (0 8=.), gen(spray)
recode bs5m11 (0 8=.), gen(traffic)
recode bs5m12 (0 8=.), gen(ocuppy)
egen radical=rowmean(spray traffic ocuppy)
sum radical
sum radical, det

recode bs5m4 (0 8=.), gen(letter)
recode bs5m8 (0 8=.), gen(sign)
recode bs5m9 (0 8=.), gen(march)
egen pacific=rowmean(letter sign march)
sum pacific
sum pacific, det

gen gini=.
replace gini= 31.5 if idcntry== 36 // Australia
replace gini= 28.3 if idcntry== 957 // Belgium (Flemish)
replace gini= 30.8 if idcntry== 100 // Bulgaria
replace gini= 50.5 if idcntry== 152 // Chile
replace gini= 51.3 if idcntry== 170 // Colombia
replace gini= 27.4 if idcntry== 196 // Cyprus
replace gini= 22.9 if idcntry== 200 // Czech Rep
replace gini= 22.8 if idcntry== 208 // Denmark
replace gini= 35.1 if idcntry== 826 // England
replace gini= 33.3 if idcntry== 233 // Estonia
replace gini= 24.2 if idcntry== 246 // Finland
replace gini= 26 if idcntry== 276 // Germany
replace gini= 33.9 if idcntry== 300 // Greece
replace gini= 43.1 if idcntry== 344 //Hong Kong
replace gini= 29 if idcntry== 348 //Hungary
replace gini= 32.7 if idcntry== 380 // Italy
replace gini= 33.9 if idcntry== 428
replace gini= 32.3 if idcntry== 440
replace gini= 24.7 if idcntry== 578
replace gini= 29.5 if idcntry== 616
replace gini= 30.5 if idcntry== 620
replace gini= 29.8 if idcntry== 642
replace gini= 34.7 if idcntry== 643
replace gini= 25 if idcntry== 703
replace gini= 22.8 if idcntry== 705
replace gini= 25.4 if idcntry== 752
replace gini= 26.2 if idcntry== 756
replace gini= 36.8 if idcntry== 840

*** Homologar variables ***		

** Sex	
tab itsex	
tab itsex, nol
recode itsex (8=.) (2=0) (1=1), gen(S_GENDER)
tab S_GENDER itsex

** Education
recode bsgedum bsgeduf (8 0=.)  // OJO, diferentes categorÌas que ICCS, luego homologar
				gen hisced=bsgedum
				replace hisced=bsgeduf if bsgeduf>bsgedum
				replace hisced=bsgedum if hisced==.
				replace hisced=bsgeduf if hisced==.  // para rescatar cuando existe informaciÛn sÛlo de 1 de los padres
				
gen S_HISCED=hisced    // revisar equivalencias de idioma.
recode S_HISCED (1=1) (2 3=2) (4=3) (5 6=4) (7=5)
	lab def hised 1"Básica imcompleta" 2 "Básica completa" 3"Media completa" 4"Técnica completa" 5"Univesitaria completa"
	lab val S_HISCED hised
	label var S_HISCED "Higher Parent's education level"
tab S_HISCED, nol

recode S_HISCED (1 2 3 4=0) (5=1), gen(University)

recode bsgp1 (0 8=.), gen(internet)

** Civic knowledge
gen PV1CIV = knowlmle
sum PV1CIV, det
*hist PV1CIV


gquantiles pct_know = PV1CIV, xtile nquantiles(100) by(idcntry)

gen study=1999

gen year_study=1999 + idcntry

gen totwgts=totwgt

keep radical marcha ocupa trafico rayar gini PV1CIV pct_know S_GENDER S_HISCED University internet idcntry idschool study year_study totwgts

save "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/subset_CIVED_1999.dta", replace

******
svyset [pweight = stdwgt], strata(idstrat) psu(idschool)
svydes
svy: mean marcha rayar trafico ocupa radical pacific

svy: mean marcha, over(cntryid)
svy: mean rayar, over(cntryid)
svy: mean trafico, over(cntryid)
svy: mean ocupa, over(cntryid)
svy: mean radical, over(cntryid)
svy: mean pacific, over(cntryid)



graph hbar marcha [aw=totwgt], over(cntryid, gap (*.2) sort(marcha) label(nolabels)) ///			
				ytitle(" " "Disposition to march 1999") /// 
				intensity(40) ///
				yline(0.427) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/marching99.png", replace
		

graph hbar ocupa [aw=totwgt], over(cntryid, gap (*.2) sort(ocupa) label(nolabels)) ///			
				ytitle(" " "Disposition to Occupying buldings 1999") /// 
				intensity(40) ///
				yline(0.139) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ocupying99.png", replace
		
				
graph hbar trafico [aw=totwgt], over(cntryid, gap (*.2) sort(trafico) label(nolabels)) ///			
				ytitle(" " "Disposition to Blocking the traffic 1999") /// 
				intensity(40) ///
				yline(0.137) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/blocking99.png", replace
		
				
graph hbar rayar [aw=totwgt], over(cntryid, gap (*.2) sort(rayar) label(nolabels)) ///			
				ytitle(" " "Disposition to Spray walls 1999") /// 
				intensity(40) ///
				yline(0.168) ///
				ylabel(, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/spraying99.png", replace
		

graph hbar radical [aw=totwgt], over(cntryid, gap (*.2) sort(radical) label(nolabels)) ///			
				ytitle(" " "Mean's disposition to radical actions 1999") /// 
				intensity(40) ///
				yline(1.68) ///
				ylabel(0(1)4, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/radical99.png", replace

graph hbar pacific [aw=totwgt], over(cntryid, gap (*.2) sort(pacific) label(nolabels)) ///			
				ytitle(" " "Mean's disposition to pacific actions 1999") /// 
				intensity(40) ///
				yline(2.24) ///
				ylabel(0(1)4, labsize(small)) ///ysc(r(300 600)) ///
				blabel(group, position(base) color(white) size(vsmall)) ///
				bargap (50) ///
				ysize(6)
				graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/pacific99.png", replace
		 
		 *ylabel(0 "Certainly Not" 1 "Prob not" 2 "Prob do" 3 "Certainly do") ///




gen idnctry = cntryid

bysort idcntry: asgen marcha_mean = marcha, w(totwgt)
bysort idcntry: asgen ocupa_mean= ocupa, w(totwgt)
bysort idcntry: asgen trafico_mean= trafico, w(totwgt)
bysort idcntry: asgen rayar_mean= rayar, w(totwgt)
bysort idcntry: asgen radical_mean= radical, w(totwgt)
bysort idcntry: asgen pacific_mean= pacific, w(totwgt)


**Correlaciones
preserve
collapse (mean) marcha ocupa trafico rayar radical pacific gini, by(idcntry)
pwcorr gini marcha ocupa trafico rayar radical pacific
restore



*** Gini y protestas
twoway (scatter marcha_mean gini, mlabel(cntryid)) (lfitci marcha_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to peacefull marches 1999)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.289")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_marcha99.png", replace
		
twoway (scatter trafico_mean gini, mlabel(cntryid)) (lfitci trafico_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to blocking the traffic 1999)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.284")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_trafico99.png", replace
		
twoway (scatter ocupa_mean gini, mlabel(cntryid)) (lfitci ocupa_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to occupy building 1999)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.188")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_ocupa99.png", replace
		
twoway (scatter rayar_mean gini, mlabel(cntryid)) (lfitci rayar_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to spray walls 1999)") ///
		xtitle("Gini Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.426")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_rayar99.png", replace

twoway (scatter radical_mean gini, mlabel(cntryid)) (lfitci radical_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to radical actions 1999)") ///
		xtitle("Gini Index") ///
		ylabel(1(0.5)4) note("Correlación de Pearson= 0.172")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_radical99.png", replace

twoway (scatter pacific_mean gini, mlabel(cntryid)) (lfitci pacific_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Disposition to pacific actions 1999)") ///
		xtitle("Gini Index") ///
		ylabel(1(0.5)4) note("Correlación de Pearson= 0.598")
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gini_pacific99.png", replace
		

****  Models: Disposition to blocking the traffic ***
xtmixed radical ||idcntry: ||idschool:
estat icc

xtmixed radical gini ||idcntry: ||idschool:

margins , at(gini=(25 45))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 1999)") ///
		xtitle("Gini Index") ///
		xlabel(25 "Low Gini 0.25" 45 "High Gini 0.45") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ginieffect_radical99.png", replace
		
		
xtmixed radical gini University  ||idcntry: ||idschool:

margins , at(University=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 1999)") ///
		xtitle("Highest parent's education") ///
		xlabel(0 "Non" 1 "University") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/edu_radical99.png", replace
		
		
xtmixed radical gini University PV1CIV ||idcntry: ||idschool:
xtmixed radical gini University PV1CIV S_GENDER ||idcntry: ||idschool:
		
margins , at(PV1CIV=(60 80 100 120 140))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2009)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(60 "-2SD" 80 "-1SD" 100"Average" 120"+2SD" 140"+2SD") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/know_radical99.png", replace
		
margins , at(S_GENDER=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 1999)") ///
		xtitle("Sex of the student") ///
		xlabel(0 "Boys" 1 "Girls") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gender_radical99.png", replace

	
**** For ISPP		
xtmixed radical gini University PV1CIV S_GENDER internet||idcntry: ||idschool:

margins , at(internet=(1 2 3 4))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 1999)") ///
		xtitle("Freq of internet information") ///
		xlabel(1 "never" 2 "Montly" 3"Weekly" 4"Daily") ///
		ylabel(1(0.5)4)
			graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/internet_radical99.png", replace
		

****  Models: Disposition to marching in peaceful rallys ***
xtmixed pacific ||idcntry: ||idschool:
estat icc

xtmixed pacific gini ||idcntry: ||idschool:

margins , at(gini=(25 45))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 1999)") ///
		xtitle("Gini Index") ///
		xlabel(25 "Low Gini 0.25" 45 "High Gini 0.45") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ginieffect_pacific99.png", replace
		
		
xtmixed pacific gini University  ||idcntry: ||idschool:

margins , at(University=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 1999)") ///
		xtitle("Highest parent's education") ///
		xlabel(0 "Non" 1 "University") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/edu_pacific99.png", replace
		
		
xtmixed pacific gini University PV1CIV  ||idcntry: ||idschool:
xtmixed pacific gini University PV1CIV S_GENDER ||idcntry: ||idschool:

		
margins , at(PV1CIV=(80 100 120))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 1999)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(80 "Low" 100"Average" 120 "High") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/know_pacific99.png", replace
		
margins , at(S_GENDER=(0 1))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 1999)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(0 "Boys" 1 "Girls") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/gender_pacific99.png", replace


*** MODELS to report **
xtmixed radical ||idcntry: ||idschool:
eststo model00
xtmixed radical gini ||idcntry: ||idschool:
eststo model01
xtmixed radical gini University  ||idcntry: ||idschool:
eststo model02
xtmixed radical gini University PV1CIV ||idcntry: ||idschool:
eststo model03
xtmixed radical gini University PV1CIV S_GENDER ||idcntry: ||idschool:
eststo model04
xtmixed radical gini University PV1CIV S_GENDER internet ||idcntry: ||idschool:
eststo model05

*xtmixed pacific ||idcntry: ||idschool:
*eststo model05
*xtmixed pacific gini ||idcntry: ||idschool:
*eststo model06
*xtmixed pacific gini University  ||idcntry: ||idschool:
*eststo model07
*xtmixed pacific gini University PV1CIV  ||idcntry: ||idschool:
*eststo model08
*xtmixed pacific gini University PV1CIV S_GENDER ||idcntry: ||idschool:
*eststo model09


esttab // check
esttab using  "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/table99.rtf", replace nogaps abs star(* 0.05 ** 0.01) transform(ln*: exp(2*@) exp(2*@)) b(2) /// 
				stats(ll df_m, fmt(3 1) label(-2LL df))
eststo clear



********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
use "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/subset_ICCS_2016.dta", clear

append using "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/subset_ICCS_2009.dta"

append using "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/subset_CIVED_1999.dta"

tab idcntry study

recode study (1999=1) (2009=2) (2016=3), gen(year)
table year study

gen country_year= idcntry + year

egen country_yr = concat(idcntry year), punct(-)

tab country_year
tab country_year if year==1
tab country_year if year==2
tab idcntry country_year if year==3
tab idcntry year

tab S_GENDER, nol
table study S_GENDER


xtmixed radical  || country_yr: ||idschool:

xtmixed radical i.year  ||country_yr: ||idschool:

xtmixed radical i.year gini ||country_year: ||idschool:

xtmixed radical i.year gini University ||country_year: ||idschool:

xtmixed radical i.year gini University pct_know  ||country_year: ||idschool:

xtmixed radical i.year gini University pct_know S_GENDER ||country_year: ||idschool:

xtmixed radical i.year gini University pct_know S_GENDER internet ||country_year: ||idschool:

xtmixed radical i.year gini University pct_know S_GENDER internet i.year##c.gini ||country_year: ||idschool:

margins , at(gini=(25 45))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to pacific actions 1999)") ///
		xtitle("Gini Index") ///
		xlabel(25 "Low Gini 0.25" 45 "High Gini 0.45") ///
		ylabel(1(0.5)4)
		graph export "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/ginieffect_pacific99.png", replace


margins , at(year =(0 1 2))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Predisposition to radical actions)") ///
		xtitle("Year of Study") ///
		xlabel(0 "1999" 1 "2009" 2 "2016") ///
		ylabel(1(0.5)4)

xtmixed radical i.study gini i.study##c.gini i.country_year ||idcntry: ||idschool:

xtmixed radical gini University pct_know S_GENDER i.study i.idcntry ||idcntry: ||idschool:

		

margins , at(study =(1999 2009 2016))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Predisposition to radical actions)") ///
		xtitle("Year of Study") ///
		xlabel(1999 "1999" 2009 "2009" 2016 "2016") ///
		ylabel(1(0.5)4)


margins , at(pct_know=(5 25 50 75 95))
marginsplot, scheme(s2mono) scale(0.8) graphregion(fcolor(white) lcolor(white))  ///
		ytitle("Pr(Disposition to radical actions 2016)") ///
		xtitle("Levels of Civic Knowledge") ///
		xlabel(5 "5%" 25 "25%" 50"50%" 75"75%" 95 "95%") ///
		ylabel(1(0.5)4)

		
 table country_yr, c(mean year mean gini mean University)
 table country_yr, c(mean pct_know mean S_GENDER mean internet)
		
xtmixed radical i.year gini University pct_know S_GENDER i.year##c.gini ||country_yr: ||idschool:

xtmixed radical if e(sample)|| country_yr: ||idschool:
eststo model00
xtmixed radical i.year if e(sample)  ||country_yr: ||idschool:
eststo model01
xtmixed radical i.year gini if e(sample) ||country_year: ||idschool:
eststo model02
xtmixed radical i.year gini University if e(sample) ||country_yr: ||idschool:
eststo model03
xtmixed radical i.year gini University pct_know if e(sample) ||country_yr: ||idschool:
eststo model04
xtmixed radical i.year gini University pct_know S_GENDER if e(sample) ||country_yr: ||idschool:
eststo model05
xtmixed radical i.year gini University pct_know S_GENDER i.University##i.year if e(sample) ||country_yr: ||idschool:
eststo model05a
xtmixed radical i.year gini University pct_know S_GENDER c.pct_know##i.year if e(sample) ||country_yr: ||idschool:
eststo model05b
xtmixed radical i.year gini University pct_know S_GENDER i.S_GENDER##i.year if e(sample) ||country_yr: ||idschool:
eststo model05c
xtmixed radical i.year gini University pct_know S_GENDER c.gini##i.year if e(sample) ||country_yr: ||idschool:
eststo model05d


esttab // check
esttab using  "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/model_99_09_16.rtf", replace nogaps abs star(* 0.05 ** 0.01) transform(ln*: exp(2*@) exp(2*@)) b(2) /// 
				stats(ll df_m, fmt(3 1) label(-2LL df))
eststo clear

*** Random Slopes Tests ***

xtmixed radical i.year gini University pct_know S_GENDER if e(sample) ||country_yr: University ||idschool:
eststo model06
xtmixed radical i.year gini University pct_know S_GENDER c.gini##i.University if e(sample) ||country_yr: University ||idschool:
eststo model06a

xtmixed radical i.year gini University pct_know S_GENDER if e(sample) ||country_yr: pct_know ||idschool:
eststo model07
xtmixed radical i.year gini University pct_know S_GENDER c.gini##c.pct_know if e(sample) ||country_yr: pct_know ||idschool:
eststo model07a

xtmixed radical i.year gini University pct_know S_GENDER if e(sample) ||country_yr: S_GENDER ||idschool:
eststo model08
xtmixed radical i.year gini University pct_know S_GENDER c.gini##i.S_GENDER if e(sample) ||country_yr: S_GENDER ||idschool:
eststo model08a

esttab // check
esttab using  "/Users/daniel/Dropbox (MIDE)/MIDEUC/Conferencias/2022_INVALSI/presentation/files/results_iea/random_99_09_16.rtf", replace nogaps abs star(* 0.05 ** 0.01) transform(ln*: exp(2*@) exp(2*@)) b(2) /// 
				stats(ll df_m, fmt(3 1) label(-2LL df))
eststo clear

********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************

* CCIVED
use "/Users/daniel/Dropbox (MIDE)/MIDEUC/1.publicaciones/Corruption and trust/1_Bases_de_Datos/0_Respaldo/1_CIVED1999.dta", clear

tab1 bs5m10 bs5m11 bs5m12
tab1 bs5m10 bs5m11 bs5m12, nol
recode bs5m10 (3 4=1) (1 2=0) (0 8=.), gen(spray)
recode bs5m11 (3 4=1) (1 2=0) (0 8=.), gen(traffic)
recode bs5m12  (3 4=1) (1 2=0) (0 8=.), gen(occupy)
tab spray bs5m10

egen act=rowtotal(spray traffic occupy)
tab act

gen activism=0
replace activism=1 if spray==1 | traffic==1 | occupy==1


tab act activism
tab activism spray
tab activism traffic
tab activism occupy

pwcorr spray traffic occupy

svyset [pweight = stdwgt], strata(idstrat) psu(idschool)
svydes
svy: mean act activism spray traffic occupy

svy: mean spray, over(cntryid)
svy: mean traffic, over(cntryid)
svy: mean occupy, over(cntryid)

table cntryid, c(mean idcntry)

gen gini=.
replace gini= 31.5 if idcntry== 36
replace gini= 28.3 if idcntry== 957
replace gini= 30.8 if idcntry== 100
replace gini= 50.5 if idcntry== 152
replace gini= 51.3 if idcntry== 170
replace gini= 27.4 if idcntry== 196
replace gini= 22.9 if idcntry== 200
replace gini= 22.8 if idcntry== 208
replace gini= 35.1 if idcntry== 826
replace gini= 33.3 if idcntry== 233
replace gini= 24.2 if idcntry== 246
replace gini= 26 if idcntry== 276
replace gini= 33.9 if idcntry== 300
replace gini= 43.1 if idcntry== 344
replace gini= 29 if idcntry== 348
replace gini= 32.7 if idcntry== 380
replace gini= 33.9 if idcntry== 428
replace gini= 32.3 if idcntry== 440
replace gini= 24.7 if idcntry== 578
replace gini= 29.5 if idcntry== 616
replace gini= 30.5 if idcntry== 620
replace gini= 29.8 if idcntry== 642
replace gini= 34.7 if idcntry== 643
replace gini= 25 if idcntry== 703
replace gini= 22.8 if idcntry== 705
replace gini= 25.4 if idcntry== 752
replace gini= 26.2 if idcntry== 756
replace gini= 36.8 if idcntry== 840

**Correlaciones
preserve
collapse (mean) radical act activism spray traffic occupy gini, by(idcntry)
pwcorr gini radical act activism spray traffic occupy
restore

tab itsex,
tab itsex, nol
recode itsex (8=.), gen(s_gender)

tab bsgedum
tab bsgedum, nol
recode bsgedum (8 0=.), gen(edumam)

tab bsgeduf
tab bsgeduf, nol
recode bsgeduf (8 0=.), gen(edupap)

tab bsgbook
tab bsgbook, nol
recode bsgbook (8 =.), gen(homelit)
tab homelit

egen radical=rowmean(bs5m10 bs5m11 bs5m12)
hist radical

xtmixed bs5m10 ||idcntry: ||idschool:
estat icc 

xtmixed bs5m10 gini||idcntry: ||idschool:
xtmixed bs5m10 s_gender edumam edupap homelit gini ||idcntry: ||idschool: 
xtmixed radical s_gender edumam edupap homelit gini ||idcntry:s_gender ||idschool: , itera(5)
xtmixed radical s_gender edumam edupap homelit gini i.s_gender##c.gini ||idcntry:s_gender ||idschool: 

margins , at(s_gender=(1, 2))
marginsplot

exp(0.77)

bysort idcntry: egen ocupa_mean= mean(ocupa)
bysort idcntry: egen trafic_mean= mean(trafic)
bysort idcntry: egen spray_mean= mean(spray)

twoway (scatter spray_mean gini, mlabel(cntryid)) (lfitci spray_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Participación)") ///
		xtitle("GINI Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.34")
		*graph export "C:\Users\DM\Dropbox (MIDE)\MIDEUC\Columna_CIPER\gini_marcha.pdf", replace


use "/Users/daniel/Dropbox (MIDE)/Zfiles/Trento/all.dta", clear
tab idcntry, nol

tab1 is2p31g is2p31h is2p31i
tab1 is2p31g is2p31h is2p31i, nol
revrs is2p31g is2p31h is2p31i

recode is2p31g (1 2=1) (3 4=0), gen(spray)
recode is2p31h (1 2=1) (3 4=0), gen(traffic)
recode is2p31i (1 2=1) (3 4=0), gen(occupy)
tab spray is2p31g

egen act=rowtotal(spray traffic occupy)
tab act

gen activism=0
replace activism=1 if spray==1 | traffic==1 | occupy==1


tab activism spray
tab activism traffic
tab activism occupy

svyset [pweight = totwgts], strata(jkzones) psu(idschool)
svydes
svy: mean act activism spray traffic occupy

svy: mean spray, over(idcntry)
svy: mean traffic, over(idcntry)
svy: mean occupy, over(idcntry)

gen gini=.
replace gini= 28.9 if idcntry== 40
replace gini= 35.2 if idcntry== 100
replace gini= 48.5 if idcntry== 152
replace gini= 31.8 if idcntry== 158
replace gini= 52.7 if idcntry== 170
replace gini= 28.9 if idcntry== 196
replace gini= 24.3 if idcntry== 203
replace gini= 24.3 if idcntry== 208
replace gini= 47.7 if idcntry== 214
replace gini= 31.9 if idcntry== 233
replace gini= 24.4 if idcntry== 246
replace gini= 32.7 if idcntry== 300
replace gini= 47 if idcntry== 320
replace gini= 42.8 if idcntry== 344
replace gini= 46.8 if idcntry== 360
replace gini= 30.1 if idcntry== 372
replace gini= 33 if idcntry== 380
replace gini= 31.7 if idcntry== 410
replace gini= 36.6 if idcntry== 428
replace gini= 0 if idcntry== 438
replace gini= 34.8 if idcntry== 440
replace gini= 28.3 if idcntry== 442
replace gini= 27.2 if idcntry== 470
replace gini= 44.8 if idcntry== 484
replace gini= 25.8 if idcntry== 528
replace gini= 31.9 if idcntry== 554
replace gini= 24.5 if idcntry== 578
replace gini= 49.5 if idcntry== 600
replace gini= 30.7 if idcntry== 616
replace gini= 35.6 if idcntry== 643
replace gini= 25.8 if idcntry== 703
replace gini= 25.5 if idcntry== 705
replace gini= 32.4 if idcntry== 724
replace gini= 26.9 if idcntry== 752
replace gini= 29.7 if idcntry== 756
replace gini= 40.7 if idcntry== 764
replace gini= 33.7 if idcntry== 926
replace gini= 26.8 if idcntry== 956

tab sgender,
tab sgender, nol
gen s_gender= sgender

tab fisced
tab fisced, nol
gen edupap= fisced

tab misced
tab misced, nol
gen edumam= misced

tab homelit

egen radical=rowmean(revis2p31g revis2p31h revis2p31i)
hist radical

xtmixed radical ||idcntry: ||idschool:
estat icc 


xtmixed radical gini||idcntry: ||idschool:
xtmixed radical s_gender edumam edupap homelit gini ||idcntry: ||idschool: 
xtmixed radical s_gender edumam edupap homelit gini ||idcntry:s_gender ||idschool: , itera(5)
xtmixed radical s_gender edumam edupap homelit gini i.s_gender##c.gini ||idcntry:s_gender ||idschool: 


**Correlaciones
preserve
collapse (mean) act activism spray traffic occupy gini, by(idcntry)
pwcorr gini act activism spray traffic occupy
restore


logit activism i.hisced if idcntry==152

xtmixed act  || idcntry: || idschool: 

estat icc

bysort idcntry idschool: egen nse_school=mean(hisced)


xtmixed act hisced || idcntry: hisced || idschool:  
xtmixed act c.hisced##c.nse_school if idcntry==152 || idcntry: hisced || idschool: hisced

bysort idcntry: egen ocupa_mean= mean(ocupa)
bysort idcntry: egen trafic_mean= mean(trafic)
bysort idcntry: egen spray_mean= mean(spray)

twoway (scatter spray_mean gini, mlabel(idcntry)) (lfitci spray_mean gini), scheme(s2mono) scale(0.8) ///
        graphregion(fcolor(white) lcolor(white)) legend(off)  ///
		ytitle("Pr(Participación)") ///
		xtitle("GINI Index") ///
		ylabel(0(0.1)1) note("Correlación de Pearson= 0.34")


		
