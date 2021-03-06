#Original Model in Stata
********************************************************************************************
* Stata syntax for estimating LAC mortality standard and fittting mortality schedule for   * 
* each country, year and sex                                                               * 
********************************************************************************************

clear all
set more off
capture log cabin
log using "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\LACmortality.log", replace

*Reading database with deaths and exposure by five-year age group, country, year and sex:
clear all
import excel "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\dados_mortalidade_to_stata.xls", sheet("LAC") firstrow
gen sexcod=1 if sex=="f"
	replace sexcod=2 if sex=="m"
sort ctry year sex
save "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\LACmortalityPAA.dta", replace


*Reading input dataframe with completeness of deaths estimate for each country, sex and year:
clear
import excel "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\death_counts_under-registration.xls", sheet("Plan1") firstrow
sort ctry year sex
save "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\death_counts_under-registration.dta", replace
clear
use "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\LACmortalityPAA.dta"
merge ctry year sex using "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\death_counts_under-registration.dta"
tab _merge
drop _merge
sort ctry year sex age



*Correcting the number of deaths due to undercounting and calculating mortality rates:
gen fator=1/grau
gen agegavg=age+2.5
gen Mx_obs=death/pop
gen Mx=Mx_obs
	replace Mx=Mx_obs*fator if fator>=1
gen logMx=log(Mx)
gen logitMx=logit(Mx)
save "C:\Users\MarcosRoberto\Dropbox\PAA_2014\CompLatAmr paper\Resultados\Novo_10_22_15\NCorrigindoKmenorque1\LACmortalityPAA.dta", replace


**** Estimating LAC Standard ***********

*Model with year and country without interaction factor:
char agegavg[omit] 2.5
xi: reg logitMx i.agegavg i.year i.ctryname if sex=="m"
drop _I*
xi: reg logitMx i.agegavg i.year i.ctryname if sex=="f"
drop _I*

*standard based on the coefficients of the model above:
*Males:
gen	logitMxStd	=	-1.243527	+	0.117131	+	-2.662012	+	-3.203116	if	agegavg	==	7.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-2.775057	+	-3.203116	if	agegavg	==	12.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-2.083447	+	-3.203116	if	agegavg	==	17.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-1.628187	+	-3.203116	if	agegavg	==	22.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-1.509320	+	-3.203116	if	agegavg	==	27.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-1.382974	+	-3.203116	if	agegavg	==	32.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-1.174007	+	-3.203116	if	agegavg	==	37.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-0.904424	+	-3.203116	if	agegavg	==	42.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-0.583744	+	-3.203116	if	agegavg	==	47.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	-0.226486	+	-3.203116	if	agegavg	==	52.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	0.148321	+	-3.203116	if	agegavg	==	57.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	0.535991	+	-3.203116	if	agegavg	==	62.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	0.935633	+	-3.203116	if	agegavg	==	67.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	1.367976	+	-3.203116	if	agegavg	==	72.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	1.802013	+	-3.203116	if	agegavg	==	77.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	2.202457	+	-3.203116	if	agegavg	==	82.5	&	sex	==	"m"
replace	logitMxStd	=	-1.243527	+	0.117131	+	2.870275	+	-3.203116	if	agegavg	==	87.5	&	sex	==	"m"

*Females:		
replace	logitMxStd	=	-1.580279	+	0.095565	+	-2.681835	+	-3.022645	if	agegavg	==	7.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-2.918712	+	-3.022645	if	agegavg	==	12.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-2.438456	+	-3.022645	if	agegavg	==	17.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-2.159798	+	-3.022645	if	agegavg	==	22.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-1.972644	+	-3.022645	if	agegavg	==	27.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-1.751146	+	-3.022645	if	agegavg	==	32.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-1.475118	+	-3.022645	if	agegavg	==	37.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-1.174171	+	-3.022645	if	agegavg	==	42.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-0.871301	+	-3.022645	if	agegavg	==	47.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-0.500965	+	-3.022645	if	agegavg	==	52.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	-0.132228	+	-3.022645	if	agegavg	==	57.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	0.292248	+	-3.022645	if	agegavg	==	62.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	0.721200	+	-3.022645	if	agegavg	==	67.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	1.196135	+	-3.022645	if	agegavg	==	72.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	1.683997	+	-3.022645	if	agegavg	==	77.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	2.152500	+	-3.022645	if	agegavg	==	82.5	&	sex	==	"f"
replace	logitMxStd	=	-1.580279	+	0.095565	+	2.960074	+	-3.022645	if	agegavg	==	87.5	&	sex	==	"f"

*Inserting single year age on dataset:
egen id=concat(ctry year sexcod)
destring id, replace
tsset id age
tsfill
bysort id: carryforward year, replace
bysort id: carryforward ctry, replace
bysort id: carryforward sexcod, replace
bysort id: carryforward ctryname, replace
bysort id: carryforward sex, replace
drop id

*Extending the Standard to all simple ages by using weighted least squares regression (Himes, Preston et al)
*combinacao ctry/year-->population j
egen ctry_year=concat(ctry year)
destring ctry_year, replace

*Males:
wls0  logitMxStd age if age>4 & sex=="m", wvar(ctry_year) type(abse) graph
predict logitMxStdm if sex=="m" & age>4
drop  _wls_wgt _wls_res  _wgt_res
*Females:
wls0 logitMxStd age if age>4 & sex=="f", wvar(ctry_year) type(abse) graph
predict logitMxStdf if sex=="f" & age>4
drop  _wls_wgt _wls_res _wgt_res

*Merging logit m(x) estimates:
gen logitMxStd2=logitMxStdm
	replace logitMxStd2=logitMxStdf if logitMxStdm==.
	drop logitMxStdm logitMxStdf logitMxStd
ren logitMxStd2 logitMxStd

*Fitting the relational model for each country and year by using the Extended Standard by LAC data:
gen pmxstdf=. // empty variable for predictions
tempvar pmxstdf // temporary variables for each set of predictions
levelsof ctry, local(levels)
foreach x of local levels {
   foreach z of numlist 1920/2012 {
      capture reg logitMx logitMxStd if ctry==`x' & year==`z' & sex=="f", noconstant
      if !_rc {
	  predict `pmxstdf' // predictions are now in temporary variable
      replace pmxstdf=`pmxstdf' if ctry==`x' & year==`z' & sex=="f" // transfer predictions from temp variable
      drop `pmxstdf' // drop temporary variables in preparation for next regression
     }
   }
 }

gen pmxstdm=. // empty variable for predictions
tempvar pmxstdm // temporary variables for each set of predictions
levelsof ctry, local(levels)
foreach x of local levels {
   foreach z of numlist 1920/2012 {
      capture reg logitMx logitMxStd if ctry==`x' & year==`z' & sex=="m", noconstant
      if !_rc {
	  predict `pmxstdm' // predictions are now in temporary variable
      replace pmxstdm=`pmxstdm' if ctry==`x' & year==`z' & sex=="m" // transfer predictions from temp variable
      drop `pmxstdm' // drop temporary variables in preparation for next regression
     }
   }
 } 
 
 *juntando pmxstdf/pmxstdm:
gen pmxstd=pmxstdf
	replace pmxstd=pmxstdm if pmxstdf==.
	drop pmxstdm pmxstdf

*Getting Mx with predict values after age 84:
gen MxStd=1/(1+exp(-pmxstd)) if age>4
gen logMxStd=log(MxStd)


*Getting lxlogit in all ages:
bysort ctry year sex: gen qxstd=(2*MxStd)/(2+MxStd)
gen lxstd=100000 if age==5
bysort ctry year sex: replace lxstd=(1-qxstd[_n-1])*lxstd[_n-1] if age>5 & age<=110

*Survival Function at all ages by relational model:
bysort ctry year sex: gen Sxstd=(lxstd/lxstd[6])

*Deaths function at all ages:
bysort ctry year sex: gen dx=lxstd-lxstd[_n+1]
bysort ctry year sex: replace dx=lxstd if age==110

*Person-year function at all ages:
bysort ctry year sex: gen Lx=lxstd[_n+1]+0.5*dx
bysort ctry year sex: replace Lx=lxstd/MxStd if age==110

*Person-year starting age x function:
bysort ctry year sex: gen Tx=Lx if age==110
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	110
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	109
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	108
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	107
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	106
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	105
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	104
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	103
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	102
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	101
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	100
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	99
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	98
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	97
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	96
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	95
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	94
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	93
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	92
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	91
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	90
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	89
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	88
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	87
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	86
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	85
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	84
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	83
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	82
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	81
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	80
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	79
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	78
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	77
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	76
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	75
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	74
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	73
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	72
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	71
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	70
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	69
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	68
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	67
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	66
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	65
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	64
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	63
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	62
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	61
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	60
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	59
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	58
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	57
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	56
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	55
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	54
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	53
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	52
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	51
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	50
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	49
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	48
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	47
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	46
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	45
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	44
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	43
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	42
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	41
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	40
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	39
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	38
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	37
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	36
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	35
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	34
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	33
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	32
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	31
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	30
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	29
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	28
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	27
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	26
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	25
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	24
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	23
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	22
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	21
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	20
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	19
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	18
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	17
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	16
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	15
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	14
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	13
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	12
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	11
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	10
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	9
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	8
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	7
bysort ctry year sex: replace Tx=Lx+Tx[_n+1] if age	<	6

*Life Expectancy at age x:
bysort ctry year sex: gen Ex=Tx/lxstd
