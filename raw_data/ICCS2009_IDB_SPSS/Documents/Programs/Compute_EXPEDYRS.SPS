*                                                                              .
*SPSS Program to Calculate the Student's Expected Years of Education (EXPEDYRS).
*                          ICCS 2009 IDB User Guide                            .
*                                                                              .


numeric EXPEDYRS (F2.0).

var lab EXPEDYRS "Expected years of further education".

Do if (country="AUT").
recode SISCED(0=0)(1=0)(2=4)(3=7)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="BFL").
recode SISCED(0=0)(1=0)(2=4)(3=6)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="BGR").
recode SISCED(0=0)(1=0)(2=4)(3=7)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="CHL").
recode SISCED(0=0)(1=0)(2=4)(3=8)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="TWN").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="COL").
recode SISCED(0=0)(1=1)(2=3)(3=6)(4=7)(else=sysmis) into EXPEDYRS.
Else if (country="CYP").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="CZE").
recode SISCED(0=0)(1=1)(2=5)(3=8)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="DNK").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="DOM").
recode SISCED(0=0)(1=0)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="ENG").
recode SISCED(0=0)(1=0)(2=4)(3=6)(4=7)(else=sysmis) into EXPEDYRS.
Else if (country="EST").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="FIN").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="GRC").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="GTM").
recode SISCED(0=0)(1=1)(2=3)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="HKG").
recode SISCED(0=0)(1=1)(2=5)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="IDN").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="IRL").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="ITA").
recode SISCED(0=0)(1=0)(2=5)(3=8)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="KOR").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="LVA").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="LIE").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="LTU").
recode SISCED(0=0)(1=2)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="LUX").
recode SISCED(0=0)(1=1)(2=5)(3=7)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="MLT").
recode SISCED(0=0)(1=2)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="MEX").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="NLD").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="NZL").
recode SISCED(0=0)(1=1)(2=3)(3=5)(4=6)(else=sysmis) into EXPEDYRS.
Else if (country="NOR").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="PRY").
recode SISCED(0=0)(1=1)(2=4)(3=8)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="POL").
recode SISCED(0=0)(1=1)(2=4)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="RUS").
recode SISCED(0=0)(1=1)(2=3)(3=6)(4=7)(else=sysmis) into EXPEDYRS.
Else if (country="SVK").
recode SISCED(0=0)(1=1)(2=5)(3=7)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="SVN").
recode SISCED(0=0)(1=1)(2=5)(3=7)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="ESP").
recode SISCED(0=0)(1=2)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
Else if (country="SWE").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=7)(else=sysmis) into EXPEDYRS.
Else if (country="CHE").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=9)(else=sysmis) into EXPEDYRS.
Else if (country="THA").
recode SISCED(0=0)(1=1)(2=4)(3=6)(4=8)(else=sysmis) into EXPEDYRS.
end if.

execute.

means EXPEDYRS by country/cel mean min max.