*                                                                              .
*     SPSS Program to Calculate the Parents' Years of Education (PAREDYRS)     .
*                          ICCS 2009 IDB User Guide                            .
*                                                                              .


numeric HISCED (F1.0)/ PAREDYRS (F2.0).

mis val MISCED FISCED (7,8,9).
compute HISCED=max(MISCED,FISCED).
EXECUTE .

cros country by hisced/cel row.

Do if (country="AUT").
recode HISCED (0=2)(1=4)(2=8)(3=12)(4=15)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="BFL").
recode HISCED (0=2)(1=6)(2=8)(3=12)(4=14)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="BGR").
recode HISCED (0=2)(1=4)(2=8)(3=12)(4=15)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="CHL").
recode HISCED (0=2)(1=6)(2=8)(3=12)(4=16)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="TWN").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="COL").
recode HISCED (0=2)(1=5)(2=9)(3=11)(4=14)(5=15)(else=sysmis) into PAREDYRS.
Else if (country="CYP").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="CZE").
recode HISCED (0=2)(1=5)(2=9)(3=13)(4=16)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="DNK").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=15)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="DOM").
recode HISCED (0=2)(1=6)(2=8)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="ENG").
recode HISCED (0=2)(1=6)(2=9)(3=13)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="EST").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="FIN").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="GRC").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=15)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="GTM").
recode HISCED (0=2)(1=6)(2=9)(3=11)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="HKG").
recode HISCED (0=2)(1=6)(2=9)(3=13)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="IDN").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="IRL").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="ITA").
recode HISCED (0=2)(1=5)(2=8)(3=13)(4=16)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="KOR").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="LVA").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="LIE").
recode HISCED (0=2)(1=5)(2=9)(3=12)(4=14)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="LTU").
recode HISCED (0=2)(1=4)(2=10)(3=12)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="LUX").
recode HISCED (0=2)(1=6)(2=9)(3=13)(4=15)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="MLT").
recode HISCED (0=2)(1=6)(2=11)(3=13)(4=16)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="MEX").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="NLD").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="NZL").
recode HISCED (0=2)(1=7)(2=10)(3=12)(4=14)(5=15)(else=sysmis) into PAREDYRS.
Else if (country="NOR").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="PRY").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=16)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="POL").
recode HISCED (0=2)(1=4)(2=8)(3=12)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="RUS").
recode HISCED (0=2)(1=4)(2=9)(3=11)(4=14)(5=15)(else=sysmis) into PAREDYRS.
Else if (country="SVK").
recode HISCED (0=2)(1=4)(2=8)(3=12)(4=13)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="SVN").
recode HISCED (0=2)(1=4)(2=8)(3=11)(4=15)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="ESP").
recode HISCED (0=2)(1=5)(2=8)(3=11)(4=13)(5=16)(else=sysmis) into PAREDYRS.
Else if (country="SWE").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=15)(else=sysmis) into PAREDYRS.
Else if (country="CHE").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=17)(else=sysmis) into PAREDYRS.
Else if (country="THA").
recode HISCED (0=2)(1=6)(2=9)(3=12)(4=14)(5=16)(else=sysmis) into PAREDYRS.
end if.

EXECUTE .

means PAREDYRS by country/cel mean min max.

