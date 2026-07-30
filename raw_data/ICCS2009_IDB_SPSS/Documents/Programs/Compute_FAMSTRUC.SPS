*                                                                              .
*          SPSS Program to Calculate the Family Structure (FAMSTRUC)           .
*                          ICCS 2009 IDB User Guide                            .
*                                                                              .


recode IS2B01A IS2B01B IS2B01C IS2B01D  (sysmis=8).
exec.

numeric FAMSTRUC (f1.0).
missing value IS2B01A IS2B01B IS2B01C IS2B01D  ().
execute.
count nbcount=IS2B01A IS2B01B IS2B01C IS2B01D  (1).
do if (nbcount=1). 
compute FAMSTRUC = 1.
else if (nbcount=2 and IS2B01A=1 and IS2B01C=1).
compute FAMSTRUC=2.
else if (nbcount=2 and IS2B01A=1 and IS2B01D=1). 
compute FAMSTRUC=3.
else if (nbcount=2 and IS2B01B=1 and IS2B01C=1).
compute FAMSTRUC=3.
else if (nbcount=2 and IS2B01B=1 and IS2B01D=1).
compute FAMSTRUC=3.
else if ((IS2B01A=7) and (IS2B01B=7) and (IS2B01C=7) and (IS2B01D=7)).
compute FAMSTRUC=7.
else if ((IS2B01A=8) and (IS2B01B=8) and (IS2B01C=8) and (IS2B01D=8)).
compute FAMSTRUC=8.
else if ((IS2B01A=9) and (IS2B01B=9) and (IS2B01C=9) and (IS2B01D=9)).
compute FAMSTRUC=9.
else if ((IS2B01A=7 or IS2B01A=8 or IS2B01A=9)
   and (IS2B01B=7 or IS2B01B=8 or IS2B01B=9) 
   and (IS2B01C=7 or IS2B01C=8 or IS2B01C=9)
   and (IS2B01D=7 or IS2B01D=8 or IS2B01D=9)).
compute FAMSTRUC=9.
else. 
compute FAMSTRUC=4.
end if.
execute.
missing value IS2B01A IS2B01B IS2B01C IS2B01D  FAMSTRUC (7,8,9).
var lab FAMSTRUC 'Family Structure'.
val lab FAMSTRUC 	
      1 'Single parent family'
      2 'Nuclear family'
      3 'Mixed family'
      4 'Other'
			7 'N/A'
			8 'Invalid'
			9 'Missing'.
execute.


