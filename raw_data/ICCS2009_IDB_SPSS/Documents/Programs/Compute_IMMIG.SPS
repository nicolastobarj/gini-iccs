*                                                                              .
*    SPSS Program to Calculate the Immigration Status of the Student (IMMIG)   .
*                          ICCS 2009 IDB User Guide                            .
*                                                                              .


*** IMMIGRATION STATUS ***.

recode IS2B02A IS2B02B IS2B02C (sysmis=8).
exec.
mis val IS2B02A IS2B02B IS2B02C ().
recode IS2B02A IS2B02B IS2B02C (1=1)(2 thru 10=2)(96,97,98,99=9)
   into SBORN MBORN FBORN.
freq SBORN MBORN FBORN.

compute IMMIG=(SBORN*100)+(MBORN*10)+FBORN.

recode IMMIG(222,229,292=3)(122,129,192=2)
   (199,299,911 thru 999=9) (111,121,112,212,221,191,119,291,219,211=1).

if (IS2B02A=97 and IS2B02B=97 and IS2B02C=97) immig=7.
if (IS2B02A=98 and IS2B02B=98 and IS2B02C=98) immig=8.

formats immig (f1.0).
mis val immig IS2B02A IS2B02B IS2B02C (96,97,99) .
var lab immig 'Immigration background'.
val lab immig 
    1 'Native students'
		2 'First-Generation students'
		3 'Non-native students'
    7 'Invalid'
    8 'Not administered'
    9 'Missing'.
exec.

mis val immig(9).

freq immig.

cros country by immig/cel row .