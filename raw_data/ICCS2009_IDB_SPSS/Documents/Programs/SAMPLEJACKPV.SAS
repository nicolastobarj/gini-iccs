/******************************************************************************/
/*                                                                            */
/*               Sample SAS Program to Invoke the JACKPV Macro                */
/*                          ICCS 2009 IDB User Guide                          */
/*                                                                            */
/******************************************************************************/

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <datpath> must be edited ;

LIBNAME ICCS2009 "<datpath>" ;

* Issue the include command to include the JACKPV macro in the program ;
* <macpath> must be edited following the format x:\xxx\ ;

%INCLUDE "<macpath>" ;

* Get the student data and select cases of interest ;

DATA ISGALLC2 ;
     SET ICCS2009.ISGALLC2 ;

     * Select cases with no missing data ;

     WHERE NMISS (SGENDER) = 0 ;

* Define formats for the variables used ;

PROC FORMAT LIBRARY = WORK ;

 VALUE COUNTRY
    40 = 'AUT'
   100 = 'BGR'
   152 = 'CHL'
   158 = 'TWN'
   170 = 'COL'
   196 = 'CYP'
   203 = 'CZE'
   208 = 'DNK'
   214 = 'DOM'
   233 = 'EST'
   246 = 'FIN'
   300 = 'GRC'
   320 = 'GTM'
   344 = 'HKG'
   360 = 'IDN'
   372 = 'IRL'
   380 = 'ITA'
   410 = 'KOR'
   428 = 'LVA'
   438 = 'LIE'
   440 = 'LTU'
   442 = 'LUX'
   470 = 'MLT'
   484 = 'MEX'
   528 = 'NLD'
   554 = 'NZL'
   578 = 'NOR'
   600 = 'PRY'
   616 = 'POL'
   643 = 'RUS'
   703 = 'SVK'
   705 = 'SVN'
   724 = 'ESP'
   752 = 'SWE'
   756 = 'CHE'
   764 = 'THA'
   926 = 'ENG'
   956 = 'BFL' ;

 VALUE SEX
     0 = 'BOY'
     1 = 'GIRL' ;

* Invoke the JACKPV macro to compute the statistics ;

%JACKPV (TOTWGTS, JKZONES, JKREPS, 75, IDCNTRY SGENDER, 5, ISGALLC2) ;

* Print the results ;

PROC PRINT DATA = FINAL NOOBS ;
     VAR IDCNTRY SGENDER N TOTWGTS MNPV MNPV_SE PCT PCT_SE ;
     FORMAT IDCNTRY COUNTRY. SGENDER SEX. N 6.0 TOTWGTS 10.0
            MNPV MNPV_SE PCT PCT_SE 6.2 ;
RUN ;
