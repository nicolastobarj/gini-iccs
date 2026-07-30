/******************************************************************************/
/*                                                                            */
/*              Sample SAS Program to Invoke the JACKREGP Macro               */
/*                          ICCS 2009 IDB User Guide                          */
/*                                                                            */
/******************************************************************************/

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <datpath> must be edited ;

LIBNAME ICCS2009 "<datpath>" ;

* issue the include command to include the JACKREGP macro in the program ;
* <macpath> must be edited following the format x:\xxx\ ;

%INCLUDE "<macpath>JACKREGP.SAS" ;

* Get the student data and select cases of interest ;

DATA ISGALLC2 ;
     SET ICCS2009.ISGALLC2 ;

     * Select cases with no missing data ;

     WHERE NMISS (SGENDER) = 0 ;

     * Dummy recode of SGENDER ;

     SELECT (SGENDER) ;
      WHEN (0)  REGSEX = 0 ;  * Girls ;
      WHEN (1)  REGSEX = 1 ;  * Boys ;
      OTHERWISE REGSEX = . ;  * ALL MISSING DATA SET TO OMITTED ;
     END ;

     * Copy Plausible Values to new variables;

     PVCIV01 = PV1CIV ;
     PVCIV02 = PV2CIV ;
     PVCIV03 = PV3CIV ;
     PVCIV04 = PV4CIV ;
     PVCIV05 = PV5CIV ;

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

* Invoke the JACKREGP macro to compute the statistics ;

%JACKREGP (TOTWGTS, JKZONES, JKREPS, 75, IDCNTRY, REGSEX, PVCIV0, 5, ISGALLC2) ;

* Print the results ;

PROC PRINT DATA = REG NOOBS ;
     VAR IDCNTRY N MULT_RSQ SS_TOTAL SS_REG B00 B00_SE B01 B01_SE ;
     FORMAT IDCNTRY COUNTRY. N 6.0 MULT_RSQ 5.3 SS_TOTAL SS_REG 10.0
            B00 B00_SE B01 B01_SE 6.2 ;
RUN ;
