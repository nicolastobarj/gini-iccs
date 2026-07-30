/******************************************************************************/
/*                                                                            */
/*            SAS Program for ICCS 2009 IDB User Guide Example 1              */
/*                ICCS 2009 International Report Table 6.2                    */
/*                                                                            */
/******************************************************************************/

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <datpath> must be edited ;

LIBNAME ICCS2009 "<datpath>" ;

* Issue the include command to include the JACKGPV macro in the program ;
* <macpath> must be edited following the format x:\xxx\ ;

%INCLUDE "<macpath>JACKPV.SAS" ;

* Get and sort the school data ;

PROC SORT DATA = ICCS2009.ICGALLC2 OUT = ICGALLC2 ;
     BY IDCNTRY IDSCHOOL ;

* Get and sort the student data ;

PROC SORT DATA = ICCS2009.ISGALLC2 OUT = ISGALLC2 ;
     BY IDCNTRY IDSCHOOL ;

* Merge the two files ;

DATA MERGED ;
     MERGE ICGALLC2 (IN = INICG)
           ISGALLC2 (IN = INISG) ;
     BY IDCNTRY IDSCHOOL ;
     IF INICG AND INISG ;

* Read the merged file and select cases of interest ;

DATA MERGED ;
     SET MERGED ;

     * Select cases with no missing data ;

     IF NMISS (IC2G06B) = 0 ;

     * Combine categories of IC2G06B ;

     SELECT (IC2G06B) ;
      WHEN (1,2) NEW06B = 1 ;  * STUDENTS HAVE BEEN INVOLVED IN ;
      WHEN (3,4,5) NEW06B = 2 ;  * STUDENTS HAVE NOT BEEN INVOLVED IN ;
      OTHERWISE  NEW06B = . ;
     END ;

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

 VALUE NEW06B
     1 = 'Students have been involved in'
     2 = 'Students have not been involved in' ;

* Invoke the JACKPV macro to compute the statistics ;

%JACKPV (TOTWGTS, JKZONES, JKREPS, 75, IDCNTRY NEW06B, 5, MERGED) ;

* Print the results ;

PROC PRINT DATA = FINAL NOOBS ;
     VAR IDCNTRY NEW06B N TOTWGTS MNPV MNPV_SE PCT PCT_SE ;
     FORMAT IDCNTRY COUNTRY. NEW06B 1.0 N 6.0 TOTWGTS 10.0
            MNPV MNPV_SE PCT PCT_SE 6.2 ;
RUN ;
