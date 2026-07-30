/******************************************************************************/
/*                                                                            */
/*            SAS Program for ICCS 2009 IDB User Guide Example 1              */
/*                ICCS 2009 International Report Table 6.18                   */
/*                                                                            */
/******************************************************************************/

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <datpath> must be edited ;

LIBNAME ICCS2009 "<datpath>" ;

* Issue the include command to include the JACKGPV macro in the program ;
* <macpath> must be edited following the format x:\xxx\ ;

%INCLUDE "<macpath>JACKGEN.SAS" ;

* Get the teacher data ;

DATA ITGALLC2 ;
     SET ICCS2009.ITGALLC2 ;

     * Select cases with no missing data ;

     IF NMISS(IT2G28A) = 0;

     * Combine categories of IT2G28A ;

     SELECT (IT2G28A) ;
      WHEN (1,2) NEW28A = 1 ;  * Confident in teaching human rights ;
      WHEN (3,4) NEW28A = 2 ;  * Not Confident in teaching human rights ;
      OTHERWISE  NEW28A = . ;  * All other responses ;
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

 VALUE NEW28A
     1 = 'Confident' 
	 2 = 'Not conf.' ;

* Invoke the JACKGEN macro to compute the statistics ;

%JACKGEN (TOTWGTT, JKZONET, JKREPT, 75, IDCNTRY NEW28A, IDCNTRY, ITGALLC2) ;

* Print the results ;

PROC PRINT DATA = FINAL NOOBS ;
     VAR IDCNTRY NEW28A N TOTWGTT PCT PCT_SE ;
     FORMAT IDCNTRY COUNTRY. NEW28A NEW28A. N 6.0 TOTWGTT 10.0
            PCT PCT_SE 6.2 ;
RUN ;
