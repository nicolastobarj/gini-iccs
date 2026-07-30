/******************************************************************************/
/*                                                                            */
/*            SAS Program for ICCS 2009 IDB User Guide Example 1              */
/*                ICCS 2009 International Report Table 3.10                   */
/*                                                                            */
/******************************************************************************/

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <datpath> must be edited ;

LIBNAME ICCS2009 "<datpath>" ;

* Issue the include command to include the JACKGEN macro in the program ;
* <macpath> must be edited following the format x:\xxx\ ;

%INCLUDE "<macpath>JACKGEN.SAS" ;

* Get the student data and select cases of interest ;

DATA ISGALLC2 ;
     SET ICCS2009.ISGALLC2 ;

     * Select cases with no missing data ;

     WHERE NMISS (SAGE) = 0 ;

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

* Invoke the JACKGEN macro to compute the statistics ;

%JACKGEN (TOTWGTS, JKZONES, JKREPS, 75, IDCNTRY, SAGE, ISGALLC2) ;

* Print the results ;

PROC PRINT DATA = FINAL NOOBS ;
     VAR IDCNTRY N TOTWGTS MNX MNX_SE PCT PCT_SE ;
     FORMAT IDCNTRY COUNTRY. N 6.0 TOTWGTS 10.0
            MNX MNX_SE PCT PCT_SE 6.2 ;
RUN ;
