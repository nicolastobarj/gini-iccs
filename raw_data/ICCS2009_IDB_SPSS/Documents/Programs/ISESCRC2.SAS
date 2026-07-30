/******************************************************************************/
/*                                                                            */
/*     SAS Program to Score the ICCS 2009 European Module Knowledge Items     */
/*                         ICCS 2009 IDB User Guide                           */
/*                                                                            */
/******************************************************************************/

OPTIONS NONOTES ;

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <path> must be edited ;

LIBNAME LIBDAT "<path>" ;

* Specify the grade (I for target grade, J for additional grade) to be processed ;
* <grade> must be edited ;

%LET GRADE = <grade> ;

* Adapt the list of countries to be processed, if desired ;

%LET COUNTRY = AUT BFL BGR CHE CYP CZE 
               DNK ENG ESP EST FIN GRC 
               IRL ITA LIE LTU LUX LVA
               MLT NLD POL SVK SVN SWE ;

* List of multiple-choice items with A key ;

%LET ARIGHT = ES2T01A ES2T01B ES2T01C ES2T09B ES2T09C ES2T09D
				ES2T09E ES2T10 ES2T11C  ;

* List of multiple-choice items with B key ;

%LET BRIGHT = ES2T02 ES2T04 ES2T06 ES2T09A ES2T11A ES2T11B ES2T12  ;

* List of multiple-choice items with C key ;

%LET CRIGHT = ES2T03 ES2T07 ES2T08  ;

* List of multiple-choice items with D key ;

%LET DRIGHT = ES2T05  ;

%MACRO SCOREIT (ITEM, TYPE, RIGHT, NR, NA, OM, OTHER) ;

   * Code for multiple-choice items ;

   %IF %UPCASE(&TYPE) = "MC" %THEN %DO ;
      SCORE = 0 ;
         IF &ITEM = &RIGHT THEN SCORE = 1 ;
         IF &ITEM = &NR    THEN SCORE = 0 ; * Not Reached ;
         IF &ITEM = &NA    THEN SCORE = . ; * Not Administered ;
         IF &ITEM = &OM    THEN SCORE = 0 ; * Omitted ;
         IF &ITEM = &OTHER THEN SCORE = 0 ; * Other Missing ;
      &ITEM = SCORE ;
   %END ;

%MEND SCOREIT ;

%MACRO DOIT ;

   * Start of country processing loop ;

   %LET I = 1 ;
   %LET GRADE = %SCAN(&GRADE,&I) ;
   %DO %WHILE(%LENGTH(%SCAN(&COUNTRY,&I))) ;
      %LET CTRY = %SCAN(&COUNTRY,&I) ;

      * Get the student achievement data ;

      DATA LIBDAT.&GRADE.CE&CTRY.C2; * Output File ;
           SET LIBDAT.&GRADE.SE&CTRY.C2; * Input File  ;

           ARRAY ARIGHT &ARIGHT ;
           ARRAY BRIGHT &BRIGHT ;
           ARRAY CRIGHT &CRIGHT ;
           ARRAY DRIGHT &DRIGHT ;

           DO OVER ARIGHT ; %SCOREIT (ARIGHT, "MC", 1, .R, .A, ., .I) ; END ;
           DO OVER BRIGHT ; %SCOREIT (BRIGHT, "MC", 2, .R, .A, ., .I) ; END ;
           DO OVER CRIGHT ; %SCOREIT (CRIGHT, "MC", 3, .R, .A, ., .I) ; END ;
           DO OVER DRIGHT ; %SCOREIT (DRIGHT, "MC", 4, .R, .A, ., .I) ; END ;

      RUN ;

      * End of country processing loop ;

      %LET I = %EVAL(&I + 1) ;
   %END ;

%MEND DOIT ;

%DOIT ;
