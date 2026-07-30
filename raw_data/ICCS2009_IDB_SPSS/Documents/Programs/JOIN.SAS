/******************************************************************************/
/*                                                                            */
/*       SAS Program to Join SAS Data Files into a Single SAS Data File       */
/*                          ICCS 2009 IDB User Guide                          */
/*                                                                            */
/******************************************************************************/

OPTIONS NONOTES ;

* Specify the file type (ICG, ITG, ISA, ISC, ISG, ISR, ISS, ISE, ICE, ISL, ICL, JSA, JSC, JSG, JSR, JSE, JCE);
* <type> must be edited ;

%LET TYPE = <type> ;

* Specify the path for the SAS data files following the format x:\xxx\ ;
* <path> must be edited ;

LIBNAME LIBDAT "<path>" ;

%MACRO DOIT ;

   * Adapt the list of countries to be processed, if desired ;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = G OR %UPCASE(%SUBSTR(&TYPE,3,1)) = A OR %UPCASE(%SUBSTR(&TYPE,3,1)) = R %THEN %DO ; * All countries (international items);

      %LET COUNTRY = AUT BFL BGR CHE CHL COL CYP CZE DNK
                     DOM ENG ESP EST FIN GRC GTM HKG IDN
                     IRL ITA KOR LIE LTU LUX LVA MEX MLT
                     NLD NOR NZL POL PRY RUS SVK SVN SWE
                     THA TWN ;

   %END ;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = S %THEN %DO ; * Asian Module countries ;

      %LET COUNTRY = HKG IDN KOR THA TWN ;

   %END ;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = E %THEN %DO ; * European Module countries ;

      %LET COUNTRY = AUT BFL BGR CHE CYP CZE
                     DNK ENG ESP EST FIN GRC
                     IRL ITA LIE LTU LUX LVA
                     MLT NLD POL SVK SVN SWE ;

   %END ;

   %IF %UPCASE(%SUBSTR(&TYPE,3,1)) = L %THEN %DO ; * Asian Module countries ;

      %LET COUNTRY = CHL COL DOM GTM MEX PRY ;

   %END ;

   DATA &TYPE.ALLC2 ;
        SET %LET I = 1 ;
            %DO %WHILE(%LENGTH(%SCAN(&COUNTRY,&I))) ;
               %LET CTRY = %SCAN(&COUNTRY,&I) ;
                  LIBDAT.&TYPE&CTRY.C2
               %LET I = %EVAL(&I + 1) ;
            %END ; ;
   RUN ;

   * Specify the sorting variables ;
   * Dependent of the file type ;

   %LET SORTVARS = IDCNTRY IDSTUD ;

   %IF %UPCASE(%SUBSTR(&TYPE,2,2)) = CG %THEN %DO ;
      %LET SORTVARS = IDCNTRY IDSCHOOL ; %END ;

   %IF %UPCASE(%SUBSTR(&TYPE,2,2)) = TG %THEN %DO ;
      %LET SORTVARS = IDCNTRY IDTEACH ; %END ;

   PROC SORT DATA=&TYPE.ALLC2 OUT=LIBDAT.&TYPE.ALLC2 ;
        BY &SORTVARS ;
   RUN ;

%MEND DOIT ;

%DOIT ;
