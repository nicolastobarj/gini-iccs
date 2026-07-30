*                                                                              .
*         SPSS Program to Score the ICCS 2009 European Module Items            .
*                          ICCS 2009 IDB User Guide                            .
*                                                                              .

DEFINE SCOREIT (TYPE  = !CHAREND('/') /
                ITEM  = !CHAREND('/') /
                RIGHT = !CHAREND('/') /
                NR    = !CHAREND('/') /
                NA    = !CHAREND('/') /
                OM    = !CHAREND('/') /
                OTHER = !CHAREND('/') ) .

* Code for multiple-choice items .

 !IF (!UPCASE(!TYPE)=MC) !THEN
    !DO !I !IN (!ITEM) .
       RECODE !I (!RIGHT = 1      )
                 (!NR    = 0      ) /* Not Reached      */
                 (!NA    = SYSMIS ) /* Not Administered */
                 (!OM    = 0      ) /* Omitted          */
                 (!OTHER = 0      ) /* Other Missing    */
                 (ELSE   = 0      ) .
    !DOEND .
 !IFEND .

!ENDDEFINE .

DEFINE DOIT (COUNTRY = !CHAREND('/') ) .

* Specify the path for the SPSS data files following the format x:\xxx\ .
* <path> must be edited .

!LET !LIBDAT = !UNQUOTE("<path>") .

* Country loop .

!DO !CTY !IN (!COUNTRY) .

* Get the student achievement data .

ADD FILES
  / FILE = !QUOTE(!CONCAT(!LIBDAT,'ISE',!CTY,'C2.sav')) .

* Score multiple-choice items with A key .

SCOREIT TYPE  = MC      /
        ITEM  = ES2T01A ES2T01B ES2T01C ES2T09B ES2T09C ES2T09D
				ES2T09E ES2T10 ES2T11C /
        RIGHT = 1       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with B key .

SCOREIT TYPE  = MC      /
        ITEM  = ES2T02 ES2T04 ES2T06 ES2T09A ES2T11A ES2T11B ES2T12 /
        RIGHT = 2       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with C key .

SCOREIT TYPE  = MC      /
        ITEM  = ES2T03 ES2T07 ES2T08  /
        RIGHT = 3       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with D key .

SCOREIT TYPE  = MC      /
        ITEM  = ES2T05  /
        RIGHT = 4       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

EXECUTE .

* Save the student data .

SAVE OUTFILE = !QUOTE(!CONCAT(!LIBDAT,'ESC',!CTY,'C2.sav')) .

!DOEND .

!ENDDEFINE .

* Specify the list of countries to be processed .

DOIT COUNTRY = AUT BFL BGR CHE CYP CZE
			   DNK ENG ESP EST FIN GRC
			   IRL ITA LIE LTU LUX LVA
			   MLT NLD POL SVK SVN SWE  .
