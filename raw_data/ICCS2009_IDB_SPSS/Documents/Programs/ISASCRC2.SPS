*                                                                              .
*                 SPSS Program to Score the ICCS 2009 Items                    .
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

* Code for constructed-response items .

 !IF (!UPCASE(!TYPE)=CR) !THEN
    !DO !I !IN (!ITEM) .
       RECODE !I (2      = 2      )
                 (1      = 1      )
                 (0      = 0      )
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
  / FILE = !QUOTE(!CONCAT(!LIBDAT,'ISA',!CTY,'C2.sav')) .

* Score multiple-choice items with A key .

SCOREIT TYPE  = MC      /
        ITEM  = CI2GLM2 CI2FSM1 CI2SCM2 CI2CNM1 CI2ETM2 CI2SRM1
				CI2SRM3 CI2PFM2 CI2PCM2 CI2VOM1 CI2VOM3 CI2HRM1
				CI2ECM2 CI2PJM2 CI121M1 /
        RIGHT = 1       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with B key .

SCOREIT TYPE  = MC      /
        ITEM  = CI2SHM1 CI2TGM1 CI2TGM2 CI2BPM1 CI2GLM1 CI2ASM2
				CI2CNM2 CI2CCM1 CI2CCM2 CI2SRM2 CI2HRM2 CI2PFM1
				CI2JOM1 CI101M1 CI113M1 CI115M1   /
        RIGHT = 2       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with C key .

SCOREIT TYPE  = MC      /
        ITEM  = CI2COM1 CI2MLM1 CI2MLM2 CI2BPM2 CI2GFM1 CI2BCM1
				CI2OMM2 CI2DCM1 CI2PCM1 CI2PGM1 CI2CEM1 CI2ORM1
				CI2PJM1 CI2REM2 CI108M1 CI119M1 CI120M1 CI127M1
				CI132M1 CI129M1 CI106M1  /
        RIGHT = 3       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score multiple-choice items with D key .

SCOREIT TYPE  = MC      /
        ITEM  = CI2MOM1 CI2RDM2 CI2SHM2 CI2FDM1 CI2SCM1 CI2ASM1
				CI2PRM1 CI2OMM1 CI2OMM3 CI2VOM2 CI2DLM1 CI2PGM2
				CI2ECM1 CI2CEM2 CI2RCM1 CI2REM3 CI109M1 CI128M1
				CI137M1 CI110M1 CI104M1 CI130M1  /
        RIGHT = 4       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

* Score Constructed-Response Items .

SCOREIT TYPE  = CR      /
        ITEM  = CA2PDO1 CI2BIO1 CI2ETO1 CI2RRO1 CI2WFO1 CI2WFO2 /
        RIGHT = 0       /
        NR    = 6       /
        NA    = SYSMIS  /
        OM    = 9       /
        OTHER = 7       .

EXECUTE .

* Save the student data .

SAVE OUTFILE = !QUOTE(!CONCAT(!LIBDAT,'ISC',!CTY,'C2.sav')) .

!DOEND .

!ENDDEFINE .

* Specify the list of countries to be processed .

DOIT COUNTRY = AUT BFL BGR CHE CHL COL CYP CZE DNK
			   DOM ENG ESP EST FIN GRC GTM HKG IDN
			   IRL ITA KOR LIE LTU LUX LVA MEX MLT
			   NLD NOR NZL POL PRY RUS SVK SVN SWE
			   THA TWN  .
