CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   y   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:49:10Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       1.0    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      software_version      51.10 (version 30.06.2020 for ARGO_simplified_profile)         :   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                     3�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    4   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    4    REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    4$   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    44   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    4D   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    4T   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  4\   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  4�   STATION_PARAMETERS                       	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                    4�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        5�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    5�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    5�   PARAMETER_DATA_MODE                   	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    5�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     5�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     6   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     6,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    6L   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        ?q        6P   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    6X   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >��	4E�        6\   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           6d   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           6l   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    6t   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    6x   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        6�   	PARAMETER            
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                    6�   SCIENTIFIC_CALIB_EQUATION            
               	long_name         'Calibration equation for this parameter    
_FillValue                    8�   SCIENTIFIC_CALIB_COEFFICIENT         
               	long_name         *Calibration coefficients for this equation     
_FillValue                    @�   SCIENTIFIC_CALIB_COMMENT         
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    H�   SCIENTIFIC_CALIB_DATE            
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  p  P�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    P�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    P�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    P�   PROFILE_DOXY_QC                	long_name         #Global quality flag of DOXY profile    conventions       Argo reference table 2a    
_FillValue                    Q    PRES         	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  Q   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  Sd   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  UH   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  Y�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Z   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  ]�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^L   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `0   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  b   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  b�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  dt   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  fX   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  j�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  k   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  n�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  o\Argo synthetic profile          1.0 1.2 19500101000000  20201028144910  20201028144910  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�:;����1   @�:>b��@@H�bM���32� ě�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014291 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559292015101515592920151015155929                                                        20161116115901A   B   B   B       @   @�  @�  @�  A   A   A@  A`  A�  A�  A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C9  CC  CM  CW  Ca  Cl  Cv  C�  C�  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�  C�� C�� C�  Cʀ Cπ C�  C�  Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D9  D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}  D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       @   @�  @�  @�  A   A   A@  A`  A�  A�  A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C9  CC  CM  CW  Ca  Cl  Cv  C�  C�  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�  C�� C�� C�  Cʀ Cπ C�  C�  Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D9  D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}  D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A?A?�wA?��A?�;A?��A?��A?�
A?�
A?�
A?�;A?�TA?�;A?�;A?�A?��A@JA@1A@JA@bA@{A@bA@�A@�A@{A@1'A@=qA@JA?��A?�mA?�mA?�A?/A=�7A<-A<bNA;�FA;t�A:�uA9�A9%A8�DA8$�A7�A7`BA6�HA6r�A65?A5��A5t�A5�A5�A4�\A4-A3�A3�;A3��A3�7A2��A2�A1�^A1��A1�A0bA0  A.n�A&Q�A&�uA$bA �A%��A$�A"��A(�A�DA�PA n�A�FAO�A�A��A��@�@��y@��@۝�@�|�@�&�@�?}@��@���@�7L@���@�j@���@��-@�-@�^5@��;@���@���@��@��j@��R@��@��R@��`@��@��h@�  @~�R@|�@y��@vȴ@uV@s�
@r=q@pb@nv�@l�@kdZ@j�H3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�A?��A?��A?��A?�
A?�
A?�
A?�;A?�TA?�;A?�;A?�A?��A@JA@1A@JA@bA@{A@bA@�A@�A@{A@1'A@=qA@JA?��A?�mA?�mA?�A?/A=�7A<-A<bNA;�FA;t�A:�uA9�A9%A8�DA8$�A7�A7`BA6�HA6r�A65?A5��A5t�A5�A5�A4�\A4-A3�A3�;A3��A3�7A2��A2�A1�^A1��A1�A0bA0  A.n�A&Q�A&�uA$bA �A%��A$�A"��A(�A�DA�PA n�A�FAO�A�A��A��@�@��y@��@۝�@�|�@�&�@�?}@��@���@�7L@���@�j@���@��-@�-@�^5@��;@���@���@��@��j@��R@��@��R@��`@��@��h@�  @~�R@|�@y��@vȴ@uV@s�
@r=q@pb@nv�@l�@kdZ@j�H4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBs�Bt�Bs�Br�Bs�Br�Br�Bs�Bs�Bs�Br�Br�Br�Br�Br�Br�Br�Br�Br�Bq�Bq�Br�Bq�Bq�Bs�Bs�Bq�Bp�Bo�Bp�Bq�Bk�BYBJ�BXBP�BM�BC�B8RB33B/B,B%�B$�B �B�B�B�B�BoBoBJB1B+B1B
=B	7BBBB+B%BBJBB��BĜB�XB�B#�BC�BXB�BJ�B�PB�'B��B��B�yB��B�mB1'B\B$�B��B�}By�BW
B33B+B;dBH�B9XB,B"�B�B1B  B�B�B�;B�B��B��BĜB�jB�LB�B��B��B��B��B��B��B��B��B��B��B��B��B��3333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�B�}B�}B�vB�vB�}B�{B�|B�vB�vB�vB�vB�vB�tB�wB�tB�vB�qB�qB�vB�oB�pB�~B�|B�pBlB~cBiB�nBzLBg�BY�Bf�B_�B\�BRUBGBA�B=�B:�B4�B3�B/~B-rB+gB*bB$9B!+B!+BB�B�B�B�B�B�B�B�B�B�B�BB�B��B�OB�B��B2�BRTBf�B.rBYzB�B��B��BۘB�EB��B�B?�B�B3pBWB�B�WBe�BA�B9sBI�BW+BG�B:yB1BB$�B�BmBB��B��B�tB�TB�#B��B��BūB�}B�YB�ZB�ZB�BB�!B�B�B�B�B�B�B�B�	4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cr��Cr��Cr�4G�O�Cr��CrөCr�;Cr�=Cr�Cr�bCr�+Cr�~Cr��Cs�Cs)FCs=�Cr��Cr��Cq�[Cr��Cr�Cr3�CrJ4Cq�Cp��Cp3�Cq�Cq��Cpt�CkֈCe��Cb�mC_�C_��Cc��Ce�5Cg�`Cj��Cl|�Cl��Cmm&Cm��Cm��Cm��Cl�7CmCmrClc�CmACl�CCl�ECl��Ck��Cj7Ch��Cg{�Ce/�Cb�Cb�Cb8C`��C_]�CZ��CSLCI��C@4}CA �C>oC7 6C1��C.vLC,�\C- VC,N�C+�C+��C,��C,�AC,�@C0��C6�C>��CB�;CDd5CJ?CN��CT�$CX�0C\��C^��C^�UC_��CbQ�Cd�Ce��Cg�XCi\�Ck#�Cl�Cn�<Co�,Cqs�Crn�Csj�Cth~CudCvcdCw`7Cx^cCx�*Cx��Cx�rCy�Cz�CzH�Cy�BCy�6Cz�Cz2�Czc�Czz|333 333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333               G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    C�C��C�	G�O�CҦC�C��C�wC�iC�-C�^C�C�ZC��C�oC�C�C��CC�+C2�CE�CY�C~�3C~�C}~RC~��CMC}�Cy��Ct,`Cq}�CnܫCo iCr��Ct��Cv.CCy�Cz�iCz�C{xpC{�WC{�jC{��C{#�C{:�C{R
Cz�cC{~DCz�Cz��C{�Czt�Cy#�Cw�mCvzsCtu�CrtECr�Cq�Cp�
Cop�Ckp0Cd�'C\["CTz�CUFCR�GCL�7CG�pCD̟CC��CDiCC`SCB��CB�tCC��CC�4CC�{CGēCM��CUԢCY�C[SkC`��Ce�FCk�Cn��Cr�Ct�*Ct��Cu��Cx
�Cy��C{f�C};C~��C�;�C��C��C�hC�=vC��{C�/�C���C�"HC��JC��C��C��tC��bC���C�Q�C�j�C��C�? C�X�C�q�G�O�G�O�G�O�111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444   @\�@_>@b�G�O�@e�@h�@k�@n<@q@t@u�@x�@�,@��@��@��@<�@Ia@�0@g�@�T@o@�@��@,�@��@�A@�M@�4@�@��@y@�@@�l@2@@=@Mm@Z @f�@��@ v@�@�@�B@�H@��@I�@׳@d�@p�@}M@
@�@�@&>@�@4�@>�@Ƕ@�'@�V@
@�@�?��?�4c?�H<?�=�?�I�?�O�?�]�?�q?܎?۵�?�ޯ?�M?�!�?�H ?�?鴑?��Z?�'�?�a�@S�@u@i@
�H@_Q@|�@�A@4z@�@�_@9@9.@Z7@}p@�@�Z@b�@��@(O@�\@k�@�@�@@R@��@N@1@N]@�U@ �@ .�@Ƒ@��@ ?G�O�G�O�G�O�