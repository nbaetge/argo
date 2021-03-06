CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   y   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:01:21Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        >�EȠ�Q)        6\   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        :�o     �  o\Argo synthetic profile          1.0 1.2 19500101000000  20210302050121  20210302050121  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�,:�`�1   @�,<;Z��@G�ȴ9X�12� ě�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99956 (+/- 6e-05) , vertically averaged dS =-0.017318 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cu  C  C�  C�  C�� C�� C�� C�� C�� C�  C�  C�� C�� C�� C�  Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,@ D2  D8@ D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cu  C  C�  C�  C�� C�� C�� C�� C�� C�  C�  C�� C�� C�� C�  Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,@ D2  D8@ D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��9A��9A��jA��jA���A���A���A�A���A�A�A��jA���A�  Ay?}A_ƨAYl�AW�AU&�AS��AQ�APVAO�TAOp�AM�
AK��AJ�9AI�-AH �AE`BAD1'AC"�ABbNA@��A@ �A?p�A>�9A=��A=�A;��A:�HA:bA9x�A9�A8��A8I�A7�#A7\)A6�HA6M�A5�#A5"�A4^5A3��A2��A2(�A1K�A0��A/�A/�A.bA,I�A*$�A(1'A'�A&��A#t�A�wA�RA�;A^5A��A��AAhsA�A
jA{AI�A%A 9X@��@�@���@�+@�@�X@�+@��@���@��@��@�x�@� �@���@���@��@�V@��@�n�@��D@��@���@�9X@���@��@�x�@��F@��7@�j@K�@}O�@y��@w+@w
=@u`B@r��@q&�@pA�@m��@l�/3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A��9A��9A��jA��jA���A���A���A�A���A�A�A��jA���A�  Ay?}A_ƨAYl�AW�AU&�AS��AQ�APVAO�TAOp�AM�
AK��AJ�9AI�-AH �AE`BAD1'AC"�ABbNA@��A@ �A?p�A>�9A=��A=�A;��A:�HA:bA9x�A9�A8��A8I�A7�#A7\)A6�HA6M�A5�#A5"�A4^5A3��A2��A2(�A1K�A0��A/�A/�A.bA,I�A*$�A(1'A'�A&��A#t�A�wA�RA�;A^5A��A��AAhsA�A
jA{AI�A%A 9X@��@�@���@�+@�@�X@�+@��@���@��@��@�x�@� �@���@���@��@�V@��@�n�@��D@��@���@�9X@���@��@�x�@��F@��7@�j@K�@}O�@y��@w+@w
=@u`B@r��@q&�@pA�@m��@l�/3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�dB�jB�jB�qB�jB�jB�qB�qB�qB�jB�qB�sB�`B�5B�ZB�TB�yB�B�sB�B�B�yB�BȴB�qB�?B��B�B}�Bx�Bs�Be`B]/BXBQ�BJ�BE�B=qB33B-B'�B$�B"�B!�B�B�B�B�BhB
=BB��B�B�B�`B�NB�/B�B��BÖB�LB�B�'B�}B�-B��B�bB�Bl�BffBq�B�DB�\B�PB�B�qB��B�wB�!B��B�hBr�B\)BJ�B=qB&�BJB�B�#BȴB��B�1Bo�BO�B2-B$�B �B �B�B\B1B��B��B��B�yB�ZB�)B�B�B�
B��B��B��B��B��B��B��B��B��3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B��B��B�dB�mB�mB�rB�mB�mB�tB�rB�tB�mB�sB�oB�gB�>B�bB�aBׂBܝBցBٓBڕBׄB�&B��B��B�UB��Bp(BlBf�Ba�BS�BKQBF1B@B8�B3�B+�B!ZB9BBB�B�B�B	�B�B�B��B�mB�BB�B��B��BӖBІB�cB�FB�B��B��B�LB�gB��B�mB��B~�Br_BZ�BT�B_�By�B}�B{�B�JB��B�"B��B�gB��B�BaBJ~B9B+�BEB��B��BɍB�B�CBv�B^B>`B �BeBKBKB
,B��B��B�B�`B�LB�	B��BʺBȲBǭBŠB�oB�_B�uB�uB�eB�jB�wB�cB�g3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�<#�
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
C]��C]�aC]r'C]s�C]q�C]s�C]M�C]NC]Q�C]z C]S�C]�BC^�=C_�XC[�MCZiRCY�CY3"CX��CYD�CZ�,C\)�C\��C\̢C\�C\{C\�C\BCY��CYRCY�DCZ)�CY}�CXR�CVܼCUw�CT��CS��CRyUCQ��CP��CQc�CRvuCS�CR��CS�QCT��CP�DCN�uCOn�CRKCUFCV�CXa�CV�TCS��CP�$COXuCNIiCM�RCK�YCGҵCBoC>��C;��C7�UC5O0C2�sC/��C->�C-�$C-��C-V/C-�C-tnC-[ZC.?�C/��C1CC3�C5'yC6��C9�C<V�C?z�CA� CC��CF�CI�GCM\2CP�kCS�CV�	CYOC\4�C_�RCc�pCe��Cfc�Cf��Ch ?Ci�Ck-aCl{�Cma]Cm�Co�Cph.CqtLCq��Cq�RCr/�CsK%Cs�JCr�UCr��Cr�cCq�CqwCq�Cn�3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�Cv��Cv��Cv� Cv�,Cv�tCv��Cv�6Cv�JCv�$Cw0�CxY�Cyf�Ct�Cs�%Cr��Cr8�Cq��CrQ�Ct 
Cu�lCvBCvL Cv
�Cu��Cv=Cu�Cr�xCr;	Cr� Csn�Cr��Cqe�Co�UCn;�Cm��Cl��Cj�BCi��Ch�ZCi��Cj�Ck��Ckx�Cl�gCm��Ci;Cf��Cg�YCj��Cm�/Co��Cq�lCp�ClLQCi`wCg�XCfm�Ce��Cc�=C_A�CYA�CT�CQͩCMpOCJ�8CG��CD[�CA��CB):CB [CA�hCBCB#CA�CB��CDm�CFpCH}CJ�7CL��CO@�CR�CVl�CY/�C[i�C^��Ca�0Cf�Ci��Cm3Cp��Cs�fCv��C{NC*FC��gC�-�C�|�C�-�C�;GC��vC��IC�-�C���C�d�C��C��}C��	C��HC��C���C��C�K�C�8ZC�;�C��?C���C�jC�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�@��@��@��@��@���@��:@��{@��@��L@�M�@�j�@�m@�"�@��$@���@舜@�"�@�l@�\3@��@�h\@�q�@�38@�&@�*&@�t@�@芘@�')@�$@��T@�@�0 @�@��@�@ᅱ@���@ߌ�@�c.@��@�(1@�e@�N@� �@��.@ݬo@�\@�/�@�[`@�dd@���@�hN@�ؠ@�
�@�V�@�6X@ܙ�@ڞO@�S�@Б@�u)@�iF@�8�@�@���@��D@��R@�e@�\�@�@�P�@�I@�2�@�-(@��[@�/�@�x@«�@Ĩ!@���@�s�@��;@��@Ң�@�ǥ@���@��q@�gZ@�;@��@��C@��6@�J@��a@�&�@�@���@��O@���@�GI@���@���A 0sA�A�	A�A=�AS&A��A#�AM�A�A�uAȬAt�A;�A�`A ��