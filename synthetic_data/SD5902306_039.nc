CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   p   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:04:20Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                  p  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  S4   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  T�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ud   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W$   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  X�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  YT   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  \�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]D   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  `�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a4   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  b�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  d�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  e$   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  h�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  i   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  j�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  l�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  mArgo synthetic profile          1.0 1.2 19500101000000  20201030040420  20201030040420  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               'A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @���8�1   @���I��@G�ě��T�.�ě��`1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013219 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161500201510161615002015101616150020130604092428                                          20170502151425A   A   B   B   ?�  @�  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Cڀ C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DW� D^  Dd@ Dj� Dp� Dw@ D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�` DÀ DƠ D�� D�� D�� D�  D�@ D�@ D܀ Dߠ D� D�� D�� D�  D�  D�` D�� D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @�  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Cڀ C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DW� D^  Dd@ Dj� Dp� Dw@ D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�` DÀ DƠ D�� D�� D�� D�  D�@ D�@ D܀ Dߠ D� D�� D�� D�  D�  D�` D�� D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aw�Aw�7Au��Ak��Ad�AYhsAQ&�AO
=ANz�AM�^AJ��AI�TAI"�AH�+AG��AF��AF �AE��AD��AD1ACp�AB�AA/A@~�A?�
A?�-A?C�A>�DA=��A=�A<�A<A�A;%A:n�A9�A9�^A9p�A7��A6n�A7ƨA7hsA6�+A6$�A5�;A5��A5A5O�A4�`A4�+A4��A4A�A3��A2�A1�A05?A.�+A-33A++A)��A%�A$�DA$$�A#33A�FA��A�AVA�AhsA	�AA	��A��@�^5@� �@��@�5?@�$�@Ցh@�b@�l�@���@�l�@�j@���@�E�@��#@��@�9X@�V@�M�@���@��u@��`@���@�7L@�Q�@~ȴ@|9X@y�^@w��@u�T@t1@rn�@p�`@n�y@m�-@kƨ@i&�@g
=@e�@dj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                Aw�Aw�7Au��Ak��Ad�AYhsAQ&�AO
=ANz�AM�^AJ��AI�TAI"�AH�+AG��AF��AF �AE��AD��AD1ACp�AB�AA/A@~�A?�
A?�-A?C�A>�DA=��A=�A<�A<A�A;%A:n�A9�A9�^A9p�A7��A6n�A7ƨA7hsA6�+A6$�A5�;A5��A5A5O�A4�`A4�+A4��A4A�A3��A2�A1�A05?A.�+A-33A++A)��A%�A$�DA$$�A#33A�FA��A�AVA�AhsA	�AA	��A��@�^5@� �@��@�5?@�$�@Ցh@�b@�l�@���@�l�@�j@���@�E�@��#@��@�9X@�V@�M�@���@��u@��`@���@�7L@�Q�@~ȴ@|9X@y�^@w��@u�T@t1@rn�@p�`@n�y@m�-@kƨ@i&�@g
=@e�@dj1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B�\B}�B��B�B�B�B��B��B��B��BɺBÖB�}B�jB�LB�9B�'B��B��B��B�uB��B��B��B�hB�1B�B}�Bo�Bl�BjBl�Bk�BXBK�BaHB]/BS�BP�BO�BO�BO�BL�BH�BF�BH�BE�B@�B;dB49B$�B�BhB%B��B�B�BBVB�mB"�BDB�RB�qBĜB�!B�qB��B�XB��B�hB{�BH�B&�B��B��B��B�uB�BO�B+B�B%BB��B��B��B�mB��B�jB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�B��B��B�B��B��B�CB�ZB�BB�BݒB܌BۆB�kB�KB�/B�B��B��B��B��B�YB�9B�B�NB�SB�8B�B��B��B��B}EBz/Bx(Bz5By-Be�BYhBn�Bj�Ba�B^�B]�B]�B]�BZoBVXBTJBVVBSEBN&BIBA�B2yB'5BB�B�B�B�B�B�B� B0kB�B��B��B�*B��B��BSB��B�8B��B�kBV3B4bB
bB�MB�bB��B�oB]=B8`B#�ByBtB'BB.B��B�#BɴB��B�eB�WB�EB�;B�'B�B�B��B��B��B��B��B��B��B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
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
Cy)�G�O�Cxs�CuCr�fCpCk0�Cg�LCh�CgM�CgU�Cf��Cf��Cg~�Cg��Cg�jCf��Cf,Cf9�CfI7Ce��Cd�RCd�Ccc�Cd<)Cf��ChFfChU�Che�ChwLCh��CidZCit9Ci�Cj]�Ck7�Cj}Ce�
Ch@BClC�ClVCj�3Cj�Cj,Cj@BCjQ'CjbCi�yCh�bCiChJ=Ci1�Cf3uCc3uC`/�CZ�7CV,�CO��CM��CF��CB�C>$ZC;��C9�C8	�C6�mC7cTC9VC9��C;�}C=O\C=l�CA�HCC�jCF|jCH��CM�CQ0�CS�
CW�?C[�?C^zCaѪCg��Ck�Cn;dCp�Cq��Cs��Cs��Ct��Cw<)Cz�HC|i�C}f%C~bNC~�'C~�5C~�CdC�)C�$ZC�;�C�RC�i�C���C�0!C���C��
C���C���C~ȴ3 33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333    G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                        C���G�O�C��_C��hC�4C���C{R�Cw��Cw�iCv�Cv�mCu��CvDCv��CwCw�Cv:\Cug�Cuq]Cu�Ct�HCs��Cr�GCr#{CsJCu�ECw�uCw�Cw��Cw��Cw�Cx��Cx�Cx��Cy��Cz��Cz{Ct��Cwn�C|RC|�Cz[Czl�Cy��Cy�ICy�ACy��Cy /Cx,CCxA�CwnBCxqwCu�Cq�sCn&_Cg��Cb�C[�CY�CP��CL�	CGD�CD�CA�3C@9�C>k�C?1�CAgCA��CC�%CE��CE�CJR=CMCO� CR��CWwC[�C^T�Cb�fCgn�Cj24Cm�$Ct?Cx��C{��C~y�C�fC��<C��mC�`rC���C���C��fC�7�C��tC��XC��"C��C�$+C���C��`C��+C���C�FC�-�C���C�_�C�w�G�O�G�O�G�O�1 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@+pTG�O�@*��@(\�@&~�@$��@ ��@|(@�@��@�<@m%@t�@�@�@h@��@@Z@�@��@@��@��@��@R@�@�W@�1@�l@�-@Dz@K�@T�@�8@ ��@ �@�@[x@!Jr@!VA@ :O@ E�@��@��@�@��@\G@Ԧ@�@[@ �@ѵ@��@jz@x�@�@�@
�@�{@��?�8?��N?�7L?�W?�e?���?�(?�J,?���?�#?�>�@|P@7@��@��@	��@�^@J�@2�@�@�@C�@QT@=�@!�@"�C@#��@$ʊ@$�@%�,@'m�@)��@+@+̚@,M@,�)@,��@,��@,�@-�@-�@-��@.7@./�@.N�@-�7@.��@.��G�O�G�O�G�O�