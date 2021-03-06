CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   p   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:03:44Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  mArgo synthetic profile          1.0 1.2 19500101000000  20201030040344  20201030040344  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               #A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @��I��J1   @���4`@H+Ƨ�/C��$�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013218 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161459201510161614592015101616145920130604092420                                          20170502151424A   A   B   B       @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  Cˀ C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D  D@ D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DW� D^  Dd@ Dj� Dq  Dw@ D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�@ D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  Cˀ C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D  D@ D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DW� D^  Dd@ Dj� Dq  Dw@ D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�@ D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AUAU"�AU+AU/AU&�AR �AO��AOG�AO�AM�7AJ~�AH(�AC��AC�ABn�AAXAAhsAB��AChsAD�AC��AA�AA&�A?�A>I�A=l�A<�+A<v�A<��A<n�A<  A;�7A;7LA:��A:�A9��A9;dA8�!A85?A7�#A7��A7�A8�A8JA7�
A7��A7p�A6��A6~�A5�
A5`BA4��A4  A3t�A2��A2A�A1\)A0�A/hsA-��A+��A)K�A&v�A!��A�jA��AƨAA1AbNAK�AAl�A9X@��@�  @�5?@���@�w@�\)@ӝ�@��@ŉ7@���@��9@��^@��@�  @�|�@�hs@���@�/@�{@��y@���@��T@��F@�$�@�  @��+@�hs@� �@~�R@{�@y7L@w
=@t�j@so@pb@m�-@l�@k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                AUAU"�AU+AU/AU&�AR �AO��AOG�AO�AM�7AJ~�AH(�AC��AC�ABn�AAXAAhsAB��AChsAD�AC��AA�AA&�A?�A>I�A=l�A<�+A<v�A<��A<n�A<  A;�7A;7LA:��A:�A9��A9;dA8�!A85?A7�#A7��A7�A8�A8JA7�
A7��A7p�A6��A6~�A5�
A5`BA4��A4  A3t�A2��A2A�A1\)A0�A/hsA-��A+��A)K�A&v�A!��A�jA��AƨAA1AbNAK�AAl�A9X@��@�  @�5?@���@�w@�\)@ӝ�@��@ŉ7@���@��9@��^@��@�  @�|�@�hs@���@�/@�{@��y@���@��T@��F@�$�@�  @��+@�hs@� �@~�R@{�@y7L@w
=@t�j@so@pb@m�-@l�@k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B��B��B��B��B��B��B��B��Bp�BiyB`BBS�BYBv�B�B��B�uB~�B�Bv�BiyB`BB[#BaHBiyBl�BiyBgmBffBcTB^5BZBW
BQ�BN�BM�BQ�BXB]/B^5B]/B^5BZBT�BO�BG�BB�B=qB6FB33B1'B+B#�B�B{B+B��B�TB��B�B�B��B��B�-B��B�+B{�Bu�Br�Bo�B^5BR�BB�B5?B%�BB�HBɺB�B�DBe`BJ�BE�B?}B/B�B��B�ZB�B��B��B��BȴBĜB�wB�^B�RB�9B�3B�B�B��B��B��B��B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�B��B��B��B��B�<B�PB��B��B�RB�\B~OBw Bm�Ba�Bf�B�tB��B�YB�"B��B��B�tBw Bm�Bh�Bn�BwBz3BwBuBtBp�Bk�Bg�Bd�B_�B\}B[uB_�Be�Bj�Bk�Bj�Bk�Bg�Bb�B]�BURBP0BKBC�B@�B>�B8�B1vB*HB"B�B[B��B�hB��B��B�'B�rB��B�<B��B�oB�HB�3B}#Bk�B`pBPBB�B3]B{B�B�)B�sB��Br�BX BS BL�B<wB#�B2B�B�aB�B�3B�/B�B��B��BǬBśB��B��B�XB�JB�AB�3B�+B�B�B�B�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C��=C���C���C���C�J^C}�CzTCw��Cur-Cs"�CpCns�Cl!Cl2-ClD�Cm/ClffCk�bCkCl��Cmu?Ck�bCk6Ci�'Ch.Cgu�ChMCi��ClZ�Cm�qCmE�Cn}Cn0�CnBNCnP�CndCnvCm��Cm`Cm� Cn��Co�RCo�sCo�#Co�PCo�BCo(sCnpbCn�Cn��Cn��Cn��Cn�CnVFCl"�Ci!�Cd�=CbR�C`�C_s�CY=�CR6CI��CA��C>�9C9�XC8T�C7��C8�HC9d�C:H1C;��C=�%C@�CD.�CGx�CI.VCK�-CMi�CQ�sCTڠCWb�C[�C^׍Cc��CfO�CgB�Ci�uClcCoÖCs�Cu�Cwt{CxmCx�Cy��Cz�-C{�bC{�XC|�FC}�C}��C~bC�C<)C~�C��C�%C�C�bC��C~�u3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                C��C��C�P�C�\�C���C���C�pC���C�uQC�!�C�S�C~׿C|7C|,�C|=�C}-�C|^�C{�<C{��C|��C}�C{�YC{CCyDCw�YCv�bCw��Cyx[C|8�C~yC}?�C~4/C~F$C~W�C~dNC~wYC~�zC}��C|�C}עC~��C��C�C�C��C�|CJ<C~v�C~�#C~��C~�KC~�hC~�jC~E&C{�aCxW Cs�Cp�GCn�CmBICf*�C^$CTF�CK9CG�CB(�C@c�C?�#C@s�CA_&CBNCDCE��CI��CM2$CP�=CR��CU}�CW_C[�fC_�Cb`�Cf��Cj��Cp^Cr�CCs�Cv�	Cy�(C}\2C���C��C���C�t�C��C�hC��cC�3�C�F�C��iC�bLC�x�C���C��C�6aC���C�fC�~�C���G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@2��@2�e@2R�@2bH@1�@-�z@+�R@)�@(D?@&�z@$BI@#p@!Za@!dT@!o\@"�@!�L@!�@!�@!�v@"Q�@!09@ �5@��@i�@��@z@�0@!l@"�/@"?@"��@"�@@"�o@"ώ@"۾@"�X@"_�@!�@"u�@#S@#��@#��@#�h@#��@#�@#b�@"ۂ@"�}@"�@"�Y@#�@#.X@"��@! �@� @�j@�@��@؁@N�@+�@�<@?���?���?�B?�,�?�Vn?��?���?�	?�j�@�@SJ@�y@��@�L@	�g@��@�@��@�a@(�@�2@p+@�@�@�a@"&�@$��@&Ua@'��@(C�@(c�@)@)ʼ@*x@*��@+L@,�@,�@,?�@,�@-h@,�S@-Oz@-o@-�_G�O�G�O�G�O�