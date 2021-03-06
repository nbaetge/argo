CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   p   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T03:54:38Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  mArgo synthetic profile          1.0 1.2 19500101000000  20201030035438  20201030035438  5902305 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-012                9,02                            841 @ռK����1   @ռM�~� @H��"��`�2�Ƨ1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            A= 0.985; B= 21.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A=1.166; B=-7.060                                                                                                                                                                                                                                               No significant pressure drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                   No significant temperature drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                Method OW : no significant drift or offset - Error = maximum [ statistical uncertainty, 0.01] - Method 127 see report : bilan_5902305.pdf on http://www.ifremer.fr/lpo/ovide/data/argo_profiling_floats.htm                                                     Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20130605144532201306051445322013060514453220130604093719                                          20170427114127A   A   B   B   ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C1  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C� C�  D @ D  D  D  D@ D� D%� D,  D2@ D8� D>� DE� DK@ DQ� DW� D^@ Dd@ Dj� Dp� Dw  D}� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�` D�` D�� D�� D�� D�  D�  D�@ D�` DÀ Dƀ Dɠ D�� D�� D�  D�@ D�@ D�` D߀ D�� D�� D�� D�  D�  D�` D�` D�� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C1  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C� C�  D @ D  D  D  D@ D� D%� D,  D2@ D8� D>� DE� DK@ DQ� DW� D^@ Dd@ Dj� Dp� Dw  D}� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�` D�` D�� D�� D�� D�  D�  D�@ D�` DÀ Dƀ Dɠ D�� D�� D�  D�@ D�@ D�` D߀ D�� D�� D�� D�  D�  D�` D�` D�� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AMAN(�AN5?AN5?AN=qANI�ANQ�ANZAN^5AN^5ANbNANffANbNAM�FAG�AC�FAAƨA@  A>r�A=XA;+A9��A8�A7l�A6�A5A3�FA2r�A0�HA.��A+��A*��A(��A(9XA&�A%%A"�\A��A1A�yA�7A$�Ar�A�7A��A�`A��A��AK�A�HA�A��A��A��@��;@�(�@��y@�5?@�n�@�@�-@�o@�+@�Z@�5?@�/@υ@��;@�v�@�j@�^5@�^5@��u@���@�A�@��@�bN@�S�@�x�@���@��m@�/@��@��!@���@�b@�@�V@�(�@}?}@|9X@x��@v��@t�/@st�@q��@pr�@m�T@kƨ@i�#@gK�@e@dz�@b�\@`Ĝ@_�@^��@\�@[�F@Z~�@YX@XbN1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                AMAN(�AN5?AN5?AN=qANI�ANQ�ANZAN^5AN^5ANbNANffANbNAM�FAG�AC�FAAƨA@  A>r�A=XA;+A9��A8�A7l�A6�A5A3�FA2r�A0�HA.��A+��A*��A(��A(9XA&�A%%A"�\A��A1A�yA�7A$�Ar�A�7A��A�`A��A��AK�A�HA�A��A��A��@��;@�(�@��y@�5?@�n�@�@�-@�o@�+@�Z@�5?@�/@υ@��;@�v�@�j@�^5@�^5@��u@���@�A�@��@�bN@�S�@�x�@���@��m@�/@��@��!@���@�b@�@�V@�(�@}?}@|9X@x��@v��@t�/@st�@q��@pr�@m�T@kƨ@i�#@gK�@e@dz�@b�\@`Ĝ@_�@^��@\�@[�F@Z~�@YX@XbN1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBr�B`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BBbNBl�BaHBP�BB�B7LB,B�B+B��B�B�mB�B��B��B�?B��B� Bq�BbNB_;BQ�BD�B/B�B
=BB  B��B�B��BBB�B�#B��B�B�Bq�B�bB�B�B��B�B��B��B��B�PB�bB��B��BB�B�BB��B�'B�3B��B�bBy�BffBW
B33B!�B
=B�B�B�B��B��B��B��B��BŢBŢBB�wB�wB�LB�?B�9B�3B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�B`BB`BB`BB`BB`BB`BB`BB`BB`BB`BB`BBbNBl�BaHBP�BB�B7LB,B�B+B��B�B�mB�B��B��B�?B��B� Bq�BbNB_;BQ�BD�B/B�B
=BB  B��B�B��BBB�B�#B��B�B�Bq�B�bB�B�B��B�B��B��B��B�PB�bB��B��BB�B�BB��B�'B�3B��B�bBy�BffBW
B33B!�B
=B�B�B�B��B��B��B��B��BŢBŢBB�wB�wB�LB�?B�9B�3B�-B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CmCm�FCn	�CmU�Cmi7Cm|�Cm��Cm�ZCm��CmClNCj��Cf��C^iyCT��CT/CT�!CWT�CX*CX7LCY�CY�CY&%CXlJCW��CV�
CUs3CU� CS0�COO\CM�mCMԼCOq�CO� CM0!CIL�CEffCC�CCdC@ �C>v�C>�C>�+C>��C=	�C9�C6�!C5D�C3��C22�C2-�C3�`C5n�C6Q�C9��C=�C@��CDCGBNCI�dCL�qCO{dCPc�CMW�CNNVCP�CSO�CV�CY�CZ�9C]W
C_�}Cbb�Cd��ChECk�CmU�Cqz�Cs9Ct��Cv��Cw��Cx��Cx�5Cy�Cz%Cz1hCz`�C{]�C{�C{��C|��C|��C}-C}>�C}m�C}��C}�=C}��C~��C#�CRoC�HC��C� C�fC�C�5?CɺC��CSuC|�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                C�,�C��*C���C�P�C�\1C�g�C�s^C�~�C��XC� �C��C�}C{5�Cq6�Ce�Cd��Ce��Ch��Ci�OCi��Cj�Cj��Cj��Ci��Ch� Cg�Cf�Cf#`Ccg#C^�C\��C\�C^�,C^��C\ �CW�]CR�CP"�CP"�CL{�CJ��CJ��CJ��CJ�ICH�hCE<�CA�C?��C=��C;�MC;ʇC=�\C?k	C@t�CD'�CH��CLB�CO�CS�CV�eCZ3?C]HC^CZ��C[�C]�Ca{�Ce0�Ch�Cj�Cl�~Co�4Cr�CurECyJqC}	�C~��C��5C��sC��pC�٠C�dC��&C�	`C�#C��&C���C��0C�o�C���C���C�/�C�HrC�a�C�zC���C��UC��	C��NC�pVC���C���C��vC��tC��C�lC� �C�9�C���G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@%X%@%��@&�@%��@%��@%��@%�d@%�D@%��@%H�@$��@#�o@ �@`�@2�@��@V@�C@zp@T@}@L@f@��@�q@l�@E�@I�@��@��@j8@n'@�Z@�}@��@	�o@�d@5@4�@ޛ@�X@�@��@�b@ ��?�v�?���?�ZH?��?�?�_e?�?��?�W�?��@ g}@��@f@}t@	J~@��@zl@&@�S@�@�@O,@�q@C@�t@�(@m�@>@�@��@!��@#2e@&<m@'x�@(Ů@*R@*��@+r@+�@+�@,f�@,�@,��@-[�@-x�@-��@.Q�@.q6@.�R@.��@.і@.�X@/�@/1@/��@0�@0)�@0J�@0l@0�,@0�@0��@0��@0tTG�O�G�O�G�O�