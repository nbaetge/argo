CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   p   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T03:54:46Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  mArgo synthetic profile          1.0 1.2 19500101000000  20201030035446  20201030035446  5902305 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-012                9,02                            841 @վ�����1   @վ�k$�@H���l��2x���@1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            A= 0.985; B= 21.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A=1.166; B=-7.060                                                                                                                                                                                                                                               No significant pressure drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                   No significant temperature drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                Method OW : no significant drift or offset - Error = maximum [ statistical uncertainty, 0.01] - Method 127 see report : bilan_5902305.pdf on http://www.ifremer.fr/lpo/ovide/data/argo_profiling_floats.htm                                                     Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20130605144536201306051445372013060514453620130604093721                                          20170427114127A   A   B   B       @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C;  CD  CN  CX  Cb  Ck  Cv  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D � D� D� D@ D@ D� D&  D,@ D2� D8� D?  DE  DK� DQ� DW� D^  Dd� Dj� Dq  Dw@ D}@ D�� D�  D�  D�  D�� D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�  D�� D�  D�  D�@ DÀ DƠ D�� D�  D�  D�  D�@ D�` D�` D߀ D�� D�� D�� D�  D�  D�@ D�` D�� D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C;  CD  CN  CX  Cb  Ck  Cv  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D � D� D� D@ D@ D� D&  D,@ D2� D8� D?  DE  DK� DQ� DW� D^  Dd� Dj� Dq  Dw@ D}@ D�� D�  D�  D�  D�� D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�  D�� D�  D�  D�@ DÀ DƠ D�� D�  D�  D�  D�@ D�` D�` D߀ D�� D�� D�� D�  D�  D�@ D�` D�� D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AN�!AN�`AN��AOVAO
=AOVAO�AO"�AO"�AO"�AO�AOoAOoAN�AN�HAN9XAM�AL��AJjAH�+AG+AE��AC�#AB�AAdZA?��A>ffA>A<��A;�A;`BA9��A7��A4jA3S�A2~�A2�9A1��A0��A.�/A-x�A,��A,M�A+�A*M�A*JA(jA'��A&��A$ZA"VA�PA�AZA�A��A��AjA�
@���@�j@���@ۍP@��@�bN@ѩ�@��H@�33@�l�@Ł@��h@���@���@�  @�`B@�b@�l�@�K�@�j@�Z@�A�@�/@��@��F@��@��9@�j@��#@�1'@~E�@|��@x�`@w�@u�@t�@r�\@p��@p1'@o�@mV@j�\@i7L@h1'@f{@d�D@c"�@aX@` �@^��@\�/@[�F@[o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                AN�!AN�`AN��AOVAO
=AOVAO�AO"�AO"�AO"�AO�AOoAOoAN�AN�HAN9XAM�AL��AJjAH�+AG+AE��AC�#AB�AAdZA?��A>ffA>A<��A;�A;`BA9��A7��A4jA3S�A2~�A2�9A1��A0��A.�/A-x�A,��A,M�A+�A*M�A*JA(jA'��A&��A$ZA"VA�PA�AZA�A��A��AjA�
@���@�j@���@ۍP@��@�bN@ѩ�@��H@�33@�l�@Ł@��h@���@���@�  @�`B@�b@�l�@�K�@�j@�Z@�A�@�/@��@��F@��@��9@�j@��#@�1'@~E�@|��@x�`@w�@u�@t�@r�\@p��@p1'@o�@mV@j�\@i7L@h1'@f{@d�D@c"�@aX@` �@^��@\�/@[�F@[o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�jB�9B�9B�?B�?B�?B�?B�?B�?B�?B�9B�9B�9B�'B�-B�B�qB�qB�B��B�{B�7Bw�Bm�B[#BG�B>wB<jB2-B)�B%�BhB��B��BǮBɺB�B��BƨB�LB�B��B��B��B��B��B��B��B�VBv�Be`BB�B-B�B �B�BB	7B��B�fB��B�?B��B��B�^B��B�'B��BB�B�B��B��B�hB�DBu�BffBXB;dB-B�BbB1B��B�B�BB��BĜB�}B�jB�^B�9B�3B�!B�'B�B�B�!B�'B�B�B�B�B�B��B��B��B��B��B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�B�9B�?B�?B�?B�?B�?B�?B�?B�9B�9B�9B�'B�-B�B�qB�qB�B��B�{B�7Bw�Bm�B[#BG�B>wB<jB2-B)�B%�BhB��B��BǮBɺB�B��BƨB�LB�B��B��B��B��B��B��B��B�VBv�Be`BB�B-B�B �B�BB	7B��B�fB��B�?B��B��B�^B��B�'B��BB�B�B��B��B�hB�DBu�BffBXB;dB-B�BbB1B��B�B�BB��BĜB�}B�jB�^B�9B�3B�!B�'B�B�B�!B�'B�B�B�B�B�B��B��B��B��B��B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CoW
Cog�Cn�{Cn��Cn�dCn�CoCo�Co*=Cnu�Cm�Cm�9ClW
Cj�Cf9XC^t�CS�%CQ5?CRCRۦCS�VCT��CUWLCUffCV;�CW3CX�oCX�CXfCX�wCX�CV��CS�sCS�3CU3uCY-�CZ�CZ-CZ �CZ,JCYnVCY~5CX�NCW<�CU�?CT.CR�CPS�CLo\CIPbCD�qC>WLC8�%C4�jC4>5C5�C6��C7�dC:?C<�JCB'+CEa�CIf�CK�mCM�XCP�5CQ�+CR��CU8�CW��CYp�C[,�C]�uC_qhC`a�Cb�7Ceu?Cg�jCj��ClB�CnTCoƨCp��CsR-Cu9Cw�Cyi7C{0bC{aHC{��C|�7C}�ZC}��C}{C}D�C}roC}�NC} BC}.�C~,�C~Z^C}��C}�`C~�C9CAHCo\C��C�PC�dCZ^C~��3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                C�~�C��EC� �C�,uC�7�C�CMC�O%C�Z�C�e�C���C��8C��_C��C�NCz\�CqI�Cd�aCa�Cb�gCc��Cd��Ce}CfgsCfr�Cga�ChQ"Cj-�Cj=tCiZ�CjP�Cj],Cg�[Cc��Cc��Ce��CjnCkj�CksCk}FCkCj�&Cj�)CiǹCg�BCf+QCda�Cb�'C_��C[CGCW�CR'CJ�dCDaC?h�C>lC?ICAUCBfCDՖCG�ZCM�BCQ��CV�CX�CZ�?C^��C_��C`�HCc�CfylChh�CjfCmE�CoAvCpN�Cs+�CvoCx�C{ƫC}��C��C��tC�h�C��C��AC�WvC�UzC�YC�q�C��*C�wC��|C���C�cC�~�C��C���C�Q�C�k�C��vC�VC��wC�ΆC�a�C�z�C���C���C���C��8G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@'�@'@&�|@&�N@&��@&��@&˴@&�m@&��@&b=@%�)@%�o@$��@#��@ ;|@l�@B�@�6@�@�@E�@�h@uh@|�@�@��@��@��@X�@�X@�1@?�@�H@�q@o@	@��@�@��@��@#�@-�@��@x|@N�@)�@��@?�@T@	�@i�@�_?�*?� �?�?��S?��?�X�?��?�{T@��@!�@	�@
�@ �@�S@'p@�l@�Q@��@��@(@ڒ@�@��@�@{(@R�@!"�@"e�@#��@$�8@%��@'�@(�h@*�d@+�@->�@-^�@-{@.6[@.�$@/�@.�8@.��@.��@.��@.}7@.�@/X�@/wx@.�z@/�@/�	@/��@01@09@0Z�@0{)G�O�G�O�G�O�