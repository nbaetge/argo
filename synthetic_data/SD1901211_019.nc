CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:47:28Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                  x  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  S\   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  U<   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  Yt   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  ]�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^$   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  a�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  b\   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  d<   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  f   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ht   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  jT   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  j�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  n�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20201028144728  20201028144728  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�;q�1   @�<^i`@H-�2���P1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014238 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559272015101515592720151015155927                                                        20161116115859A   B   B   B       ?�  @�  @�  @�  A  A0  AP  A�  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C0  C:  CC  CM  CW  Ca  Ck  Cv  C�  C�� C�  C�� C�� C�� C�� C�  C�  C�  C�  C�  C�� C�� Cŀ Cʀ C�  C�  C�  C�  C�  C� C� C� C�� D @ D� D� D  D@ D@ D%� D,  D2@ D8� D>� DE  DK  DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�` D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɠ D�� D�� D�  D�  D�@ D�` D�` D� D�� D�� D�  D�  D�@ D�` D�� D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @�  @�  @�  A  A0  AP  A�  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C0  C:  CC  CM  CW  Ca  Ck  Cv  C�  C�� C�  C�� C�� C�� C�� C�  C�  C�  C�  C�  C�� C�� Cŀ Cʀ C�  C�  C�  C�  C�  C� C� C� C�� D @ D� D� D  D@ D@ D%� D,  D2@ D8� D>� DE  DK  DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�` D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` Dɠ D�� D�� D�  D�  D�@ D�` D�` D� D�� D�� D�  D�  D�@ D�` D�� D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AC�mAD1'ADA�ADA�ADQ�AD^5ADZADZADVADZADZADZAD^5ADbNADffADjADn�ADr�AD�AD�DAD�AD�uAD��AD��AD��AD�\ADr�AC�ACO�AB�jAA�AAt�AA�A@��A@9XA>^5A=�hA=&�A<z�A:=qA8VA7VA6�jA4�9A3��A4�+A2  A,1A,�9A/��A.9XA'/A'\)A*�!A+x�A+
=A&�A��A �A�FAĜA�A��A33A$�A�An�A?}AK�A��@��@�+@�$�@�@�h@�5?@�9X@�S�@�J@�O�@��h@��
@���@���@��P@�~�@�I�@���@��@��@�t�@�1@�{@��@���@�7L@�@�J@�j@
=@|�D@{"�@y&�@w;d@u��@s�@qhs@o�P@l�@k33@g�;@ep�@c��@d9X@bn�@^�y@]/@[dZ@Z�\@Yx�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�ADQ�AD^5ADZADZADVADZADZADZAD^5ADbNADffADjADn�ADr�AD�AD�DAD�AD�uAD��AD��AD��AD�\ADr�AC�ACO�AB�jAA�AAt�AA�A@��A@9XA>^5A=�hA=&�A<z�A:=qA8VA7VA6�jA4�9A3��A4�+A2  A,1A,�9A/��A.9XA'/A'\)A*�!A+x�A+
=A&�A��A �A�FAĜA�A��A33A$�A�An�A?}AK�A��@��@�+@�$�@�@�h@�5?@�9X@�S�@�J@�O�@��h@��
@���@���@��P@�~�@�I�@���@��@��@�t�@�1@�{@��@���@�7L@�@�J@�j@
=@|�D@{"�@y&�@w;d@u��@s�@qhs@o�P@l�@k33@g�;@ep�@c��@d9X@bn�@^�y@]/@[dZ@Z�\@Yx�444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB]/BL�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BH�BI�BH�BH�BH�BI�BH�BH�BG�BF�BH�BH�BF�BF�BD�B@�B<jB(�B$�B#�B�BB�B�NB�sB��BŢB�TB��Bu�B�=BÖB�9B]/Bm�B�BÖB�wBv�B�B��B�TB1'B�B�fB�B�B��B��BVBZBQ�B�B(�B!�BB��B�NBȴB�dB��B� Bx�B{�BL�B49B%�BhB��B�B�B�sB�HB��B��BB��B�jB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�{B�{333311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�BX~BX{BX~BX~BX~BX~BX~BX~BX{BX{BX{BX}BXzBXzBWxBX~BWuBWuBWuBX{BWuBWwBVnBUhBWxBWwBUkBUiBS]BODBK(B7�B3�B2�B+gB�B�FB�B�'BߖB�RB�
B�4B�hB��B�EB��Bk�B|5B��B�EB�%B�oB&(B�YB��B?�B$B��B%!B%B�B�B�Bh�B`�B�)B7�B0^B�B_B��B�:B��B�,B�}B�QB�eB[EBB�B4TB�B?B�B��B��B�B�VB�/B��B��B��BB��B�jB�gB�UB�UB�AB�4B�-B�"B�"B�B�B�B��B��B��B�B��B��B��B��B��B��444411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cty�Ct�.Ct��Ct��Ct��Ct��Ct�3Ct��Ct�JCt��Ct�dCt��Ct�Ct#Ct9�CtPCtf�CtDCs�OCs�kCs�!Ct�Ct(|Csu�Cs�@Cr׮Cr&�Cqn�Cn[�Cj�Ci�eCi��Cf�CCd�VC`�DC]��C\ٖC\�C[h,C\?�C[��CZ�ECYG7CW��CW%CVMCP�jCOF�CP�CSY�COx�CM�hCPQ�CR�CR�hCL�LCA��C8�C2��C42�C4TC0x�C.)�C-xUC-��C/1�C0�OC2��C3t�C6��C<GaC=�C@u�CC��CFA�CI�CL�CO]ICSp�CW��CZ�C[ UC_�Cc:�Ce�6ChR�Ck��Cmm�Cnf�Co_hCq%ZCr��Cs�Cu��Cu�OCvړCw�CxBCyKCy2tCyagCy�eCz�Cz��Cz�CzP\Cz��Cz��Cz�MC{��C|�C}'C|f�C|��C|ȴC}�kC}�C~&�C~V�C~�Y333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                C���C���C��	C��C��UC��7C��.C��+C��CC��"C��C��C�X�C�b�C�lRC�v:C��C���C�;6C�D�C�O�C�Y�C�c]C�DC�mC�CXC~ixC{��CxSzCw��Cw��Cu�Cs!�Co��CmoCl��Ck�CkM�Cl-�Ck�MCk�Ci��Ch��Cg�Cg;mCb�MCa�?Cb�\Cd�[Ca�C`�dCb�dCd��Cd�hC_)�CU��CM�qCIF_CJ�xCJ��CG|\CEҼCE+�CEk�CGY)CH�2CJi�CK CN)QCS�CU?mCW��CZ�2C],uC`T�Cb��CeݤCi̶Cm��CpCp�=Cu�#Cx�,C{H�C}�C�u?C�JkC���C�8�C�_C��C�_vC�5�C�ObC�ƛC�@�C�Z�C��C��UC�FC��C��C���C��%C��6C���C��VC���C�JgC�f C�� C��SC���C��'C�C C�\HG�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@v�@~2@��@�@�M@�j@�5@�@�N@�B@�!@��@1@?f@M�@\0@j�@zi@�@@%�@4p@B�@К@ݚ@kQ@�:@�K@��@/@��@�!@�@@7@�}@�l@X@�@k�@��@}d@6@�@
�@	��@	'@�N@�3@��@C�@� @Ʃ@S1@�@�@�E?��-?�=?� ?�t?���?���?���?�
�?�"I?�?�?�e�?䍬?�T?��?��4?�)?�YD?���?�Ĉ@ �,@�@��@RK@	�@@��@),@L�@mW@�@��@ҋ@�Q@��@2�@UX@z)@@<�@Zp@��@�z@�S@^D@|s@�@�6@ [�@ {z@ �D@ 3n@ RS@ oF@ �Z@!/�@!K�@!�@!��@!�|@!�!@"k&@"��G�O�G�O�G�O�