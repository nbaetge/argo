CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:46:29Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20201028144629  20201028144629  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�
��}'�1   @�
�Յ��@H@ ě���2��`A�01   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014263 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559262015101515592620151015155926                                                        20161116115858A   B   B   B       @   @@  @�  @�  @�  A   A@  A`  Ap  A�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C9  CC  CM  CW  Ca  Ck  Cv  C�  C�� C�  C�  C�  C�� C�� C�  C�  C�  C�� C�� C�  C�  C�  C�  C�  C�  Cـ Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D1� D8@ D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` DƠ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @   @@  @�  @�  @�  A   A@  A`  Ap  A�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C9  CC  CM  CW  Ca  Ck  Cv  C�  C�� C�  C�  C�  C�� C�� C�  C�  C�  C�� C�� C�  C�  C�  C�  C�  C�  Cـ Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D1� D8@ D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` DƠ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�1'A�7LA�=qA�=qA�=qA�=qA�?}A�A�A�E�A�E�A�C�A�A�A�E�A�G�A�hsAoAdA�A_�A]�
A[�wAYAX(�AUƨAS�
ASK�AR=qAPbANZAM�AL(�AJE�AHv�AGS�AEADr�AB�yAA|�A@ffA?G�A>��A=�
A<ĜA<  A:��A9�A9|�A8r�A7l�A5�FA4�`A3|�A2-A1oA/�;A.=qA,��A+"�A)S�A'�
A&��A&�RA(��A&A��A�A�A��A��@�b@�V@�^5@�x�@�R@�x�@�V@���@̋D@�p�@��@Ə\@�%@��F@��@�~�@���@��@�I�@��@��@���@��P@�G�@��@�&�@���@�X@��!@�ff@���@�bN@~ff@|(�@z^5@y�@x1'@v$�@rM�@pbN@o;d@nV@lj@k33@j-@i7L@h�u@g�@f�+@e?}@c��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�A�=qA�=qA�=qA�=qA�?}A�A�A�E�A�E�A�C�A�A�A�E�A�G�A�hsAoAdA�A_�A]�
A[�wAYAX(�AUƨAS�
ASK�AR=qAPbANZAM�AL(�AJE�AHv�AGS�AEADr�AB�yAA|�A@ffA?G�A>��A=�
A<ĜA<  A:��A9�A9|�A8r�A7l�A5�FA4�`A3|�A2-A1oA/�;A.=qA,��A+"�A)S�A'�
A&��A&�RA(��A&A��A�A�A��A��@�b@�V@�^5@�x�@�R@�x�@�V@���@̋D@�p�@��@Ə\@�%@��F@��@�~�@���@��@�I�@��@��@���@��P@�G�@��@�&�@���@�X@��!@�ff@���@�bN@~ff@|(�@z^5@y�@x1'@v$�@rM�@pbN@o;d@nV@lj@k33@j-@i7L@h�u@g�@f�+@e?}@c��444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB6FB/B/B.B.B.B.B.B.B.B.B.B.B/B/B5?B�fB_;BcTBYBJ�B<jB0!B�B%BB��B�TB��BǮB��B�B��B�=By�Bk�B\)BN�BE�B:^B5?B-B#�B�BPBBB��B�B�5B�BĜB�?B��B��B�DB|�Bm�B^5BS�BZB~�B�}B�LB� B{�Bt�B�B��B�JB��B�qB�B�HB�-B|�Bq�Bz�B|�Bn�B�uB��Bs�B9XB)�B1'B�B)�B�B
=B��B�mB�BB�)B��B��BȴB�wB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��333311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�B<�B<�B<�B<�B<�B<�B<�B<�B<�B<�B=�B=�BD	B�CBn!Br9Bg�BY�BKIB>�B%bB�B�B�B�+B��B�B�ZB��B�RB�B��BzIBj�B]�BTeBI BD B;�B2�B)ZBB�B�B�B�SB��B�B�NB��B��B�TB��B��B|8Bl�Bb�Bh�B��B�,B��B��B��B�_B*:B��B��B�]B��B�%B��B��B�qB�'B�aB�mB}B��B�B�3BG�B8lB?�B)B8lB(	B�B:B��B�B�B�WB�CB�B��B�hB�JB�FB�/B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cg��Cg�Cf�Cf�Cf��Cf�
Cf��CgCg�Cg	�CgPCg�Cg!�Cfk�Cd�C]�FCX_5CVi'CV]?CVe�CVpXCWC�CWQCX&CX��CZ�{CZ�KCY�CY�oCZ�CZ0CYd5CX��CW�)CX�CX�CXCX��CYCY�CY&CY7
CYHnCZ#�CX�'CW�!CW+�CW=�CWM�CW[\CWk�CW{�CV�aCU;CR�CO�?CL��CH�CD�C@0�C;�C8yWC8��C3'�C/CC.��C-
�C-�sC/uoC1�C3��C6[C6��C:?`C@�CFGeCJXCL�lCO_�CQ�
CR֖CT��CZFC_'nCa�,Cb��Ce��Cf(�Ch�yCjv�Cm��Co��Cp��CrZZCsT�CtPCv�Cw� Czv�C{t�C{�AC{�uC|��C|8�C|h�C{�hC|�.C}��C}�)C}V�C}��C}�>C}��C~C}x�C}�xC}�=C}:C}j4C|�"333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Cr��Cr��Cq�gCqޝCqߚCq�Cq�RCq��Cq�Cq�qCq�MCq�SCr�CqdSCp�Ci�Ce�;Cd��Cd�5Ce�Ce2�Cf�Cf)�Cg�CgݩCiV�Cis�Ch�Ci�Ci.�CiImCh�uCh3�Cg��Cg��Cg�gChVCh�nCh��Ci�Ci.MCiH�Cig�Cj5<Ch�UChY�Cg��Cg��Cg�Ch�Ch5cChX�Cg�Cf~Cd�TCaܛC_1�C["�CWǉCTgjCPWCM�CM��CH�bCE��CEp�CDKCE��CG�}CJSdCK��CN*�CN�2CQ�ECXxC]��Cat�Cc��Cf(Ch��Ci@�Cj��Cp8MCu �Cwj�CxJ�C{v�C{�DC~@C�9C�n�C�JRC�ĈC��!C�}C��EC�\�C�4C�m�C��C��OC�C��)C�N�C�e4C�"tC���C�EC�,�C��C� �C��C�3CC�K�C��C�_C�6G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@E�@J�@��@�(@Ѝ@��@�!@��@��@�j@�3@��@�@x3@��@t@
z_@	9@	1p@	7@	=�@	�@	�s@
U�@
��@�D@�@}#@��@�u@�q@!j@
�-@
3�@
>�@
H�@
P�@
��@
��@
��@
��@�@�@��@
�L@
+�@	��@	�&@	�T@	�@	ޱ@	��@	q�@w�@�&@ (@)@ �?�X?� �?��?� o?�H�?�Q�?�U�?�i�?�~�?ގ�?��Q?㶀?��?��?�5�?�e�?���?��@�@�@� @QN@�@Q@��@ю@qd@X@1�@Mh@��@�@6�@W�@�K@@�N@\;@��@�@ K�@ �n@!>@!-@!Г@!k�@!��@!$@!��@"j�@"�@"#@"Ba@"`�@"�@"�l@"8�@"X�@"w�G�O�G�O�G�O�