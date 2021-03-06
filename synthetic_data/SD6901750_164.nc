CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:55:24Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20210302045524  20210302045524  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��;UUUU1   @��<5y� @I ě��T�0�333301   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99963 (+/- 4e-05) , vertically averaged dS =-0.014577 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114928                                          20210205114928A   B   B   B   ?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CW  Ca  Ck  Cv  C�  C�� C�� C�� C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  Cŀ C�  C�  CԀ Cـ Cހ C�  C� C� C�  C�  D   D� D� D  D@ D� D%� D,  D2@ D8� D>� DE@ DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�  D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CW  Ca  Ck  Cv  C�  C�� C�� C�� C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  Cŀ C�  C�  CԀ Cـ Cހ C�  C� C� C�  C�  D   D� D� D  D@ D� D%� D,  D2@ D8� D>� DE@ DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�  D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A@�!A@��A@��A@�A@�!A@�jA@�RA@��A@�A@��A@�9A@�jA@�jA@�RA@�RA@�jA@�!A@ȴA@��A@��AA%AA�A@ȴA?7LA<��A;x�A:�A9��A8A7?}A6v�A5A4��A3��A3C�A2�+A1;dA0�9A0ffA/�A/|�A.�9A-S�A,�jA,�jA,z�A+�TA*(�A)O�A'�A'%A&��A&JA%+A$�\A$^5A$��A%VA%�A%\)A%�A%�
A#�mA {A�7A��A��AbAM�A  A�AVA33A9XA
^5A��A�wAt�A �H@�&�@��y@�O�@�+@�7L@Ұ!@§�@�z�@���@��-@�(�@��@���@���@��@�$�@��@��j@�~�@�G�@�I�@���@�x�@�A�@��H@��^@���@�Z@~ff@{��@z=q@x��@xQ�@vv�@s�m@r=q@p�`@o|�@n{@l�j@l1311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A@�!A@��A@��A@�A@�!A@�jA@�RA@��A@�A@��A@�9A@�jA@�jA@�RA@�RA@�jA@�!A@ȴA@��A@��AA%AA�A@ȴA?7LA<��A;x�A:�A9��A8A7?}A6v�A5A4��A3��A3C�A2�+A1;dA0�9A0ffA/�A/|�A.�9A-S�A,�jA,�jA,z�A+�TA*(�A)O�A'�A'%A&��A&JA%+A$�\A$^5A$��A%VA%�A%\)A%�A%�
A#�mA {A�7A��A��AbAM�A  A�AVA33A9XA
^5A��A�wAt�A �H@�&�@��y@�O�@�+@�7L@Ұ!@§�@�z�@���@��-@�(�@��@���@���@��@�$�@��@��j@�~�@�G�@�I�@���@�x�@�A�@��H@��^@���@�Z@~ff@{��@z=q@x��@xQ�@vv�@s�m@r=q@p�`@o|�@n{@l�j@l1311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�/B�;B�;B�5B�;B�5B�;B�5B�5B�5B�5B�5B�;B�BB�BB�BB�NB�yB��B��BB1BVBbB�B�BbB1B��B�B�B�ZB�/B��B��BƨB�wB�dB�XB�FB�3B�B��B��B��B��B��B�VB�%Bz�Bt�Br�Bn�BhsBe`BjBu�B�B�{B��B�B��BȴB��By�B|�Bv�B�B�XB�mB
=BJB��B�jBȴB�B�BƨB��B��B�{B~�BM�Bn�BB�B�hBp�BVBB�B/B�BPB  B��B�B�`B�;B�#B�B��B��B��B��BɺBǮBƨBĜB��B��B��BŢBĜBB��BÖBĜBĜBƨBȴ331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B�/B�;B�1B�*B�0B�*B�0B�*B�*B�*B�*B�*B�0B�5B�5B�7B�EB�mB�B��B��B�#B�GBQByB	�BTB�%B��B�B�uB�QB�&B��B��B��B�tB�aB�WB�BB�/B�B��B��B��B��B��B\Bw&Bk�Be�Bc�B_�BY~BVhB[�Bf�BsB�}B��B�B��B��B��Bj�Bm�Bg�B�B�\B�jB�5B�AB��B�lB��B�B�#B��B��B��B��BpB>�B_�B�&B�-B��Ba�BG:B3�B VB�B��B�CB�B��B֤BЃB�lB�VB�GB�5B�"B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Ck�+Ck��Ck�ZCk��Ck�Ck�	Ck� Ck�Ck��Ck�Ck�{Ck��Ck��Ckq�Ck6�CjИCjKCi^�Ch̙Chc�Cg��Cf6�Cc��C^��CZ��CY��CZ�CZ�CZV!CZg5CZ/YCY��CYn�CYCXc�CW)!CV�CCV�8CV��CV�CW~`CW%�CU)jCT�CR�`CQ��CP��CP{�CP/nCO~�COaCOBlCN&}CMX�CM5^CM�[CM�iCNݼCNVCL3�CH�	CF�_CB�rC>OC:�C6 dC42C1�C0��C05�C0fC/O�C0E�C0	3C0�{C1� C3��C4ԥC6ØC8�uC<?C>��CB5!CBE�CI2CN��CQӰCUǹCY�C\��C_�7Cc	�Ce�Ch|�Cj2�Cl,3Cm��Cn��Co��Cp��Cq��CrJ5CsCs�vCtd�Ct�*Cu�CuͪCvg�Cvg;Cv[fCu��Cv�Cv|\CvS^Cu��Cu�Ct�UCswECq��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�C��uC�ـC���C�ٗC���C��(C���C���C��5C���C�ûC��dC���C��C�\�C�C��C�BVC�	�C��XC�ZC|��CwK�Cr�NCrCrI�CrI}Cr��Cr�pCrg�Cr!{Cq��Cq.�Cps?Co�Cne"Cn}�Cn�_Cn�FCo��Co"�Cl�ACk��Cj�Cij�Ch0OCg��Cgv!Cf��Cf�Cfu�Ce<OCdY�Cd5Cd�Cd�.CfCe�Cc#uC_�HC]CXǟCS��CO�CJ��CH��CF�CD�CD2�CDWCC<�CDS�CD\CE�CF5�CH��CI�'CK��CN�CQ�yCTtbCX��CX��C`Z�Cf��Cj�CnuUCr��Cv4�Cy�qC}YC�U�C���C��;C�ҟC��cC�k�C���C�P0C��kC�Q]C���C�3�C���C���C��EC�\�C��oC��{C��^C�DC���C��MC���C�a5C��C���G�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�@�;@�;3@�;�@�;_@��@�M@�J@�a@�'q@�*q@�f@��x@��@���@�K�@��@�Ɗ@�-r@��q@�a@�s8@��@�gD@�/@�h@�{@�x@��;@���@�c@�r@���@�8@��	@�`@���@��@��@�J�@��N@��@�v@�B=@�*W@�m@��@ރa@�3�@�z@�\�@�=�@��@�7�@�@@ۭ�@��,@��@�Qw@��@֚1@�*�@��@��@ƿ�@@���@�@��&@�Y�@�4�@�mZ@�y8@�=w@�+�@�H=@��=@�q�@È3@��@�C�@���@�۔@���@�a/@�Z�@�@��@��@�[�@���@�6�@�g @�@���@��@��A A �gA ��Ay\A�AP�A�4A�A=�Av�A�A4AA7.A41A�tA�AO�A=SA�qA��A[�G�O�G�O�