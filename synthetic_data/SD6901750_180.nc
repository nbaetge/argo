CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:59:23Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20210302045923  20210302045923  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�;�[1   @�<��k`@H|�hs�/ڟ�v��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99958 (+/- 5e-05) , vertically averaged dS =-0.016365 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CD  CM  CW  Ca  Ck  Cu  C  C�  C�� C�� C�  C�� C�� C�� C�� C�� C�  C�  C�� C�� Cŀ Cʀ Cπ CԀ C�  C�  C�  C�  C�  C�  C�� D @ D  D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DX@ D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�  D�` Dƀ Dɠ D�� D�� D�  D�@ D�@ D�` D߀ D� D�� D�� D�� D�  D�@ D�` D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CD  CM  CW  Ca  Ck  Cu  C  C�  C�� C�� C�  C�� C�� C�� C�� C�� C�  C�  C�� C�� Cŀ Cʀ Cπ CԀ C�  C�  C�  C�  C�  C�  C�� D @ D  D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DX@ D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�  D�` Dƀ Dɠ D�� D�� D�  D�@ D�@ D�` D߀ D� D�� D�� D�� D�  D�@ D�` D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AJ�\AJ�\AJ�\AJ��AJ��AJ��AJ��AJ��AJ��AJ~�AJr�AGhsACO�A>1A:ĜA:E�A9�A7��A4�A3`BA2�9A1�wA0��A0��A0  A/7LA.z�A.�uA.�+A.bA,��A,��A++A*�RA*��A*ȴA*��A*I�A+
=A+�A+33A*A)A(v�A'
=A'��A)S�A)�A)A*  A*$�A* �A*A)t�A)�A(I�A'�A'&�A'"�A&�A%�;A"��A n�A�A!p�AZAA��A+AA�A�A(�A��A��An�A5?A ~�@�|�@��m@�F@��/@�b@��@֧�@˥�@̓u@Ɨ�@�(�@���@�&�@�%@�r�@���@�V@��F@��u@��-@�\)@�&�@��!@���@�
=@�V@���@��j@~V@}V@z=q@z�\@u�T@s�m@rJ@p1'@pb@o
=@m��@l��@k�@i��@h�`331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                AJ�\AJ�\AJ�\AJ��AJ��AJ��AJ��AJ��AJ��AJ~�AJr�AGhsACO�A>1A:ĜA:E�A9�A7��A4�A3`BA2�9A1�wA0��A0��A0  A/7LA.z�A.�uA.�+A.bA,��A,��A++A*�RA*��A*ȴA*��A*I�A+
=A+�A+33A*A)A(v�A'
=A'��A)S�A)�A)A*  A*$�A* �A*A)t�A)�A(I�A'�A'&�A'"�A&�A%�;A"��A n�A�A!p�AZAA��A+AA�A�A(�A��A��An�A5?A ~�@�|�@��m@�F@��/@�b@��@֧�@˥�@̓u@Ɨ�@�(�@���@�&�@�%@�r�@���@�V@��F@��u@��-@�\)@�&�@��!@���@�
=@�V@���@��j@~V@}V@z=q@z�\@u�T@s�m@rJ@p1'@pb@o
=@m��@l��@k�@i��@h�`331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�BuBBBBBB��B��B��B�B�B�B�mB�BB�B�/B�BB�)B��B��B�XB�?B�LB�^B�XB�FBÖBĜBƨB�LB��B��B�uB��B�wBÖBɺB��B��B��B��B��B��BĜB�qB�dB�qB�^B�?B��B�VB��B��B�jB~�B�bB�}B��B�BB�BB�/B�B��B��B�DBffBE�B\)B|�Bk�BC�BPB�#B�B�B��B��BffB)�B�B�B�BB��B�B�ZB�5B�B�ZB��B��B��B��B��B��BȴB��BǮBƨBĜBÖBɺB��B��B��B��B��B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B�B�B�B�B�B�B�B�B�B�B�B�B�B�8B�!B� B�#B�%B�B�	B��BܰB۩BݸB֋B�fB�8B�OB�bB�KB��B�B�B�fB�rB��B�B�pB��B��B��B�rB�B��B��B��B��B��B��B�B�B� B�B��B��B��B��B��B��B��B�hB��B}�B��B��B��Bn,B�B��B�'B�kB�jB�VB�CB�	B��BzyBU�B4�BKhBl'BZ�B2�B��B�vB�B�jB�PB��BU�BjB
B�B�B�zB�0B��B��BͬB�B��B�ZB�aB�jB�dB�JB�EB�2B�UB�)B�&B�B�B�8B�@B�DB�VB�ZB�]B�]333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C{�tC{�SC{�C|�C|(C|	C|�C|:�C|<�C|N�C|W�C|)�Cz�Cv��Cu��CtduCq��Cm�ClTClfCk��Ck�sCji�Ci�@Ci��Ci�Cik�Ch�Ch��Ch�lCi��Ch(�Cg�`ChW�Ci{�Cj)�Ci�ChM�Ch=�Ch&[Cg�KCgݙCg��CfZ#Cer�Cf�Cgp�ChR�Ci��CkE�Ck�Cj��Ci�Ch�Cf]�Cg��CgħCfYC`��C[q�CU�CG�C?��C>�C8�C5�C3_C2 �C/pC.e�C.�C.$�C.p�C/>C1�)C2� C5'�C7�GC:'�C;
�C<sC>DCB�CFlCJE�CJ�3CN)CRGJCT�NC['�C`4�Cb�Cc�DCeQ.ChSvCj<;ClsCmt�Cn��Cm ZCo��Crf�Cr�lCrv5Cs�Cs�Cs�<Ct>�Cst�Ctg.Ct�Cu�Ct�wCs*gCr�;Cr3�CqL�Cp��CpCnT)333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�C�1�C�I(C�J�C�KEC�L�C�gC�h�C�r�C�w�C�^�C�9�C�K�C�ܽC�C�}�C�BC���C�p�C�F�C�4�C��"C�'ZC��C�!C�C���C��,C��bC��C�V�C�+2C�s�C��C�y�C�$yC�s9C�k�C�`
C� �C�:C�/+C�d�C��C��oC��C��2C�U�C�*�C�nC���C�r�C�f�C�uDC�3C�@C�O�Cz��Ct�PCn��C^� CU��CTϺCN>�CJ�CH7�CF٘CCݚCB�VCBVLCBz�CBԳCC��CFw�CG�<CJiCMdlCP	�CQCR:CT�2CX��C]ۆCb/�Cb��Cf�,Ck5ECnCu/yCz�iC}��C~��C�TC�wC�\C�&xC��C���C��#C�;�C���C��EC���C�1�C��
C���C��]C�q�C��C�JtC�h�C�-7C�YXC�
�C�յC�W�C�	G�O�G�O�441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�A�2A��A�A��A�IA�eA��A�ZA�xA�ZA�DA��AcEA��A�@��x@���@�]d@�@���@���@��(@�� @���@��L@�Ӄ@�Ȧ@�.�@��;@�T�@�@��
@���@��J@�ߡ@��O@�|�@�fz@��@�s@��@���@���@�O@���@���@�>@��N@�X�@�s�@�u�@�r�@���@�@�)
@�[�@���@���@��@��3@�Q�@�L�@���@��@�5|@��_@��@��\@��I@��!@�	�@��H@��w@��D@�Pf@�-4@Ƿ�@ȰW@��V@�01@�>�@���@�#�@���@�[@�̽@��@�`�@�Ә@�r/@��@�d	@��@���@���@�<�A 2T@���A �"AS�AgMAb�A�IAA3mAe�A��A�HA̘A�A��A�!A��Af�A��A��G�O�G�O�