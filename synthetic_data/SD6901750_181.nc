CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:59:37Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                  t  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  SH   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  U   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W\   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Y,   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [p   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ]@   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]�   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  aT   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  c�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  eh   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  e�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  g�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  i|   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  i�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  k�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  m�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  nArgo synthetic profile          1.0 1.2 19500101000000  20210302045937  20210302045937  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @���8�1   @��P�� @HE\(��1H�9X`1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99958 (+/- 5e-05) , vertically averaged dS =-0.016491 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�  C�  C�� C�� C�� C�  C�  C�� C�  C�� Cŀ C�  C�  C�  Cـ C�  C�  C�  C�  C�  C�  D @ D� D  D  D@ D� D%� D,  D2@ D8� D>� DE  DK  DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D̠ D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�` D�` D�� D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�  C�  C�� C�� C�� C�  C�  C�� C�  C�� Cŀ C�  C�  C�  Cـ C�  C�  C�  C�  C�  C�  D @ D� D  D  D@ D� D%� D,  D2@ D8� D>� DE  DK  DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D̠ D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�` D�` D�� D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AM�AM&�AM&�AM"�AM"�AM"�AM&�AM&�AM"�AM+AM+AM+AM+AK��AAC�A=�;A<��A:ĜA8��A7�TA6�HA5�-A5hsA4��A3�;A3S�A2��A2^5A2�A2JA1�-A1S�A0�A0A�A0��A/�mA.��A.��A.�\A.bNA.�A-�A-�A-��A-G�A-%A,��A,VA+ƨA+��A+C�A*�!A)�mA)�A(��A(��A'`BA&bNA%�^A$jA#+A!C�AE�Al�A�+A�A��A�AVA�TAbAv�A�A
  AG�AQ�AS�@��^@�n�@�7@��@�=q@֗�@�A�@��@��#@��@���@�M�@���@�%@���@�+@��@�Q�@��#@��
@���@���@��@�1'@��u@�1'@z-@tj@r-@p�u@o�@nV@m�@l9X@k��@j�\@h�`@h �@g�P33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                AM�AM&�AM&�AM"�AM"�AM"�AM&�AM&�AM"�AM+AM+AM+AM+AK��AAC�A=�;A<��A:ĜA8��A7�TA6�HA5�-A5hsA4��A3�;A3S�A2��A2^5A2�A2JA1�-A1S�A0�A0A�A0��A/�mA.��A.��A.�\A.bNA.�A-�A-�A-��A-G�A-%A,��A,VA+ƨA+��A+C�A*�!A)�mA)�A(��A(��A'`BA&bNA%�^A$jA#+A!C�AE�Al�A�+A�A��A�AVA�TAbAv�A�A
  AG�AQ�AS�@��^@�n�@�7@��@�=q@֗�@�A�@��@��#@��@���@�M�@���@�%@���@�+@��@�Q�@��#@��
@���@���@��@�1'@��u@�1'@z-@tj@r-@p�u@o�@nV@m�@l9X@k��@j�\@h�`@h �@g�P33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB:^BA�BB�BB�BC�BD�BD�BC�BC�BC�BC�BC�BC�BC�BO�BE�BI�B=qB0!B+B �B�B{B\BBB��B��B��B��B��B�B�B�B�B�B�`B�TB�`B�`B�TB�ZB�mB�`B�NB�HB�/B�)B�B�B��B��BŢBBĜBB�LB�B�B��B��B��B�bB��B�!B�RB��B��B��B�JB�oB��B�B�jB�mB�BĜB�bB��By�BffBG�B�B��B�B�7B`BBN�B=qB)�B�BBB��B��B�B�sB�HB�#B�B��B�B�B��BB��B��BBBĜBŢBɺB��B��B��B��33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B:^BA�BB�B1�B2�B3�B3�B2�B2�B2�B2�B2�B2�B2�B>�B4�B8�B,fBB�B�B|BvB�YB�B�B��B��B��B��B��B�BܓBقB�BۏB�cB�WB�bB�dB�VB�ZB�oB�dB�RB�LB�4B�.B�
B�	B�B��B��B��B��B��B�WB�'B�B��B��B��BtB��B�0B�bB��B�B��B{]B��B��B�-B�yB�zB�B��B{B��Bh�BU�B6�B�B��B�PBx{BO�B>"B,�BLB�B�uB�^B�LB�.B��B��BЧBʂB�dB�?B�oB�{B�4B��B��B��B��B��B� B�B�B�-B�?B�EB�F33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C{w�C{orC{qPC{vC{w�C{M�C{McC{P)C{T�C{&C{(aCzۅCyfaCr��ClلCj�wCg�5Cf�Cdx�Ca�uC^=�C^��Ca�C_U�C^��C_&�C^��C_hCb�Cb�Ca��C^c�C\��C^D�C`\?C[��CZ�UC]gC_��C_�C`�Cc&$Cd��Ce'QCd��Cd'&Cb��C`�.C`zC`�?C^#�CZ[NCU��CU��CW��CRE%CLe�CI�CDЅC@d�C>�PC9L�C5��C60�C4L�C2T�C/��C.�\C,�C+�qC,�QC-�%C.s�C/j�C/��C0��C2�wC6hxC7�YC;)�C>I�CA�\CF+CCK�CSiCW�@C]��C`�Cb��Ce�Cg�YCjzCj��Ck��Cl�7Cn@CoQ�Cp�wCqM�CrgCCs)^Cqa;Cq��Ct��Cv5Cv�nCv�3Cv'
CuȟCudCt{�Cs`�Cr1�CqA�CpXCnC�33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�C���C���C� �C��QC��aC��C���C���C��ZC��EC���C�L�C���C��?C��C���C~{1C{��Cw��Cx�Cz�Cx��Cx2�Cx�*Cx�Cx�C{קC{�VC{u�Cw�9Cu�Cw��Cz�Cu5(Cs��Cv\�Cyk�Cy��Cz�:C}=�C3�C$C&C~f�C|��Cz�CzT�Cz��Cw�>Cs��Cn"CndCp�`Cj�ACd�Ca' C[��CV�CT��CNĮCJ�QCKWCI@CG`CDuECB�C@�\C@�CA�CBlCB��CC��CD7�CEF�CG�fCK�CM�LCQ8�CT��CXs�C]��Cc�GCl#cCq�HCx�Cz�C}�OC�0�C��2C�C�n�C���C��&C�Q�C�hC��.C�2C��C�C6C�G-C�b|C��C�QC�C�C�[�C��C�ׇC���C�#�C���C��2C�^�G�O�G�O�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�AcVAf	Ag,AP�AP�AROAT�A<A=uAAN�A�r@�8}@���@���@�� @�MN@��@���@�%Q@�L@���@�EV@�=@�-@�� @���@��)@�f�@��`@�v@���@�Z@�f;@��N@��@�qt@練@�@��@��m@�F�@��d@�9�@���@��@�Q�@�M@��O@�˻@䛍@���@���@�,h@���@�%q@��@�@�@�_@���@�4�@�3N@��@���@��@���@�Y�@�_v@�Hh@��@�"�@�^h@�bq@��$@öi@�WS@�ځ@�:}@��N@��A@ڎ@�T@��1@�/@�@�p�@� @���@���@�Y�@�O�@�j�@���A ��AYtAɜAdEA��A��A�%A��A��A��A�A�ATA!CA�zApAr�A�G�O�G�O�