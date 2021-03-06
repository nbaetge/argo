CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   u   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:02:51Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  SP   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  U$   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Wp   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  YD   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  ]d   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]�   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  a�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  c�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  e�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  g�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  i�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  j<   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  m�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  n\Argo synthetic profile          1.0 1.2 19500101000000  20210302050251  20210302050251  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�;:�O��1   @�;;��ޠ@Gѩ��l��0������1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99954 (+/- 6e-05) , vertically averaged dS =-0.017979 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       @   @@  @�  @�  @�  A   A  Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CD  CN  CX  Cb  Ck  Cu  C  C�� C�� C�� C�� C�� C�  C�  C�  C�� C�� C�  C�  C�� Cŀ Cʀ Cπ CԀ Cـ C�  C�  C� C� C� C�  D @ D� D� D  D@ D� D%� D+� D2  D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw@ D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�� D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       @   @@  @�  @�  @�  A   A  Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CD  CN  CX  Cb  Ck  Cu  C  C�� C�� C�� C�� C�� C�  C�  C�  C�� C�� C�  C�  C�� Cŀ Cʀ Cπ CԀ Cـ C�  C�  C� C� C� C�  D @ D� D� D  D@ D� D%� D+� D2  D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw@ D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�� D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A�~�A�~�A��A��A��A��A��A��+A��uA���A�x�A�&�A\��ARA�AM�PALffAK�AJ�DAIdZAHjAGC�AF5?AE/AD(�AB�jAA��A@=qA?VA>1A=O�A<ȴA;��A:��A9�7A8ZA7�mA7dZA7�A6ĜA6�\A6VA5�hA4�A4ffA3��A3dZA3�A2��A21A1p�A1/A0��A0=qA/�7A.��A.(�A-��A,��A+�-A*jA(��A'�FA$=qA Q�A!��A ��AO�A�wA�/A�HA��A�yA�9A
=A��A�PAr�@��F@�$�@�9X@��@�?}@���@ى7@ڧ�@Ə\@�z�@��@���@��D@��P@��;@�@�1'@��`@�&�@�@��T@�"�@��!@��j@��-@���@�M�@��@�@�(�@}V@{"�@w\)@t�@q�^@p  @n��@n$�@m�-311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�|�A�~�A�~�A��A��A��A��A��A��+A��uA���A�x�A�&�A\��ARA�AM�PALffAK�AJ�DAIdZAHjAGC�AF5?AE/AD(�AB�jAA��A@=qA?VA>1A=O�A<ȴA;��A:��A9�7A8ZA7�mA7dZA7�A6ĜA6�\A6VA5�hA4�A4ffA3��A3dZA3�A2��A21A1p�A1/A0��A0=qA/�7A.��A.(�A-��A,��A+�-A*jA(��A'�FA$=qA Q�A!��A ��AO�A�wA�/A�HA��A�yA�9A
=A��A�PAr�@��F@�$�@�9X@��@�?}@���@ى7@ڧ�@Ə\@�z�@��@���@��D@��P@��;@�@�1'@��`@�&�@�@��T@�"�@��!@��j@��-@���@�M�@��@�@�(�@}V@{"�@w\)@t�@q�^@p  @n��@n$�@m�-311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�1B�1B�7B�7B�7B�7B�=B�7B�7B�DB�JB�=Bt�B�dB�XB�FB�RB�RB�?B�!B��B��B��B�VB�%Bx�Bq�Be`BYBQ�BM�BJ�BD�B<jB1'B(�B%�B#�B!�B�B�B�B�B\BJB+BBB��B��B��B��B�B�B�fB�NB�B�B��B��B��B�^B�^B��Bs�B��B�B�'B��B��B��B�}B�
B�#BB�BoB�B�VB�B� Bv�BW
B<jB �BC�B�/B�B{�BhsBG�B33B$�B�B�BPB��B�BƨB�5B�#B�B�5B�ZB�B�HB�B��B��B��BǮBB��B��B��BBĜ331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B�1B�1Bv�Bv�Bv�Bv�Bw�Bv�Bv�Bx�By�Bw�BbB��B��B��B��B��B��B��B�_B�%B��B{�Bs�BfBB_BR�BF�B?aB;EB87B2B)�B�BpB`BPBFB2B1B
)B B��B��B��B�B�B�xB�\B�EB�EB�7B�B��B��BǤBËB�eB�JB�B��B��B� BaLB�UB��B��B�B��B�yB�BĘBȯB�B
0B��B�B{�Br�Bm�BdlBD�B*BvB1?B��B��Bi�BVCB5�B!B�B�BvB�-B��B�B��B�B�	B�hB�B�@B�bB�.B�B��B��B��B��B�}B�pB�pB�vB�|B��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C[XkC[3�C[57C[5<C[5BC[7TC[C[�CZ�CY�lCX�cCWMCQgCT_�CWGqCX��CX��CY�_CY�CZn�CZN�CZ#7CYŽCY��CYonCYHBCYՊCY�CX��CYCCY�CY�*CY�xCY$tCY��CZ��C[�C\S�C\�C] GC]�\C]��C]��C]�3C]8�C]	C]!C]%�C\��C\�C\��C]\�C]6�C]�C\��C\��C\��C\f�C\blCZ�-CU�CS�CMۃCM�\CGH�C;��C5��C1� C/!0C.0�C,5�C+�aC*��C*�cC*��C+%�C-MwC03�C4]�C6�cC9�C:�DC=�*C@��CD�YCFg	CNCR�7CW��CZ��C^gMCa6�Cc<ZCd}Ce��CgкCjyCk8�ClICm�Cm�#CoNGCp�bCo�Co�CqL�Crk�Cs�nCt?�Ct�Cv	Cw�CwY�CwZ�Cv��Cu�0CtɁ331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�Ct��Ct��Ct�+Ct�qCt�Ct��Ct��Ct;#Cs�CqؑCp�Ci��Cm�CpWCq��Cr*�Cr��CsG�Cs��Cs��Cs��Cs3�Cs<Cr�*Cr�&CsOrCrs�Cr$�Cr�Cs�Cs%�Cs.Cr��Cs(9Ct�DCuxCv3Cv|#Cw�Cw�$Cw�iCw�9Cw�dCwE�Cw�Cw�Cw7}Cv�Cv�Cw�CwXCwW�Cw �Cv�gCv��Cv��Cv}hCv~�Ct�4CoIzCl¢CfV�Cf�C_OCR�CKq�CF��CDCC
�C@��C@P�C?/C?s�C?H�C?�+CB/)CEs~CJ#�CM!CO��CQ|�CT��CX!�C\k'C^��Cg�Cl��CrP�CuYCy�GC|�<C�C�9�C��C�rC�dfC��C���C�i�C���C�gIC�?C���C�îC���C�8vC���C�F\C���C�NyC��}C��C��C�ƲG�O�G�O�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144   G�O�@�ܒ@�ޠ@���@��%@��@�-@�
@�v6@�`x@�,@�u�@�i?@��@��@�3�@�{@�%@錀@�'A@�w@��E@�yq@�_�@�!q@��@��@���@�u`@��}@�T�@�k�@�s�@��@�nL@��7@릀@�Ze@�!@�;#@��`@���@��@��L@�a�@�3�@�8d@�S�@��@��@�0�@��@�s@�>W@��D@��J@���@�Z@좴@�Q@�.@�J6@� &@�G�@�@ɴd@�N`@���@�;7@�=M@�!@��=@�l�@��%@���@��@�j�@���@��@��@�5�@��@�T@�|x@Ӛ@՛J@��[@�/�@�_@눯@��@�N@��!@�1�@��`@��@�E�@���@���A �A ESA�A�*Ag�A_�A']AŐAamAȪA�A�7AZA��A��A9�G�O�G�O�