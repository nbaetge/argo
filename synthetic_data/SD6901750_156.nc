CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   w   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:53:23Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  SX   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  U4   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  Yd   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  ]�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  a�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  b<   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  d   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  e�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  fl   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  hH   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  j$   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  j�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  lx   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  nT   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302045323  20210302045323  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��;1   @��;ۗS @Ia$�/�1/�;dZ 1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99965 (+/- 4e-05) , vertically averaged dS =-0.013678 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114928                                          20210205114928A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CN  CX  Ca  Ck  Cu  C  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�  C�  C�  C�  C�  Cـ Cހ C� C�  C� C� C�� D � D� D� D� D@ D� D%� D+� D2  D8� D>� DE  DK  DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�� D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @   @@  @�  @�  @�  @�  A   A  Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CN  CX  Ca  Ck  Cu  C  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�  C�  C�  C�  C�  Cـ Cހ C� C�  C� C� C�� D � D� D� D� D@ D� D%� D+� D2  D8� D>� DE  DK  DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�� D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A�ƨA���A���A���A���A���A�ƨA�A� �A��A���AsAO|�AL9XAI;dAFr�AC�AAA?K�A> �A<v�A8�HA8��A8�A7�7A6A4bA3�A2-A1+A0�DA0  A/+A.��A.�HA/VA/XA/��A/�;A0bA0$�A05?A0A�A0A�A0E�A0I�A0Q�A0VA0M�A0M�A0Q�A0E�A05?A/�A/A-�FA,��A*��A(I�A%|�A#l�A!��A�A�PAjA�!A��A$�A	��A�
A -@���@��F@��@�@��@�bN@�7L@�bN@���@�1'@�33@��@��@��!@��
@�hs@���@�1'@�t�@�r�@�J@�t�@���@�"�@��@��F@���@���@�@�p�@�Z@~�y@}�@{�
@{S�@z�@xA�@w\)@u@t�/@s�m@r~�@q�^@p�`@p�@p1'33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A���A��A�ƨA���A���A���A���A���A�ƨA�A� �A��A���AsAO|�AL9XAI;dAFr�AC�AAA?K�A> �A<v�A8�HA8��A8�A7�7A6A4bA3�A2-A1+A0�DA0  A/+A.��A.�HA/VA/XA/��A/�;A0bA0$�A05?A0A�A0A�A0E�A0I�A0Q�A0VA0M�A0M�A0Q�A0E�A05?A/�A/A-�FA,��A*��A(I�A%|�A#l�A!��A�A�PAjA�!A��A$�A	��A�
A -@���@��F@��@�@��@�bN@�7L@�bN@���@�1'@�33@��@��@��!@��
@�hs@���@�1'@�t�@�r�@�J@�t�@���@�"�@��@��F@���@���@�@�p�@�Z@~�y@}�@{�
@{S�@z�@xA�@w\)@u@t�/@s�m@r~�@q�^@p�`@p�@p1'33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�mB�NB�NB�fB�fB�fB�fB�fB�BB�BB�5BB��B��B�=Bw�B[#BM�B@�B5?B �B��BB  B�B�HB��B�wB�?B�B��B��B��B��B��B�B�3B�XB�}BBĜBƨBǮBǮBȴBȴBɺBɺBɺB��B��B��B��BǮB�qB�B��B��B�Bn�Be`B\)BVBR�BH�B2-B�B{B  B�B�
B�-B�;B��B��BB��B�yB�BB��B��B�PB~�Bo�BjBP�B8RB(�B�B+B��B��B�B�sB�TB�#B�
B��B��B��BƨBĜBB��B��BÖBB��B��B�}B��B��B��B��BBĜBƨ33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B�B�B�mB�#B�#B�:B�<B�<B�?B�<B�B�B�
B��B��B�sB|	Bi�BL�B?�B2YB'B�B�B��B��B�B�,B��B�`B�)B��B��B��B��B��B��B��B�B�>B�eB�vB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�XB�B��B�uBv	B`�BWSBNBG�BD�B:�B$)B�BxB�B�B�B�;B�EB��B��B�'B��BۅB�LB��B�BhBqBa�B\�BCB*qBB
�B�RB�#B��B��BڠBՀB�QB�9B�.B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Ca�gCa��Ca�"Ca��Ca��Ca�0Ca�dCa�@Caa�Ca?�C`ޖC_M�C[]�CS��CW��CX�ICY�CX��CYk�CY�CZ|	C[�C]^�C^�fC]4�C]�wC^��C`�:Cb}Cb�Cc9UCc�hCci CcX�Cd�sCd�Ce�OCf��Cg�.ChH�Ch�!CiBCirFCi}.Ci��Ci��Ci��Ci�`Ci�ZCip�Ci;DCh�5ChsXCg�.CfaCd3�Ca]�C^�HC[��CWe4CS��CP�RCMD�CH��CC�
C>}C9�C5�aC2lC0yC03�C1}C2C4@C4{�C5x>C8l<C:��C>f�CAtgCD{CG��CK��CP$CR7�CT�KCWnC[�C^��Ca�hCdJ�Cg?Ci�CjŁCl�
Cn+GCoX�Cp�8Cqr�Cr"eCsWqCtSjCu0JCu�NCv:�Cv��Cv�VCvMCv�Cw
,Cw>Cw^-Cv�Cv��Cv�xCv"?Cu^+Cs�Cq��33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�Czv�CztUCzJaCzH�Cz#�Cy�qCyԨCy��CyE,Cw�eCs0�Cj�Co�Cp*Cp�%Cp� CqFCq�DCrJCs��Cu�Cw,CuYCuȹCv��Cy-�Cz�C{�)C|*C|��C|OC|?�C}�KC~CiC��C���C���C�IQC�rC��C��vC��^C��C���C���C���C���C�z&C�EC��C��iC��C}k�CzHCwD�Ct.Co>�Ck%�Ch+�Cd�C_-�CYWACS��CN��CIʿCE��CD5CC� CD�fCF�CH|�CH�LCI�4CM0zCO��CS��CWMxCZ@ C^|�Cb�.Cg�Cj�Cm*�Co�0CtCx�C{H�C~ZWC���C��oC��C���C���C�jC�/�C���C�#C���C�A�C���C��C�\C��C���C�p�C��\C��aC���C�IC��C���C��PC�t�G�O�G�O�G�O�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 G�O�G�O�@�q�@�o�@�Gh@�E�@�"?@���@��d@��@�L�@���@�v�@��@�u�@�s
@�@��3@�r�@���@�@��z@�@�I@��@���@�s@�6@��@�}F@���@�x@�7q@�(�@�g @��,@��2@��!@��<@�z@�:�@���@���@��@��%@���@���@���@�@��@���@�2�@��R@���@��@�H�@�E$@�`�@�i�@嬿@Ὤ@��;@��@�@\@Х�@�<�@�Q`@��e@�w@�[�@��@�@�%l@�w�@��r@���@��W@�R�@�h1@ΰ�@х=@Ֆ{@ٖ@@�`@එ@�Z@�TW@�H@�,Q@�;�@�-�@�4�@�^�@�.�@��@�ޟA �A ѡA:7A��ADdAΜAH2A�AݎAjAsA�qAwA]|Ad�A�<AWtA4�AGWA�VG�O�G�O�G�O�