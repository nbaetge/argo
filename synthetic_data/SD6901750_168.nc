CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   n   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:56:23Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                  p  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  S,   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  T�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  UT   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  X�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y4   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Z�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  \�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  `�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  `�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  b�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  dd   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  d�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  hD   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  h�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  jl   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  l$   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20210302045623  20210302045623  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��;I��J1   @��<Lk��@I3S����-	7KƧ�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99962 (+/- 5e-05) , vertically averaged dS =-0.01504 (+/- 0.01)                                                                                                                                                                                            C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CC  CM  CW  Ca  Ck  Cu  C�  C�  C�� C�� C�  C�� C�� C�  C�  C�� C�� C�� C�� C�� C�  C�  Cπ CԀ Cـ Cހ C� C�  C� C� C�� D � D@ D� D  D  D� D%� D+� D2@ D8� DE  DQ@ D^  Dj� Dw  D}@ D�� D�  D�` D�� D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CC  CM  CW  Ca  Ck  Cu  C�  C�  C�� C�� C�  C�� C�� C�  C�  C�� C�� C�� C�� C�� C�  C�  Cπ CԀ Cـ Cހ C� C�  C� C� C�� D � D@ D� D  D  D� D%� D+� D2@ D8� DE  DQ@ D^  Dj� Dw  D}@ D�� D�  D�` D�� D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A7�7A7G�A7K�A7K�A7K�A7O�A7K�A7K�A7K�A7C�A7G�A7G�A7K�A7dZA7p�A7x�A7;dA733A7;dA7G�A7C�A7?}A7;dA7?}A7C�A7K�A7O�A7S�A7XA7\)A7`BA7`BA7`BA7dZA7dZA7XA7"�A6z�A5l�A4M�A2�!A1�#A1/A0�jA0�A/�hA.��A.�`A.1A-7LA+�#A+�A*��A*-A)��A(��A( �A'�7A&��A&�DA%�^A%%A$=qA#33A"A�A!��A�A�+A;dAVA�^A��A�7A��A%A �/@��#@�u@׶F@ʏ\@��@�ȴ@���@��@���@��@�l�@��@��@���@��;@�E�@�b@�(�@�33@���@�ƨ@�`B@�w@|��@v�@sS�@r�@q�@m/@jJ@hQ�@d��@b�@d9X33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          A7�7A7G�A7K�A7K�A7K�A7O�A7K�A7K�A7K�A7C�A7G�A7G�A7K�A7dZA7p�A7x�A7;dA733A7;dA7G�A7C�A7?}A7;dA7?}A7C�A7K�A7O�A7S�A7XA7\)A7`BA7`BA7`BA7dZA7dZA7XA7"�A6z�A5l�A4M�A2�!A1�#A1/A0�jA0�A/�hA.��A.�`A.1A-7LA+�#A+�A*��A*-A)��A(��A( �A'�7A&��A&�DA%�^A%%A$=qA#33A"A�A!��A�A�+A;dAVA�^A��A�7A��A%A �/@��#@�u@׶F@ʏ\@��@�ȴ@���@��@���@��@�l�@��@��@���@��;@�E�@�b@�(�@�33@���@�ƨ@�`B@�w@|��@v�@sS�@r�@q�@m/@jJ@hQ�@d��@b�@d9X33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�mB�/B�B�
B�
B��B��BŢBɺBÖB�^B�B��B��B��B��B��B��B�uB�bB�\B�uB��B��B��B�FBƨBB�-B��B�VB�^B�?B��B��B�3B��BȴBv�B.B��B�FB�uBq�B�B�Bt�BbNBN�B49BuBbBhBJB��BB+B��B��B��B�B�mB�ZB�B�B�`B�TB�ZB�BB�BB�f33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          B�mB��B��B�JB�LB�OB�OB�TB�OB�PB�OB�KB�PB�GB�GB�EB�]B�]B�]B�YB�]B�`B�`B�eB�]B�`B�eB�]B�eB�cB�eB�eB�eB�eB�bB�\B�\B�?B�B��BͱBɚBǍBǌB�nB�MB�)B�?B�B��B��B��B�jB�RB�CB�.B�B� B��B�B�B�B�TB�uB��B�0B�B��B�B~�B��B��B�hB�fB��B�sB�CBgdB�B�B��B�.BbiBr�Bs�Be{BSB?�B%BAB,B5B�B��B��B��B��B�B�B�B�?B�0B�UB�gB�7B�%B�/B�B�B�<33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cn��Co^Co/Co	�Co�CohCo�Cn�Cn�WCn�QCn�!Cn�Cn�Co�Cn��Cn��Cm��Cm��Cm�uCm��Cm^,Cl�ZCl��Cm �Cm�Cl�Cl�RCl�PCl�Cl|�Cl�CkۑCk�pCkcCj�+CiވCgpCc��C`�rC]��C\6C[q`C[LCZ��CZ.CY��CY.�CX�CW��CV��CV+�CUОCU`CT��CT�CS�CRb>CQ��CQhCP*�CM��CK��CG?oCCW�C<h4C7�rC3�GC2q�C1�C1	�C/C/JC/�C3A�C6sC7A�C:��C@Q�CG�CMsCT�CXS=C[��C[��C\/C]�WC_�RCb��Cf)Ci�Cj�Cj��CkCl��ClE�Ck��Cl�]Cm��Cm^�Cm�CoR'Co�Cm��CmyMCnh�CnXhCn2�Cn��Cmv+Cl;33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�C�ÇC��!C��>C��GC���C��IC���C���C���C���C��,C���C�ĢC��3C�#�C��C�(�C��C��]C��]C���C���C��C��BC���C���C�~�C�tFC�<�C�NC���C���C��C��C���C} �Cy��Cv�Ct��Ct�Cs�HCs0�Cr�eCr,�Cq�Cp޸Co�Cn�>CnL�Cm�[Cmn�Cl��Cl�Cj�$Cj$3Ci�<Ch�iCg�YCerCb�C]�rCY��CQ�CL�nCHSCF�CF<JCE>�CCCC_dCC��CG�CJ�qCLe~CP/1CV�2C^5�Cd�@Cl�ICq�9Cu�iCud/Cu�Cw��Cz,|C}WC��bC��C�ëC��C�h�C�x.C�FC���C�k|C�޷C���C�C���C��\C� \C�ނC�g�C�bC�PmC��;G�O�G�O�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144  G�O�G�O�@��@��!@��D@��T@��}@���@���@�ӟ@�ӱ@���@��}@��@��-@�{�@���@���@��O@���@�=�@�׽@�ȳ@��	@���@��@��{@��C@�xw@�d]@���@��c@���@�A�@��3@���@��@� �@�x@��J@��@�P/@��@�v}@��$@�|�@��@�<6@�P�@�cn@��`@�e$@��7@�v�@� @�n�@�Ɨ@�+@�he@�n�@��@�|-@���@�γ@�i�@�kJ@��@�џ@�N\@�Z�@�Gv@���@���@��M@���@�8x@��c@��@�Rl@ۯ�@�O�@���@���@�`@��@��@�*�@��@���@��T@�H@���@�NH@�Wc@���@�4�@�?@�@@��@�g�A �A ZD@�\�@��A �A F@���A +G�O�G�O�