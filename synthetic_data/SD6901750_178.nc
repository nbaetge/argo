CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:58:53Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20210302045853  20210302045853  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�;I��J1   @�<= @I"M����0>5?|�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99959 (+/- 5e-05) , vertically averaged dS =-0.016152 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CX  Ca  Ck  Cu  C�  C�� C�� C�  C�  C�� C�� C�  C�� C�� C�  C�� C�� C�� Cŀ Cʀ C�  CԀ Cـ Cހ C� C�  C� C�  C�  D � D� D� D  D@ D� D%� D,  D2  D8� D>� DE  DK� DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CX  Ca  Ck  Cu  C�  C�� C�� C�  C�  C�� C�� C�  C�� C�� C�  C�� C�� C�� Cŀ Cʀ C�  CԀ Cـ Cހ C� C�  C� C�  C�  D � D� D� D  D@ D� D%� D,  D2  D8� D>� DE  DK� DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A5��A5��A5��A5��A5��A5��A5��A5��A5��A5��A5��A5�A5hsA0�\A/�PA//A.�\A+�A*ȴA*n�A*9XA*�RA++A+/A+A*��A*�\A*bNA*9XA*{A)��A)��A)dZA);dA)+A)"�A)
=A(�A(��A)oA)S�A(�HA(I�A(�+A(VA'�A't�A'?}A&��A&�yA&��A&(�A&A%��A%��A%�wA%��A%�^A%�A%��A%l�A$�A#�;A$��A"�HA!��A+AO�A��A�A^5AA�A�A�mA�Ar�A&�AZA�A�A�@�bN@�l�@�V@�\)@�%@��`@�ff@�I�@�A�@�5?@��@�Z@�Q�@�S�@�;d@��@��\@�r�@��j@�$�@�|�@��@�-@��
@���@�b@}�@z�\@w�;@vff@v{@v5?@s�
@qhs@oK�@mp�@kƨ@j^5@i��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A5��A5��A5��A5��A5��A5��A5��A5��A5��A5��A5��A5�A5hsA0�\A/�PA//A.�\A+�A*ȴA*n�A*9XA*�RA++A+/A+A*��A*�\A*bNA*9XA*{A)��A)��A)dZA);dA)+A)"�A)
=A(�A(��A)oA)S�A(�HA(I�A(�+A(VA'�A't�A'?}A&��A&�yA&��A&(�A&A%��A%��A%�wA%��A%�^A%�A%��A%l�A$�A#�;A$��A"�HA!��A+AO�A��A�A^5AA�A�A�mA�Ar�A&�AZA�A�A�@�bN@�l�@�V@�\)@�%@��`@�ff@�I�@�A�@�5?@��@�Z@�Q�@�S�@�;d@��@��\@�r�@��j@�$�@�|�@��@�-@��
@���@�b@}�@z�\@w�;@vff@v{@v5?@s�
@qhs@oK�@mp�@kƨ@j^5@i��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B��B�B�B��B��B�B�B��B��B��BɺB��B�/B�5B�;B�/B�5B�BB�;B�/B�)B�B�B�
B�B��B��B��B��B�B�/B�
B��B��B��B��BĜB��B�wB�qB�XB�9B�-B�B�B�!B�'B�'B�9B�3B�B��B��B�FB��B��B�DB��B�LB��B��B��B�B1B�B�B&�B�B �BhB��B��B�B[#BE�BF�B$�BJB��B�ZB��B��Bt�B�Bm�BB�BhB#�B �BoB��B�B�B�NB�B��B��B��BǮBĜBĜBȴB��B��B��B��B��B��B��B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B�B�B�B��B�B�B�B�B�B�B�B�
B�B�B�B�B��B�KB�$B�#B�B�WB̉B͎BΖB̋B͒BϞBΖB̊B˅B�xB�oB�jB�dB�ZB�\B�[B�VB�uB̋B�iB�AB�PB�EB�%B��B��B��B��B��B��B��B�B�zB��B��B��B��B��B�xB�KB�.B��B�KB�0Bz�B�XB��B�%B�B�RB��B��B�B�BCBB B �B�;B�HBq~BJ�B5B6%BaB��B�KB��B�YB�zBdWBp�B]-B23BBBpBB�yB�XB�5B�B��B��B��B�~B�eB�UB�VB�hB��B�~B��B�zB�xB�{B�B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C��rC��EC���C���C���C��C��JC���C���C���C���C���C��@Cy�5Cw��CwKCw�CvLcCuJ�CtrCt-�CtF�Cr��Cp�cCoW�CoRCn�hCo(�Co.\CoZ�Co��Co��Co:Co�Cn�kCn�DCnYCm�;Cm�cCnmTCn0Cmp�Cmj�CmsCm.Cm��Cn@CnM�Cnd�Cn*Cm�\Cm�;Cm�[Cn�CnJ�Cn�FCn[�Cn*kCmдCm�Cl�
Cj�7Ci'tCd��C^dCV�CP��C?k
C4�hC2�LC1�C/mqC/Y�C.o�C.[LC.�-C.z�C.�IC/U�C1*C4�C7�C:�C>N�C@�9CB��CE��CH��CKA�CMmJCP,nCT3CX"9CX��C[�aC`KnCd�Cd"�Ce�`Ch0�Ck��Cl��Cn[8Co��CqV�Cr�vCs�Cs�vCt�jCu�Ct��Cs��Cr��Cs�Cr�WCr�RCr�lCrM�Cq��CoY�333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�C���C�CC��C��IC��C�՘C��C���C���C��C�E\C���C��bC���C��%C��C���C��C��C��uC�?XC� @C�A�C��C��C�+BC�/�C�I�C�`�C�cC�;�C�)�C�FC��C��C�h�C�TC��PC��gC�IC�GbC��C�C�_ZC��wC��fC�ڱC��C�aeC���C��3C���C��RC��C��C��TC���C��C��C���C��C~��Cw�YCo��ChƷCU��CI��CG��CE�}CCךCCƌCB�dCB�uCB�\CB�CC5GCC��CE�oCI$�CM��CP�CT��CW��CY��C](SC`�
CcKzCe��Ch��Cm;Cq�\Crv�Cu��Cz��C��CW�C��LC���C��4C�~C�uMC�D�C�'�C���C�,C���C��C�I8C�1ZC��'C��C�8 C��C�-�C��C���G�O�G�O�441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144G�O�G�O�A��AA�fA�	A��A�JA��A�fA�xA[Au�AnfAJJA�A�A�A�A�LA},A��A�iA ��@���@��@�y�@��7@���@���A A .@���@���@�y|@�D�@��@�9�@�@�\@��,@���@���@���@���@�'�@���@��@��@��%@�+�@���@�j�@��
@�L@�eJ@� @��n@��C@�h}@���@�`�@���@���@��@��@�w@�i@��[@��@���@�.@���@���@��@�'@�	@�fX@�
1@��@��@�I<@ȏJ@�3�@���@��$@�O�@א�@�3�@܍�@߆�@��@�m@���@�E@��@���@�!F@���@��x@�B�@�b�A �A ��A��AcA��A�A��A�iA�ACA�YA�=A�A�A��Ao�G�O�G�O�