CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   n   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:57:41Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20210302045741  20210302045741  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @���l�1   @����-�@Iqhr� ��/�- 1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.9996 (+/- 5e-05) , vertically averaged dS =-0.015587 (+/- 0.01)                                                                                                                                                                                            C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C:  CD  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�  C�  C�  C�  C�� C�� C�  C�  C�  C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C�  C� C�  C�  C�  D @ D� D� D@ D� D,  D8� DD� DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�` D�� D�� D�  D�` D�� D�� D�� D�  D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ D�` D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      ?�  @   @@  @�  @�  @�  @�  A   A  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C:  CD  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�  C�  C�  C�  C�� C�� C�  C�  C�  C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C�  C� C�  C�  C�  D @ D� D� D@ D� D,  D8� DD� DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�` D�� D�� D�  D�` D�� D�� D�� D�  D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ D�` D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A+�mA,I�A,E�A,Q�A,I�A,A�A,I�A,E�A,Q�A,VA,^5A,^5A,bNA,bNA,jA,n�A,v�A,v�A,z�A,~�A,�+A,�uA,��A,��A,��A,��A,�A,�!A,�9A,�RA,�jA,��A,ĜA,ȴA,��A,�A,�HA,�`A,�A,�A,��A,��A,��A-%A-%A-VA-oA-�A-�A-"�A-�A,�A,�HA,��A,jA,A�A+�;A+�hA++A)��A&A$�yA"�9A1'A�Ap�A`BA�hAv�AA��A  A
JAO�A�@�ƨ@��@���@ܛ�@��!@��+@�&�@�J@���@��T@��`@�z�@�p�@�t�@���@�bN@~ff@{�F@yx�@w�w@v5?@u`B@s��@r=q@p�9@p  @oK�@n��@n@l��@lj@j��@ix�@h�@g�;33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          A+�mA,I�A,E�A,Q�A,I�A,A�A,I�A,E�A,Q�A,VA,^5A,^5A,bNA,bNA,jA,n�A,v�A,v�A,z�A,~�A,�+A,�uA,��A,��A,��A,��A,�A,�!A,�9A,�RA,�jA,��A,ĜA,ȴA,��A,�A,�HA,�`A,�A,�A,��A,��A,��A-%A-%A-VA-oA-�A-�A-"�A-�A,�A,�HA,��A,jA,A�A+�;A+�hA++A)��A&A$�yA"�9A1'A�Ap�A`BA�hAv�AA��A  A
JAO�A�@�ƨ@��@���@ܛ�@��!@��+@�&�@�J@���@��T@��`@�z�@�p�@�t�@���@�bN@~ff@{�F@yx�@w�w@v5?@u`B@s��@r=q@p�9@p  @oK�@n��@n@l��@lj@j��@ix�@h�@g�;33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�TB�`B�NB�ZB�`B�`B�fB�ZB�`B�`B�`B�`B�`B�ZB�`B�`B�`B�`B�`B�`B�ZB�ZB�ZB�ZB�ZB�`B�ZB�ZB�ZB�ZB�ZB�ZB�ZB�ZB�`B�ZB�ZB�`B�`B�ZB�ZB�ZB�`B�ZB�`B�ZB�`B�ZB�ZB�ZB�HB�HB�/B�#B�B��B��B��B��B�wBB�}B�9B�1B�%B�oBĜB
=B7LBA�BL�B-B��B�
B��Bv�BVBB�B��B��Bn�B?}BBB�B�)B��B��BȴBƨBÖB��B�wB�qB�jB�dB�dB�^B�XB�^B�^B�qB�}B��BŢBŢBƨBȴBɺ33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          B��B�TB�`B�;B�JB�LB�NB�WB�JB�QB�QB�OB�QB�OB�JB�QB�QB�OB�OB�OB�OB�IB�KB�KB�KB�KB�OB�KB�KB�KB�KB�HB�JB�JB�KB�OB�KB�KB�OB�OB�KB�KB�KB�OB�KB�OB�KB�QB�KB�IB�JB�:B�9B�B�B�B��B��B��B��B�kB��B�tB�1Bx4Bv"B�nB��B�,B'7B1tB<�B�B��B�B��Bf�BFB2�B�
B��B^�B/�B�EB�LB��B�jB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cl��Cl�1Cl��Cl�5Cl�JCl�wCl�6Cl� Cl��Cl�ACl��Cl��Cl�OCl��Clz�Cl��ClvxCl�!Clw Cl�bClp�ClY�ClBzClSLClf}ClS�Cld0ClQ�Cl<�ClM�Cl8�ClK�Cl6�ClI�Cl23ClMCl+fClJCl �Cl�Ck��Cl�Ck��Ck�Ck�yCk�Ck��Ck��Ck�kCk��CkQ�Cj�dCi�ChMCg"QCezoCb	-C\�CW)�CK3 CA�KC?sgC;��C4�PC19NC/�C.�C/�C.�@C.̯C/��C00�C2'iC4g�C6�JC:IVC>
dCA��CEabCQ�-CU>CZ��C`��Ch�ACi��CmCo�|CqյCr�|Ct	Ct��Cug�Cv2�Cv�Cw�Cw`qCwe�Cw��Cw�Cxp.CxCw�yCw)CvYHCu��CtU�Ct2+Cs�Cs�Cq�z33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�C���C���C��\C���C���C���C���C��rC���C���C���C��UC�~�C��nC�C���C��-C��=C��<C�u�C�j%C�t�C���C�w�C��FC�y{C�oC�y�C�ocC�{^C�p�C�|�C�q:C�eFC�p"C�e�C�[C�gC�\�C�h�C�] C�QC�_�C�S�C�I�C�R�C�I�C�=�C�	�C��C�DwC�_pC���C�;C{�hCv	�Co��CbdxCX&CUX|CQJeCI�CE�CC5`CC�CC�#CC-tCC
�CC�	CD�rCF�VCI`�CL-�CO��CT2^CX]=C\o�CjtCn>�Ct��C{.C�bC���C���C��7C�:�C���C��DC�ڻC�H
C��C��cC�CC�pVC�v�C��>C�җC��C��C���C�aC��C���C��tC���C���G�O�G�O�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144  G�O�G�O�@��,@���@��@@��i@���@���@��n@��@��f@��)@��e@���@�x�@���@�y1@���@�@��8@�}@@�gW@�P�@�ev@�|�@�k8@�?@�n\@�Zw@�n�@�Z�@�q�@�^	@�u@�^�@�G�@�\j@�Ho@�4@�J�@�7\@�N;@�7�@� �@�=@�& @�n@�#�@��@��@���@�#Z@�@�eR@�(�@�e�@��@�2?@��@�V6@π�@���@��C@��d@���@�fq@�Dv@��J@�^�@�=�@�(@���@��)@�R�@��@Ǫ4@˵x@ϵ�@Ӟ�@�z@�H@���@��@��`@��(@�z�A �|A�8Aq�AtAaWA�GA:�Ax_A�:A�A��AdAEA�JA]�A5A�AtEAE�Ae�AU�A!(G�O�G�O�