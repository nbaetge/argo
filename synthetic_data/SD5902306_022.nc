CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   n   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:01:52Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030040152  20201030040152  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @�ȋ`�`1   @�ȌͿ��@G����S��1�$�/ 1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013192 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161457201510161614572015101616145720130604092353                                          20170502151423A   A   B   B       @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D � D� D� D  D� D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dq  Dw  D}� D�  D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�  D�� D�  D�  D�@ D�` Dƀ Dɠ D�  D�� D�  D�@ D�@ D�` Dߠ D�� D�� D�� D�  D�  D�` D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D � D� D� D  D� D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dq  Dw  D}� D�  D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�  D�� D�  D�  D�@ D�` Dƀ Dɠ D�  D�� D�  D�@ D�@ D�` Dߠ D�� D�� D�� D�  D�  D�` D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AKx�AK�PAK�hAK�7AKx�AK`BAK\)AK\)AK\)AKS�AKK�AK?}AK&�AKoAJ��AJ�HAJz�AJ^5AJ=qAJ�AJAI�;AI��AI��AI��AI�PAIp�AE�A@��A=\)A<E�A;�hA=%A<��A<{A9�A9l�A8VA7dZA5A3��A4(�A49XA09XA.(�A,n�A+oA)�PA)VA(I�A&n�A${A�AȴA|�A�^A�AO�A�AƨA
$�A��A�@��@�9X@�!@�ȴA�@��!@�$�@��@�V@Л�@�l�@�?}@���@�&�@��@��@��9@���@�%@�hs@�l�@�A�@��@��^@�ȴ@���@�Ĝ@�J@�\)@�`B@�bN@~{@{t�@z-@w|�@v$�@s��@qx�@o\)@mp�@l9X@j��@h�`@g|�@ep�@d�D@bn�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          AKx�AK�PAK�hAK�7AKx�AK`BAK\)AK\)AK\)AKS�AKK�AK?}AK&�AKoAJ��AJ�HAJz�AJ^5AJ=qAJ�AJAI�;AI��AI��AI��AI�PAIp�AE�A@��A=\)A<E�A;�hA=%A<��A<{A9�A9l�A8VA7dZA5A3��A4(�A49XA09XA.(�A,n�A+oA)�PA)VA(I�A&n�A${A�AȴA|�A�^A�AO�A�AƨA
$�A��A�@��@�9X@�!@�ȴA�@��!@�$�@��@�V@Л�@�l�@�?}@���@�&�@��@��@��9@���@�%@�hs@�l�@�A�@��@��^@�ȴ@���@�Ĝ@�J@�\)@�`B@�bN@~{@{t�@z-@w|�@v$�@s��@qx�@o\)@mp�@l9X@j��@h�`@g|�@ep�@d�D@bn�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�yB�yB�sB�sB�mB�mB�mB�`B�ZB�HB�HB�)B�#B�B�
B�B��B��B��B��B��B��B��BYB33B)�B(�BE�BG�B>wB&�B �B{B1B��B�NB�B��B��B��B�{B�B|�B~�B� Bt�BffBL�B�B�!B��B�TB33BR�BD�BR�B$�B#�B��B�B{Bn�B�XBw�B'�B#�B%B�BB�BJB�B�B��Bk�BcTB0!B/B �B�BhB
=B��B�B�ZB�B��BŢB�qB�dB�FB�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�B�CB�=B�=B�0B�0B�(B�+B�%B�!B�&B�B�B� B��B��B��B��B�B�B�BޛBݑBݑB܋B�{B�4Bf�B@�B7�B6�BSDBUUBLB4�B.cB"B�B\B��B�6B\B�B��B�B��B�zB��B��B�DBs�BZRB*.B��B�+B��B@�B`sBRB`rB2[B1QB	_B*&B!�B|B��B�QB5jB1NB�B��B�B�B(B�B�Bx�Bp�B=zB<zB.B(�B�B�B0B��B�B�gB�.B��BʾBȰBÔB�zB�nB�ZB�RB�FB�>B�:B�*B�&B�B�B�B�B�B� 44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cs�FCs�Cs1'CsD�CsX�Cr��Cr��Cr��Cr��Cr��Cr@BCrS�Crg�Cr{#Cr�VCq�^Cq�Cr �CqK�Cq^�Cp��Cp�qCp�!Cp�Cp��CpBNCm��Cf-�C]�oC\�oC]��C`�CcI7CcYXCb�C_��C^�PC[�CYkDCW�CU�?CY��CU�-CN��CLRoCJCHz^CF%�CF1�CD�sCC�CA��C<(�C6�C3��C3�=C3�5C4��C4��C6dZC9��C<XC>�?CAԼCC�`CE5?CB�oCBMPCG��CL��COQhCSk�CW��CW��CW�`CW��C\JCcV�Cf�fChh�CmT�Cm~5CoB�Cp9�Cq-PCr�uCt��Cv{#CxE�Cy9�Cz2oC{��C|��C}��C~�C  CS�C}qC�C�jC�Ce�C�FC~�?C�oC�x�C���C�!C�(�C��m33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                          C��C��C�%�C�0�C�<C���C���C��iC���C��C���C��fC��}C��OC��lC�a�C�kC�u�C��C�}C��tC��4C��"C��gC��eC�v�C~V�Cuh�Ck�2Cj�
Ck�'CnCeCq�Cr�Cq,�Cm��Cl�ZCi@�Cf��Cc��Cb2:Cf�BCbT�CZ<�CW��CT�NCS!.CPs�CP~�CN�:CMհCK->CD�C>�SC:ƇC:�{C:�C;�C<�C=�CA��CD.CF�CJ}\CLg�CN?�CK˛CK-wCQp�CV�QCY��C^Cb��Cb�	Cb�Cc�Cg�tCo��CsH�Cu=�Cz�pCz֥C|�3C}�C~��C�o�C�g�C�b<C�aC��7C�m#C�j�C���C���C��C�(�C�D�C�Y�C�sC���C���C�D�C�^MC��C���C��C���G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444  @'
L@&��@&��@&�@&�4@&0c@&>�@&MS@&[�@&j/@%�@%��@&/@&	@&�@%��@%��@%��@%1�@%?X@$�v@$�8@$��@$�[@$�m@$n�@"��@�@��@'@�u@}@�m@�n@Y�@F@�/@HM@�c@�@��@�@�-@��@	�~@@2@z@h�@o�@O�@��@z?�	�?���?��?�?�?e?�	?��?��?�à?�E?��D@��@ѳ@��@m�@�@
�@	m@4f@$@ �@�@3�@L�@<s@dM@��@�>@ o @ �_@!�@"w�@#!:@$e�@%�m@&�$@(*M@(�z@)�t@*�2@+x)@,-�@,�[@- �@-$�@-?�@-`�@-�@-�)@-$�@-E�@,�!@-��@.< @.��G�O�G�O�G�O�