CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   p   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:03:54Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  S4   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  T�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ud   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W$   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  X�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  YT   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  \�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]D   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  `�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a4   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  b�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  d�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  e$   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  h�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  i   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  j�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  l�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  mArgo synthetic profile          1.0 1.2 19500101000000  20201030040354  20201030040354  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               $A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @�닎8�1   @�댚�� @Hj��vȴ�07���01   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013226 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161459201510161614592015101616145920130604092422                                          20170502151425A   A   B   B       @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  CЀ C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D� D@ D� D� D%� D,@ D2@ D8� D?  DE@ DK@ DQ� DW� D^  Dd@ Dj� Dq  Dw  D}� D�� D�  D�@ D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�` D�@ D�` D߀ D� D�� D�� D�  D�  D�` D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  CЀ C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D� D@ D� D� D%� D,@ D2@ D8� D?  DE@ DK@ DQ� DW� D^  Dd@ Dj� Dq  Dw  D}� D�� D�  D�@ D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�` D�@ D�` D߀ D� D�� D�� D�  D�  D�` D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A[+A[7LA[S�A[S�AX(�AP��AO�FAN{ALQ�AJ��AJA�AI�wAI7LAG�FAE�-ADz�AC�FAB�A@�\A?��A?hsA>�/A>bNA=�FA=7LA< �A;�-A;�A:bA9G�A8��A7S�A7A7�wA7t�A6��A7VA6��A7�A7;dA7hsA77LA6�A6jA6bA5��A5hsA5oA4�\A4jA4�A3�A2��A2{A1`BA0��A/A-��A+|�A)O�A& �A"�Al�A{A;dA�hA�
A�A�A	dZA
=A��@���@���@�;d@���@�?}@�J@�l�@�=q@�n�@ޗ�@Ձ@υ@�Ĝ@�n�@��F@�t�@��@�Ĝ@�=q@�33@��H@��@��@���@�C�@�?}@�ȴ@��@�dZ@�V@��j@}@{C�@uV@s"�@w\)@v�R@y�@sdZ@p�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                A[+A[7LA[S�A[S�AX(�AP��AO�FAN{ALQ�AJ��AJA�AI�wAI7LAG�FAE�-ADz�AC�FAB�A@�\A?��A?hsA>�/A>bNA=�FA=7LA< �A;�-A;�A:bA9G�A8��A7S�A7A7�wA7t�A6��A7VA6��A7�A7;dA7hsA77LA6�A6jA6bA5��A5hsA5oA4�\A4jA4�A3�A2��A2{A1`BA0��A/A-��A+|�A)O�A& �A"�Al�A{A;dA�hA�
A�A�A	dZA
=A��@���@���@�;d@���@�?}@�J@�l�@�=q@�n�@ޗ�@Ձ@υ@�Ĝ@�n�@��F@�t�@��@�Ĝ@�=q@�33@��H@��@��@���@�C�@�?}@�ȴ@��@�dZ@�V@��j@}@{C�@uV@s"�@w\)@v�R@y�@sdZ@p�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�VB�VB�PB�PB�bB��B�{B�uB�VB�=B�1B�+B�%B�B�B�+B�JB�JB{�Bw�Bu�Bq�Bn�Bl�BiyB^5B\)B_;BVBN�BI�B=qBA�BP�BP�BL�BP�BR�BVB[#B_;B^5B[#BVBQ�BL�BJ�BG�BA�B@�B<jB?}B6FB/B(�B)�B �BbB��B�B�
BǮB�dB��B�BuB��B��B�=B{�BgmBk�BiyBT�BP�BB�B@�B}�B��BŢB��B`BB33B�BuB��B�B�'B�{B�BaHB?}B,B'�B!�B
=BB��B�B�5B��BƨBɺBÖB�qB�B�B�wBB��B��B�w4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�B�B�B�B�8B�+B�$B�B��B��B��B��B��B��B��B��B��B��B�yB�jBSB|BBz1Bw Bk�Bi�Bl�Bc�B\|BWYBKBO+B^�B^�BZrB^�B`�Bc�Bh�Bl�Bk�Bh�Bc�B_�BZpBXcBUQBO(BN(BJ	BMBC�B<�B6�B7�B.cB�BxB�B�B�EB��B�rB�B!B	|B�WB��B�nBt�ByBv�BbB^cBPBN B�yB�B�-B�&Bm�B@�B%B �BGB�B��B��B�jBn�BL�B9aB5LB/%B�BYB-B��B�B�,B��B�	B��BʼB�_B�UB��B��B�B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C�z=C��C��C���C�\C���C~��C{��Cz��CyYXCw�Cw"�Ct-Cp2�Cm޸Cm�Cm*Ck�`Cj�mCie�Civ�Ci�mCjaCl�Cl-CkYXCkj=Cj��Cj�5CjΘCj�Ci[dCj3uCl�CnD�Co�7Co�dCpPCp �Cn�;Cn�-CnĜCn�Cn�Cn1'CnC�Cm��Cm��Cm��Cl�Cm
Ck��Cj��CjV�Ch$�Ce"�CaS�C[!CT�CM��CF�C?�7C;�C9��C6|jC6�?C6�C8T{C:�C;�'C>'�C?�RCBT�CE��CGR�CI��CK��CJCH��CJQhCM��CQ�CU�CW�;CX��C[dC^r�Cb�CdX�Cf�CjG�Cm�`Coi�Co�hCp�Cs BCtXCu�Cv��Cx�CzpbC{lCz�yC{�%C|�`C~��C}�hC{�PCz9�Czh1Cz��Cwp!3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                C��C��C�.�C�:�C��C�y�C��XC��C��bC��xC�ŭC�\9C��C�g�C~!/C}A0C}J�C{��Cz��Cx�nCy gCyLCz�C{�0C{�oC{�C{"mCzP/CzVfCzd�Cy�Cx�yCy��C|m\C~H C�C��C�%�C�0�C~��C~�C~��C~fC~5C~*!C~<�C}j�C}}5C}��C|��C|БC{&�Czm�Cy�WCw2�Cs��Con�Ch\�CaB�CY=�CQ.�CI�CD�jCA�tC>maC>��C>�0C@;cCB>CC��CF��CHg�CK(oCNưCP��CSnCUMbCS�kCR,uCT�CW�C\>�C_�&Cb�Cc��Cf�\Cj6�Cn�*Cp��Cs�>CwE�Cz�C|�PC}dC~(mC��%C�	�C���C���C��C��]C��C���C�D�C���C��jC�u%C�0kC�]<G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@0��@0��@0�}@0�@2t@1?�@.�)@,�	@,*�@+
�@)��@)k�@'0�@$[�@"��@"=@"W@ �\@ l	@Q�@\k@f�@ �@!.�@!9=@ �&@ ��@ 3Q@ 7L@ @\@��@+�@ǥ@!��@"�q@#��@#�e@$!@$�@"�x@#�@#�@"�@"�6@"�S@"�B@"/�@";�@"F�@!�@!�*@ ��@ FC@ϔ@5@`@<�@�W@*�@�@�R@ �1?���?�C$?�4?��e?���?��?�\x?��	?�#_@ BC@f@V3@�`@P�@�r@|m@��@�@
�@��@I�@|@�3@��@�@ց@�@��@A*@ ��@!�3@!�J@"�<@$|X@%+@%�@'$�@(g�@)�%@*Y@)�[@*�?@+Kh@,�s@,@*{j@)mG�O�G�O�G�O�