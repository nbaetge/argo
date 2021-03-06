CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:48:25Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20201028144825  20201028144825  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�-���-�1   @�-�d� @G��E���2��"��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014234 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559282015101515592820151015155928                                                        20161116115900A   B   B   B       ?�  @�  @�  @�  @�  A  A0  A`  Ap  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CX  Ca  Ck  Cv  C  C�� C�� C�� C�� C�  C�  C�  C�  C�� C�� C�� C�� C�� Cŀ C�  C�  C�  Cـ Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8@ D>� DE  DK@ DQ� DW� D^  Dd@ Dj@ Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @�  @�  @�  @�  A  A0  A`  Ap  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CX  Ca  Ck  Cv  C  C�� C�� C�� C�� C�  C�  C�  C�  C�� C�� C�� C�� C�� Cŀ C�  C�  C�  Cـ Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8@ D>� DE  DK@ DQ� DW� D^  Dd@ Dj@ Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AD�AD�ADbAD{ADbAD{AD$�AD$�AD1'AD1'AD1'AD$�AD$�AD9XAD1'AD$�ADE�AD�/ADr�ADJAC��AC+ABE�A?�mA>ȴA>�A=�A<��A<^5A;�mA;�A:�yA: �A8��A7�;A7�PA5�mA4ZA3��A5"�A41A3�A0��A.��A.��A0-A.�/A-�PA,$�A)dZA'�wA&�A&��A&n�A$��A#C�A!C�A VA��At�AdZA�yA�uA
�jAp�A	&�A�A�hA�@�v�@�-@�^5@��@�$�@���@؛�@ԣ�@�1@�^5@�bN@�~�@���@��m@�%@�X@�1'@��@��@���@���@�o@���@�ƨ@�hs@���@�J@�b@�l�@���@�  @|I�@z^5@x��@u�@t(�@s�F@up�@s33@o��@l�@h�9@g�@gK�@e�@c��@b^5@`r�@_��@^V@]��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�ADbADbAD{AD$�AD$�AD1'AD1'AD1'AD$�AD$�AD9XAD1'AD$�ADE�AD�/ADr�ADJAC��AC+ABE�A?�mA>ȴA>�A=�A<��A<^5A;�mA;�A:�yA: �A8��A7�;A7�PA5�mA4ZA3��A5"�A41A3�A0��A.��A.��A0-A.�/A-�PA,$�A)dZA'�wA&�A&��A&n�A$��A#C�A!C�A VA��At�AdZA�yA�uA
�jAp�A	&�A�A�hA�@�v�@�-@�^5@��@�$�@���@؛�@ԣ�@�1@�^5@�bN@�~�@���@��m@�%@�X@�1'@��@��@���@���@�o@���@�ƨ@�hs@���@�J@�b@�l�@���@�  @|I�@z^5@x��@u�@t(�@s�F@up�@s33@o��@l�@h�9@g�@gK�@e�@c��@b^5@`r�@_��@^V@]��444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBZBYBZB\)B[#B\)B\)B\)B^5B^5B]/B\)B\)B\)B\)BZB]/BjBgmBdZB`BB[#BQ�B=qB49B/B+B%�B"�B�B�B�BuB+B��B��B�B�B�B�B�TB�B�XB��B��BÖB�?B�B��B{�BjBjBn�Bp�BaHBW
BH�BI�BE�B9XB#�B��B��B�fB��B�B8RBF�BM�BI�B-B�B�B�yB�B�B�mB��B�B��BgmB#�B,B&�B�B+B��B�B�B��BĜBȴB��BɺBÖB�qB�LB�RB�9B�B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��333311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�Bi�Bi�Bj�Bj�Bj�Bl�Bl�Bk�Bj�Bj�Bj�Bj�Bh�Bk�ByEBv3Bs BoBi�B`�BL3BB�B=�B9�B4�B1�B.zB+eB'LB"-B�B	�B�B�4B��B�B�HB�B��B�
B��B��B�FB��B��B�\B��By%By#B}:BLBo�Be�BWSBXZBTCBG�B2rBiB�eB��B�B-QBF�BUFB\oBXSB;�B'(B&B�B�B�0B��B�B��B� Bu�B2IB:}B5ZB*B�B:B�B��B�VB�B�B�VB�B��B��BŮBƳBB�tB�UB�HB�GB�'B�/B�:B�gB�RB�4B�B��B��B�B� B�B��B��B��B��B��444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CxaCw�vCw��Cw�Cw��Cw��Cw��Cw�8Cw��Cw��CwƚCw�Cw�Cx CwN�Cwe�Cv�xCvCuP�Cuc%Ct��Cs4[Cn��Cj�NCi86CiL�Cj'HCk�Ch�/Ce�dCcdYCa�C^��C^�C]_dCY�WCW��CX�C]�Cbn6C_Y C[{zCTt�CS�yCV�wCW�^CRWCO<#CL�CKhCKuXCK��CJ��CHv�CD��CAsC>S|C;�tC9�C8-�C3�{C.ԃC.jC.+�C/	_C/��C2iC4�C6�NC8��C<B�C?�xCB�CF�CH�CJJ�CL�COTPCSj�CV��C\nCb�CbC$Cd�Cf�}Ci�Ck��Cl�gCnf�CqÉCs�wCr�rCrNCsHKCtC�CvSCv<Cvk�Cwg�Cxd�Cy`�Cy��Cx��Cy�!Cz�Cy��Cx�QCy_Cz+C{�C|?C{k8C{�VC{ʝC{� C|*�C|Y�C|�C|��C|P333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                C�E9C��3C��C��C��\C��	C���C���C��VC��XC���C��C�C��C�ʛC��:C���C�2#C��C��lC��?C� nC{�`Cx��Cwg�Cw�xCxM�CysCw�Ctj�Crm�Cpn�Cnr�Cn�CmNCi�Ch��Ch�Cm�6Cq�CoE�Ck��Ce�VCeECh#gCh�*CdfCa`�C_k�C^<�C^a�C^x�C]�C[�_CXs�CU��CS"@CQ -CO�CM�bCJ	�CFHRCFwCF3�CGN�CG��CJ0CK�eCN�CPxYCS��CV��CY"uC]�kC_rCCaCb�)Ce�wCi�qCl�<CrP�Cw��CxqCy��C|zC~�?C�u�C��C��'C�X#C�1�C��LC���C�3C���C�h�C��"C��!C��C���C��C�!�C��6C�V�C�oC�*�C��C��nC�s3C���C�fC� �C�7�C�QC�i�C��{C�� G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@�r@x�@|�@}}@&@�f@��@��@��@�@��@��@�o@�b@F�@U�@�$@q�@ �@A@�]@��@��@8�@B�@O�@۳@g�@�f@��@��@}@��@�y@��@5�@
=c@
G	@�@�`@�\@w�@��@�@	�a@
�@� @�^@'�@.@6�@>�@Ž@ K�?��%?��t?�?�$?���?뿉?�ɓ?��V?�ג?��?��?�6�?�]�?怶?顬?��%?���?�' ?�V�?��E@ \�@w�@��@��@N�@	m(@=@��@��@�f@�@/�@�P@pB@��@��@�u@v�@>@�c@TV@zs@�@�u@V�@��@�@��@R�@��@ @��@I*@gG@ 	�@ � @!N�@ �v@!A@!%�@!C�@!b�@!� G�O�G�O�G�O�