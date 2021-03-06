CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   y   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:47:36Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                  |  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  Sd   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  UH   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  Y�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Z   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  ]�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^L   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `0   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  b   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  b�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  dt   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  fX   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  j�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  k   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  n�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  o\Argo synthetic profile          1.0 1.2 19500101000000  20201028144736  20201028144736  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @����I�1   @��9�H @HP�`A�7�2�\(�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014256 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559272015101515592720151015155927                                                        20161116115859A   B   B   B       @   @�  @�  @�  @�  A  A0  AP  Ap  A�  A�  A�  B  B4  Bd  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CW  Ca  Ck  Cv  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�  C�� C�  Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�  D @ D� D� D  D@ D� D%� D,  D2� D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}� D�� D�� D�  D�  D�  D�` D�� D�� D�� D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       @   @�  @�  @�  @�  A  A0  AP  Ap  A�  A�  A�  B  B4  Bd  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CW  Ca  Ck  Cv  C  C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�  C�� C�  Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�  D @ D� D� D  D@ D� D%� D,  D2� D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}� D�� D�� D�  D�  D�  D�` D�� D�� D�� D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��ADjADjADr�ADr�ADn�ADv�ADr�ADr�ADr�ADv�ADv�ADz�AD~�AD�+ADJAC��ACx�AC�ACC�AC33AC7LAC33AC7LAC7LAC+ACoACoAB�AB �A@ȴA?�TA?`BA=�FA<1'A;\)A:bA8�A7hsA6  A3�TA1��A/�#A-K�A-+A.1A.ZA.^5A.n�A1�A2�A2�A2ffA1��A1;dA1VA0^5A/��A.�A-�;A-7LA,n�A*jA&��A"�A/A��A?}A�A7LA��AS�A�A�RA`BAV@��@�bN@ى7@���@�E�@��@�{@�b@�Ĝ@��@���@���@��@�K�@���@�Q�@�~�@��H@�ƨ@�p�@���@�@��H@�A�@��@�Z@|��@{"�@y�@z-@x  @u��@t(�@t1@p1'@n�R@m�@j��@iG�@fff@c�m@bJ@a&�@`bN@`��@`bN3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�ADn�ADn�ADv�ADr�ADr�ADr�ADv�ADv�ADz�AD~�AD�+ADJAC��ACx�AC�ACC�AC33AC7LAC33AC7LAC7LAC+ACoACoAB�AB �A@ȴA?�TA?`BA=�FA<1'A;\)A:bA8�A7hsA6  A3�TA1��A/�#A-K�A-+A.1A.ZA.^5A.n�A1�A2�A2�A2ffA1��A1;dA1VA0^5A/��A.�A-�;A-7LA,n�A*jA&��A"�A/A��A?}A�A7LA��AS�A�A�RA`BAV@��@�bN@ى7@���@�E�@��@�{@�b@�Ĝ@��@���@���@��@�K�@���@�Q�@�~�@��H@�ƨ@�p�@���@�@��H@�A�@��@�Z@|��@{"�@y�@z-@x  @u��@t(�@t1@p1'@n�R@m�@j��@iG�@fff@c�m@bJ@a&�@`bN@`��@`bN4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB^5B^5B]/B^5B^5B^5B^5B^5B^5B]/B^5B^5B]/B^5B[#BYBYBYBXBXBXBXBXBXBW
BW
BW
BT�BO�BD�B;dB5?B'�B�BhBB��B�B�5BǮB�!B��B�7B��B�B�FB�XBĜB��B�B!�B�B�B�B�B�BhBDBBB��B�B��B�?Bn�BT�B49B�B��B1'B!�B;dBq�B�B�1B�B��B��B��BÖB�B�dB�XB�B�bBv�BZBI�B5?B&�B�BuB%B��B�B�B��BÖB�^B�9B�B��B��B��B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��3333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�Bl�Bl�Bl�Bl�Bl�Bl�Bk�Bl�Bl�Bk�Bl�Bi�Bg�Bg�Bg�Bf�Bf�Bf�Bf�Bf�Bf�Be�Be�Be�Bc�B^�BS\BJ#BC�B6�B&HB #B�B�B�EB��B�\B��B�dB��B�+B��B��B�B�MB�B,jB0�B.xB*_B)\B(UB$<B #B�B�B�B
�B�MB��B��B}:Bc�BB�B,LB�B?�B0^BI�B�KB��B��B$B�OB�BPB�B��B��B��B��B��B�CBh�BX,BC�B5[B(B!�B�BGB�B�~B�IB��BȾBB�vB�GB�AB�/B�aB�[B�WB�QB�gB�:B�<B�<B�#B�#B�B��B��B��B��B��B�4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cs��Cs�UCs�Cs�KCs�gCs��Cs�=CsζCs�/Cs�9Cs�ACs�Cs��Cs=oCsQDCr��Cr�eCr��CrݜCr��Cs	�Cs�Cs6CsLCCr�/Cr��CpnnCl��Cfe�C`�{C_x�C]/CZ�CY_*CYq<CY~bCZVCZd�CX��CW\�CTD&CPb�CQ4�CS�lCTxCUSoCV1	CX�iC^�WCalCcgCc�zCc��CcEsCaĵCa.C`Q
C_�"C^�1C]Y�C[�CU��CM�5CB�C5�"C2l�C/Q�C0-�C1�C2�	C4aC5HbC5i�C7� C:`�CA��CFjCH��CI�FCPN�CR�CS��CT�gCW<�CZ�%C]�C`k�Cb*�Ce��CgD�Ci�CjCl�[Cm��CoR�Cq�Cs��CutfCw=/Cx8ACy5RCz2<CzbFCz�[Cy$�CySeCy��Cy�uCy�Cz�Cz?Czo4C{n�C{�$C|�C|ͷC|��C}-�C}^�C{�C|q3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       C�1�C�3�C�5�C�6�C�7
C�7�C�:
C�<C�> C�@C�A�C�D�C�I�C�C�#Cy\C��C�BC��C��CڐC�JC� �C�
�Cx�C��C}��Cz+�Ct�:Co�Cn��Cl�Cj�!Ci��Ci��Ci�>Cj��Cj�ACipzCh71Ce�uCbE�Cc&<CeJ(Ce��Cf��Cg}3Ci�hCo;Cq&�Cr��Csf`Cs�Cr�sCq�pCp�FCp\�Co��Co,	CmߵCl�*CgDcC`��CWMCK��CI7�CF�CGڭCH��CJc%CL#�CL�UCL�;CO9/CQ�#CX�UC]�C_όC`��Cf�0Ci4=Cj�Cj��CmV(Cp�7Cr��Cv0%Cw�YC{jC|��C~n�C\�C���C�`YC�4�C�o�C�F^C��C�� C�p�C��mC�c*C�|xC���C��C��C�&�C�>�C��jC�rqC��C��MC�CC�3�C���C���C��C��jG�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444   @��@�_@��@ �@-@�@�@	j@G@�@@u@�@�p@�!@H�@St@a�@o@~l@�K@�l@��@��@C�@S@�F@k�@tZ@�0@r@��@�@/@)�@2*@�;@ť@
�@	��@ٲ@]�@�p@o�@��@�Q@	$@
��@��@E2@Su@ߪ@��@t!@}�@(@�@h@�g@�=@�@Ů@��?���?�X}?�bx?�h�?�]?⟜?��r?��?�
�?�5M?�d ??��I?��j@ �@.�@Q(@��@�4@�@	�S@�@~@��@�H@�@%@$A@��@h�@.@*�@�@�@L@;�@�S@~I@  &@ >�@ ]�@s�@��@��@ͼ@g�@ 	�@ (c@ G,@ �@!	@!��@!�V@!�@"�G�O�G�O�G�O�