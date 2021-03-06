CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   w   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:49:19Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20201028144919  20201028144919  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                                A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�<�����1   @�<��?`@H�1&�x��3U\(�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014291 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559292015101515592920151015155929                                                        20161116115901A   B   B   B       ?�  @@  @�  @�  @�  A  A@  A�  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CW  Ca  Ck  Cu  C�  C�� C�� C�  C�  C�  C�  C�� C�� C�  C�  C�� C�� C�� Cŀ Cʀ Cπ CՀ Cـ Cހ C�  C� C� C� C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D?  DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�  D�` D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @@  @�  @�  @�  A  A@  A�  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CW  Ca  Ck  Cu  C�  C�� C�� C�  C�  C�  C�  C�� C�� C�  C�  C�� C�� C�� Cŀ Cʀ Cπ CՀ Cـ Cހ C�  C� C� C� C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D?  DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�  D�` D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AF$�AF(�AF-AF-AF1'AF-AF$�AE��ADVADE�AD�AC��AC�AC�7AC?}AC+AC�AC7LAC;dAC&�AB�/ABz�AA�mAAp�AA�A@�`A@5?A?�7A?XA?S�A?S�A?+A>$�A=�hA=�A=dZA=33A=/A<�A<r�A<(�A<(�A<VA<�A<�A<JA<bA;�7A9C�A8ZA7��A6��A61'A5�-A4��A49XA41A4VA3S�A2 �A2A1ƨA/ƨA.ZA,�`A*I�A(A�A%�A!;dA�A��A�AE�A �A�AVAVA�jAv�A �y@���@旍@�"�@�ƨ@ě�@���@�S�@��@�ff@�b@�l�@���@��w@��/@��@���@�9X@��@��
@��#@�I�@���@�I�@�~�@�/@~ȴ@{�
@zM�@xA�@vff@u�h@t�D@s"�@q��@o�w@n5?@m?}@kƨ@jJ31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�AF1'AF1'AF-AF$�AE��ADVADE�AD�AC��AC�AC�7AC?}AC+AC�AC7LAC;dAC&�AB�/ABz�AA�mAAp�AA�A@�`A@5?A?�7A?XA?S�A?S�A?+A>$�A=�hA=�A=dZA=33A=/A<�A<r�A<(�A<(�A<VA<�A<�A<JA<bA;�7A9C�A8ZA7��A6��A61'A5�-A4��A49XA41A4VA3S�A2 �A2A1ƨA/ƨA.ZA,�`A*I�A(A�A%�A!;dA�A��A�AE�A �A�AVAVA�jAv�A �y@���@旍@�"�@�ƨ@ě�@���@�S�@��@�ff@�b@�l�@���@��w@��/@��@���@�9X@��@��
@��#@�I�@���@�I�@�~�@�/@~ȴ@{�
@zM�@xA�@vff@u�h@t�D@s"�@q��@o�w@n5?@m?}@kƨ@jJ44481111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�+B�%B�1B�+B�1B�%B�1B�+B�%B�+B�%B�%B�+B�B�B�B�B�+B�+B�%B�B{�Bv�Bv�Bt�Bl�Be`BdZBdZBcTBbNBXBS�BR�BP�BN�BQ�BW
BS�BT�BZB^5Bk�Bl�BbNBcTB[#B>wB49B/B �B�B�BbBJB{B�BhBBuB"�B�BhBJB��B��B��B�B�B�VB�^B�`BiyB�XB��B�qBs�B��B��B;dB"�B��BǮB��B�PBJ�B(�B�B�B�B	7BB��B�B�HB�
B��BȴBB�jB�RB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��33331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B{QBt%BsBsBrBqBf�Bb�Ba�B_�B]�B`�Be�Bb�Bc�Bh�Bl�BzIB{SBqBrBi�BM6BB�B=�B/B-tB)[BBB#4B-uB B�B"0B1�B%>B  B B�B�B|B��B��B��B�B�Bx6B�B�IB�8B�tBއB�{BI�B1dB[B�3B�B��BY5B7hB(B#�B(B�BqBBB��B�B�pB�CB�B��B��BƳB��B�kB�QB�2B�B�B�B��B��B�B�B�B�B�	B�B��B��44481111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C{z�C|D�C|I�C{�TC{�8C{��C{�6Cz�>Cz��Cz�Cz��Cz*Cy`bCw��Cw�CwN�Cv�Cu�UCu7"Ct��Cs�XCrU�Cq�Cq��Cq�Cr��CuVCu��Cv�CuTwCuk5Cs�Cs8�CsK�Cr��Cr��Cq��CqH[Co�rCoڝCo�Cq��Cs:QCt��CsfxCr�kCq0tCl�WCj;�CkCnR�Co3QCnw�Cm��ClACh]aCaO�Cb,�Cg�;CfS�CY�CRڨCO~CJkCE˟C>�oC8� C4�sC/�7C/1RC/E/C.�KC.��C,h�C,��C-g�C-��C0��C6i6C;HdC@�wCE	�CI�+CN�CS��CV8�C[��C`	LCcbsCdXCeQZCg޸Ci��Ckg�Cm*Cn�Cp��Cq�uCr�Cs��Ct�Cu��Cwp�Cxm�Cx�}Cy�=Cy�GCzǫCz��C{&�Cz�!Cy�8Cz�CzC�CzrPCz��Cz��C{�C{/<33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             C��KC��C���C��C���C���C��7C�I�C�R�C�TC�V\C�uC��C�5C��C��C���C�1oC��bC��C�FkCBC~�nC~�GC~�.C�SC���C�PVC�Z$C�IC�dC�n/C�$�C�1bC�C�%C=�C~��C}NC}e�C}z�C~�EC�1�C��C�A-C�1C~��Cz��Cx�}Cyi�C|NWC}&�C|�C{�Cz��Cw;�Cp�8Cq��Cv��Cu�Cj{PCdPzCacC]	�CYOCR�CM��CJA�CF[�CF@CFt,CEνCEߑCC{CCc�CDtFCD�,CG� CM�vCR])CX0C[�C`��CeU�CjClq-Cq�eCu�Cy6Cz �Cz�C}R�C~��C�U|C�-%C�OC���C�U�C���C�G�C���C�7�C�	C���C��C��C�6nC���C��C��cC���C�UcC�m�C��.C���C��gG�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @ �@!s�@!w@ ��@ ��@ ��@! #@ ��@ �1@ ��@ �r@ D@��@��@��@F�@�@b"@�@~@@@�(@�O@�
@M�@ۼ@h�@u@�@j@@�K@��@Dn@Q@�@k�@uh@��@��@��@�q@�\@Ƴ@P�@\s@b�@��@u�@�@�@��@)|@3�@��@3@�s@Xx@h�@i@�W@�Q@�4?�-�?�-[?�,�?�6V?�6	?�?!?�X�?�t�?ߕ�?ܮ�?���?��X?��?�PR?�|n?��?��?�5O@7&@W�@y�@	�@��@b@��@#�@�m@e�@��@��@��@�@V@�6@R�@�@��@7_@\�@��@y@��@۳@ �@ �h@ ��@ V|@��@ D@ +A@ I)@ g�G�O�G�O�G�O�