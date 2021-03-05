CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:02:42Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  S0   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  T�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U\   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  X�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  YD   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [    TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  \�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ],   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  `�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  b�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  d�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  d�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  ht   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  h�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  j�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  l\   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030040242  20201030040242  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @�׋UUUU1   @�׎+<@@H�$�/�2["��`@1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013174 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161458201510161614582015101616145820130604092405                                          20170502151424A   A   B   B       @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  CՀ C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D� D� D%� D,  D2@ D8� D?@ DE  DK@ DQ� DW� D^  Dd� Dj� Dq  Dw� D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�` D�` Dƀ Dɠ D�� D�� D�  D�@ D�` D܀ D߀ D� D�� D�� D�  D�@ D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  CՀ C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D� D� D%� D,  D2@ D8� D?@ DE  DK@ DQ� DW� D^  Dd� Dj� Dq  Dw� D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�` D�` Dƀ Dɠ D�� D�� D�  D�@ D�` D܀ D߀ D� D�� D�� D�  D�@ D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AH�yAH��AH�yAD�ABI�AB��AAXAA\)ABVAD  AC`BAA;dA@bA?oA>�A>(�A=7LA<�A;VA9�#A8�`A8ZA8r�A9��A8M�A7��A8��A:�\A<�DA=�A<�/A;O�A:�RA:�A:�!A9�mA8VA7�TA7��A7/A6�A5�A3�A0��A.�+A,��A*ȴA(��A'��A&bNA&E�A%��A!x�A�A(�A�A�RAp�A�A�A��Al�A ��@�?}A�A��A��A ��@��;@��y@�M�@��@�@��@�l�@ǝ�@���@�{@���@�o@� �@�E�@�n�@���@�ff@���@��/@�b@�/@���@��@�K�@�X@|j@z�\@zJ@v��@tz�@r~�@p�`@o\)@m�@k��@j�H@j~�@h��@g��@f5?@dj@c��@cS�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             AH�yAH��AH�yAD�ABI�AB��AAXAA\)ABVAD  AC`BAA;dA@bA?oA>�A>(�A=7LA<�A;VA9�#A8�`A8ZA8r�A9��A8M�A7��A8��A:�\A<�DA=�A<�/A;O�A:�RA:�A:�!A9�mA8VA7�TA7��A7/A6�A5�A3�A0��A.�+A,��A*ȴA(��A'��A&bNA&E�A%��A!x�A�A(�A�A�RAp�A�A�A��Al�A ��@�?}A�A��A��A ��@��;@��y@�M�@��@�@��@�l�@ǝ�@���@�{@���@�o@� �@�E�@�n�@���@�ff@���@��/@�b@�/@���@��@�K�@�X@|j@z�\@zJ@v��@tz�@r~�@p�`@o\)@m�@k��@j�H@j~�@h��@g��@f5?@dj@c��@cS�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�hBXBH�BW
BA�BB�BVBn�BffBG�B8RB,B+B �BuBB��B�`B�B��B�B�B�BB�#B�BVB33B;dB8RB"�B�B�B�BuB  B��BBBB�B�B�XB��B�{B�Br�Be`B]/BaHBiyBH�B%�B�B�)BE�B�FB�JB�uB��B{B��BPB�jB�B��B��B�hB�\BgmB�=B+B��B�HB�qB��B��BŢB�oBr�BZB6FB!�B�B	7B�B�B��BĜB�}B�XB�9B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�B�Be�BV[Bd�BO-BP1Bc�B|@BtBUSBE�B9�B8�B.eB!B�B]B��B�B߄B�B� B��B�B�5B�B@�BIBE�B0qB*NB+PB,WB!B�B�B�B�B�B JB�B��B�nB�B��B�<Br�Bj�Bn�BwBV;B3eB�'B�BS&B��B��B� B�HB!�BOB�B��B�B�GB�LB��B��Bt�B��B8|B=B�B��B�dB�`B�B��B�Bg~BC�B/&B%�B�B��B�aB�B��B��BƥB��B�3B�%B�1B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C��G�O�C���C�s�C|��Cy��Cx�Cx?�Cw�Cv�XCv$ZCt�Cs��Ct �Ct�Ct&fCuHCu�Cu�Cv BCv-Cu\�Ct�Ct�Cu��Cu��Cu��Cu��Cu��Cu)7Cu<�Ct��Cs͑Cs��Cq��CoM�Cm �CdgmCf�uCj��CiQhCcܬC_�RCY��CWa�CTF�CR�jCPi7CN�CN#TCN1�CK�1CE�C?qhC;�C6�5C7��C;�C8ÖC8�C8�=C:��C=�C?nVC;��C>'mCAjCC�CDФCGMPCJ��CK�fCQ�jCUJCW��CZVFC\'C[i�C^�}Cb��Cf33Ch��Cl�Cn�Co��Cr)yCvT{Cy��Cz�C{��C|�C}��C~��C�1C�H�C�`�C���C���C��C�'+C�>�C�U�C�C��C�˦C��TC���C��#C��hC��XC��33 3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333     G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                    C���G�O�C���C��[C�q'C��C�M-C��#C��}C�#pC��C�ۣC�p�C�x)C���C��C��C�\C���C��}C���C�*eC�ªC���C�H�C�R�C�`C�o�C��NC�hC�%�C��C�OxC�ZC�oC�C|��Cs�CuթCzZ�Cx��CrnCm�hCf��Cd�C`��C^��C\^CZO�CYq!CY��CV�CO��CH��CC�hC>��C?�iCC�7C@�vC?�C@�NCB|�CE#�CG��CC�tCF�CJ2CLOCM�CP��CTSCUgC\,C`Cb�YCe��Cg��Cf��Cj��Co�Cr�]Cu��Cy@�C|�C}�C�C�PC�2�C��gC�A�C���C�Y'C��C���C���C��C��vC���C�ѹC��C�\C��C���C�٫C�~IC���C���C�UMG�O�G�O�G�O�1 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @5G�O�@5Q`@1�@-]�@+)@*�:@*�@)�@)#@(��@'�@&�	@' 4@'�@'v@'��@'��@(W�@(`�@(j@'�X@'_�@'s@(@($@()�@(<�@(RO@'�)@'�r@'SO@&�@&ٱ@%12@#��@!��@�=@U�@ :&@!�@'�@Ll@�]@��@��@��@��@�>@)�@3�@	��@�Y@ Z=?�ܮ?��)?�MS?�V	?���?���?���?���?�V�?���?��?�E1@g�@��@��@��@�E@��@1@n@7�@�@1(@�7@!@�@a�@,@��@!V?@!�D@#�&@&��@)6�@)��@*�d@+E@+�<@,��@-�v@.T@.-�@.�&@/ �@/ �@/B�@/c@/�n@/	�@/+@.� @.֦@.��@.��G�O�G�O�G�O�