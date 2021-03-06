CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:04:11Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030040411  20201030040411  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               &A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @����l�1   @���D�� @G�r� Ĝ�-b��`A�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013218 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161500201510161615002015101616150020130604092426                                          20170502151425A   A   B   B       @�  Ap  A�  B  B8  Bd  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�� C�  C�  C�  CՀ C�  C�  C�  C�  C�  C�  C�  D @ D� D  D  D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DW� D^@ Dd� Dj� Dp� Dw  D}� D�� D�  D�  D�` D�@ D�` D�� D�� D�� D�  D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�` D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�� D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     @�  Ap  A�  B  B8  Bd  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�� C�  C�  C�  CՀ C�  C�  C�  C�  C�  C�  C�  D @ D� D  D  D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DW� D^@ Dd� Dj� Dp� Dw  D}� D�� D�  D�  D�` D�@ D�` D�� D�� D�� D�  D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�` D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�� D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Ak33Akp�Ak�PAk�7Ad�\AT(�ANI�AO/AN�RAMoAK�wAJv�AHI�AG�-AG�7AF�\AEt�AD(�ABJAA`BA@n�AB-AB�AC;dAC&�AA;dA>{A<��A;��A:�A:n�A9��A:9XA;O�A:$�A9�7A933A8ĜA8Q�A8$�A8A7�A7�#A7C�A6�A6bNA6(�A5��A5S�A5A4�9A41'A3O�A2r�A0��A/hsA-��A,bA)"�A&~�A#�hA33A  A��A�DA+A
=A��Al�A
��AK�A33A ��@�hs@�
=@�@�V@�ff@�V@��@җ�@�K�@��\@��9@���@��!@���@���@��T@�-@���@�X@���@��/@�\)@��^@�A�@��@��@~��@|I�@y�#@w�;@up�@t9X@rn�@q�@o
=@l��@k@jM�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             Ak33Akp�Ak�PAk�7Ad�\AT(�ANI�AO/AN�RAMoAK�wAJv�AHI�AG�-AG�7AF�\AEt�AD(�ABJAA`BA@n�AB-AB�AC;dAC&�AA;dA>{A<��A;��A:�A:n�A9��A:9XA;O�A:$�A9�7A933A8ĜA8Q�A8$�A8A7�A7�#A7C�A6�A6bNA6(�A5��A5S�A5A4�9A41'A3O�A2r�A0��A/hsA-��A,bA)"�A&~�A#�hA33A  A��A�DA+A
=A��Al�A
��AK�A33A ��@�hs@�
=@�@�V@�ff@�V@��@җ�@�K�@��\@��9@���@��!@���@���@��T@�-@���@�X@���@��/@�\)@��^@�A�@��@��@~��@|I�@y�#@w�;@up�@t9X@rn�@q�@o
=@l��@k@jM�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�\B��B��B��B��B�?BɺB�B�sB��BB�-B�{B�bB�oB�%By�BiyBQ�BM�BH�Bq�B�B�%B�%Bl�BF�BB�B49B=qB;dB2-BJ�BaHBXBS�BT�BVBW
BXBYB\)B^5BYBQ�BN�BK�BG�BE�BA�B?}B;dB49B0!B#�B{B%B��B�NB��B��B��B�wB�B�`B�`B�B�'B��B�3B��B{�Bn�BbNBT�Bz�BhsBR�BK�B1'BB�3B�PBs�BI�B7LB'�B{B%B��B�NB��B��BȴBÖB�}B�jB�RB�3B�!B�B�B��B��B��B��B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�B�kB�eB�:B��B�mB�<B�(B�B�CB��B�&B�B�B��B��Bw B_�B[wBVYBRB��B��B��Bz2BTJBP1BA�BKBIB?�BXfBn�Be�Ba�Bb�Bc�Bd�Be�Bf�Bi�Bk�Bf�B_�B\{BYiBUOBSFBO+BMBIBA�B=�B1tB"B�B
�B��B�yB�B�lB�B�B��B��B�#B��B�BB��B�7B�jB|Bo�Bb�B�fBu�B`mBYEB>�B�B��B��B�BWBD�B5IB!�B}B)B�B�MB�*B�B��B��BɴBŜB��B�nB�YB�NB�>B�2B�1B�,B�&B�B�B�B� 441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C|�}G�O�C|�Cy+Ct%�Cql�Cmf�Cl�sCl��Cl�7Cl�^Cl�DCl�#ClC�ClT�Clf%Ck��Ck�wCj;#Ci�NCi��Ci�}Ci��Ch��ChECf��Ce<�Cc�LCcŢCd��CgCh�+CjF�Ck!�Ck0!Ck@�ClCl��Cm�Cm��Cn�XCo�{Co�fCo�XCn�}CnG�CnZ�CnlCn}�Cn�bCn�NCm��Cl��CjZ�Cf�JCa��C\��CW�COSuCHKDCC�C=[�C:B�C8��C7<)C6�C6��C9�C:��C:��C>"�C?�CBW�CDФCGM�CH9�CJ�
CM9CO�;CRG+CW3�C\�yC`:�Cc��Cg�CjD�ClfCn�Cp[#Cs�^CvKDCx�Cy
Cz�C{C{��C|��C}(1C}Q�C~R�C~~5C~�yC~��CC3�CbC�'C�;C�PC}�7C{NV3 3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333     G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                    C���G�O�C�|�C��<C��wC�:ZC}�XC|�C}lC}�C}C}'C}-C|Y�C|lRC|z�C{��C{�)Cy�Cy�Cy!�Cy=#CyV0Cx�bCw��Cu�Ct,�Crm*Cru�Csa�CvjCw�CyϒCz�XCz�Cz�PC{�SC|�SC|�HC}��C~�\CǝC�|C�CsC~E�C~Y�C~jVC~|�C~��C~��C}��C|ACy�Cua�Cp"7Ci��Cd�kCZ�iCR�GCMz�CFBCB�eC@��C?7,C>M�C>[vCA�CB�fCB��CF��CHiCK0WCM�ECP�-CQ�CTolCW1�CZC\�/Cb?�Ch��Cl'�Co�Ctq�CwB�Cy1�C|}C}��C�� C�J�C�E�C��^C�X;C��yC�pC���C��C�(�C��oC�ͅC���C��&C��C�,�C�EeC�^�C�wG�O�G�O�G�O�1 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @-�G�O�@-l�@+7W@'��@%ii@"k@!�@!�@!�@!��@"�@"\@!�@!�@!�7@!�@!�@�C@j�@q�@�J@�R@?@�|@l*@E�@'%@,�@��@�w@��@�@ �@ �J@ �
@!0�@!�J@!֔@"s�@#@#��@#��@#�D@#B5@"��@"��@"�j@"�@"�@"�^@"x�@!qP@��@K@�@��@]@�@�B@��?��$?�=�?�&?���?�?�E?��?�e�?��1?�0&@ C6@
u@��@�'@4�@�d@	�H@��@N3@̔@�@#�@��@q�@?@|
@!Ly@"�&@$��@&��@(=@(�;@)f�@*4@*��@+��@+�c@+�@,o�@,�/@,��@,��@,��@-_@-%�@-F7@-e%G�O�G�O�G�O�