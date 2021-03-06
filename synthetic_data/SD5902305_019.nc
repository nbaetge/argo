CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T03:54:54Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030035454  20201030035454  5902305 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-012                9,02                            841 @��Kwwww1   @��M�'�@ID9XbN�2I��l�@1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            A= 0.985; B= 21.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A=1.166; B=-7.060                                                                                                                                                                                                                                               No significant pressure drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                   No significant temperature drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                Method OW : no significant drift or offset - Error = maximum [ statistical uncertainty, 0.01] - Method 127 see report : bilan_5902305.pdf on http://www.ifremer.fr/lpo/ovide/data/argo_profiling_floats.htm                                                     Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20130605144540201306051445412013060514454020130604093724                                          20170427114127A   A   B   B   ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Cـ C�  C�  C�  C�  C�  C�� D @ D� D� D  D� D� D&  D,@ D2@ D8� D>� DE� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�` D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�` D�@ D܀ D߀ D� D�� D�� D�  D�  D�@ D�` D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Cـ C�  C�  C�  C�  C�  C�� D @ D� D� D  D� D� D&  D,@ D2@ D8� D>� DE� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�` D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�` D�@ D܀ D߀ D� D�� D�� D�  D�  D�@ D�` D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AH9XAH=qAHVAH^5AHbNAHbNAHjAHr�AHz�AH~�AH~�AH�+AH�\AH��AH�9AH�9AHQ�AHA�AG�#AG�PAG�PAG�7AGdZAFM�AD{AA%A<�jA:��A9dZA8�RA8A7dZA7�TA6��A5
=A3�TA1��A0=qA/��A.�\A,�yA+\)A)�A'S�A%�#A%p�A$�A$$�A"ȴA ��A�TA�AA�A�/AA  AA �@��/@���@�~�@��@�I�@��@�C�@�j@ץ�@�X@ӍP@�t�@Ϯ@őh@�ȴ@��+@�I�@��@���@��@��T@��@�b@�hs@�bN@�v�@��@�?}@�X@�l�@�?}@���@�33@�7L@�S�@�v�@~�R@x�`@v$�@tZ@st�@p  @n�@l�@j�@iX@g|�@f$�@d�j@cS�@a��@`bN@^ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             AH9XAH=qAHVAH^5AHbNAHbNAHjAHr�AHz�AH~�AH~�AH�+AH�\AH��AH�9AH�9AHQ�AHA�AG�#AG�PAG�PAG�7AGdZAFM�AD{AA%A<�jA:��A9dZA8�RA8A7dZA7�TA6��A5
=A3�TA1��A0=qA/��A.�\A,�yA+\)A)�A'S�A%�#A%p�A$�A$$�A"ȴA ��A�TA�AA�A�/AA  AA �@��/@���@�~�@��@�I�@��@�C�@�j@ץ�@�X@ӍP@�t�@Ϯ@őh@�ȴ@��+@�I�@��@���@��@��T@��@�b@�hs@�bN@�v�@��@�?}@�X@�l�@�?}@���@�33@�7L@�S�@�v�@~�R@x�`@v$�@tZ@st�@p  @n�@l�@j�@iX@g|�@f$�@d�j@cS�@a��@`bN@^ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�bB�\B�VB�%Br�BS�B+B�BDBB��B��B
=B  B�B�BBɺB�jB�RB�B��B�\B~�BcTB^5BaHB_;B[#BR�BI�BB�B.BN�B"�BoB�B�jB��B�7B�By�Bs�B�1B�hB��B�TB�XB�?B�/B�B�B�XB��B��B��B��B�1BdZBN�BC�B9XB0!B�B�BVBB�B�B�NB�TB�;B�B��B��BĜB�LB�3B�'B�-B�B�B�B�B��B��B��B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�bB�\B�VB�%Br�BS�B+B�BDBB��B��B
=B  B�B�BBɺB�jB�RB�B��B�\B~�BcTB^5BaHB_;B[#BR�BI�BB�B.BN�B"�BoB�B�jB��B�7B�By�Bs�B�1B�hB��B�TB�XB�?B�/B�B�B�XB��B��B��B��B�1BdZBN�BC�B9XB0!B�B�BVBB�B�B�NB�TB�;B�B��B��BĜB�LB�3B�'B�-B�B�B�B�B��B��B��B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Co6�Co@BCn�Cn��Cn�{Cn��CnۦCn�hCoTCo�CnbNCnvCn��Cm��Cm�sCm3uCmF%CmX�Cn4{Cm~�Cl�7Cj��Ce� C`o�C\��CYr�CW�CW�CW5�CWECX/CV��CTL�CR�mCRDCR��CT��CT��CSӶCQ��CM�}CJ��CE׍CB��CC��CC��CB�dCAT{C?�C=t9C;"�C8�C4*C2��C2�qC2�oC3��C6�C9R-C=V�C@�RCC�CFQ�CH�CI��CI�uCN�3CP`�CP�CQoCT��CX�FCZ��C[}C\oC^��CbN�Ce�Cg[�Ci/Cj߾Cl��Co0bCoZCq BCrFCsٚCt��Cu�Cu��Cv��Cw��Cx#Cy�Cz�BC{��C|��C}1C}7
C}eC}��C}��C}�C~^C~G�C~v�C~�`C~�9C�C0�C~��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                             C�W�C�]�C��{C�C��C��C�#�C�0XC�:�C�FDC��C��C��;C���C���C�-C�6�C�AqC��DC�UDC��nC/Cy�dCsg�Cn�@CkCCi3�Ci3�ChPVCh^CiUnCg�vCd��Cc�Cb-�Cc!"Cd��Cd�1CdeCac�C\�|CY%CS��CO�5CP��CP�rCP�CN;�CLgCI��CF��CC1;C>�C<�C<�aC<�tC=z�C@/tCC��CHW!CLqCO�^CR�|CUh[CVq�CV�RC\9+C^-C^FXC_[DCc(�Cg��Ci�Cj´Ck�ECn�^Cr�?CvD�Cx06Cz/C|0�C~2�C���C���C��}C�,�C�+CC��C�F�C�^�C���C�{\C��tC�!�C�$�C��pC�@�C�[C�umC��cC��xC���C��hC��C��C� �C�:�C�T3G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @&��@&�L@&X�@&g�@&v�@&�@&��@&�H@&��@&�W@&9�@&H|@&WV@%В@%߅@%Xm@%d�@%r|@&�@%��@%d@#QT@�!@ǘ@�{@zh@?�@?�@�L@�@Ue@0�@| @T�@�@\�@�U@��@��@?�@P�@
�@u
@�@��@� @$H@�f@�[@
�?���?�؝?�j?�?�?��?�?��M?���@ 7�@��@��@�&@��@	>�@	eS@�c@"@A�@�@a�@U@�L@?,@�U@�(@73@��@�-@ &@!f�@"��@$��@$��@%�D@&��@'�t@(�:@)P@)n�@*&@*�W@*�W@+�2@,�@-��@.gs@.�@.��@.�-@.� @/@/%�@/E�@/e@/�(@/�@/��G�O�G�O�G�O�