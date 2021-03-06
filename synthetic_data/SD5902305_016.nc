CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T03:54:30Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030035430  20201030035430  5902305 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-012                9,02                            841 @չ�wwww1   @չ�	+@@Hr-V�2��
=p�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL                                                                                                                                                                                                                                            DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            A= 0.985; B= 21.1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A=1.166; B=-7.060                                                                                                                                                                                                                                               No significant pressure drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                   No significant temperature drift detected - Calibration error is manufacturer specified accuracy                                                                                                                                                                Method OW : no significant drift or offset - Error = maximum [ statistical uncertainty, 0.01] - Method 127 see report : bilan_5902305.pdf on http://www.ifremer.fr/lpo/ovide/data/argo_profiling_floats.htm                                                     Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20130605144528201306051445282013060514452720130604093717                                          20170427114127A   A   B   B   ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Cހ C� C�  C�  C�  C�  D @ D� D  D@ D� D� D&  D,@ D2� D8� D?  DE� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�  D�` D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�  D�  D�  D�` D�` Dƀ Dɠ D�� D�� D�  D�` D�` D�` D߀ D� D�� D�  D�  D�  D�` D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  Cހ C� C�  C�  C�  C�  D @ D� D  D@ D� D� D&  D,@ D2� D8� D?  DE� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�  D�` D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�  D�  D�  D�` D�` Dƀ Dɠ D�� D�� D�  D�` D�` D�` D߀ D� D�� D�  D�  D�  D�` D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AW�-AW�^AW�-AW�
AW�;AW�;AW�TAW�mAW��AX1AXJAX{AX �AW�AVJAV��AVjAU\)AR�AN�!AKVAI/AEl�ACXA@=qA>bNA=oA;XA:{A9��A:�A7�A4ffA2�!A1��A/33A-+A*�9A)�^A'��A%�A$��A#|�A!��A�RAA�A7LA�^A�hAƨAn�A
~�A�A�9@���@�{@�bN@�@�$�@��m@ۍP@���@�9X@�7L@��@�
=@��
@�j@�n�@�M�@��@���@�J@��D@��
@�b@�O�@�"�@��j@�`B@���@�J@��m@�@��F@�@��D@��m@�~�@�Ĝ@~E�@z�@x1'@t��@q��@pb@n$�@l1@j�H@hĜ@f��@dZ@b�@a%@^v�@\Z@Z��@Y��@Wl�@U�-@T�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             AW�-AW�^AW�-AW�
AW�;AW�;AW�TAW�mAW��AX1AXJAX{AX �AW�AVJAV��AVjAU\)AR�AN�!AKVAI/AEl�ACXA@=qA>bNA=oA;XA:{A9��A:�A7�A4ffA2�!A1��A/33A-+A*�9A)�^A'��A%�A$��A#|�A!��A�RAA�A7LA�^A�hAƨAn�A
~�A�A�9@���@�{@�bN@�@�$�@��m@ۍP@���@�9X@�7L@��@�
=@��
@�j@�n�@�M�@��@���@�J@��D@��
@�b@�O�@�"�@��j@�`B@���@�J@��m@�@��F@�@��D@��m@�~�@�Ĝ@~E�@z�@x1'@t��@q��@pb@n$�@l1@j�H@hĜ@f��@dZ@b�@a%@^v�@\Z@Z��@Y��@Wl�@U�-@T�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1'B2-B2-B2-B2-B2-B2-B2-B2-B33B33B49B9XBx�B�B�B"�B�B�B��B��B��Bv�B_;B?}B-B�BPB  BBhB�BɺB�RB�B�uB~�BgmB^5BL�BB�BS�BJ�B:^B�B�B�;B�#B��B�B��B�DBw�Bu�Bx�B��B��B��B�hB�uB��B��B��B��B��B�1B�=B�1B�+B�+Bs�B[#Be`BdZBQ�BD�B<jB$�B�BbBB�B�B�`B�/B�B��B��B��BǮBÖB�wB�dB�LB�9B�9B�3B�'B�'B�!B�B�B��B��B��B��B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�B2-B2-B2-B2-B2-B2-B2-B33B33B49B9XBx�B�B�B"�B�B�B��B��B��Bv�B_;B?}B-B�BPB  BBhB�BɺB�RB�B�uB~�BgmB^5BL�BB�BS�BJ�B:^B�B�B�;B�#B��B�B��B�DBw�Bu�Bx�B��B��B��B�hB�uB��B��B��B��B��B�1B�=B�1B�+B�+Bs�B[#Be`BdZBQ�BD�B<jB$�B�BbBB�B�B�`B�/B�B��B��B��BǮBÖB�wB�dB�LB�9B�9B�3B�'B�'B�!B�B�B��B��B��B��B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cm}qCm��Cl�-Cl�fCl1�Cm�Cm!Cli�Ck�LCkCjL�Cg<�Ca�'CV�CMIyCNCNdCN'�CN2�CN:CO�9CP�fCOdCN^�CNg+CP�CP-CP�COcTCP9CPG�CM/�CJ�wCH�VCC��C>k�C8�C5�C3��C1��C2�1C3�XC0|jC.)yC+��C,��C/�C/�C/�C/�)C/�C/%�C/��C1�sC4޸C5� C84�C;t9C?yCECI�CK�?CNoCP�VCSVCU�DCW@�CY�C[��C]�;Cb)Cc�Cc0!Cd�Cf�ChmPCj��Cm~5Cnt�Cp6Cq�RCs�Ct�oCu��Cwr-Cw�jCx��Cx�sCyŢCy�-Cz��C{�fC|?C}�C}:�C}h�C}�
C}��C}��C~%`C~Q'CM�C}/C�DC��C�l�C��C�2C���C��7C�xR333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                             C��JC���C�nC�+�C��}C�B�C�M�C��AC�{>C��CP<C{�>Cul&Cg�}C]sNC^] C^l�C^tuC^l�C^`C`'.Ca�C_+�C^B/C^7�C`"C`C`0C_5*C`,C`@�C\�fCY�CW�CQ�TCK!�CD�QC@$C>F�C<k�C=J*C><�C:�C7�OC5 lC5�%C8�-C8}�C8vlC9J?C9KkC8N�C9&`C;NC>��C?��CBxCF/�CJ��CQ0DCU�hCX��C[oC^I5Ca#5Cc�Ce�WCh¶Cj�zCm��CrP�CsA�CsugCuzDCwlvCygC|P|C)C� C��C�C�xC��{C�3�C�7�C�K�C��C���C��-C��LC�*�C���C���C�_aC�u�C���C���C���C��C���C��C��:C��hC��C���C�zC���C�2uG�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @%��@%��@%F�@%V�@$��@%s�@%��@$��@$t�@#��@#f�@!�@@Eo@�o@P@Y�@_@Z1@Q�@u<@
�@�Y@>�@83@e4@i�@k�@�X@x^@�@+�@b�@	�A@ 6@+?���?�ݟ?�?�-�?�Js?��?��_?�H?箟?��?�*w?�%�?��?�+�?�-V?��;?���?�`d?�}?�\#?��?��@��@�s@�8@
�i@p@CU@�@�T@ �@�@?@@@�@�B@@Y�@�@!{1@#Mx@#��@%D�@&��@'��@(�-@)7�@*��@*��@+Y�@+y#@,2%@,Nv@-I@-�R@-�]@.��@.�@.�@@.��@/
@/+�@/M�@/k	@0$!@0E�@0eC@0�@1@#@1`�@0�mG�O�G�O�G�O�