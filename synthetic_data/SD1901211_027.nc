CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   y   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:48:35Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  o\Argo synthetic profile          1.0 1.2 19500101000000  20201028144835  20201028144835  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�0;��-�1   @�0?��a�@G�������2~��"��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014233 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559282015101515592820151015155928                                                        20161116115900A   B   B   B       @   @@  @�  @�  @�  A   A@  AP  Ap  A�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C:  CD  CN  CX  Cb  Ck  Cv  C�  C�� C�� C�  C�  C�  C�� C�� C�� C�� C�� C�  C�  C�  C�  Cʀ Cπ C�  C�  Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DD� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       @   @@  @�  @�  @�  A   A@  AP  Ap  A�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C:  CD  CN  CX  Cb  Ck  Cv  C�  C�� C�� C�  C�  C�  C�� C�� C�� C�� C�� C�  C�  C�  C�  Cʀ Cπ C�  C�  Cހ C� C� C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DD� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AA�AA��AB�AB�AB�AB1ABA�AB9XAB^5ABZAB^5AB^5AB~�AB�DAB�\AB�uABbNABZAB1'AA��AA��AA�hAAx�AA�AA�A?
=A>z�A=�mA=�wA=C�A=�A<�+A<{A<�A=\)A=\)A<��A<jA:ȴA7x�A5XA4bA4�jA4��A2��A0=qA.�+A*�HA)"�A(�!A'��A&�jA&�A%�A%�A"��A �!AXA1'A�FAVA�A7LA�#A33A�+A�AffA �A��A��@��@�j@�%@��@ݲ-@ӝ�@�\)@��@��@���@�o@�E�@�?}@��T@���@�@��
@��H@�
=@���@�5?@���@��h@�Q�@�
=@���@��u@~$�@{S�@yX@w�;@v�@u`B@tI�@q�#@o�@n$�@l�@j�H@i%@g�@e�-@d�@b��@`��@^�R@]p�@\z�@\I�@[��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�AB�AB�AB1ABA�AB9XAB^5ABZAB^5AB^5AB~�AB�DAB�\AB�uABbNABZAB1'AA��AA��AA�hAAx�AA�AA�A?
=A>z�A=�mA=�wA=C�A=�A<�+A<{A<�A=\)A=\)A<��A<jA:ȴA7x�A5XA4bA4�jA4��A2��A0=qA.�+A*�HA)"�A(�!A'��A&�jA&�A%�A%�A"��A �!AXA1'A�FAVA�A7LA�#A33A�+A�AffA �A��A��@��@�j@�%@��@ݲ-@ӝ�@�\)@��@��@���@�o@�E�@�?}@��T@���@�@��
@��H@�
=@���@�5?@���@��h@�Q�@�
=@���@��u@~$�@{S�@yX@w�;@v�@u`B@tI�@q�#@o�@n$�@l�@j�H@i%@g�@e�-@d�@b��@`��@^�R@]p�@\z�@\I�@[��4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBYBR�BT�BS�BT�BT�BT�BS�BT�BS�BT�BS�BT�BT�BT�BS�BR�BQ�BVBT�BT�BT�BXBVB>wB6FB33B49B0!B9XB)�B'�B6FBG�BL�BI�BF�B1'B+B�B�5B�B��B�5B��B�B�Bs�Bp�BjBe`BbNBe`Bt�BW
BF�B?}B6FB$�B#�BPB�BɺB�5BǮB��BVBoBS�B~�BffBA�B�B�BB�B��B~�Bu�BZB33B#�B&�B�BbB��B��B�mB�
B��BǮB��B�RB�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�uB�{B��3333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�Bb�Bb�Bc�Bc�Bc�Bb�Bc�Bb�Bc�Bb�Bc�Bc�Bc�Bb�Ba�B`�Bd�Bc�Bc�Bc�Bf�Bd�BM5BEBA�BB�B>�BHB8�B6�BEBVlB[�BX|BUiB?�B�B�AB��B�JB�B��B�9B��B��B�\BHBy%BtBp�BtB�_Be�BUGBNBD�B3xB2qB�B�B�GB��B�;B�SB�B!Bb�B��Bu BP!B)2B(+B�B�B�8B�xB�<Bh�BA�B2HB5[B*B�BfB
QB��B�mB�JB�B��BƳBĤB��B�|B�mB�PB�DB�1B�)B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C{m�C{r:C{xmC{| C{~�C{�C{��C{�+C{�C{�C{��C{��C{��C{�C{�CzfCy��Cx��Cw�*Ct|�Ct��Cs�2Cra^CoW�Cm|ClYvCk��Cj$�CirCg�Cfw+Cf��Cb��C^�CZ3�CX�wCW��CWBCXDCX)CX7�CU�pCS�CR�CP��COٸCO�CN^�CM�CL�CK`}CJ��CI�lCHd|CF�CB�CAg�C?zC<¢C:j�C7WC1��C.��C.uC.1HC/�C2BjC3�1C6d�C7L�C7mpC:�^C=�UCB�aCEFJCH��CL�.CRN�CU��CX�C\=�C`T�Cb��Cc��Ce��Ch"�Ci�/Ck��Co JCp��Cq�[Cs�ECuKCvECvwvCwp�Cw��Cx��Cx��CyȁCy��Cz)?CzZ�Cz�+Cz�yCz��C{DC{KtC{{JC{��C{ٴC|	�C|8�C|h1C|�2C|�GC|�aC}%�C}V�C|�C{�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       C���C��jC��_C���C���C���C��C��+C���C���C��C���C���C�НC��pC�4C��C���C��[C��C��3C�U�C]�C|�Cz��Cz)Cy�CxE�Cw�pCv]�Cu0Cu'�Cq�Cm��Cj-bCh��Ch:zCg�ChyYCh�!Ch��Cf�SCd�DCc~�CbF2Ca�xCa9�C`�wC`9�C^�C^Q)C]�,C]�C[˫CY��CW"BCU�LCS�zCQ�CP�CM]CH�zCFy�CFBCF1�CG@�CJ`?CK��CN!ZCN�iCN��CQ��CU0�CY�RC\9fC_Y�CcM�Ch��Ck�CnR�Cr7�Cv0QCx�;CySC{,QC}��CF�C�u�C�	�C��C�]7C�3�C�iC���C��IC� C�.�C���C��6C�7�C�R-C�kXC���C���C��cC��=C���C��C��C�6C�OiC�hC���C���C��8C���C��xC��(G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444   @ �@ ��@ ��@ �H@ �@ ��@ �g@ �0@! �@!G@!@!
�@!@!#B@ �V@ AS@��@[]@j�@x�@��@@�@-�@��@C@�j@�!@g�@s=@x@�F@s@#@�-@
�L@
9�@	��@
P,@
W�@
a@�R@p@yS@�@8@�@�@��@�)@)F@�@:�@ @O?���?��?��J?��?��?�E?��?��?߹�?���?��f?�c?�,?�M?�v�?��?�Ʌ?��;?�%�?�V??���@ ^@�@��@�6@
Q�@�1@�m@1>@�Q@�q@�F@�z@�x@��@�@��@�"@��@��@�@\�@z�@�@:�@�|@�a@ f@ :@ Y@ x�@ �@ �J@ �!@ �@!,@!/,@!M�@!l@!�]@!�@!�8@!�@"�G�O�G�O�G�O�