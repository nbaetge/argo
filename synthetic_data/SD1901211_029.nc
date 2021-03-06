CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:48:54Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20201028144854  20201028144854  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�5<�l1   @�5?W@@H�1&��2ɺ^5?�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014266 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559292015101515592920151015155929                                                        20161116115900A   B   B   B       @   @@  @�  @�  @�  A  A0  A`  Ap  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CN  CX  Ca  Ck  Cv  C  C�� C�  C�  C�  C�  C�� C�  C�  C�� C�� C�� C�  C�  C�  C�  Cπ CԀ Cـ Cހ C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D&  D,  D2@ D8� D>� DD� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @   @@  @�  @�  @�  A  A0  A`  Ap  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CN  CX  Ca  Ck  Cv  C  C�� C�  C�  C�  C�  C�� C�  C�  C�� C�� C�� C�  C�  C�  C�  Cπ CԀ Cـ Cހ C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D&  D,  D2@ D8� D>� DD� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AH�/AH�`AH�`AH�`AH�AH�AH�AH��AH��AH��AIAI%AIoAI�AH�`AH�DAH1AF�jAF  ACXAB�!AB�DABbNAB�AEAF��AGoAFz�AFn�AD�A?�A<��A;�mA:�yA:jA:M�A:A�A:VA<ZA=A<I�A9�mA6�A5��A4��A4 �A5XA5dZA4�A2��A2�uA2 �A1�A0M�A.��A-7LA+��A)��A'�;A&  A$��A"bA�uAM�AQ�A��Ar�AZA|�A�A�\@�33@���@�dZ@�O�@���@�ƨ@��y@�I�@���@җ�@̃@���@��@��m@� �@��!@��7@�z�@�|�@��@�hs@���@��;@�Z@�x�@���@���@��@��@�E�@��;@���@���@�1@|�D@x�`@t��@p��@n��@l��@k�m@j^5@h��@g��@f��@d��@c�F@a7L@_l�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�AH�AH�AH�AH�AH��AH��AH��AIAI%AIoAI�AH�`AH�DAH1AF�jAF  ACXAB�!AB�DABbNAB�AEAF��AGoAFz�AFn�AD�A?�A<��A;�mA:�yA:jA:M�A:A�A:VA<ZA=A<I�A9�mA6�A5��A4��A4 �A5XA5dZA4�A2��A2�uA2 �A1�A0M�A.��A-7LA+��A)��A'�;A&  A$��A"bA�uAM�AQ�A��Ar�AZA|�A�A�\@�33@���@�dZ@�O�@���@�ƨ@��y@�I�@���@җ�@̃@���@��@��m@� �@��!@��7@�z�@�|�@��@�hs@���@��;@�Z@�x�@���@���@��@��@�E�@��;@���@���@�1@|�D@x�`@t��@p��@n��@l��@k�m@j^5@h��@g��@f��@d��@c�F@a7L@_l�444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�uBp�BgmBgmBgmBffB��B�wBĜB�dB�^B��BVB)�B�BhBJBVBuB�B@�BZBE�B#�B��B�B�sB�TB  B%B��B�sB�`B�`B�fB�#B��B�qB�!B��B�bB~�B~�Bt�BcTBe`BP�B7LB��B!�BC�BF�BhB�B��B.BS�B!�BB�`BB�B�BŢBĜB�qB�}B�-B��B�%Br�B]/BI�B1'B�B
=B��B�B�B�ZB�BB�
B��BĜB��B�}B�XB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B�{333311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�B�wB�wB�|B�xB�xB�xB�xB�xB�vB�xB�B��B��B��B�}B�>BkBv3Bv1Bv2Bu(B�xB�DB�jB�4B�-B�SBd�B8�B+hB #BBB"-B(VBOBBh�BTbB2�B	�B�[B�%B�B�B�ByB�%B�B�B�B��B�|B�!B��B�oB�B��B��B�bBq�BtB_�BE�BmB0cBR1BUBB�B�B^B<�Bb�B0_B�B��B�B�B�B�(B� B��B�B��B� B��B�-Bk�BX-B?�B'B�BYBB��B��B�B�qB�2B��B��B��BǻBB�sB�NB�B�B��B��B��B��B��B��B��B��B��B��444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Ck�C~�RC~�4C~��C~�}C~�_C~��C~��C~ƝC~��C~��C~ՄC~�C{��Cvv�Cr�OCp[Cn��Cl��Ck�Cjh�Ch��Ci �Ckj�Cn��Co�Co��Cn�CmfACi�oCf||Cd�nCb�C_�KC^�C`��C`�zC_��C^lCC\�1CZ�[CXT�CW�tCV��CV$�CUkoCVFCU��CT"CSRECQ��CPI�CN��CN�CL��CJ�3CIr�CG�CD ]CA�kC>��C9*(C5M�C2;�C/%�C-��C.}�C.�FC/v�C1#JC4a�C6\C8��C8��C:cC>r�CA��CE��CF�*CJ=CMPbCOӓCQ�CT�CT;CVƼCYPUC[�zC^e<C`��Cb�"Cf�Ch�&Cj\�Cl�Cm�Co�LCp�CCq�eCr��Ctg"Cua�Cu��Cu��Cv��Cw�ZCx��Cx�Cz�lCz�YC{�C{A�C{q�C{��C{ўC|�C|1�C{�4C|�qC|³333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                C�@�C��!C���C��C��oC��<C��C��eC���C���C���C���C��SC��8C�H�C.oC}3�C{�dCz Cx�Cx<wCv�CwCy)C{�vC|�hC|�]C{��Cz��Cwc�Ct�8Cs��Cq�pCo Cn��Co��Cp�CojGCm��Cl�Cj�5Ch��Ch@�Cg�fCgyCfyjCg'�Cf�CeAfCd�Cc]0Cb	�C`��C`$�C^��C]�C\\�CZjCW�zCU�RCS'-CN��CKJ�CH��CF6�CE'�CF_�CFsCGE�CH�CL1�CM��CPM'CPH�CQ��CU��CX�C\�oC]�-C`ɓCc� Cf^Ch�Cjb�Cj�<Cl�%Cod�Cq��CtC-Cv��Cxk�C{�C~xC�&C��C��RC�idC��C�X;C���C���C�%�C�@�C�ZC��LC�K�C��0C���C���C���C��cC��C��C�6�C�OIC�g�C��\G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@#x3@"�]@"��@# �@#<@#p@#�@#
@#y@#�@##@#@"��@!/<@�@G=@��@��@n�@x�@�@!@R@��@�N@Ga@U@�F@�@y=@��@��@/@D@��@��@�:@N�@Y�@e>@��@
s�@	�s@	��@	E@��@	"�@��@��@>�@G�@M�@T�@�@�@�s@ �!?�ޜ?��?���?��d?��?�r?�#�?�0�?�B?�Ya?�wd?��?⼈?��f?�M?�4%?�^�?�A?�� ?��?�(�?�\_@Hy@f�@J@,@��@�(@	t�@�@��@UE@��@�@;m@�-@��@��@A�@d�@J@��@F�@j�@T@)M@F�@�N@�c@-%@Io@ p�@ ��@ �i@ ��@ �{@!Z@!)�@!Io@!g�G�O�G�O�G�O�