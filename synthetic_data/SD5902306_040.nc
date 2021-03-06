CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:04:30Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030040430  20201030040430  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               (A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @����O��1   @������@H(�\)�,s�E���1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013223 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161500201510161615002015101616150020130604092429                                          20170502151425A   A   B   B   ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,@ D2@ D8� D?  DE@ DK� DQ� DX@ D^  Dd@ Dj� Dp� Dw@ D}� D�� D�� D�@ D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�` D�� D�� D�� D�� D�  D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D܀ Dߠ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,@ D2@ D8� D?  DE@ DK� DQ� DX@ D^  Dd@ Dj� Dp� Dw@ D}� D�� D�� D�@ D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�` D�� D�� D�� D�� D�  D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D܀ Dߠ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;A�mAx�AyXAjM�AY��AS�APjANA�ALn�AK��AJ(�AI\)AH�AF�!AF �AE��AF��AE�mAEC�AD�9ADVAC;dAA�AA�FA@v�A?��A>��A=�;A=�TA=x�A=
=A;��A:�`A:1A9+A8bNA7ƨA9�-A9t�A8=qA8jA7��A4�HA3�hA2�uA2jA2�A333A4(�A3�mA2ȴA0A�A-?}A,1'A)��A'�;A&jA"v�A"v�A!�AƨA�AE�A�A7LA��AVAAVA�jAVA%@���@��H@��@��
@�=q@�~�@���@��@�r�@���@�dZ@�ȴ@���@���@��D@�C�@��@���@�Q�@�%@�^5@��9@�"�@���@��-@��`@\)@{��@w�;@u��@s"�@q��@p�u@oK�@n��@l�j@i��@h��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             A�;A�mAx�AyXAjM�AY��AS�APjANA�ALn�AK��AJ(�AI\)AH�AF�!AF �AE��AF��AE�mAEC�AD�9ADVAC;dAA�AA�FA@v�A?��A>��A=�;A=�TA=x�A=
=A;��A:�`A:1A9+A8bNA7ƨA9�-A9t�A8=qA8jA7��A4�HA3�hA2�uA2jA2�A333A4(�A3�mA2ȴA0A�A-?}A,1'A)��A'�;A&jA"v�A"v�A!�AƨA�AE�A�A7LA��AVAAVA�jAVA%@���@��H@��@��
@�=q@�~�@���@��@�r�@���@�dZ@�ȴ@���@���@��D@�C�@��@���@�Q�@�%@�^5@��9@�"�@���@��-@��`@\)@{��@w�;@u��@s"�@q��@p�u@oK�@n��@l�j@i��@h��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBÖBĜBÖB��B��B�}B�dB�dB�FB�FBB�XB�9B��B��B��B��B�LB�9B�B��B��B��B�VB�VB}�Bs�BgmBbNBiyBhsBffBZBO�BK�BA�B=qB:^Be`B]/BQ�BYBL�B(�B�BhB�B�B/B>wB=qB33B�BBB�B�B�sBȴB�B  B��BbBbB �B8RBo�Bo�B}�Bl�BD�B  B��B��Bo�BW
B6FB(�B+B��B�wB��Bx�BXBF�B-B�BVB��B�fB�TB�NB��BȴBÖB��BB��B��B�wB�^B�FB�?B�-B�-B�-B�3B�FB�9B�'B�'441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�B�OBߩB�B�3B�B�B��B��B�DB�	B��B��B�FB�;B�VB��B��B��B��B��B�XB�B�B��B�_BuBo�BwBvBtBg�B]�BYjBO-BKBG�BsBj�B_�Bf�BZpB6�B)CBB#B$ B<�BLBKB@�B)CB�B�B KB�B�B�IB�;B�BRB�B�B.]BE�B}=B}=B��Bz+BR8B�B�KB�$B}!Bd�BC�B6oB�B�;B��B�UB�:BenBTB:iB,B�BB�B�B�B�FB�B��B��B��B��B��B��BǨBÎBB�vB�vB�yB�|BÑB��B�pB�o441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cs�CrW�Cp�
Ci��Cf��Ci��Ci��CjmPCk;#CjyCi�XCh�qChD�Ci�Ci*Ci:^CiJ=Ch��Ch�qCh�VCg�FCg=�Ce��CeCdKDCb��Cb�CaX�Cag�Cb@�CbPbCa�LC`uC_ZC`1'C_w�C`O�Ca��Cd]/Cc��CeIyCeZ^Ca{#C]�
C\hC[VFC\/\C]ҰCb�}Ce�Cf��Cds3C^F�CWGmCR�DCG�uCJ2�CF[�CA��C=�uC:��C8r�C5[dC6<)C6R�C6g�C6yXC7[#C8AC9��C=>�C@��CCҰCGjCI��CL�\CP:CQ�!CV�CZ�C]��C`Cd��ChP�Cj-CmmCna�Cq��CtN�Cv{CwCw8�Cy�\C{��C{ŢC{��C{P�C|NC|{�C|��C|�9C}ѪC}�;C~*C~X�C~��C}�C~�C~AC}��C|�333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                             C���C�%4C�MeCz[�CwXCy݌Cy��CzrKC{P�Czk(Cy��Cx��Cw�Cx�Cx�,Cx�Cx��Cx �Cx-�Cx=�Cwi�Cv�bCtۇCt�Cs03Cqt/Cp��Co��Co�
Cp�
Cp��CpsCnFCmo6Cn^oCm�CCnw�CpN�Cs�CrIKCt=Ct0�CoǧCkM�Ci��Ch��Ci�Ck��Cp�hCs�kCu�@Cs
�Ck��Cc��C^�CR;�CU�CP��CKD2CF��CC]�C@��C=$�C>rC>&{C>5_C>MC?>5C@=(CB8CE�lCII�CL�CP|�CS9�CV�CZ��C\a�C`�DCf\4Ci*�Ck��Cql�CuCw
+Cz�9C{�RC��C�.�C�(oC���C���C�:�C�8�C�OC�gJC�	_C���C��3C�ƞC�ۡC�g�C��C���C���C��C�jaC��G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @' �@&�@%��@ :�@.@��@��@ I&@ �h@ D�@� @)�@��@8\@>1@G�@Q�@�i@խ@߸@X@��@�u@*"@��@��@�@u@}w@�@$�@�@
@�A@�\@�@��@��@��@0@;@H@u�@�P@x�@�0@�J@��@9�@x@4�@��@a@�@��@��@U�@��@*?���?�R?��?��?�M_?�dt?�w�?��?�ʁ?��?�o�?�M@ �,@!@n�@/@	�o@�J@L@��@n6@:	@�@��@�f@�@ |M@!$�@#��@%Z�@&�(@'K*@'g�@)@�@*�@@*��@*��@*Io@*�}@+�@+;�@+V�@,	�@,(}@,D�@,d�@,�3@,I@,.<G�O�G�O�G�O�