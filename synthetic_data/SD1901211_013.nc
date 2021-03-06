CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   y   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:46:38Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  o\Argo synthetic profile          1.0 1.2 19500101000000  20201028144638  20201028144638  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�;�8�1   @�<Ӡm@@H	XbM��2�E���1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014257 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559262015101515592620151015155926                                                        20161116115858A   B   B   B       ?�  @@  @�  @�  A   A   A@  A`  Ap  A�  A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C0  C:  CD  CN  CX  Ca  Ck  Cu  C  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� Cŀ Cʀ Cπ C�  Cـ Cހ C�  C�  C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE@ DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�` Dɠ D̠ D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�@ D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       ?�  @@  @�  @�  A   A   A@  A`  Ap  A�  A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C0  C:  CD  CN  CX  Ca  Ck  Cu  C  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� Cŀ Cʀ Cπ C�  Cـ Cހ C�  C�  C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE@ DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�` Dɠ D̠ D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�@ D�@ D�` D�� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AuƨAu��Au��Au��Au�
Au�
Au�
Au��Au�
Au��Au��Au��Au�#Au�mAv  AvJAmt�A_��AZ�`AX�AV5?AU/ASAR�AQ��AQ&�AOt�AN-AMK�AL��AL{AI��AHȴAG�AF��AEhsAD-AD��AB9XA?��A>{A=hsA>��A?��A<r�A;G�A8��A7�A5�^A4ȴA4bA2��A1G�A0A�A.��A.(�A-?}A+��A*jA)�7A'K�A#"�A��A�#AVA��AĜA�PA�A	;d@�bN@� �@��@��@��@�b@�X@Χ�@���@ȋD@�C�@�+@�Z@��\@���@�j@�A�@�l�@��@�ƨ@���@�z�@�33@��+@��F@�V@���@�@��@~V@z��@xr�@w�@v$�@t�@so@qG�@p1'@o\)@nV@l�@k��@j�H@i�7@h �@f��@e�@dI�@c@aX@`��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�Au�
Au�
Au�
Au�
Au��Au�
Au��Au��Au��Au�#Au�mAv  AvJAmt�A_��AZ�`AX�AV5?AU/ASAR�AQ��AQ&�AOt�AN-AMK�AL��AL{AI��AHȴAG�AF��AEhsAD-AD��AB9XA?��A>{A=hsA>��A?��A<r�A;G�A8��A7�A5�^A4ȴA4bA2��A1G�A0A�A.��A.(�A-?}A+��A*jA)�7A'K�A#"�A��A�#AVA��AĜA�PA�A	;d@�bN@� �@��@��@��@�b@�X@Χ�@���@ȋD@�C�@�+@�Z@��\@���@�j@�A�@�l�@��@�ƨ@���@�z�@�33@��+@��F@�V@���@�@��@~V@z��@xr�@w�@v$�@t�@so@qG�@p1'@o\)@nV@l�@k��@j�H@i�7@h �@f��@e�@dI�@c@aX@`��4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB49B49B2-B5?B5?B6FB5?B49B5?B49B33B2-B49B6FB:^BK�B�B?}B:^B-B!�B�BVB+B  B��B�fB�B��B��BŢB��B��B�bB�+Bx�Bn�B{�B\)B6FB!�B�B8RBG�B�BVB�B�B��B��B�jB�B��B��B�=B�B{�Bn�BcTB^5BL�B0!B&�B��B��BD�BA�B^5BC�B�B�-B��B��B��B�3B~�Bq�B{�B�=B{�Bp�B�BT�BB�B,B$�B.B�B{B%B��B�B�BB�RB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��3333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�BDBDBEBDBCBDBCBA�B@�BCBEBI'BZ�B�zBN_BI@B;�B0�B(vB/BB�B�B�;B��B߷B��B�uB��B�B�-B��B��B}]B��Bj�BEB0�B+jBGBVpB'OBB _B��B�sB�8B�B��B�qB�6B��B��B��B}>Bq�Bl�B[rB>�B5�BcB�BS9BP$Bl�BR0B*;B��B�SB�B�B��B�{B�)B�dB��B�dBB��BcwBQB:|B3OB<�B.*B"�B�BfB��B�kB��BƳB�sB�\B�<B�"B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��4448111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Ch�>ChŪCh��G�O�Ch�gCh�ZCh� Ch��Ch��Ch�SCh�GCh��Ch3�ChF�Cf��Ca[�CV�zCVV9CW�SCYn�CZBC[6C[��C\ɟC\�eC\#;C[i9C[{�C\T�C\e�C\x�C]O�C]_<C\�2C\��C]�UC^k�C^}�C`�Ca�UCa�,CaC^��C_��CaR�C`�|C`��C_��C_0�C\�`C[_�CZ��CY�@CY.�CW�wCV&0CT��CQ��CM��CH!4CE�C<rtC64�C2PxC/5OC,��C- C-�C-�kC/� C3�GC6�C9O�C;�uC?��CE��CI��CKJqCM�CPS�CR��CUdCCZElC]��C`�Cb�uCc��Cf)�Ch��CjyPCm
�Co�Cr�`Cu�ZCxCy�Cz�C{bC|�C}'C}=QC~<>C~k�C}�C~ �C~.�C}��C}��C}�C~%C}��C}�+C}�C~�C}u�C}��C}�C~gC~6�C}�gC}��333 333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333               G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Ct=�Ct?oCtD'G�O�CtG�CtKCtOSCtS�CtWCtY�Ct_�CtdCs��Cs��CrzCm�{Cd��Cd�Cf~�Ch
�Ch��Ci�Cj}OCkI�CkdUCj͍Cj@�CjbzCk.�CkBPCk_�Cl>fClZ�Ck�Ck�Cl�Cm��Cm�=Co� Cp�TCp�{CpL�Cn3OCn�:Cp��Co��Cp&�Co�SCoLCm�Ck�.Ck27Cj��Cj
�Ch�wCgzCf0/Cc�SC`#�C[U�CX��CQ^�CLCH�xCF��CD=�CD�CD�CE�CG�ECK�eCNrCQ*�CS�CWg#C\�1C`�#CbMgCc�Cf��Cia�Ck��CpXgCs~xCv�YCxXTCy4�C{��C~aC��C�BC�F�C��nC�>C�JC��'C�=�C���C�,�C��eC���C�8XC�P�C�(C�'C�?VC���C�&C�.>C�F�C�C�;C�3HC�LMC�(C�!C�9�C�QrG�O�G�O�G�O�111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444   @��@�c@��G�O�@ B@�@�@�@m@@u@o@��@�9@�S@:�@	pN@	,�@
$�@(@@�]@9�@�@M�@X�@�Y@lM@x@@�@�@��@��@7*@BF@ͧ@Y]@d�@��@|_@�&@Z@�@(�@4�@��@�@N�@�i@^R@fR@�@w�@
�/@
k@	3@{@�@�o@ @?�:�?�6W?�9O?�>
?�D<?�^i?�r?ݍ�?ަ�?���?��2?�	�?�3?�dm?��C?���@�@*@6�@T-@��@�@��@ѽ@�@@��@M�@�h@3@��@X�@�@$@˓@k�@ �@ �?@!S
@!�8@"�@"��@"�J@"oe@"��@"�X@"H@"g�@"��@"�@"@�@"`@")@"�o@"7@"U�@"u�@"�uG�O�G�O�G�O�