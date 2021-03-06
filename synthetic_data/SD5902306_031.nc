CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   n   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:03:07Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  S,   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  T�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  UT   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  X�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y4   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Z�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  \�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  `�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  `�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  b�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  dd   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  d�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  hD   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  h�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  jl   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  l$   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030040307  20201030040307  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @��UUUU1   @���N�@H��"��`�0�j~�� 1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013221 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161459201510161614592015101616145920130604092411                                          20170502151424A   A   B   B       @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D� D� D%� D,  D2@ D9  D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw@ D}� D�  D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�� D�` D�� D�� D�� D�  D�  D�@ D�` DÀ Dƀ Dɠ D�� D�  D�  D�@ D�` D܀ D߀ D� D�� D�  D�  D�@ D�@ D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D� D� D%� D,  D2@ D9  D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw@ D}� D�  D�� D�  D�  D�` D�` D�� D�� D�� D�� D�  D�  D�� D�` D�� D�� D�� D�  D�  D�@ D�` DÀ Dƀ Dɠ D�� D�  D�  D�@ D�` D܀ D߀ D� D�� D�  D�  D�@ D�@ D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AM+AM
=AJz�AG�AFr�AF(�AE�AD�RADffAC�AC33AAG�A?l�A>-A=XA=A<�`A<�uA<{A;��A;�7A;��A;��A;XA;/A;�A;
=A;A;�A;C�A;33A:�yA9�wA8�!A8JA7hsA6ȴA61'A5��A5�hA5�^A5��A5��A5\)A5+A4��A4��A4��A4~�A4Q�A4Q�A3�TA3��A3S�A3�A2�DA2�A1�A/��A.I�A+K�A'?}A$�A ��A�HA��AG�A�-A�\A��A~�A��At�A �u@�-@�{@���@��@�D@�1'@���@�O�@�Q�@���@��@�p�@��m@��!@�=q@�bN@��^@��+@�o@�t�@�o@�7L@��H@�Q�@�G�@�  @�-@���@}�T@{t�@y�#@y&�@w
=@t��@qX@p1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          AM+AM
=AJz�AG�AFr�AF(�AE�AD�RADffAC�AC33AAG�A?l�A>-A=XA=A<�`A<�uA<{A;��A;�7A;��A;��A;XA;/A;�A;
=A;A;�A;C�A;33A:�yA9�wA8�!A8JA7hsA6ȴA61'A5��A5�hA5�^A5��A5��A5\)A5+A4��A4��A4��A4~�A4Q�A4Q�A3�TA3��A3S�A3�A2�DA2�A1�A/��A.I�A+K�A'?}A$�A ��A�HA��AG�A�-A�\A��A~�A��At�A �u@�-@�{@���@��@�D@�1'@���@�O�@�Q�@���@��@�p�@��m@��!@�=q@�bN@��^@��+@�o@�t�@�o@�7L@��H@�Q�@�G�@�  @�-@���@}�T@{t�@y�#@y&�@w
=@t��@qX@p1'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB|�B~�By�Bu�Bu�Bu�Bz�B~�B|�Bz�Bx�Bv�Bu�Bv�Bx�By�Bz�Bx�Bs�Bq�Bo�Br�Br�Bp�Bo�Bn�Bn�Bo�Br�Bt�Bt�Bp�B`BBS�BM�BG�BB�B>wB<jB;dBA�BC�BB�B@�B?}B>wB>wB<jB=qB<jB>wB:^B:^B9XB8RB49B0!B%�B�BJB�B��BŢB��B�=B��B��B��B��B�bBe`Bl�BjB_;Bl�B~�Bl�BT�B<jB1B�B��B�wB��B�\Bz�BffBP�B=qB�B�B
=B��B�B�/B�B��BǮB�^B�FB�'B�!B�B��B��B�B��B��B��B��44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�B��B�nB�mB�mB��B��B��B��B��B�rB�kB�qB�}B��B��B�B�^BSB}GB�WB�VB~LB}FB|BB|AB}DB�ZB�eB�eB~LBm�Ba�B[yBURBP1BLBJBIBO(BQ8BP1BN$BMBLBLBJBKBJBLBHBG�BF�BE�BA�B=�B3�B*HB�B�!B�UB�6B�hB��B�B�kB�/B�=B��Br�BzBxBl�BzB�~BzBbBI�B�B��B�RB��B�5B��B�DBs�B^EBJ�B-B&�B�B	<B��B�}B�hB�;B��BǫBÕB�tB�lB�JB�;B�:B�RB�GB�=B�B�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C�:^C�?�C��FC�ۅC��C�3C}�yC|&fC{qhC{�TCzCw�hCv'�Cui�Cs�fCs/CsA�Cr��CqՁCq��Cr�Cs��Cs�-Cr�jCs\Cs#Cs��Ct-Ct�VCu˅CuCs�
Cr�Cq^wCqk�Cq�HCq�-Crl�Cr~5Cr�bCr��Cq�JCq��CrhCr��Cs �Cs�Cs%�Cs8CsJ�Cr�3Cr��CqL�Cq{�Cp� Coy�CmECk��Ch�Cb��CV�CK�CG%�C@� C<��C9��C9*=C9=qC8��C9a�C;ؓC<��C?8RC@�CB�CDS�CFևCIT�CL��CQ�sCTPCW^�CY��C^C`��Cc�Ce�Ci�Ck��Cm\�Co!�Cp�Cr�CuD�Cv8Cx�Cx�qCzɺC|�bC}�bC}�jC~�C~�C�C>�C~��C~��C~�C��C~~w33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                          C��PC��C�fDC�wyC��|C�RkC��C�'-C���C�șC��C���C���C�C�C�e�C��
C�UC��<C�6xC�@.C���C�8�C�CnC�۸C���C���C�m�C�xLC��mC�s_C�EC�29C�T�C��qC��)C���C�TC�{�C��kC��"C���C�1�C�;�C�E�C��C��/C��"C��(C��SC��gC��UC��C��C��C��C��C}�C{g$Cw�kCp�8Cb��CU�wCQ~mCJH�CE�	CB3�CA^?CA^�C@xkCA\�CDcCD��CG�8CI�3CKrCMg~CP+ICR�CV�5C[�SC^�+Cb]dCe-KCi�Cl�6CpF�Cr,�Cu�Cx� Cz�C|��C~�C�J5C���C�<qC�<dC���C��C���C�Q�C�g<C��C�;C��C�8}C�ܚC��G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444  @3�@3%6@3�@2�r@1l$@/Ũ@.w@,��@,zh@,��@+le@)��@(��@(�@&��@&b�@&o�@%�@%dq@%p�@&�@&�
@&��@&7�@&E@&S(@&�@' a@'��@(A�@'��@&��@%�z@%�@%	*@%i@% l@%��@%�7@%Ԧ@%�@%^m@%kz@%w�@&!@&$@&0�@&=�@&J�@&W�@%�X@%��@$�@%�@$�@#�!@!�@ ��@�0@+�@��@��@y@v$?�&r?��%?���?��Q?�\�?�� ?��V?�!<?���@ ��@4�@uo@:l@��@	P�@�h@��@ߜ@�Y@��@g-@��@��@b�@5@ o`@!��@"�@$6@&�@&��@'�a@(��@)�@+7 @+��@,	C@,�z@,�V@,��@-@,�}@,��G�O�G�O�G�O�