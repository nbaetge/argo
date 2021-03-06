CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   p   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:04:03Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  S4   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  T�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ud   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W$   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  X�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  YT   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  \�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]D   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  `�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a4   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  b�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  d�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  e$   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  h�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  i   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  j�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  l�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  mArgo synthetic profile          1.0 1.2 19500101000000  20201030040403  20201030040403  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               %A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @��I��J1   @��q��@HSt�j~��,���v�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013204 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161500201510161615002015101616150020130604092424                                          20170502151425A   A   B   B   ?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C� C�  C�  C�  C�  D @ D� D� D@ D@ D� D%� D,  D2@ D9  D>� DE  DK@ DQ� DX  D^  Dd@ Dj� Dp� Dw  D}� D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�` D�� D�� D�  D�� D�  D�  D�@ D�` DƠ D�� D�� D�� D�  D�  D�` D�` D߀ D�� D�� D�  D�  D�  D�@ D�� D�� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C� C�  C�  C�  C�  D @ D� D� D@ D@ D� D%� D,  D2@ D9  D>� DE  DK@ DQ� DX  D^  Dd@ Dj� Dp� Dw  D}� D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�  D�@ D�` D�� D�� D�  D�� D�  D�  D�@ D�` DƠ D�� D�� D�� D�  D�  D�` D�` D߀ D�� D�� D�  D�  D�  D�@ D�� D�� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aap�Aap�Aax�A`�A_`BA\��AR{AL�AJn�AIx�AH1'AG�-AE�ADJAC�ABM�AA33A@��A?ƨA>�!A=�FA=%A<-A;�FA;�wA;�hA;
=A:z�A:{A9��A9&�A8��A8�\A8I�A8�A7��A7��A7�A77LA6��A6�A5�#A5?}A4�A4�uA4=qA3�A4jA3�A3p�A3`BA2z�A1hsA0��A/`BA-�7A*A�A'&�A#�A ��A�9A��AhsA��A=qA��A�A��A�A�@�K�@��/@���@�+@�o@�p�@��@��@���@�J@��@���@�  @��T@��@���@��/@��u@�ff@�1@���@�z�@���@���@�M�@� �@}��@{�@x �@v@s�m@rM�@o;d@mO�@j^5@hQ�@fȴ@e?}@cC�@c�F@f��@e�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                Aap�Aap�Aax�A`�A_`BA\��AR{AL�AJn�AIx�AH1'AG�-AE�ADJAC�ABM�AA33A@��A?ƨA>�!A=�FA=%A<-A;�FA;�wA;�hA;
=A:z�A:{A9��A9&�A8��A8�\A8I�A8�A7��A7��A7�A77LA6��A6�A5�#A5?}A4�A4�uA4=qA3�A4jA3�A3p�A3`BA2z�A1hsA0��A/`BA-�7A*A�A'&�A#�A ��A�9A��AhsA��A=qA��A�A��A�A�@�K�@��/@���@�+@�o@�p�@��@��@���@�J@��@���@�  @��T@��@���@��/@��u@�ff@�1@���@�z�@���@���@�M�@� �@}��@{�@x �@v@s�m@rM�@o;d@mO�@j^5@hQ�@fȴ@e?}@cC�@c�F@f��@e�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBz�B{�B{�By�Bv�Bw�B�PB�=B�B�B{�B�B�B�B�B�B}�B{�Bv�Bm�BgmBcTB]/B^5BcTBffBbNB^5B]/B\)BW
BT�BS�BT�BVBVBW
BW
BVBT�BO�BI�BC�B@�B=qB:^B;dBE�B<jB:^B;dB33B.B,B�BDB�B�)B��BƨB��B�HB�BƨB��B�ZB�BB�1Bq�B\)BO�B@�B)�BhB�B��B�jB��B�\Bz�BjBO�B9XB�B
=B��B�mB�BB�
B��B��BB�qB�RB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�B��B��B�wB�~B��B��B��B��B��B��B��B��B��B��B��B��B�sB{:BuBp�Bj�Bk�Bp�BtBo�Bk�Bj�Bi�Bd�Bb�Ba�Bb�Bc�Bc�Bd�Bd�Bc�Bb�B]�BW]BQ9BN!BKBG�BIBSDBJBG�BIB@�B;�B9�B+PB�B�(B�B�SB�;B�B��B�B�9B�B��B�=B�B��B)Bi�B][BN B7zB�B�B�aB��B�TB��B�EBw�B]=BF�B,B�B$B��B�B�YB�-B�B��BʾBŞB�vB�dB�QB�?B�2B�/B�(B�B�B��B��B��B��B��B�B�bB�_4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C{��C{�}C{�FC{�C{)yCy��Cx�Cv�Cu\)Cs�Cq��Cp�Cm��Cm�}Cm��ClI7Ck�PCj�{CjdCh�XCh��Ch��Ch��CjgmCl��Cl�Cl+�Cm`Cm�;Cm��Cm8�CmI�Cn%Cn6Co�Co  Co3�CoF%CoW�CoiyCo{�Co��Cn�{Cn��Cn��Cn@�Cm�7Cm��Cl�HCl�uCl;dCk�VCj#Ch�XCd�C^��CU[�CK'�CD�C>�TC;�PC9C�C7�%C7�C7(�C7AC8qC:��C>��CAdCC��CF�CI^wCK��CO�uCT
CWY�CYFC[��C^)yC`��Cd%Cg\�Cj��Cn
=Cp�LCs(sCu��Cv��CxyCyr�Czn�C|6�C|b�C}a�C~\jC~��C~��C�oC��C�RC��C�5C�M/C�c�C�z�C���C��1C���C;#C|��Cy�y3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                C�-tC�1�C�BHC�J�C�� C��C�=�C�>C�\MC�~�C�.BC�Q�C}��C}�9C}�'C|CuC{haCz�xCy�Cx>Cx_CxVCx'Cy�C|�
C|�C{��C|�HC}�'C}��C}!C}2�C~*C~;�C3CB�CXClZC~cC�?C�KC�C~ޏC~�C�C~1/C}_PC}u=C|��C|��C{��C{UCyvOCwؒCs��Clt1Ca��CV�CN�CG�CD4-CA�xC?�HC>��C?PC?�C@�CB�[CG�CI�ACL��COECR��CU��CZ/�C^�}CbZXCd@�CgCi�Cl�ZCpX�Ct
�Cw��C{nC~>�C���C���C���C�~ C�OC���C���C��xC�4C��.C��YC���C�xiC���C���C���C��C��C�UC� �C�9RC�Q2C�iG�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@-�@-�@-!�@-,�@,��@+t�@*��@)E<@($:@'�@%Y�@$?�@"��@"�v@"�-@!r�@ �@ ^a@�,@�@�=@�-@�M@�@!�@!ˍ@!D=@!�.@"|�@"��@" �@"	@"�A@"��@#S�@#]�@#k�@#x�@#�@#�(@#��@#��@#�@#*@#5�@"��@"(�@"6�@!��@!�@!4X@ �j@��@�@��@T�@x!@	w@�	?��1?�$?��?���?�l�?�=?���?��w?�3�?��)@)@�@�@�@�L@��@��@ݩ@@�@�3@sm@҄@/�@�3@ �L@"�i@$��@&[�@'�@(O\@(�@)��@*��@+�@+��@,x�@,��@,�S@-f�@-��@-�@-ĳ@-�o@.@.%@.>@.]�@.|i@.�G�O�G�O�G�O�