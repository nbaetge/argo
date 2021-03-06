CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   w   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:48:44Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  SX   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  U4   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  Yd   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  ]�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  a�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  b<   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  d   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  e�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  fl   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  hH   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  j$   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  j�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  lx   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  nT   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20201028144844  20201028144844  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�2��8�1   @�2�y\� @H�t��2�5?|�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014239 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559292015101515592920151015155929                                                        20161116115900A   B   B   B       ?�  @@  @�  @�  A   A   A0  Ap  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C0  C9  CC  CM  CX  Ca  Ck  Cu  C  C�  C�� C�� C�� C�� C�� C�� C�� C�� C�  C�  C�� C�� Cŀ C�  Cπ C�  C�  Cހ C� C�  C� C� C�� D @ D� D� D  D@ D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd  Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�@ D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @@  @�  @�  A   A   A0  Ap  A�  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C0  C9  CC  CM  CX  Ca  Ck  Cu  C  C�  C�� C�� C�� C�� C�� C�� C�� C�� C�  C�  C�� C�� Cŀ C�  Cπ C�  C�  Cހ C� C�  C� C� C�� D @ D� D� D  D@ D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd  Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�@ D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��ABZAB�`AB�`AB�AB�yAB�AB�AB�AB�HAB��ABz�AB=qAA��AA��AAK�AA&�AA�AA
=A@��A@��A@�9A@�RA@�A@�+A@bNA@9XA@�A?��A>1'A=�^A=XA=�-A>{A>1'A<ĜA;�mA:1A933A7��A7/A7;dA6r�A69XA4��A1��A1�mA1�A.�A,�\A+33A)l�A)%A(�A(�\A'�A&  A#�hA!dZAp�A��AoA=qA�!A
-A�F@��@��@��A��@��
AS�@�E�@�V@���@�n�@���@с@���@ř�@��@���@���@��D@�Ĝ@�dZ@��`@��@��P@��T@��@��/@�ff@�A�@�p�@���@�"�@���@�9X@~ff@}V@zJ@vff@vff@uV@s"�@r��@q�^@pĜ@m@l��@kdZ@h�`@f��@e`B@cC�@a��@_�P@]�-@]�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�AB�yAB�yAB�AB�AB�AB�HAB��ABz�AB=qAA��AA��AAK�AA&�AA�AA
=A@��A@��A@�9A@�RA@�A@�+A@bNA@9XA@�A?��A>1'A=�^A=XA=�-A>{A>1'A<ĜA;�mA:1A933A7��A7/A7;dA6r�A69XA4��A1��A1�mA1�A.�A,�\A+33A)l�A)%A(�A(�\A'�A&  A#�hA!dZAp�A��AoA=qA�!A
-A�F@��@��@��A��@��
AS�@�E�@�V@���@�n�@���@с@���@ř�@��@���@���@��D@�Ĝ@�dZ@��`@��@��P@��T@��@��/@�ff@�A�@�p�@���@�"�@���@�9X@~ff@}V@zJ@vff@vff@uV@s"�@r��@q�^@pĜ@m@l��@kdZ@h�`@f��@e`B@cC�@a��@_�P@]�-@]�44481111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBr�BQ�BR�BR�BR�BR�BR�BR�BQ�BP�BP�BQ�BP�BP�BN�BM�BL�BL�BK�BK�BJ�BJ�BJ�BI�BI�BH�BI�BI�B8RB8RB7LBB�BM�BR�BA�B9XB$�B�BoBPBoBPBVB��B�B�B�B�^B��B�oB}�B�B�DB�\B�PB}�BiyBW
BC�B$�BB��B�BĜB�B��B�B�B6FB8RBq�BhsB8RB�B�B��B��B�jB��B�B�DB}�B`BB=qB&�B
=B��B�ZB�HB�B��BȴBB�XB�?B�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��44331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�Ba�Ba�Ba�Ba�Ba�B`�B_�B_�B`�B_�B_�B]�B\�B[�B[�BZ�BZ�BY~BY�BY�BXyBX{BWsBX|BX|BGBGBFBQOB\�Ba�BPIBHB3�B,nB!*B
B!(BBB�B�B��B��B�B�ZB�B��B��B��B�B��B��BxBe�BR4B3xB�BcB�+B�)B��B�B��B�BD�BF�B�IBwBF�B*9B$B�B�\B��B�~B��B��B�qBn�BK�B5ZB�B:B��B�B�B�JB�B��BǺBàBB��B�jB�ZB�ZB�>B�B�;B�=B�,B�?B�;B�DB�B�)B�&B�B��B��B��B��B��B��B��44481111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cz�Cz�LCz�qG�O�Cy�Cy��Cz iCz�CyD�CyH�CyJ�Cx�CwބCw(�Cvu�Cv��Cv�Cu�dCv�CuU�Ct�Cs�Cs=�Cp�XCo~rCn�Ck�{Ck��Cmq�Ck�Cl�Cl�Cn�8Cm	�Ci.cCfCatNC]��C[I�C\$�C`%�C`5�C_~�C\jaCYQ�CZ.CVNTCRhyCONDCL��CLA�CK��CJ��CJCF*C?��C=��C<�C<��C8*,C1��C/�AC.&aC.3MC.FC1}C3#SC4ԜC5��C7q�C8Z�C:�>C>�ICB�XCF9�CH��CM��CP"CSi�CW�aCXu$C[ {C^PNCbn�Cd�)Ci:Ck��Cn76Co0�Cp)Cq�7Cs�:Ct�/Cv{Cv�ECv��Cw��Cx WCx/�Cx_�Cy]�CzZ.Cx� Cy�CyKpCy|DCy�JCy�yCzFCz<Czk�C{lC{�C{�GC{�?C|(�C|X^C}Y�C|��333 3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333             G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                            C�N C�T�C�V�G�O�C� �C��C��C��C��C���C���C�f�C��C��qC�~�C��PC���C�EtC�PRC��C���C�d�C�C~-YC|��C{�lCy�Cy�C{-�Cy��Cy�qCz��C|#ACz�uCwy�Ct�Cp�Cmj�CkuCl?~Co�Co�1CoD�Cl�FCj,Cj��Cge�CdtCa�^C_�C^��C^]�C]��C]�CY�#CT;�CRM�CQ�CQ�CN�CH�TCF�CE�/CFE�CF��CI��CKm�CLԇCMq�COyCOCRBCV�CZ�C]�C_�pCd9�Cf��CiɐCm�qCn�'CqCt1�Cx �Cz��C~{�C�u�C���C�$+C���C�s�C�I3C��IC��KC��6C��sC�E�C�apC�{JC���C��C���C��-C���C��C�*�C�D�C�Z�C�wC��~C���C��C�9~C�Q�C�k�C��~G�O�G�O�G�O�111 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @ e1@ r�@ ugG�O�@�Q@�T@  C@ �@�@��@�@#@��@.j@�	@�@�d@f�@t+@�@��@�@��@9�@F�@S�@��@��@�@�@@��@�v@�M@<h@Fw@Jo@џ@XP@�=@t/@~�@	Z@�@�@��@	'�@�C@��@3@�w@B\@�$@P�?��{?���?��?��?��?�?�r?��?��r?���?� ?�/�?�K�?�v�?��?���?��?�+�?�Y�?���?���@ u�@��@.�@M�@	�@
�i@)D@G�@��@��@0�@�P@uA@�@��@֊@��@��@�f@�j@��@��@��@��@��@�@ 9�@O\@n@�q@��@͠@��@ �@ &i@ D�@ ��@!@!%�@!C�@!a�G�O�G�O�G�O�