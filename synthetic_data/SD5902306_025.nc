CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   p   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:02:18Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  mArgo synthetic profile          1.0 1.2 19500101000000  20201030040218  20201030040218  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @����O�1   @�����@H �n���0MV�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013194 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161457201510161614572015101616145720130604092359                                          20170502151423A   A   B   B       @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C� C�  C�  C�  C�  D @ D� D� D  D� D� D%� D,@ D2� D8� D>� DE� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�� D�  D�@ D�` DÀ DƠ D�� D�� D�� D�  D�  D�@ D܀ Dߠ D� D�� D�  D�  D�  D�@ D�� D�� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @�  A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C� C�  C�  C�  C�  D @ D� D� D  D� D� D%� D,@ D2� D8� D>� DE� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�� D�  D�@ D�` DÀ DƠ D�� D�� D�� D�  D�  D�@ D܀ Dߠ D� D�� D�  D�  D�  D�@ D�� D�� D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AC�AC��AD�AD(�AD5?ADA�AD5?AC��AC�wAC��AC��ACA@�RA?�^A>M�A=��A=�A=+A=/A<�A<ĜA<n�A<�A;��A;��A;A;�^A;�A;�7A;hsA;�
A;�A:�uA9�FA9+A7XA6VA4��A2��A2�A1��A09XA/C�A.�yA.n�A.�A-G�A,��A,$�A,5?A,ȴA*�!A(A�A%��A#O�A  �A��Az�A�A��A��A��A1'A"�A�/A �A 5?@�M�A�TA�!A�A�A�@�{@��@��`@ӕ�@�
=@�V@�~�@�+@��F@�I�@�K�@���@��D@�v�@�K�@�|�@��@�?}@���@�"�@��@�p�@��7@�7L@�t�@�Ĝ@~@y7L@u�@pA�@n�@m�-@l�@k��@j�!@ix�@f��@ep�@dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                AC�AC��AD�AD(�AD5?ADA�AD5?AC��AC�wAC��AC��ACA@�RA?�^A>M�A=��A=�A=+A=/A<�A<ĜA<n�A<�A;��A;��A;A;�^A;�A;�7A;hsA;�
A;�A:�uA9�FA9+A7XA6VA4��A2��A2�A1��A09XA/C�A.�yA.n�A.�A-G�A,��A,$�A,5?A,ȴA*�!A(A�A%��A#O�A  �A��Az�A�A��A��A��A1'A"�A�/A �A 5?@�M�A�TA�!A�A�A�@�{@��@��`@ӕ�@�
=@�V@�~�@�+@��F@�I�@�K�@���@��D@�v�@�K�@�|�@��@�?}@���@�"�@��@�p�@��7@�7L@�t�@�Ĝ@~@y7L@u�@pA�@n�@m�-@l�@k��@j�!@ix�@f��@ep�@dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�uB�hB�hB�hB�hB�hB�bB�PB�PB�\B�VB�%Bk�B^5BK�BD�B;dB>wB>wB:^B8RB5?B1'B0!B.B.B-B+B/B0!B<jBB�B6FB,B%�BPBB�B�TB�/B�B��BȴBƨBŢBB�qB�qB�qBȴB�BɺB�XB��B��B�hBt�B2-B�B��B�bBhsBA�BI�Be`BF�B^5B[#B�'BffBl�BA�BB�dB33B�B%B�!B��B��B��B��B�hB~�BS�BQ�B5?B'�B1'B)�B�BB��B�B�TB��B�B��BǮB��B�RB�!B��B��B��B�B�B�B�B��B��B��4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�B�B�B�B�B�B��B��B�B�B��By*Bk�BYkBR@BIBLBLBHBE�BB�B>�B=�B;�B;�B:�B8�B<�B=�BJBP2BC�B9�B3�B�B�BQB��B��B�B�kB�LB�?B�3B�$B�B�B�B�JB�B�OB��B��B�;B��B�DB?�B# B�RB��Bu�BOBW7Br�BT(Bk�Bh�B��BtBz*BO#B�B��B@�B%B�B��B�0B��B�cB�B��B�[BaVB_JBB�B5JB>�B7TB"�BsB	;B��B�B�EB�gB�EB��B��BŝB�mB�2B�AB�?B�KB�IB�RB�JB�7B�1B�84411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cz �CzCy[�Cyp�Cy��Cy�^Cx�+Cx1�CxE�Cw��Cv�;Cu_�CrO�Cq��Cq�sCr��Cr�CsqhCs�`Cs�Cs��Cr��Cs	�Cs�Cs0bCq��Cp��CpF�Ck�Cf��CbV�C_BNC]�/C]�C\KC\ZC[�;CYOCX�-CX�BCX��CW�oCY�uCX�CX5CWd�CWp�CU#�CR�CR�CR�PCMy�CI�CD9�C?��C:$ZC7VC3�-C4�'C4��C6��C8C�C9��C<^�C>�C?�5CAq�CC�C@9C<@ C=+C@z�CD��CJ=/CP�uCS9CV��C]+C_��CaN�C_��Ca��Cb��Ce��Ci<)CkCm�PCoR-Cn�'CpxRCr;�Cs5�Ct.VCv��Cw�}Cy�%Cx�TCy��Cz�RC{�?C}��C~�;C�dC~��C}�C|��C}��C~��C�C�sC��C�{3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                C��+C���C���C���C���C��qC�N�C��C���C���C�&SC�KC���C��C�"LC��C���C�"AC�-�C�8C�B�C��jC��pC���C��~C� pC���C�Q�C{_�Cv@Cp��CmY�Ck��Cj�CCi�Ci�%CiqCfp9Ce�(Ce�hCe�FCd�sCf��Cd�Cd��CduCd'�Ca��C]�iC^�C_CX��CTe�CN1CH�CB��C?1C;SC<2mC<�2C>LQC@�CA��CD�CFpjCH;�CJ(�CK��CH�*CD��CE�,CIF�CM�TCT�C[
�C]ѩCa�"Ch�DCku�CmaCk�Cm��Cn��CrobCvUCx�Cz��C|ԳC|C~fC�C���C�0C���C�%C�
�C���C�7�C��SC�MQC�M)C�ٚC�c�C�C�1�C��aC�e�C���C�{G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@+i-@+s@*�{@+�@+ @+$h@*�@*@*+4@)��@)&�@(#@%̸@%C�@%J�@%��@%�@&�?@&��@&�@&��@&6J@&C@&PS@&^@%H=@$ĉ@$?�@ �@�q@#@�b@�@>?@�&@�U@/@{@�@��@��@m�@��@~ @��@�@�@X@�@�@��@
��@�=@�q@ �?��?�]?�Ɓ?��c?�WJ?��?�ڜ?�q?���?� �@ &@a�@��@ lD?��?���@ �C@��@��@/�@��@T�@�@��@�2@�@;@��@(�@�@Ó@ ��@!��@![�@"�-@#޿@$�I@%9�@'�@'¿@)I@(�s@)=R@)�7@*�h@+��@,��@-L�@,�T@+�@+O�@,X@,��@,�VG�O�G�O�G�O�