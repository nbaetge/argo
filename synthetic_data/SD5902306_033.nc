CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:03:26Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030040326  20201030040326  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               !A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @��1   @��NQ)`@H�V��/���"��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013213 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161459201510161614592015101616145920130604092415                                          20170502151424A   A   B   B   ?�  @�  A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  CՀ C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,@ D2@ D8� D>� DE� DK@ DQ� DX  D^  Dd@ Dj� Dq  Dw@ D}� D�  D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�` D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` DƠ D�� D�� D�  D�@ D�  D�@ D�` Dߠ D�� D�� D�  D�  D�  D�` D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  @�  A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�  C�  C�  C�  C�  C�  CՀ C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,@ D2@ D8� D>� DE� DK@ DQ� DX  D^  Dd@ Dj� Dq  Dw@ D}� D�  D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�` D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` DƠ D�� D�� D�  D�@ D�  D�@ D�` Dߠ D�� D�� D�  D�  D�  D�` D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AO`BAOx�AO|�AO|�AO7LAM��AMK�AM/AL�9AJ�AE�AD=qAC��ABv�AAt�A@5?A>ĜA>VA=ƨA=+A<��A<�A<��A<-A;�hA;�A;%A:�DA: �A9�7A8ĜA8I�A8$�A7�mA8-A8jA81'A8Q�A8�DA8E�A7�A7O�A6-A5hsA4�A4��A4�HA45?A3�;A3�A3K�A3�A2�A2�A1�A0��A/�mA.��A,ȴA*��A(�A%A ^5A�
A�AJAdZA�#A�A��A(�A��@�=q@�{@�t�@��y@���@��@�S�@��;@�G�@�(�@�ff@�O�@���@�~�@��@�/@��+@�@�|�@�%@��R@���@��@���@�9X@�^5@��@}@|��@zJ@x�`@y��@z=q@yhs@y�@u�T@tI�@qx�@nff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             AO`BAOx�AO|�AO|�AO7LAM��AMK�AM/AL�9AJ�AE�AD=qAC��ABv�AAt�A@5?A>ĜA>VA=ƨA=+A<��A<�A<��A<-A;�hA;�A;%A:�DA: �A9�7A8ĜA8I�A8$�A7�mA8-A8jA81'A8Q�A8�DA8E�A7�A7O�A6-A5hsA4�A4��A4�HA45?A3�;A3�A3K�A3�A2�A2�A1�A0��A/�mA.��A,ȴA*��A(�A%A ^5A�
A�AJAdZA�#A�A��A(�A��@�=q@�{@�t�@��y@���@��@�S�@��;@�G�@�(�@�ff@�O�@���@�~�@��@�/@��+@�@�|�@�%@��R@���@��@���@�9X@�^5@��@}@|��@zJ@x�`@y��@z=q@yhs@y�@u�T@tI�@qx�@nff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�PB�7B�1B�1B�%B�B� B~�B{�B{�Bz�By�B}�B|�By�Bt�Bl�Bm�BjBe`BhsBjBl�BjBgmBe`Be`BbNB^5BYBQ�BN�BP�BQ�B]/BcTBbNBe`BiyBgmBcTB[#BK�B@�B;dB;dB?}B8RB5?B33B1'B1'B-B(�B%�B�B�B\B  B�B�NB��B��B��B�B�BŢB��B�-BhsBv�BcTBG�BF�B=qB>wB33BDB�sB��B�3B��B�7Bw�B^5BQ�B2-B{BPB��B�B�fB�/B��B��B��B�XB�-B�B��B��B��B��B��B�B�B�!B�B�B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B�fBz6B{:Bx'Bs	BvBx%Bz3Bx'BuBsBsBo�Bk�Bf�B_�B\}B^�B_�Bj�Bp�Bo�BsBw!BuBp�Bh�BYhBN%BIBIBMBE�BB�B@�B>�B>�B:�B6�B3�B+PB$#B�B�B�>B��B�pB��B��B��B��B�3B��B��Bu�B�LBp�BU.BT(BJ�BK�B@�B�B��B�.B��B�7B��B�3Bk�B_KB?�B!�B�B)B��B�B�B�AB�B��BƦB�wB�MB�'B�0B�B�B�BB�XB�KB�lB�SB�JB�2B�441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C��'C�S�C�^�C�iCVFC|A�Cz��Cx�Cw�CwCv�CunVCs��Cq��CoS�CmϞClKDClZ�Ck��Cj�DCj��Ck�FCk�%Ck�
Cm��Cnr-Cn��Cn�Cm�;Cm�!Cn �Cn�Cn#TCn��CoVCo �Co0�Cp��Cp�JCp��CpG�Co�!Co��Cn�JCn2-Cmz�ClCl	�CkQhCj�RCj�Ci��Cg�Ce�XCd��Ca/C^,JCZZ^CWS�CQ�CJVCEcTCA��C=�)C9��C7eC6��C7�uC9=�C;�3C>*C@��CC�CE�dCH�CJ��CM�CQ6FCUR-CX��C[(1C]�hC`;�Cb�=Cf5Ch�Cl�Cn��CpZ^Cr�Ct��Cu��Cv��CxnCyjC|NC|�C~ȴCǮC�CO�C�&�C�C�C~n�C~��C}�C~&�C~V�C�uC}�}333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                             C�7CC�͗C��JC��3C�C�M�C�s'C�+C�P�C�J�C�6�C�VXC�y�C�(yC��C}��C|2�C|A�C{nCz��Cz�C{�C{�9C{�C}�VC~��C~�`C~�$C}�C}�ZC~�C~�C~%�CC0�CG!CX&C���C���C��CC�IpC�AC̹C~�OC~$fC}S�C|�RC{��Cz��Cz�CzMCyV�Cv��CtTaCs�8CoKCkۉCgCd�C\�4CT�wCO�	CJ��CF��CB{C?UIC>��C?~"CAA�CC��CF�-CIG�CL��CN��CRd�CTCqCW�C[��C`�Cc�eCf��Ci[1Cl(]Cn�nCr�Cu��Cy5BC|�C}��C�j8C�g�C��qC�y�C�w�C��C�u@C���C���C���C��oC�D�C��,C�wC��C���C��rC��C���G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @0�@0cQ@0wi@0��@/q�@-06@,�@*t�@)]n@)UA@);�@(�@'@%R�@#��@"��@!h@!q�@ �G@ b�@ n!@!�@!/@!!�@"N�@"�L@"��@#J@"}'@"�D@"��@"�L@"�~@#E2@#Rb@#`�@#k�@$��@$��@$�7@$5
@#��@#�9@#/�@"��@"!@!��@!@ �@ j@ �@��@�,@^�@�a@%�@��@(Y@�<@ai@;�@��@�f?��?�[?��?���?�T?�^a?��*?�0�@ ��@�@Q�@��@�>@
2�@��@o�@�K@�<@Y@$@�z@N�@ ]@~?@!Iw@"�A@$_ @%�:@&R|@'q@(G@(�z@*Ӆ@+�:@,�s@-��@-�o@-$�@-��@-e8@,�@,~v@,��@,(�@,EG�O�G�O�G�O�