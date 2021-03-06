CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:46:20Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20201028144620  20201028144620  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @�;I��J1   @�@[��@H]O�;dZ�3H�9X`1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014266 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559262015101515592620151015155926                                                        20161116115858A   B   B   B       @   @�  @�  @�  A  A@  A`  A�  A�  A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�� C�� Cŀ C�  C�  CԀ C�  Cހ C�  C�  C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8� D?  DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�  D�` D�` Dɀ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @   @�  @�  @�  A  A@  A`  A�  A�  A�  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�� C�� Cŀ C�  C�  CԀ C�  Cހ C�  C�  C� C� C�� D @ D� D� D  D@ D� D%� D,  D2@ D8� D?  DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�  D�` D�` Dɀ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�;dA�=qA�=qA�=qA�=qA�;dA�=qA�?}A�=qA�;dA�=qA�1A�A�ƨA}VAj1'A_�A[p�AXJAV=qATE�AR�AO�#ANĜAN��AQhsAP�AL��AJ�AIhsAG�AG/AG33AF  AEK�AD��AD{ACl�ACS�AB�uAB��AA&�A@�/AA�
A@9XA>I�A;��A9K�A6��A5&�A3+A1�A0A1�PA4Q�A3��A2bA/��A-�hA&�DA!�AJA��A�PA�PA�`A�AbA%@��@�v�@���@�@���@�{@�V@Ǖ�@�t�@š�@��@�ff@��y@��m@���@���@��@��9@��D@��+@�|�@�|�@���@�{@��
@���@�33@���@�t�@��@�t�@��#@���@�V@��@;d@}�@zM�@w�;@v{@t��@s�@q�#@n@ko@i��@ihs@h��@f��@d1@cdZ311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�A�=qA�=qA�;dA�=qA�?}A�=qA�;dA�=qA�1A�A�ƨA}VAj1'A_�A[p�AXJAV=qATE�AR�AO�#ANĜAN��AQhsAP�AL��AJ�AIhsAG�AG/AG33AF  AEK�AD��AD{ACl�ACS�AB�uAB��AA&�A@�/AA�
A@9XA>I�A;��A9K�A6��A5&�A3+A1�A0A1�PA4Q�A3��A2bA/��A-�hA&�DA!�AJA��A�PA�PA�`A�AbA%@��@�v�@���@�@���@�{@�V@Ǖ�@�t�@š�@��@�ff@��y@��m@���@���@��@��9@��D@��+@�|�@�|�@���@�{@��
@���@�33@���@�t�@��@�t�@��#@���@�V@��@;d@}�@zM�@w�;@v{@t��@s�@q�#@n@ko@i��@ihs@h��@f��@d1@cdZ444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BF�BH�BH�BH�BI�BI�BH�BI�BI�BH�BK�BQ�BP�B�9BD�BT�B5?B{BB��B�fB��BɺB��B��B�BǮB�?B��B�DB�PB�uB�1B�B� Bz�Bx�B{�Bt�B{�BiyBo�B�Bm�BP�B0!B\B�B�#BB�3B��B�}B�B�B�5BŢB��BR�B$�BbB�BO�B6FB�LB��B��B�RB�LB��B��B�`B�hBz�Bl�B_;Bq�By�BgmB�uB�JBm�BC�B"�BPB��B�B�yB��B��B��BB��B�B�mB�NB�B��B��BȴB��B�qB�^B�LB�?B�!B�B�B�B��B��B��B��B��B��B��B��B��B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�BW�BW�BX�BX�BW�BX�BX�BW�BZ�B`�B_�B�BS~Bc�BDB#VB�B�B�=B��B؈BްB
�B�vBփB�B��B�B�B�AB��B��B��B��B��B��B��B��Bx<B~dB��B|YB_�B>�BB�GB��B�BB��B�uB�,B�LB�4B��B�RB��Ba�B3zB�B)9B^BD�B��B�4B��B��B��B�lB�iB��B��B�aB{Bm�B�)B�UBu�B��B��B|
BRB1DB�B
RB�B��B8B`BBBpB?B��B��B�B�}B�IB�2B�B��B��B��BŬBßB��B�nB�bB�`B�[B�UB�0B�B�	B�B�*B�B�B�444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Ch�vCh��Ch�`Ch��Ch��Ch��Ch��Cg�?Cg�Cg�,Cg%�CfeBCd��Cb��C]w�CX�?CW�CW�CVX�CVc�CVn�CWA�CX�CX�C[ZoC]��C\G8C[�AC[��C\qeC\��C[�C[��C\�zC[�DC\�C\�C[e:C[wAC[�pCZ�[CZCZ&TCYp
CX�}CX��CW@�CXCX%yCWk�CWz?CV��CU8$CR�hCO��CNS�CM�YCLԱCL�CH��CA+�C7ŽC2V�C/E�C,/�C,2�C-�C-�[C1 �C3��C7��C: C<�OCB2�CGCK#�CO8,CP(�CR�RCU��CUW�CW�iC\}C`Cd;�Cg��Cj�Cl�5Cm��Cm}Cn	Cn1�Cm��CoY�CpRCqM�CrI�CtCu�Cv
�Cv6#Cx
Cx/�Cx]�Cy]�Cy�Cy� Cz�0Cz�!C{JCz{C{{�C|y�C|�RC|؊C|8�C|h�C}jC}��C|��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Cs�CsWCs"cCs%XCs'LCs*�Cs0�Cr��Cr�+Cr�Cq�+Cq4XCo�Cm�Ci�Cf�VCe��Ce��CeQ6Ces7Ce�XCfpCgJPChhCj:�Cl?�Cj��Cj�ICj��Ck�Ck�]Ck
�Ck�Ck�CkQCkg�Ck�~Cj�qCj��CkiCjk�Ci݌Ci�Ci>�Ch��Ch�/Cg��Ch�'Ch��Ch1CCh[�Cg�1Cf�,Cdh�Ca��C`5�C_��C_xC^��C\C�CU��CM��CHʨCF,CC�CDU�CEsxCFb�CIpGCK�NCO�_CQ�?CT$,CY��C^Q�Cb/'Cf�Cf܆Ci);ClP�Ck��Cm�WCq�uCu��CyͩC|��Cd�C��C�^�C�!C���C��C�WWC�,C��?C� C��>C�mC�� C�_HC�z#C�Q&C�kzC���C���C�
C�4cC��qC��C�ݐC���C�zC���C���C���C�x4C���G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@�@�x@��@ޓ@��@�u@��@i@k�@l�@�=@t@|@�@�4@
�=@	�
@	�=@	.�@	5�@	<�@	��@
K<@
��@b�@�A@�a@��@� @_@�@�?@��@@N@�i@ը@�[@i�@uH@�H@@��@��@(�@
��@
��@	��@
L1@
Ul@	ޟ@	��@	o�@u�@�l@@�@��@|@�g@ �?�Bf?�:�?�FM?�Y?�f-?�i�?�{�?ޔ�?�]?��?�
?�5�?�i[?���?���@]@��@8�@�?@�=@�@
*�@��@n�@�@5�@��@y@�@��@T�@q�@?@/@�'@o@�@5�@ձ@w�@�J@�D@��@�M@�Q@�@�{@ w�@ �|@ �N@ N�@ ��@!��@!�h@!�D@!k�@!��G�O�G�O�G�O�