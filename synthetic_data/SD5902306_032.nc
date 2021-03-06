CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-30T04:03:15Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  l�Argo synthetic profile          1.0 1.2 19500101000000  20201030040315  20201030040315  5902306 OVIDE                                                           Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                                A   IF  DDDDPROVOR                          OIN-09-S3-DO-013                9,02                            841 @�ዙ���1   @��z�G�@Hu�����/��j~��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                      DOXY_ADJUSTED= A * DOXY + B                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0003 (+/- 0.0001) , vertically averaged dS =0.013217 (+/- 0.01)                                                                                                                                                                                            A= 0.997; B= 14.73                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A=1.134; B=-6.021                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                           Bias in oxygen data is corrected as a linear function of DOXY from calibrated CTD data acquired at float deployment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          20151016161459201510161614592015101616145920130604092413                                          20170502151424A   A   B   B       @�  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D � D� D� D  D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DX  D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�` D�� D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�` D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D܀ Dߠ D�� D�� D�  D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     @�  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Cl  Cv  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D � D� D� D  D@ D� D%� D,  D2@ D8� D?  DE@ DK� DQ� DX  D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�` D�� D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�` D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D܀ Dߠ D�� D�� D�  D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AOAO��AO��AO��AO�AK�-AH�AFE�AE&�ADz�ACp�AB=qAA��AA?}A@�A?C�A>��A=A<�A<Q�A;��A;+A:��A:VA9�PA9/A9A8�!A8ĜA8�RA8��A8��A8��A8�`A9VA8�A8ȴA8��A8�DA89XA7�#A7�PA7l�A7XA6�/A6^5A6r�A5�^A5`BA4I�A3�7A3;dA3VA2��A1A0�\A/S�A.bNA,��A*��A(�+A'�A%�A!A�9AVA��A�A�!A33A
�A��Ap�A ��@��m@�v�@� �@�1'@�j@���@���@Ώ\@��@�j@�E�@��@���@��^@���@�dZ@���@��;@���@��@��@��`@���@��
@�-@�V@���@�%@�  @�;@
=@{t�@w;d@u��@t9X@r�!@q7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             AOAO��AO��AO��AO�AK�-AH�AFE�AE&�ADz�ACp�AB=qAA��AA?}A@�A?C�A>��A=A<�A<Q�A;��A;+A:��A:VA9�PA9/A9A8�!A8ĜA8�RA8��A8��A8��A8�`A9VA8�A8ȴA8��A8�DA89XA7�#A7�PA7l�A7XA6�/A6^5A6r�A5�^A5`BA4I�A3�7A3;dA3VA2��A1A0�\A/S�A.bNA,��A*��A(�+A'�A%�A!A�9AVA��A�A�!A33A
�A��Ap�A ��@��m@�v�@� �@�1'@�j@���@���@Ώ\@��@�j@�E�@��@���@��^@���@�dZ@���@��;@���@��@��@��`@���@��
@�-@�V@���@�%@�  @�;@
=@{t�@w;d@u��@t9X@r�!@q7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBo�Bo�Bp�Bp�Bq�B�B�1Bz�Bx�Bz�Bv�Bu�B|�B�B~�By�B}�Bp�BjBhsBgmBdZBcTBaHB\)B\)B_;B`BBe`Be`BffBhsBl�Bo�Bq�Bq�Bo�Bm�Bl�BiyBdZBaHB`BB`BBYBS�BT�BL�BH�B9XB0!B/B2-B1'B+B �B�BuB+B��B�fB�BB��B�^B��B��B��B��By�BhsBaHBq�Bw�BXBO�BO�BA�B5?B�B#�B��B�
B�hBr�Br�BR�BD�B33B�BJB��B�B�B�TB�/B�#B��BȴBB�wB�FB�-B�-B�RB�XB�-B��B��B��B��B��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�B~MB~MBWB��B��B��B��B��B�uB�lB��B��B��B��B��B~LBx#BvBuBq�Bp�Bn�Bi�Bi�Bl�Bm�BsBsBtBvBz0B}GBRBQB}HB{<Bz4BwBr Bn�Bm�Bm�Bf�Ba�Bb�BZpBVXBF�B=�B<�B?�B>�B8�B.bB'6B!B�BSB��B��B߁B��B�qB�.B�/B�B�cBu�Bn�B/B�TBe�B]aB]aBO
BB�B,/B1RBDB�{B��B�B�B`UBQ�B@�B(�B�BOB��B��B�B�B�tB�@B�B��B��BÏB�xB�uBŜBƥB�xB�EB�=B�:B�:B�4441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C�\G�O�C�\C��sC�AhC|0bC{` C{lJCy�7Cxg�Cw��Ct�Cq��Co:^Cm�LCl6Ck}/Ck�Ck��Ck�JCl��Cm]/Cn6�CnGmCnWLCo0�Cp
=Cp�Cp��Cq�CqdCq�
Cr
CrqCr1hCrDCrWLCrh�Cq��Cq�+CqbCq"�Cq5�Cp~�Co��CoٚCo�PCo��Cp'Cp�PCp��CpU�Cn�VCl��CkO�ChN�Cf�HCc�5C^wLCWt9CS�/CO�`CJS�CD�dC?bC;yXC9��C8t{C8�C9f�C:H�C;�uC>nVC@�=CCj=CE�mCHi7CJ#TCL�CO&fCSCCW`�C]�+C`j=Cb��CfP�Ch�Cj��Cm�dCp�DCrO\Ct�Ct@�Cv�Cw�Cw�;Cx�jCz�mC{�BC{�C|�yC}C}EC|�NC}��C~��C~�=C~�C'mC}�oC|.3 3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333     G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                    C��NG�O�C��C���C�޾C�>�C���C���C��"C��C���C��C�`C�6C}�	C|[C{J]C{U�C{b�C{qC|c�C}U�C~J�C~Z�C~h�C]C�)�C�2C���C��:C��gC�?�C�K�C�V�C�bpC�l�C�w.C���C�KC�#�C��C�ĮC��!C�g6C�C��C�C��C�#=C���C���C�C�C~�YC|lPCz�?Cw^+Cu��CrH�Cl�CdVC_�1C[S	CU&CN��CH�'CD�CBCC@{�C@m�CAYmCBFuCDCF��CI�CLVCO�CQ�CSÀCVVCYMWC]�+Cbc�Ci��ClU`Co4mCr��Ct�Cw�5C{`&C~71C�C�C�$6C�",C��
C�:�C��C���C�N�C�f�C��yC��C��C��/C�R(C���C���C�G�O�G�O�G�O�1 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444 @4[EG�O�@4w$@3�@1��@-f@,zO@,|�@+`,@*F@)��@'{@%<�@#�@"u$@!Zx@ �o@ ښ@ ��@ �4@!�t@""\@"�@"Ɉ@"�?@#n�@$)@$,@$�q@$�!@$�(@%p�@%�@%��@%��@%��@%�E@%�W@%A�@%L�@$Ɖ@$��@$�*@$[&@#�|@#߃@#�@#�(@$%@$��@$��@$. @#(�@!� @ �\@P�@B�@�@�@��@-�@^@cL@]�@ Y?���?���?�a?�N�?�|�?��?�F?���@ �@�z@�@N�@�\@	GA@�@�@�@��@@�@P@s�@��@~@ �`@"��@#��@%1�@%M@&�$@'E,@'��@(��@)�@*�:@*�(@+q?@+�7@+��@+7F@+�H@,��@,�J@,ݖG�O�G�O�G�O�