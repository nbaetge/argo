CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   w   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:55:39Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        >�EȠ�Q)        6\   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302045539  20210302045539  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��8�1   @��A���@I�����0��vȴ@1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99963 (+/- 4e-05) , vertically averaged dS =-0.014685 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114928                                          20210205114928A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A   A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�  C�� C�� C�� C�  C�  C�� C�� C�� C�� Cŀ Cʀ Cπ CԀ C�  Cހ C�  C� C� C� C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�  D�` Dƀ Dɠ D̠ D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @   @@  @�  @�  @�  @�  A   A   A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CW  Ca  Ck  Cu  C  C�� C�� C�� C�  C�� C�� C�� C�  C�  C�� C�� C�� C�� Cŀ Cʀ Cπ CԀ C�  Cހ C�  C� C� C� C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�  D�` Dƀ Dɠ D̠ D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A>I�A>A�A>E�A>A�A>A�A>E�A>E�A>Q�A>Q�A>VA>VA>^5A>ffA>jA>jA>n�A>v�A>v�A>v�A>n�A>~�A>�DA>�A>v�A>jA=��A8n�A4�A3%A2��A2��A1ƨA1C�A0��A0�+A0�A/�A/ƨA/S�A.ffA-�A-x�A,��A,^5A,1'A+��A+\)A*^5A)��A(�uA'�A&�HA&  A%��A%S�A%;dA%��A%+A$r�A$A#?}A"��A $�A�DAdZA��A$�A��A��A��A
 �A	VA��A��Av�A�`A�@���@��@噚@⟾@�$�@�G�@Ĭ@�+@�~�@��R@�?}@�K�@��@��#@�{@�|�@�r�@�p�@�  @�$�@�Z@�C�@��^@�1'@�S�@�$�@�X@l�@}��@{��@{@y�#@x1'@w+@u`B@s��@r�@qG�@pbN@n�R@m�T@m�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A>I�A>A�A>E�A>A�A>A�A>E�A>E�A>Q�A>Q�A>VA>VA>^5A>ffA>jA>jA>n�A>v�A>v�A>v�A>n�A>~�A>�DA>�A>v�A>jA=��A8n�A4�A3%A2��A2��A1ƨA1C�A0��A0�+A0�A/�A/ƨA/S�A.ffA-�A-x�A,��A,^5A,1'A+��A+\)A*^5A)��A(�uA'�A&�HA&  A%��A%S�A%;dA%��A%+A$r�A$A#?}A"��A $�A�DAdZA��A$�A��A��A��A
 �A	VA��A��Av�A�`A�@���@��@噚@⟾@�$�@�G�@Ĭ@�+@�~�@��R@�?}@�K�@��@��#@�{@�|�@�r�@�p�@�  @�$�@�Z@�C�@��^@�1'@�S�@�$�@�X@l�@}��@{��@{@y�#@x1'@w+@u`B@s��@r�@qG�@pbN@n�R@m�T@m�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\BoBhBoBoBhBoBoBuBoBoBoBoBoBoBoBoBoBoBhBhBoBhBhBbBJB�B�)B��B�
B�
B��B��BǮBɺBɺB��B��BȴB��B�wB�}B�RB�^B�jB�^B�RB�B��B��B��B��B�VB�\B�\B��B��B��B��B��B��B��B�{B�Bu�Bt�B��B�B��B�Br�B�Bm�B}�B��B�#BȴBÖB}�BF�BH�B��B�?B�wB��B�B\)BD�B.B�BbBB��B�B�B�fB�BB�#B�B��B��B��B��B��BŢBÖBB��B��B��B��B��B�}B�wB�qB�qB�jB�jB�w33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B\BoBhBDBBB:BBBBBHB@BBBBB@BBBDB@BBBBBBB9B<BBB9B<B3B�B�oB�B��B��B��B��B��B��B��B��B��B��B��B�_B�VB�[B�4B�<B�HB�@B�5B��B��B��B�tB�hB;B�DB�AB�eB��B��B��B��B��B��B�`Br�Bf�Be�B�oB��B��Bs�Bc�Bt B^�Bn�B��B�B��B�~Bn�B7�B9�B��B�KB��B��BrBM@B5�B2B�B�B�EB�B��BۨB׏B�kB�MB�<B�,B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CkBCkpCkG�CkK�CkN(Ckv�Ckx�CkKSCkMCkO�Ck\�CkE"Ck+�Ck>�Ck.TCk?Ck(CkLCk�Cj��Cj��Cj��Cj`pCj[Ch�CcSEC\"�CZ�?CY,�CV�#CWA�CU�CU��CW�`CZ!�C\;C\
�C[�C[�	CZ�>CX��CW�5CX�CXaBCW�PCW�VCV��CU�mCU7CS��CS'CR/eCQ^�CP�fCOK]CN�XCM��CJ�bCIe�CH�LCGwCD�C>��C9c�C60cC3V�C1�)C0��C/WAC.��C/_�C/OcC0�EC1�8C1��C3��C5��C7�|C<a�C@sCB�QCH��CMe*CN=!CQ�%CUV�CXסC\,C_~�Cc�Ce&,Cg��Ci?{CkG�Cm�Cm��Co[�Cpn9Cq }Cq��Cr��CsHZCs�"Cty�Cu�CvUCvP�Cv[<CvmCv��Cv��Cv��Cv�Cwn�Cw�#Cwt�Cw��Cv�Ct�933311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�C���C���C��LC��C��(C��"C��<C���C���C��<C��=C��RC���C��7C���C���C���C�|:C�oLC�`�C�-�C���C�EGC|�Ct��Cs3CqNFCn\�Co2(Cmu�Cme?Co�Crm�Ct��Ct�\Ctp�Ct�Cr��Cp�Cp�Cp��Cp��Cp =Co��Cn�#Cm�Cl�FCkx�Cj�WCi�UCh�!Ch"�Cf��Cf?/CdȶCa��C`�C_�8C]yaCZ,ZCT$�CNU_CJʼCG��CF�CD�mCC?|CB�CCR�CCExCD�CE�`CFGOCHDCJ��CL��CQ�eCV&�CYSC_�[CegCe�Ci��Cm�$Cq��CurCy]%C}hXC�WC�2YC�,�C�RuC�OnC��LC���C�@C��C�"�C���C��C�<+C��C�./C�x�C���C��'C���C��C��C���C�CC�aNC�z�C�k�C��	G�O�G�O�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144 G�O�G�O�@��/@���@��_@��@��.@��#@��@@��\@���@��i@��v@�Ǩ@���@���@��@���@���@��@�oJ@�Sd@��@���@�3@�s�@��=@�O�@�M@���@��@��e@��Q@���@�@�@���@��@�B4@�3h@�G$@�l@�(1@��,@�Z@�r@�:@��@�g�@��@�m�@�g�@ߋ�@�ٻ@�X�@�	t@ۡ�@،�@�l@֞�@ԝ|@�rC@˨s@��@®%@��<@�-@��@�p%@��,@���@�u�@�M@�f@�X�@�A[@�@�}�@ɔ8@͕�@�j;@�ֽ@��@���@�S�@�qj@�7�@렽@�c�@�Es@�~�@��@��@�#q@�	%@��A KA �[AD6A��A0�AvA�A VA�tA�2A'<A0%A=AkA_�Az�A�_A�KA��A�/A��G�O�G�O�