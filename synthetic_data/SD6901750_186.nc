CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   w   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:00:51Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302050051  20210302050051  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�';��-�1   @�'<�-��@G� ě��1�Q��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99957 (+/- 5e-05) , vertically averaged dS =-0.017029 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @   @@  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CW  Ca  Ck  Cu  C  C�  C�� C�  C�  C�� C�� C�  C�  C�  C�  C�� C�� C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�  D @ D� D� D  D� D@ D%� D,  D2@ D8� D>� DE  DK@ DQ@ DX  D]� Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�� D�� D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @   @@  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CC  CM  CW  Ca  Ck  Cu  C  C�  C�� C�  C�  C�� C�� C�  C�  C�  C�  C�� C�� C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C� C� C� C� C�  D @ D� D� D  D� D@ D%� D,  D2@ D8� D>� DE  DK@ DQ@ DX  D]� Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�� D�� D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aw|�Aw�Aw�Aw�PAw�7Aw�7Aw�hAw��Aw��Aw��Av�`An��AkAb��AV�uAO33AKG�AIXAGAD�jA@$�A?/A=ƨA=%A;��A9C�A7�A6{A5t�A4��A3��A3"�A2r�A1�A1�mA1�A0�A/dZA.bNA-ƨA-S�A-/A,�A,�A,��A,1A+�7A+��A+�hA+C�A*ȴA*jA*1'A*bA)�-A)��A)�A(JA'�A&{A%33A$$�A" �A (�A�hAZA�^AĜA�RAA~�A�A/A
��A�A�jA �@�+@�Q�@���@ߝ�@أ�@ԓu@�/@�
=@��@�S�@��@���@���@���@�?}@���@��R@���@��m@��^@���@�n�@�9X@���@��#@��j@~ȴ@|�/@{"�@zn�@zM�@x �@vv�@u/@st�@q%@o�P@nȴ@m�h@o��@n�@m��33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Aw|�Aw�Aw�Aw�PAw�7Aw�7Aw�hAw��Aw��Aw��Av�`An��AkAb��AV�uAO33AKG�AIXAGAD�jA@$�A?/A=ƨA=%A;��A9C�A7�A6{A5t�A4��A3��A3"�A2r�A1�A1�mA1�A0�A/dZA.bNA-ƨA-S�A-/A,�A,�A,��A,1A+�7A+��A+�hA+C�A*ȴA*jA*1'A*bA)�-A)��A)�A(JA'�A&{A%33A$$�A" �A (�A�hAZA�^AĜA�RAA~�A�A/A
��A�A�jA �@�+@�Q�@���@ߝ�@أ�@ԓu@�/@�
=@��@�S�@��@���@���@���@�?}@���@��R@���@��m@��^@���@�n�@�9X@���@��#@��j@~ȴ@|�/@{"�@zn�@zM�@x �@vv�@u/@st�@q%@o�P@nȴ@m�h@o��@n�@m��33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBdZBbNBcTBaHBe`Be`BdZBdZBdZBdZBaHB^5BO�B?}Bm�BN�BjBo�BdZB?}BPBaHB��B9XB�B�}B��Bo�Bm�BdZBu�B|�B�DB��B�Bo�Bq�B}�B�uB��B�B�B�B��B�\B��B�LB��B��B��B��B�FB�wB�}B�wB��B�dB�B��B��B��B��B�=B�Bw�B�7BaHB�B�B��B��B��B�B�BB�B�B��B��B�BS�B.B�B�B��BƨB��BhsBI�B.B0!B)�B�B\B��B�B�sB�NB�#B�B��B��B��BɺBƨBĜBBŢBȴBŢBƨBǮBǮBĜBŢBȴBɺB�B�
B�33311111111111111111444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             BdZBbNBcTBO�BS�BS�BR�BR�BR�BR�BO�BL�B>4B-�B[�B=6BX�B]�BR�B-�G�O�G�O�G�O�G�O�G�O�B��B�/B^B\BR�BdABkmBy�B��Bp�B^B`)BloB��B�@B��B�{B��B�@B}�B�,B��B�hB�!B�B�SB��B��B��B��B��B��B��B�xB�4B�&B�Bx�Bo�BfPBw�BO�Bq�B��B�+B�!B�#B��BθB1BșB�dB�$Bs�BB�B�BFBB�hB�QB�BBW)B8pB�B�B�BCB�"B��B�zB�>B�B��B��B��B��B��B��B�xB�nB�_B�pB��B�sB�~B�}B��B�lB�tB��B��B��B��B��33311111111111111111444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cq'�Cp�eCp��Cp�sCp�Cp�OCp�6Cp|�Cp9Cp+DCmĈCj�Cg��Cd?Cb!�Ca��C`�FCa
�Ca��Cc{�Ce��Cfx�Cf��Cec�Cd��CeCdh�CcTCb�"Cb��Cc&�Cc&�Cb�iCbJ.CaҜCa��CaJCa�C`��C`rC`u&C`z�Cao_Cc-wCc;�CbWCb[�Cc�bCb��Cb��CaW�C`D8C_�C_��C`&�C_��C]��C[[CY��CXύCZ�CY�kCS&	CN(C?�C1��C.|C/��C.�C+ӰC,GC,|�C.vC.*�C-�lC1J�C4��C7%�C:�C=��CA�CEoCGsGCJ�uCO\|CTwCX`#C[��C^�4C_�pCaѷCd�CfpNCi?CkezCm>�Cn��Co��Cp�DCrLCr��Css�CtPCt��CuN4Cu��Cu$�Ct�ICuJoCt�$Ct�Ct�_Cu0�Ct׉Cs�FCr�Cpk�Co�BCn��33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�C�I�C�IC�2�C�4C�C�DC��C��C��C�o<C�?C~kWC|kC{�Czu1Cz�\C{��C}�&G�O�G�O�G�O�G�O�G�O�CoHC~��C}�	C|�dC|�AC}^�C}a�C}�C|p�C{��C{� C{Z�C{#cCz��CzqyCzwqCz��C{�C}�yC}�+C|� C|��C~9�C}_�C} C{��Cz\�Cy�Cy�[CzC�CzCwf�Ct��Cs#�Cr�Ct)�Cs#Ck�QCfQ�CU�CF�HCB�+CD;�CB��C@�C@a�C@۠CB�ZCBƒCB/CFOCJ�CL��CP:CT�CY#C\�jC_?�CcSCh&aCmj�CrJ@Cv^Cy�>Cz�wC|�$C�'�C��C���C��@C��C�ʕC�vEC��rC��hC�90C��C�ՇC�JC���C��nC��YC�c�C���C�xhC�r C�`C��%C�s�C���C�dSC�OC���C��44111111111111111111444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�A �A ��A �A ��A ��A ��A �OA {�@�f�@�o,@�ލ@�>@��3@��@�p�@��D@��@�{�G�O�G�O�G�O�G�O�G�O�@�7�@�e@�e�@��@��@�<W@�?@��@�W�@��-@�3@�L�@��@�@�l�@�r�@�{�@��@�dL@�v=@�R@�@��@�=V@���@�}�@�YC@��J@�ȩ@�A!@�O@큑@�$R@�i�@�p�@�ej@�iX@�o�@�`@��n@��@�1q@�b;@���@�e�@���@�$�@��@��@�j�@�`d@� s@ĳ�@��@��@�S@�Ņ@�Q�@��@��*@��@�3@�=x@�n@�ԛ@���@��@��*@���@�?3@�C_A p�AaA�cAfxA�CA/A\XA�EA�AE�AA��A!&A��A�A�`A �A�AL_A�A��Ad�A �=