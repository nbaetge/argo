CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   w   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:00:06Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302050006  20210302050006  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @���[1   @����	 @HDZ�1�0�$�/ 1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99958 (+/- 5e-05) , vertically averaged dS =-0.016746 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @   @@  @�  @�  @�  A   A  A   A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CD  CM  CW  Ca  Cl  Cu  C�  C�� C�� C�  C�� C�  C�  C�  C�  C�  C�� C�� C�� C�  C�  C�  Cπ CԀ C�  C�  C� C�  C�  C� C�� D @ D@ D� D@ D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @   @@  @�  @�  @�  A   A  A   A�  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CD  CM  CW  Ca  Cl  Cu  C�  C�� C�� C�  C�� C�  C�  C�  C�  C�  C�� C�� C�� C�  C�  C�  Cπ CԀ C�  C�  C� C�  C�  C� C�� D @ D@ D� D@ D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dv� D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Av�Aw33Av��Aw"�Aw�Au�FAq�PAq�Ap��Ao�7Aj^5A^��A[�7AY33AW�wATZAPQ�AN�`AM��AKAJ�AJA�AI��AI`BAH��AG�AFI�AD�9AC"�AA�mA?�FA>�A<�A;�
A;��A:�RA9
=A85?A6�A5�A4ȴA3��A3?}A2�`A2n�A1��A1�A0r�A/�hA/VA.�uA.ZA.=qA-��A-+A-�A,��A,5?A+�
A*��A)+A& �A ĜAbNAK�A�`A%A��A��A�TA�A�AJA	��A	�A�A|�A7LA 1@��;@���@�A�@���@�(�@�C�@��@ȴ9@ċD@���@� �@��@�C�@�=q@��@���@��@���@�5?@���@�r�@�V@�V@�Z@�ff@�ȴ@��@� �@�j@y��@xb@v@tZ@rJ@p��@p�u@n�@mp�@k��@ko33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Av�Aw33Av��Aw"�Aw�Au�FAq�PAq�Ap��Ao�7Aj^5A^��A[�7AY33AW�wATZAPQ�AN�`AM��AKAJ�AJA�AI��AI`BAH��AG�AFI�AD�9AC"�AA�mA?�FA>�A<�A;�
A;��A:�RA9
=A85?A6�A5�A4ȴA3��A3?}A2�`A2n�A1��A1�A0r�A/�hA/VA.�uA.ZA.=qA-��A-+A-�A,��A,5?A+�
A*��A)+A& �A ĜAbNAK�A�`A%A��A��A�TA�A�AJA	��A	�A�A|�A7LA 1@��;@���@�A�@���@�(�@�C�@��@ȴ9@ċD@���@� �@��@�C�@�=q@��@���@��@���@�5?@���@�r�@�V@�V@�Z@�ff@�ȴ@��@� �@�j@y��@xb@v@tZ@rJ@p��@p�u@n�@mp�@k��@ko33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�qB�B�B�PB��B��B��B��B��B��B�qB��B��B��B�/B�B�B�sB�/B��BǮBĜB��B��B�^B�!B��B�JBz�Bk�BS�BC�B1'B.B1'B(�B�B�BbB+B%B��B��B��B��B�B�B�B�ZB�fB�ZB�ZB�fB�HB�5B�BB�)B�B�
BȴB�wB��BgmB+B_;BffBZBJ�B��B��B�B�'B�B��BÖB�?B��B�#BɺB��B�BW
BN�B7LB#�B�B��B�B�B�wB�B��B��B��B�1B�B|�BcTBD�B5?B&�B�BoBDB��B�B��B�ZB��B��B��B��B��B��B��B��B��B��B��33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B�qB�B�B{�B��B�~B��B��B��B�wB�B��B��B��B��B�6B�8B�B��B�xB�UB�BB�2B�,B�B��B�\Bz�Bi�BZ:BB�B2PB�B�B�B�BWBNB�'B��B��B�B�B�B�B�pB�QB�CB�"B�-B� B�#B�-B�B� B�B��B��B��B��B�FB��BVJB�BNBUDBH�B9�B�mB��B��B��B��B��B�lB�B��B��B��B��Bs�BE�B=�B&9B�B}B��B�B�B�xB�B��B��B��Bw<BqBk�BRdB3�B$VBB�B�B�aB�B��B��B�B�#B�B�B�	B��B�B�B�B�B�B�33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cw�Cv��Cvj?CußCuh�Cu��Cv��Cu�YCuoCCu�Cq &Cj�Cd\�Cbx�C` :C_��C`N\C`B7C_�C_�YC_��CaU�Cb�GCb�qCa�IC`�C_LC]�C\��C[��CZ��C��CZ�;CY�CZc�C[�C[�xC]�_C[�C^�Cc[BCc��Cc	Ca�eCa�(Ca�LCd�Ce{OCe�tCe83CcM�Cc��Cc[;Cb#�CbŚCb�Ca��Ca��CaOOC[$�CZ��CJ��C;|�C2�C0��C-�MC, nC+��C,7PC-��C-��C-�=C.�C/.,C06|C2�C4\,C3�"C6q2C9dC<̤C@��CC��CFedCH��CJ�rCM�5CP2CR��CU��CWz+CYZCZ�6C\�aC^T>C_o	C`�Cck�Cf5Cg�Ci52Cj�qCk\4ClP�Cn�Cn_�Cn��Cq0Cr��Cr�Cs%Cs|CsM�Cr�;Cr`rCrCq��Cq9�Cp&�33311111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�C�<�C��CC���C�˒C�Z�C��,C��C�|�C�L�C�c�C~f`C|M�Cy��CyENCy�Cy�CyrCy�?Cy�{C{�C|ˏC|�pC{�)CzE�Cx�CwT�Cv�Ct�ZCt�G�O�Cs�1Cr�`Cs�bCtL�Ct�mCw�CuQCxomC}��C~�C}:%C{�C{��C{��C~z�C� �C�@0C�!C}�VC~_C}��C|V�C}�C}5hC{��C| :C{vSCt��Cs��Cb,NCQS�CG��CE��CB>�C@AC?��C@dfCA��CB$sCBmFCC-�CC��CD��CG�CI��CIB�CL�COVzCS+�CW��CZ��C]��C`x�Cc�Cf:�Ch�ACk�-Co(�Cq?�Cs_bCu'�Cw
YCy%CzH�C{�C~�\C��*C��PC��+C�q�C��C�u�C�z�C��tC��-C�(C�)QC�H�C�T�C�W�C��+C�H�C��C���C���C�f�C��Z44111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�A��Af�A6AR�A�!AtA:ZA�A �x@�X�@�9R@�6e@��@�L�@���@��[@�w�@�Q@@�
@��@�@�@�C%@��@�p!@�C�@�8@�?	G�O�@�&�@�;�@���@��@�$@@�!)@�4w@�s@�l*@��6@�@���@�@���@�M@��@@�=�@��h@�vf@��F@�@�>�@��r@��@��@�
v@�gZ@��L@�4h@� J@��Z@��x@��@�y�@��T@��
@��M@�=
@�`n@��W@�_#@���@��@�d@���@�5�@�ۤ@�W@ʹN@�n@� �@��@�~,@�@�@ߑ�@�j@��@癄@�@@�Y�@�(�@�.@�E�@�@��:@���@�b�@�� @�_�@�E�@�SA $A K�A �A�A�A�mA��A��A
fA�XA�#Aj�A=lA�(Aj�