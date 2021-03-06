CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:52:25Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                  t  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  SD   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  U   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  WP   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Y   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [\   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ](   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]�   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _h   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  a4   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ct   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  e@   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  e�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  g�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  iL   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  i�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  k�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  mX   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  m�Argo synthetic profile          1.0 1.2 19500101000000  20210302045225  20210302045225  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��;1   @��<�|��@Iv�t��0��Q�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99966 (+/- 4e-05) , vertically averaged dS =-0.013216 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114928                                          20210205114928A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C'  C/  C:  CC  CN  CX  Cb  Cl  Cu  C  C�� C�� C�  C�� C�� C�� C�� C�� C�� C�� C�  C�  C�� Cŀ Cʀ Cπ CԀ C�  C�  C�  C�� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^@ Dd  Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�� D�  D�@ D�@ Dƀ Dɠ D̠ D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C'  C/  C:  CC  CN  CX  Cb  Cl  Cu  C  C�� C�� C�  C�� C�� C�� C�� C�� C�� C�� C�  C�  C�� Cŀ Cʀ Cπ CԀ C�  C�  C�  C�� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^@ Dd  Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�� D�  D�@ D�@ Dƀ Dɠ D̠ D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�XA��A��A��+A��+A��+A��7A��PA��DA��DA�;dA��Ag��AN�AEA?�A>=qA<-A;+A:=qA9�7A8��A7��A6�HA5��A4��A4I�A3x�A2��A1��A0�A/�;A/C�A.��A.�jA.Q�A.(�A.I�A/"�A/��A/��A/��A/ƨA/�^A/�TA/��A/��A.�RA-�A,v�A*�A*A)�
A'�A%�A$�A!�wA�A�FAn�AjA	��A�uA��A��A  A�!A 9X@��/@��@畁@�M�@�  @�C�@�V@�$�@��@��@� �@�9X@�A�@�bN@�+@�{@���@��@���@�`B@�K�@�&�@�V@�1@�^5@�/@��;@��\@�p�@�1@}p�@y�#@yX@wl�@vȴ@u@u�-@u/@tz�@s@q��@q�7@p��@o�@o\)@nȴ3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A�v�A�XA��A��A��+A��+A��+A��7A��PA��DA��DA�;dA��Ag��AN�AEA?�A>=qA<-A;+A:=qA9�7A8��A7��A6�HA5��A4��A4I�A3x�A2��A1��A0�A/�;A/C�A.��A.�jA.Q�A.(�A.I�A/"�A/��A/��A/��A/ƨA/�^A/�TA/��A/��A.�RA-�A,v�A*�A*A)�
A'�A%�A$�A!�wA�A�FAn�AjA	��A�uA��A��A  A�!A 9X@��/@��@畁@�M�@�  @�C�@�V@�$�@��@��@� �@�9X@�A�@�bN@�+@�{@���@��@���@�`B@�K�@�&�@�V@�1@�^5@�/@��;@��\@�p�@�1@}p�@y�#@yX@wl�@vȴ@u@u�-@u/@tz�@s@q��@q�7@p��@o�@o\)@nȴ3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBv�Bs�B��B�B�B�B�B�B�B�B�B��B��B��BBBBbBB��B��B��B�B�sB�BB��B��BǮB�}B�FB�B��B��B��B�oB��B��B��B��B�!B�^B�wB��B��B��BÖBB��B�LB�B��B�PB�%B�1Bv�BcTBXBK�B8RB�B�B�B�?B�B�B�B�BVBuBBB�TB�B��B�B��B�-B��B��B��Bs�BQ�BC�B.B�BuBB��B�B�B�sB�5B�
B��B��B��B��BǮBÖB�}B�XB�^B�LB�RB�XB�jB�wB��B�wB�qB�}B��BBÖBĜ3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Bv�Bs�B��B�[B�[B�^B�\B�eB�^B�^B�\B�FB�DB�?B�\B�mB�sB�B�aB�NB�8B�B��B��BҙB�UB�"B�B��B��B�|B�+B�B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�aB�B�Bx�Bz�Bi.BU�BJyB>3B*�B-B�B�
B��B��B�B�B	B �B�B��B�B��B�B�YB�)B�AB��B�-B�?B�BfBBD}B6+B �BEBB��B�yB�FB�-B�B��BɬBĐBB�vB�cB�PB�9B�!B��B�B��B��B��B�B�B�(B�B�B�&B�/B�6B�=B�B3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cb?-CbbfCa�LCa�Ca��Ca��Ca��Ca�>Ca�8Ca�wCa�RCaG-C^��C]�JC`�C_��C^�rC_	C`l�Ca6CbB3Cb��Cc3OCc�Cc�lCd;�Cd-Cc�FCd3�Cd(�Cd�Cd=Ccl�Cb�1CbEFCb�SCcܱCd�Cd�bCf��Cg��Ch�Ch'�Cg�Cg�CgICf�dCd�aCa�YC^�rC[��CX:�CW�CXMCU�CU-CS�CN��CD�hC<(�C84C5�C4�nC3 C0�C0\�C0��C1��C3"�C5��C9y�C;]#C>�oC@��CC`�CG�CJ�_CNBCP��CS#�CW-JCZ�C]1�C`3�Cb�Ce�Cgu�CiiCk�Cle�CnsCo�CqC<CrMSCr��Cs
CtE�Ct�gCuͷCw5Cx(oCxK�Cx��Cx��Cx�gCwvhCv��Cv��Cw�Cw�CvImCu�#Cu6Ct�CrM�3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�CzSdCzR8CzM�CzO�CzR�CzRcCz$Cz)Cy�Cy��Cv��Cu��CxF�Cw�VCw�Cw"hCx��Cy�ACz��C{D�C{�AC|5<C|f�C|�9C|��C|��C|� C|�,C|�C|��C|!�C{��Cz�FC{#LC|�&C|ڨC}�DCĳC��C��!C��YC���C���C�E�C�%C}��Czd�Cw7�Cs�Co��Co�rCoèCm[�ClqDCk*�Ce��CZ[CP�eCL�CI��CH�aCF�CD��CC�fCD*�CEFsCGCI�eCN+�CPI�CTCV�CYC�C]�Ca�ZCe�uCh�Cj�ColqCs�{Cv++Cy�,C|�JC~��C��C��<C��)C���C���C���C�b{C��"C�NyC��WC��C��C���C���C�U�C�m9C���C��C���C�(C���C���C�֝C��6C�m�C��C��G�O�G�O�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144 G�O�G�O�@�P@�N�@�J�@�L�@�OC@�O@�"�@�'j@��@�t@���@�� @�Xl@��
@�$:@�?�@�J@��@�@�7�@�@��@�N%@���@��6@�@���@��S@�č@�!@�&@�J@��W@��@��@�n@�A@���@���@��@�.@���@��@�H�@��$@�b�@�`�@�Td@���@�NW@��Z@�,x@��	@��@���@�t@џ$@ȑ@Į�@���@��*@��@���@��@�Q�@�bZ@��@���@��@��@ˑ�@ͅ�@В�@�2@؛�@܂\@޿�@�@�ؿ@��@�Rf@��@�{�@���@�cZ@�,@�Un@���@�TA A�AaA��A��A>A��AA�A3qA�-A�ABqAdA�A~�A&�A)@AS&A]SA�}A�7Aa�G�O�G�O�