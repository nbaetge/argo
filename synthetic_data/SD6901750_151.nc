CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   v   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:52:10Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  ST   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  U,   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W|   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  YT   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  ]|   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]�   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  a�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  b   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  c�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  e�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  fD   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  i�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  jl   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  lD   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  n   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302045210  20210302045210  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�ϻ1   @�ϼZ��@@I~��+�0����@1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99967 (+/- 4e-05) , vertically averaged dS =-0.01311 (+/- 0.01)                                                                                                                                                                                            C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114928                                          20210205114928A   B   B   B       ?�  @   @@  @�  @�  @�  A   A  A   Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cv  C  C�� C�� C�� C�� C�  C�  C�� C�� C�� C�� C�� C�� C�  C�  Cʀ Cπ CԀ Cـ Cހ C�  C�  C�  C� C�  D @ D� D� D  D  D� D%� D,  D2@ D8� D>� DE  DK@ DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�� D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` D�� D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      ?�  @   @@  @�  @�  @�  A   A  A   Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cv  C  C�� C�� C�� C�� C�  C�  C�� C�� C�� C�� C�� C�� C�  C�  Cʀ Cπ CԀ Cـ Cހ C�  C�  C�  C� C�  D @ D� D� D  D  D� D%� D,  D2@ D8� D>� DE  DK@ DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�� D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�` D�� D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��mA��yA��yA��yA��mA��mA��`A��yA��mA��;A�l�A��+Al�A\bAU"�ALZAC��A@�A>^5A=�mA<v�A:ĜA9G�A7��A6��A5�wA4�/A4�A3%A2jA1t�A0$�A/\)A.v�A.5?A.M�A.�A.�DA/K�A/��A/��A/��A/�TA/�A/�A/�mA/�;A/�^A/t�A.�RA-\)A,1A+�A*ZA)33A(=qA'��A&$�A$��A#`BA"�A��At�A��A��A�A��A�^A
�A��@��P@���@��!@��
@�{@�@�z�@�Ĝ@��@�V@�X@��@�&�@�1@�;d@���@��#@�j@�1@��@��@�p�@�7L@���@���@��#@�z�@�;d@�n�@��@��;@�n�@�p�@�w@~$�@}p�@{�
@z^5@y%@w|�@u��@t��@r~�@q%@p �@o;d@n��@m�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A��mA��yA��yA��yA��mA��mA��`A��yA��mA��;A�l�A��+Al�A\bAU"�ALZAC��A@�A>^5A=�mA<v�A:ĜA9G�A7��A6��A5�wA4�/A4�A3%A2jA1t�A0$�A/\)A.v�A.5?A.M�A.�A.�DA/K�A/��A/��A/��A/�TA/�A/�A/�mA/�;A/�^A/t�A.�RA-\)A,1A+�A*ZA)33A(=qA'��A&$�A$��A#`BA"�A��At�A��A��A�A��A�^A
�A��@��P@���@��!@��
@�{@�@�z�@�Ĝ@��@�V@�X@��@�&�@�1@�;d@���@��#@�j@�1@��@��@�p�@�7L@���@���@��#@�z�@�;d@�n�@��@��;@�n�@�p�@�w@~$�@}p�@{�
@z^5@y%@w|�@u��@t��@r~�@q%@p �@o;d@n��@m�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�B�B�3B�FB�FB�FB�FB�FB�'B�B��B�BbBhB,B-B"�B'�B�B1B��B�B�NB�B��BƨB�jB�LB�B��B��B�bB�hB��B��B��B�?B�dB�wB��B��BBÖBÖBÖB��B�}B�LB��B��B��B�PB�B{�Bu�BgmB^5BXB`BBP�BA�B�B�B$�BhB�B�B��BȴB�TB�B��B��B
=B��B�BB�B�!B�VB|�By�BhsBXBM�B8RB"�BuB\B	7BB��B�B�`B�#B�
B��B��B��B��B��BɺBĜBÖBÖBB��B��B��B�}B�}B�wB�wB�}B��BBĜ3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B��B��B�B��B��B��B��B��B��B��B��B��B�GBB�B�BtB}B>B]BB��B�TB�B��BʇB�SB�!B��B��B��B�-B�
B��B��B�B�2B�QB��B��B��B��B�B�B�B�B�B�B��B��B�oB�B�B�Bu�BngBhIBY�BP�BJ�BR�BCmB4BIB#BlB�B8B
BĈB�PB��BB�SB�cB��B�zB��BȞB��B��Bo�Bl�B[BJ�B@�B+B�B,BB��B��B�yB�MB�B��B��BǻBǼBĬB��B��B�{B�_B�XB�UB�QB�JB�GB�GB�@B�AB�8B�;B�AB�CB�PB�`3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CewsCev?CeM�CeN�CezuCeRYCe-�Ce�Ce�Cd��Cd)Cb�2CbCa�MC`�C_wC^�C_��C`�Ca:(Ca�pCbCb��Cc4Cc��CcȤCc�Cca�Cbk�Cbf CcU	CdCc�>Cd>9Cd�JCe�:Cf��Cg8�Cg�Ch��Ch߲Ci�CioCCi|�Ci@Ch�nCg��Cf��Cd�Cb�C`��C^@C[�-CZ TCX��CW��CV�&CVf�CU��CRJ�CM�WCKP�CI�CA��C:��C4��C1�pC0uNC/�LC1�C3>�C4W�C4�&C8Y�C:�|C=_�C@��CC�CF�GCJ��CN�NCQ��CSi�CVcCX�C[D�C^�MCa�tCd�Cex0Cf�Ch�+CkPmCl�gCn~�Cp�Cp��Cq��Cr�Cr�LCsa�CtgCte�Cu��Cu��Cu�KCu�Cu��CvACva_Cv��Cvt�Cv��Cv�oCv3�Cur<Ct~�CrƔ3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�C}�'C}�wC~#3C}�C}ΤC}��C}��C}��C|�mCz��Czi�Cy�;Cx��Cw!Cv�lCw�Cy(�Cy��Cz�Cz��C{�C{�C|#C|lZC|��C{��Cz�Cz��C{�#C|��C|�rC}�C}ÖC~��C�!C�-�C���C��C�C�:�C�n;C�w"C�V�C���C��yC�cC}��C{��Cy3TCv�Cs�Cq�_CpK�Co?�CnJ�Cm��Cm RCi]"Cd~Ca�xC_ծCV�+CO �CH�"CEe6CC�cCC8�CE9CG!	CH^CI�CL��COÉCR|tCV�CY��C\�CCa��Cf$XCi�Ck%_Cnz=CqKCCs��Cw�%C{�C}�ChNC��gC��9C��C��iC���C���C�26C��:C�ͫC�*�C��C��'C�/�C�ݧC��pC��QC��C�$�C�6sC�cFC�|�C�u'C��GC��5C�[;C���G�O�G�O�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144  G�O�G�O�@���@��@���@��r@�@�|�@�f@�a�@��@��@�e�@���@�&@�>v@�̑@���@�11@@��@��,@��@��@�2@�S�@�z!@��@��@�� @���@�@�@���@�
@�sC@�a�@��@���@��=@��@��@���@���@�T�@��|@���@��@��@�@�;�@�f@�(T@�-�@�3@��@�@�a�@�n@�}@���@أ�@��@�TR@���@�{@��@�"�@�i�@�U�@�)�@�ZC@��O@Ī�@�t	@�@͎@��c@���@ؗ�@��@�ɍ@�z@��;@�i@�2�@��C@�
@�d@�0�@���@��&@��l@�,O@��ZA O�A �A(�AiHAA+�A��A�;Ad%AzAg�A�A�LA�dA�lA��A��A-cAHAܳAxrG�O�G�O�