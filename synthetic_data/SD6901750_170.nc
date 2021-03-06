CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:56:56Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20210302045656  20210302045656  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��;I��J1   @��=��,�@H�5?|��.+I�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99961 (+/- 5e-05) , vertically averaged dS =-0.015249 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CX  Cb  Ck  Cu  C  C�� C�� C�� C�� C�� C�  C�  C�� C�� C�� C�  C�  C�  C�  Cʀ Cπ CԀ C�  C�  C� C� C�  C� C�� D @ D@ D� D  D  D� D%� D,  D2@ D8� D>� DD� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɀ D�� D�� D�� D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CX  Cb  Ck  Cu  C  C�� C�� C�� C�� C�� C�  C�  C�� C�� C�� C�  C�  C�  C�  Cʀ Cπ CԀ C�  C�  C� C� C�  C� C�� D @ D@ D� D  D  D� D%� D,  D2@ D8� D>� DD� DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɀ D�� D�� D�� D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A2M�A2M�A2M�A2I�A2I�A2Q�A2Q�A2Q�A2Q�A2Q�A2Q�A2Q�A2VA2ZA2Q�A2A�A2M�A29XA29XA29XA2A�A2A�A2E�A2M�A2Q�A2ZA2^5A2bNA2r�A2z�A2~�A2�A2�DA2�uA2�uA2��A2�\A2�uA2�\A2^5A1+A/`BA.�HA.  A-�A,A�A+�A*A)�A(��A(5?A($�A'A'+A&�9A&1A$��A$�\A#A#/A!��A��A�AO�A$�A�AVAE�A�;A�FA(�A	�A��A��A"�A�!AȴA�9A 9X@���@�-@���@�Ĝ@�5?@�V@�X@��T@���@��T@��#@�$�@��+@���@�7L@�&�@��@�`B@��@�
=@��/@�\)@�/@�K�@���@|�@|9X@z�@vV@s�@p�@n�R@j��@k33@kS�@e�@c��@b��@bJ@^ȴ@\�/331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A2M�A2M�A2M�A2I�A2I�A2Q�A2Q�A2Q�A2Q�A2Q�A2Q�A2Q�A2VA2ZA2Q�A2A�A2M�A29XA29XA29XA2A�A2A�A2E�A2M�A2Q�A2ZA2^5A2bNA2r�A2z�A2~�A2�A2�DA2�uA2�uA2��A2�\A2�uA2�\A2^5A1+A/`BA.�HA.  A-�A,A�A+�A*A)�A(��A(5?A($�A'A'+A&�9A&1A$��A$�\A#A#/A!��A��A�AO�A$�A�AVAE�A�;A�FA(�A	�A��A��A"�A�!AȴA�9A 9X@���@�-@���@�Ĝ@�5?@�V@�X@��T@���@��T@��#@�$�@��+@���@�7L@�&�@��@�`B@��@�
=@��/@�\)@�/@�K�@���@|�@|9X@z�@vV@s�@p�@n�R@j��@k33@kS�@e�@c��@b��@bJ@^ȴ@\�/331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�sB�sB�sB�yB�yB�yB�yB�yB�yB�yB�yB�sB�yB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�;B�B�B��BȴBB�LB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�JB�B�B�B�%B�B�DB��B��B��B��B�^B�sB�B��B��B?}BgmBy�BM�B/B�BDB�yB�!B�VBk�BcTBo�BO�B6FB&�B�BJBB��B�B�B�mB�TB�;B�#B�
B��B��B��B��B��B��B�B�)B��B��B��B�B��B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B�sB�sB�sBٿBٿBٿB��BٿBٿBٿBٿBظBٿBغB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BٿBσB�XB�MB�!B��B��B��B�jB�YB�;B�7B�9B�/B�B�B�B��B��B��B��B��B��B~�B|�BuvBtpBr`BvzBsjB{�B��B�EB�GB�KB��B��BB�AB�(B/�BW�Bj8B>7B�BB��B��B��B~�B\BS�B`B@jB&�ByB4B��B�B�rB�GB�*B�B��B��B˿BǨBřBB��B�}B�~B�tBȫB��B�}B�|BÒBƤBÓB333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cl��Cl�.Cl�\Cl��Cl�,Cl��Cl��Cl��Cl�%Cl�SCl��Cl��Cl�VClzEClE�Cl+Ck�oCk�Ck�!Ck˿Ck��Ck��Ck��Ck��Ck��Ck��Ck�GCk�pCk͂Ck۶CkƟCk��Ck� Ck�9Cko�Ck/�Cj��Cj��Ci�Ce�C\�DCW[KCV��CU��CU%~CS�QCR��CQ��CP�-CO��COm�CN��CM��CL��CK�VCJCHȜCGc�CE�hCD>�CA&dC<�"C9Q\C5�zC3�C1p�C0v�C/c�C/
C/�C/GzC/�hC0�:C1�hC2��C2�NC2��C5McC8/�C=�?C>��C?��CBވCExCG�"CIE�CMB�CRNYCU�CZ�C[\�C[��C_��Cc*Ce'�Cg0�Cie�Ck�Cl�xCm�BCn4�CoCo�Cp�Cqe�Cq�nCr2'Cr�9CsQCr��CrʭCrhsCp�Cp��Cr�|Crc�Cq=CpvHCp�iCp�333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�C���C���C��AC���C��OC���C��C��ZC���C���C��C�izC�M�C�3�C�&�C�"�C�0C��C�=C��7C��C��@C�	�C�HC�	�C��C��C�&�C�tC�eC��C��oC��]C��]C��2C�}YC��CԎCu��Co�oCn�CnCm/�CkɚCj�5CiVCh]8CgQ�Cf�CfhRCe:hCd�Cb��C`�C_�~C]��C\sCZ�nCW]CR�CNf=CJ{�CG�fCE�6CD��CCp�CC�CC�CC_�CD�CE�CFBeCGZUCG3@CGD�CJ:�CMw�CS�1CT�jCU��CYx�C\d�C^�EC`�&Ce(]CjЄCnȴCs�VCt��Cu�qCy˦C}�]C��C�+SC�jUC�b�C�-�C��qC�'�C���C�tC���C���C�S�C�wNC��C���C��C��mC���C��C���C��;C��C��
C���C��LC�r�441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�@���@��&@���@��C@���@��n@��@���@��-@���@��6@�O�@�c@��@��<@�ǅ@��=@���@��@��@�i_@�}�@���@��@���@���@��2@�ϯ@���@��\@��Z@�zn@�i@�'�@��@��E@���@���@���@��@�[@䘲@�!@�[$@�3@� �@��@�@ݤ�@�0�@�@���@َ @��.@֘x@��@�H�@���@�y�@ɟ@�$�@�b)@��W@���@��z@��4@�D
@�C@��D@�*�@�5�@�T8@�`�@�;q@�Lt@�#�@�?�@�N�@�bR@�N�@���@ӓ�@��@ײa@���@�l@�;�@��@�3
@��m@�ͽ@�xZ@��@�C@�e�@�B�@��C@���@��TA P�A �A�A��A��A"AQ�A��Az�AkJA9�A6�A;AY�AD�A��AA�AgA4