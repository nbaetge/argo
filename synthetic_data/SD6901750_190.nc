CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   w   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:01:51Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302050151  20210302050151  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�1:�O��1   @�1<A��@H>vȴ9X�0|j~�� 1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99956 (+/- 6e-05) , vertically averaged dS =-0.0175 (+/- 0.01)                                                                                                                                                                                             C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @   @@  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CX  Ca  Ck  Cu  C�  C�� C�  C�� C�  C�� C�� C�� C�� C�  C�  C�  C�� C�� Cŀ Cʀ Cπ CԀ Cـ C�  C�  C� C� C� C�� D @ D� D� D  D@ D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111     ?�  @   @@  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CM  CX  Ca  Ck  Cu  C�  C�� C�  C�� C�  C�� C�� C�� C�� C�  C�  C�  C�� C�� Cŀ Cʀ Cπ CԀ Cـ C�  C�  C� C� C� C�� D @ D� D� D  D@ D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A�A�A���A���A�bA�VA�JA�VA�1A�n�As&�A\�!AYVAU��AT-AR(�AO�;AMx�ALz�AJ�/AIO�AG�;AFQ�AC��AA�A?�TA>M�A<�jA;p�A:r�A8�A7\)A6�RA4��A3p�A2��A2bA1p�A0�`A/A.ĜA.-A-�A-t�A,=qA*��A)�#A(ĜA'�wA'G�A&�/A'�#A)+A((�A&��A%��A$�`A#�A ��AhsA|�A|�A�A�!A�mA��AoAdZA�A�A9XA�TA
��A?}@��w@���@�1'@��@�r�@�@��T@��@� �@��H@��
@�Q�@��@���@���@���@�^5@���@�
=@��;@�bN@�@�E�@�C�@�`B@��/@�r�@��+@���@�@���@�7L@�bN@}?}@z�H@x1'@u@s��@r�@p��@o\)@n5?@mO�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A��A���A�A�A���A���A�bA�VA�JA�VA�1A�n�As&�A\�!AYVAU��AT-AR(�AO�;AMx�ALz�AJ�/AIO�AG�;AFQ�AC��AA�A?�TA>M�A<�jA;p�A:r�A8�A7\)A6�RA4��A3p�A2��A2bA1p�A0�`A/A.ĜA.-A-�A-t�A,=qA*��A)�#A(ĜA'�wA'G�A&�/A'�#A)+A((�A&��A%��A$�`A#�A ��AhsA|�A|�A�A�!A�mA��AoAdZA�A�A9XA�TA
��A?}@��w@���@�1'@��@�r�@�@��T@��@� �@��H@��
@�Q�@��@���@���@���@�^5@���@�
=@��;@�bN@�@�E�@�C�@�`B@��/@�r�@��+@���@�@���@�7L@�bN@}?}@z�H@x1'@u@s��@r�@p��@o\)@n5?@mO�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�}B�B�B�B�B�B�B�B�B�B�B�B�PB��B��B�B�B�5B��BŢB��B�-B��B��B�=Br�BcTBQ�BE�B5?B+B#�B�BVB+B�B�`B�TB�)B�B��BĜB�dB�LB�LB�9B��B�{B�7B|�Bq�Br�Br�B�%B��B��B�1B}�By�BhsBO�BE�B6FB,B"�B&�B"�B'�BL�B>wB�B�-BB��B�qB��Bq�Bm�B�%Bu�BuB�yB�B�qB1B�B�}B�dB�B�oBy�BgmBO�B<jB33B%�B�BPB��B�B�B�B�B�B�B�B�#B�/B�/B�
B��B��B��B��B��B��B��B��B��33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B�}B�B�B�~B��B�B�B�B�B��BވBcB{#B�B�B�bB�YB�B��B�vB�^B�B��B�tBxB`�BQ7B?�B3�B#,B�B�B�B�GB� B�B�WB�OB�%B�B��B��B�dB�JB�LB�:B��B�BwBBj�B_�B`�B`�Bt0B��B��Bv8BlBg�BVB=�B3�B$\BB�BB�BB:�B,�B�B�6B��B��B�xB��B_�B[�Bt9Bc�B�BץB�CB��B�\B��B��B��B�9B��BhBU�B>$B*�B!�B/B�B��B�JB�B��B��B�B��B�kB�rB�~BˉBˈB�fB�]B�JB�;B�5B�+B�.B�)B�,B�033311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CZb�CY�&CY�QCY� CY�	CY��CY��CYb�CYf�CYg>CX�YCV�WCT�CU��CU�wCUDACU\�CU�CW�CV�uCV"CW�iCW�	CX=�CX$1CW'CV�CV��CVx�CWCW�SCX~CX�zCX,4CX	�CW�CVo�C[C^�>C^�C[e]CY��CX��CX��CX�KCY�jC[EDC\I�C\	�C\&C\]C\hbC[!mCT��CT]�CW}[CY�CTf8CS�CYƶC\��C\�HCY�vCN�%CC@�C:-(C4;wC/�C-9C,��C*O5C*zbC*�C,QWC-��C0ZTC3U�C5�6C7��C:�BC?��CC5�CEҠCJ�CI5YCLtqCP�ACSCU�/CX��C[�C]��C`�DCb�Cdw<CfZaChQCi�<Ck��Cmn�Cn_�CnkKCn��Cp -Cr<�Cq׬Cq�jCq?FCq��CrwBCr�0Cs�NCsz�Cs��Cs�aCsH�Cr��CrieCq3o33311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�Cr�6Cr��Cr�>Cr�'Cr�@Crs�Crx�CryRCq��Co��CmT�Cn��Cn7�Cm�CnLCn��Co�KCo�Cn�uCp�;CpՋCqOYCq4�Cp�Co�Cou�Coa�CpSCp�DCq�\Cq�qCqS�Cq0Cp�CokhCt��Cx��Cw�JCt��Cr�Cq��Cq�Cr�Cs�Ct��Cv0Cu�\Cu�{Cu�;Cv;�Ct��Cm�ICmGCp��Cr~�CmW�Ck�Cs`�Cv��Cv�Cs��Cf�CZP�CP04CI��CCδCA�CA\%C>�QC>�|C??�C@�FCB>CEffCH��CK`�CM��CP��CV
CZ��C]�0Cb_�Cal�Ce?Cj�Cl��Coi�Cr��Cv�Cx��C{�C~x�C��C�-�C�&BC�$?C�62C�3�C��iC��RC��uC��\C��C���C��C�s:C��+C�)OC�R�C���C�ŇC��C��kC���C���C�=7C��Q44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�@���@��F@��1@��@��@��@�Ū@��d@��C@�2l@��n@�@�\@�e`@��@��@�Tq@��x@�^�@�%�@�3f@�V@��@�8@�J@@��@�Έ@��@�17@���@�1�@��@�W@��@���@�Ē@��,@�)@�1�@�13@�0M@�-{@�g�@�cC@��@�7�@��w@�8@��@�b1@��@�.[@��R@�%\@�˗@��e@�u@餲@�	r@���@��@ݭ1@ѕd@��[@��@���@���@��$@��/@�:@��R@�!�@�y	@��@��7@�>@�ND@Ȣ�@��@���@Թ$@�Q�@�h.@��o@�2@��@��?@�>�@�D�@�Ǩ@�m@�K.@���@�=@��@�ʶ@�ز@���A d�A ncA ��AOA��AS�AC�A8A5HA�A��AN3AL�AhAoqA<A�A� A&