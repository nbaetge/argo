CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:00:37Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20210302050037  20210302050037  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�$�>��?1   @�$�.z�@H�C���28�t�j�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99957 (+/- 5e-05) , vertically averaged dS =-0.016959 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CN  CW  Ca  Cl  Cv  C  C�� C�� C�� C�� C�� C�� C�  C�  C�� C�� C�  C�  C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C�  C�  C� C� C�� D @ D� D� D  D@ D   D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɀ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B8  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CC  CN  CW  Ca  Cl  Cv  C  C�� C�� C�� C�� C�� C�� C�  C�  C�� C�� C�  C�  C�� Cŀ Cʀ Cπ CԀ Cـ Cހ C�  C�  C� C� C�� D @ D� D� D  D@ D   D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɀ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��At�uAt��At��At��At��At�DAt~�At-Ar��Ao�;Ao�FAn�Am/Al�Af$�AYG�AOp�AK�AH��AE�-AC��AB1A?K�A>M�A=�A<5?A;+A:��A9/A8-A7l�A6ȴA6=qA5`BA4�HA4�+A4�A3�^A3
=A2�A1ƨA0��A/��A/x�A/"�A.v�A-��A-&�A,�/A,JA+��A,JA+�
A)��A)"�A)p�A)��A(�yA)hsA(�A'��A&~�A"�HAl�Ar�An�A��A�A�A�/AA�AVA5?A�A=qAJA~�A �@�"�@���@�R@�@��@�b@��@�;d@Ѻ^@�j@ƸR@¸R@��@��D@��@��D@��m@��@��`@���@�
=@�z�@�M�@���@���@�=q@��j@���@���@�dZ@�C�@��^@���@��@�~�@��@��@|I�@w�@up�@r~�@o�P331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                At�uAt��At��At��At��At�DAt~�At-Ar��Ao�;Ao�FAn�Am/Al�Af$�AYG�AOp�AK�AH��AE�-AC��AB1A?K�A>M�A=�A<5?A;+A:��A9/A8-A7l�A6ȴA6=qA5`BA4�HA4�+A4�A3�^A3
=A2�A1ƨA0��A/��A/x�A/"�A.v�A-��A-&�A,�/A,JA+��A,JA+�
A)��A)"�A)p�A)��A(�yA)hsA(�A'��A&~�A"�HAl�Ar�An�A��A�A�A�/AA�AVA5?A�A=qAJA~�A �@�"�@���@�R@�@��@�b@��@�;d@Ѻ^@�j@ƸR@¸R@��@��D@��@��D@��m@��@��`@���@�
=@�z�@�M�@���@���@�=q@��j@���@���@�dZ@�C�@��^@���@��@�~�@��@��@|I�@w�@up�@r~�@o�P331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�oB�VB�PB��B��B��B��B��B��B��B��B��B��B�PBr�Bl�Bn�Be`Be`B_;BC�BA�BB�B2-B+B)�B!�B�B�B{BbB	7BBBBB��B��B�B�fB�/B�)B�#B�B��B��B��BǮBǮB��B��B�FB�B�LB�}B�qB��BǮB��B�9B�DBp�B_;BB�B?}BI�Bl�B��B��B
=B�B�B�;B�B�3B��B��B�PB�Br�BaHBT�B@�B$�BPB��B�/B��B�}B�XB��Br�BI�B7LB33BB�B9XBJ�BE�B;dB1'B&�B �B�BuB\BoBPB1B��B��B��B��B�B�B�B�B�s333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B��B��B�oB|�B{�B�B�	B�B�B�&B�'B�B�B��B��B{�Ba%B[B]BS�BS�BM�B2B0B1B �B�B}BOB
)BB B��B��B�B�B�B�B�kB�NB�"B��B˾BʶBɳBėB�kB�[B�fB�>B�BB�wB�lB��B��B��B�B�B�UB�>B�B��By�B_ABM�B16B.(B8bB[/B��B�qB��B�KBFB��B��B��B�_B�,B{�Bq�BaYBO�BC�B/5B�B�	B�B��B��B�AB�B��Ba�B8�B&)B"B1fB(1B9�B4yB*@B B�B�B|BWB�?BQB�5B�B��B��B�B�B�B݁B�wB�iB�^333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cs�Cs�Cs<�CsCsCuCsF�Cs$�Cs)(CsY(Ct9Cs��Cr�CnW�CkTCfJ�Cf|�CgW�CeQ�Cc��Cb��Ca��Ca7qC`��C^�HC]3,C[�C\eC]4�C]�@C\�C\ �C[�-C[cC[�C[��C\��C[�-C[\�C[��C[-�CZ�FC[�C]��C`�C`5C](xC[;+C\�UC\>�C[�C_�C`�gC]�`CY��CY�EC]�C]C_�gCb�SC`��C`��C]�9CKT5C>)^C6]C1�8C/��C-�HC,ޕC,-�C)�mC)�C*�DC*�C-�C0ӸC2=�C4��C6��C8��C:m�C<txC>�`C@�+CC�iCF��CH��CKCM��CO��CQnCS?BCVt�C[x�C^��Ca��Cb�'Cbd�Cc�Cb��Cc�'CdՕCf�CgM�Ch�Ch�Ci��Ci�ICi��CjJCj�(Ck��Ck�Cl.�Ck��Ck�Cl~�Cl,Cl6�Ck7333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�C��`C�nhC��_C��NC�w{C�z#C��C��cC���C��gC��jC�HC�S�C�p�C��C��C}�PC|��C{��C{BCzb6Cxo�Cv��Cu)�Cu�Cv�-CwOCvb Cu|Ct�_CtKTCtICuT�Cv�CuUCt�|Cu`�Ct�CtB1Cu)7Cw�ECz�Cz*CvǕCt��Cv�Cu�.Ctu�Cx�ZCz�#CwĨCr�2Cs8�CvЎCv��Cy��C}QbCz�
C{�CwwCc�CTc�CK]�CFp?CD.rCB^CA,*C@kJC=��C=�}C>� C>�sCBE�CE��CGUhCJCL��CN��CP��CR܋CU\HCW�^C["�C^E?C`��CcUDCfJ=Chv�Cj�SCl�SCp7rCu��Cy�FC|٘C~$OC}�C6IC}��C@C�F�C�C���C�$�C���C���C�gC���C�J&C���C�F)C�iC���C�O3C�o>C�ħC���C���C��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�A�A�A�A_AMA�A(�A��AT�As�@��	@���@�c�@���@���@�`n@�~@���@�n@�T@�^H@��@��$@�[q@��@��i@�j�@�)@�E@�1�@��@ꃑ@넘@�FT@�8R@���@�:@�L@�|�@�Z�@���@�׿@��@��@���@죔@��J@ꮯ@�	�@�}@�ۃ@�4�@�~M@��,@��@��@�/i@�@��s@��@��Z@���@�;&@��<@�U�@�C�@�r@���@�+�@�(x@�T{@�;�@���@���@�\;@��@�m�@�Y?@�;@�mR@��x@�*�@�^�@�a2@ׯH@�=`@�@�*N@�(�@�"�@曡@�T@��@�i@���@��@� �@��
@�
A@�J&@���@��j@���@���@�{E@���@�~v@�|@���@��Z@�:J@��a@��@�F:@��6@���@��@���