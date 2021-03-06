CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   u   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:57:11Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  SP   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  U$   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Wp   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  YD   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  ]d   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]�   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  a�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  c�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  e�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  g�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  i�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  j<   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  x  m�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  n\Argo synthetic profile          1.0 1.2 19500101000000  20210302045711  20210302045711  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��}'�}1   @��
=p�@I6�-�.V��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99961 (+/- 5e-05) , vertically averaged dS =-0.015358 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  A   A   A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C	  C  C  C&  C/  C9  CD  CN  CX  Cb  Ck  Ct  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�  C�  C�� C�� C�  C�  Cʀ Cπ C�  Cـ Cހ C� C� C� C�  C�� D @ D� D� D  D� D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D�` D߀ D� D� D�� D�  D�  D�@ D�` D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       ?�  @   @@  @�  @�  A   A   A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C	  C  C  C&  C/  C9  CD  CN  CX  Cb  Ck  Ct  C�  C�  C�  C�  C�  C�  C�  C�� C�� C�  C�  C�� C�� C�  C�  Cʀ Cπ C�  Cـ Cހ C� C� C� C�  C�� D @ D� D� D  D� D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D�` D߀ D� D� D�� D�  D�  D�@ D�` D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A0�A0�A0��A0��A1%A1A1�A1�A1oA1%A1A1%A1
=A1VA1VA1�A1�A1�A1&�A133A1C�A1S�A1dZA1l�A1l�A1p�A1x�A1x�A1x�A1t�A1x�A1�A1�PA1�PA1�A1�A1l�A1O�A1K�A0�\A/33A-�hA-%A,��A,1'A+K�A*��A)�hA)/A)A(I�A'ƨA'+A&��A&��A&�A&z�A%XA"�yA!\)A�hAdZA{A��A�DA�A�yAO�A�AXA�Al�Av�A ��A $�@��+@�&�@�=q@��#@�  @��@�;d@��H@���@�n�@�G�@���@���@��j@�  @�1@�Z@��P@��y@�O�@�v�@���@�Ĝ@�|�@�9X@{33@x�@tj@r�\@q7L@qhs@p�u@nv�@k�F@j^5@i�@h��@fȴ@c�m@aX@_+@^$�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A0�A0�A0��A0��A1%A1A1�A1�A1oA1%A1A1%A1
=A1VA1VA1�A1�A1�A1&�A133A1C�A1S�A1dZA1l�A1l�A1p�A1x�A1x�A1x�A1t�A1x�A1�A1�PA1�PA1�A1�A1l�A1O�A1K�A0�\A/33A-�hA-%A,��A,1'A+K�A*��A)�hA)/A)A(I�A'ƨA'+A&��A&��A&�A&z�A%XA"�yA!\)A�hAdZA{A��A�DA�A�yAO�A�AXA�Al�Av�A ��A $�@��+@�&�@�=q@��#@�  @��@�;d@��H@���@�n�@�G�@���@���@��j@�  @�1@�Z@��P@��y@�O�@�v�@���@�Ĝ@�|�@�9X@{33@x�@tj@r�\@q7L@qhs@p�u@nv�@k�F@j^5@i�@h��@fȴ@c�m@aX@_+@^$�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�NB�B��BƨBĜBÖB�dB�?B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�JB�=B�B�7B�1B�1B�bB�B��B��B�B��B��B�3B�}B�^Bz�BffBl�BXBBǮB��B�hBn�BdZBI�B8RB,B�BPB��B��BDBB��B�NB�B��B��BŢBĜBŢB��B��B��B��B��B��B��B��B��B��B��B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B��B�B�B��BݹBݹBݼBݼBܴBݻBܴBܴBܴBܴBܴBܲBܶBܴBܴBݹBܶBݺBܵBܵBܵBܳBܵBܴBܴBܴBܴBܲBܲBܲBܶBܴB۰BڨBڨB�uB�0B��B��B��B��B��B�oB�7B�8B�=B�(B� B�B�B�B�B�B��B��B��B��B��B|BztBuZBypBxlBxiB��B�LB��B��B�TB��B�B�lB��B��Bk$BV�B\�BHTB�gB�	B�BB��B^�BT�B:&B(�BzB%B��B�sB�oB��B�B�JB��BɕB�aB�MB�"B�B�#B�AB�`B�`B�UB�YB�nB�mB�gB�`B�YB�\B�b333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cm0*Cl�$G�O�ClղCl�[Cl��Cl��Cl��Cl�ClئCl�<Cm�Cl�Cm �Cl��ClցCl��Cl�BCl�bCl�Cl�qClB$Cl&bClCk�9Ck�&Ck��Ck��Ck�KCks�Ck\�Ck@�Ck-�Cj�Cj�-CjNCi��Ch�NCgUCa
~CY(-CT�CT,CS3CCRE�CR%sCQ��CQKfCPzCOoCN �CM �CL��CLZ~CLRCKVjCJ�!CG�/CC�OC?�4C<��C8�aC4��C1�PC0��C/�|C/SC.�mC.��C.�CC0�C2%C3xC5�C6�C7%�C82!C9��C=�QC?��CA�:CD��CK5zCP?�CS�8CVg�CY��C[�nC^�{Ca�Cc��Ce�2Ch�Ck�CkNLCj��Ck�&Cm��Cp��Cq�yCs�;Ct��Cv
�CveCu��Ct%�Cs�Cr��Cr�CrU^Cq��Cq�Cq�pCq�Cq�@CqFCo��33 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114           G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                        G�O�G�O�G�O�C��C��C���C���C��ZC��C���C���C���C���C��TC��EC���C���C���C���C���C���C�_%C�QC�F�C�<KC�1�C�'�C�&C��C��ZC��C��XC��QC���C��C�7�C��=C��aC��oCzr�Cq�Cl��Cl'�Ck-CjPCi��Ci�yCh��Ch{Cf�UCe�Cde�Cc��Cc�uCc3,Cbp�Ca�JC^��CZ"CUجCR;XCM�VCIVGCF!CD�%CC��CC%VCC CC�CCCD�CF��CG�iCI� CK�CLXgCM��CO�"CS�5CV6�CXO#C[�^Cb�Ch�`Clh�Cou@Cr�CufaCx�C|+C~b�C�nC� C�`�C���C�-/C��/C��lC��=C�K�C�S�C��>C���C���C�`�C���C��C���C���C���C�_	C�ZYC�w�C���C�lC�"�G�O�44 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114   G�O�G�O�G�O�@���@���@���@���@��(@���@���@�ڃ@���@���@��@��)@�ʩ@���@��
@�� @���@���@�;�@� �@��@���@���@��@���@��a@�t�@�^@�B�@�1@@��@��f@��@��@��i@�$�@�n;@�E@�~�@⵶@��@ಊ@��n@�=�@߭�@���@ݵq@�Q�@�B�@گ$@�sR@��@�a�@ظ�@պ�@�]�@�J�@�Ғ@�z�@�H�@�*�@�۩@���@�W
@�3E@�N@�>�@��@��@���@��3@��@�+�@�P<@�A�@��@ͥn@Ϩ!@�@��J@�<3@���@��3@�;N@�|@���@��q@�5�@���@���@�>�@���@���@��@�6�A�A�1AߎAe�A�A'A�)A%A��A�$A|�A8�A��A�VA�A
A\A��G�O�