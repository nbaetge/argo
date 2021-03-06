CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   v   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:02:07Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302050207  20210302050207  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�3���-�1   @�3��eC @H9x����/�vȴ@1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99955 (+/- 6e-05) , vertically averaged dS =-0.017632 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @   @@  @�  @�  @�  A   A  A   A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CC  CN  CX  Cb  Ck  Cu  C  C�  C�  C�  C�  C�� C�� C�  C�  C�� C�� C�� C�� C�� Cŀ C�  Cπ CԀ Cـ Cހ C� C� C�  C�  C�  D � D� D� D  D@ D� D%� D,  D2@ D8@ D>� DE@ DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      ?�  @   @@  @�  @�  @�  A   A  A   A�  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C/  C9  CC  CN  CX  Cb  Ck  Cu  C  C�  C�  C�  C�  C�� C�� C�  C�  C�� C�� C�� C�� C�� Cŀ C�  Cπ CԀ Cـ Cހ C� C� C�  C�  C�  D � D� D� D  D@ D� D%� D,  D2@ D8@ D>� DE@ DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�@ D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�z�A��A��A�v�A�r�A�r�A��A��A��7A��+A��;Ax�A\�AS"�AOO�ALȴAJȴAIS�AH1AF��AE�hADM�ABȴAA?}A@$�A?/A>A�A=�A<��A<A;C�A:�uA9�-A8��A7��A7A69XA5�-A5?}A4ffA3ƨA3S�A2�/A2�A1�^A1K�A0��A/��A.�/A.�9A.^5A.bA,�A+��A,1A+�A,n�A,1'A+��A*{A(�DA'��A&bA$I�A#x�A �uA�yA  A�9Al�AZAG�A;dA��A-A�uA  @���@�E�@��9@�^5@�p�@�=q@��H@Ϯ@̛�@��`@�{@���@��@��@���@��!@�`B@��^@�l�@���@���@��@�ƨ@�`B@�o@�&�@�l�@�@�V@�
=@~{@|(�@|�@z�@u�T@vV@u?}@u�@sƨ@r��3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A�n�A�z�A��A��A�v�A�r�A�r�A��A��A��7A��+A��;Ax�A\�AS"�AOO�ALȴAJȴAIS�AH1AF��AE�hADM�ABȴAA?}A@$�A?/A>A�A=�A<��A<A;C�A:�uA9�-A8��A7��A7A69XA5�-A5?}A4ffA3ƨA3S�A2�/A2�A1�^A1K�A0��A/��A.�/A.�9A.^5A.bA,�A+��A,1A+�A,n�A,1'A+��A*{A(�DA'��A&bA$I�A#x�A �uA�yA  A�9Al�AZAG�A;dA��A-A�uA  @���@�E�@��9@�^5@�p�@�=q@��H@Ϯ@̛�@��`@�{@���@��@��@���@��!@�`B@��^@�l�@���@���@��@�ƨ@�`B@�o@�&�@�l�@�@�V@�
=@~{@|(�@|�@z�@u�T@vV@u?}@u�@sƨ@r��3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�`B�B�yB�B�B�B�B�B�B�B�BS�B��B��B�!B�'B�B��B��B��B�uB�7B{�Bn�BdZB]/BT�BN�BH�BD�B>wB9XB2-B'�B �B�B�BhBPBB  B��B��B�B�B�B�mB�/B��B�B��B��BB�qBBǮB�B�
B�
B��B�qB�RB��B��B��B��B�?B��B�\B�DB��B�-B�B�B�
B�!B�B{�B��B��Bw�BJ�B�NB�5B��B��B�B��B��B�1B��B��BjBR�BB�B9XB/B�BuB
=BB��B�B�B�sB�B�NB��B��B�/B��BɺB��B��B�
B��B�3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B�B�`B�B�@B�lB�jB�rB�fB�kB�hB�fB�fBA�B�mB�OB��B��B��B��B��B�jB�4Bv�Bi�B\^BR"BJ�BB�B<�B6}B2gB,FB'&B�B�B�BuBYB�=B�%B��B��B��B�B�wB�lB�bB�GB�B��B��B��B��B�rB�TB�oB��B��B��B��B��B�TB�7B��B��B��B��B�%B��B}KBy2B��B�B��B��B��B�B�wBi�B�{B��Be�B8�B�^B�EB��B��BܩB��B��BvRB��B��BX�BAB0�B'�BJB�B�B�qB�=B�B��B��B֯BغBЊB�B�B�jB�6B��B�B�+B�GB�<B�D3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CZcCZ\�CZT�CZU CZ;�CZ>CZ�CZCY�fCY��CY\WCVX�CT'QCW�yCYRzCZW�C[�C[��C[�&C\-aC\�C[��C[�_C[fC[K�C[�CZ��C[C[Q�C[K�C["HC[+C[3C[SC[nC\P�C\�~C]C�C]}kC]_C]{�C]��C]��C]�C]w{C\��C\�C\�C]O�C]X�C\�=C\�C\��C]�$C]��C]0�C[��CZ��CY��CV�PCToCS��CUHQCX��CM�VC@X�C<��C/��C,�C*�iC*�C*o�C*y	C*�C+�UC,�C/��C06C5�5C7nC8èC<f�C@2CFegCG�CH��CJ�zCM3�CbmCU'�CW�CW�\CZ�C^wUCa?`CcHCdl Ce��Ch �Cix�Cj�rCl0CmMVCnkCo%�Co�GCo�Cq�MCs�_Cs�Cq�FCs[jCt�CszCr�CqNXCp��Co`e3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�Cs�jCs�CspACsLCsP�Cs?�Cs�Cs�Cry�Co�Cl��Cp��Crx�Cs��Ct_�Cu �Cuy�Cu��Cu��CulCu%nCt�ZCt�dCt�|Ctm�Ct��Ct�"Ct�Ct�GCt��Ct��Ct�
Cu�Cv_Cv��Cw,Cw[�Cw<HCw^�Cw��Cw�=Cwy�Cwd/Cv�OCvvcCv��CwA�CwNCv��Cv��Cv��Cw��CwʳCw1Cu��Ct8�Cs/Cp4!Cmy�Clu�Cnx�Cr�DCe�KCW @CRܯCD�*CA�C?8�C>�C>�C>˥C?�C@%�CA��CD�tCEP�CK�yCM�CN��CS�CWC^J�C`�Ca�Cca#CfEG�O�Cn�BCr*�CrECt�eCy��C|�jC~�RC�!�C�rC�*�C�C��CC�~�C�1dC���C�AMC���C��C�ՕC��C���C���C���C�fsC��gC�yC��]C�a9C���4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111  G�O�G�O�@�ͅ@��2@�r@鐱@�,@��@�]S@�[�@���@�@�7�@�� @���@��.@�_@�4@�O@���@�ů@��@�W!@��@���@��@꦳@��.@�@�&@��@�Ƴ@��@�\@�7�@�-(@쨛@�6�@�v�@�X�@�y�@��@���@퓜@�~�@�A@욜@�ӥ@�]�@�j@즘@��@�_@�ź@��O@�M�@���@�s�@�t�@�r@��3@� �@��@��@��\@΅\@�mt@���@�b�@��n@�H�@��@�)�@�m�@�u�@���@�͎@�lx@�q�@���@ƭi@ʛ�@΃6@�f{@��@��@�H�@���G�O�@�i6@�z�@�^�@���@��@�=@���@��@���@��@��@��M@�d$@���A OCA �ALA1Af�Ay/A�AStA>�A�xA$�A�VA0�A��A,�