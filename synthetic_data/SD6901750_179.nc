CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   y   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:59:08Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                  |  R�   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  Sd   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  UH   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W�   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  Y�   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Z   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  ]�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^L   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `0   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  b   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  b�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  dt   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  fX   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  f�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  j�   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  k   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  l�   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  |  n�   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  o\Argo synthetic profile          1.0 1.2 19500101000000  20210302045908  20210302045908  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @���[1   @���m�@H�1&�x��.dZ�@1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99959 (+/- 5e-05) , vertically averaged dS =-0.016236 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C9  CD  CM  CX  Ca  Cl  Cu  C�  C�� C�� C�� C�� C�� C�� C�� C�  C�  C�  C�  C�� C�� Cŀ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D  D  D@ D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�� D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�@ D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C9  CD  CM  CX  Ca  Cl  Cu  C�  C�� C�� C�� C�� C�� C�� C�� C�  C�  C�  C�  C�� C�� Cŀ C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D @ D� D  D  D@ D� D%� D+� D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�� D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�@ D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A9�PA9��A9�A9�-A9ƨA9ƨA9A9��A9��A9��A9��A9��A6A1�#A0E�A/VA.�+A.�A.�A-�-A-�A-33A,�jA+��A+hsA+hsA+�A+C�A*��A*ĜA*1'A)�A)�-A)|�A)G�A)"�A(�/A(ĜA(�A(�9A'�TA'XA&z�A&r�A&�A%A%��A%XA%G�A%%A$��A$�/A$ĜA$�RA$�9A$��A$ffA$E�A$JA#�A"=qA!A"ȴA!%A��A�A�HAbNAS�AdZA�#A��A~�A�+A	7LAp�@���@�ƨ@�C�@�o@ӶF@�$�@��R@��h@��`@��@��@���@��m@�+@��@�r�@��P@��@��!@�I�@��H@�p�@�\)@�E�@��@�(�@~E�@|�D@{"�@y��@x  @v{@t�@s@q��@p�9@ol�@nv�@m��@m?}@lj@kS�@i��@hĜ@hA�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A9�PA9��A9�A9�-A9ƨA9ƨA9A9��A9��A9��A9��A9��A6A1�#A0E�A/VA.�+A.�A.�A-�-A-�A-33A,�jA+��A+hsA+hsA+�A+C�A*��A*ĜA*1'A)�A)�-A)|�A)G�A)"�A(�/A(ĜA(�A(�9A'�TA'XA&z�A&r�A&�A%A%��A%XA%G�A%%A$��A$�/A$ĜA$�RA$�9A$��A$ffA$E�A$JA#�A"=qA!A"ȴA!%A��A�A�HAbNAS�AdZA�#A��A~�A�+A	7LAp�@���@�ƨ@�C�@�o@ӶF@�$�@��R@��h@��`@��@��@���@��m@�+@��@�r�@��P@��@��!@�I�@��H@�p�@�\)@�E�@��@�(�@~E�@|�D@{"�@y��@x  @v{@t�@s@q��@p�9@ol�@nv�@m��@m?}@lj@kS�@i��@hĜ@hA�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB��B��BB  B  BBBB  BB  B��BB��B�B�B�B�B�B�B�sB�ZB�B�B�#B�TB�5B�)B�#B�
B�
B�B�B��B��B��B��B��B��BǮB��B�FB�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�1B�JB�B��B��BƨB�;B�
B��B�/B�VB��B�}BɺB��B��B^5BK�B�B�B�B��B�+B�\Bm�BdZBaHBN�B9XB+B�BB�B�mB�HB�B�
B��B��BɺBŢBŢBÖBB��B�}B�wB�qB�jB�jB�jB�jB�qB�}B��BÖBĜBƨBǮBɺB��3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       BB��B��B�SB�;B�8B�?B�<B�<B�;B�EB�;B�6B�MB�B��B��B��B��BٺB��BװBӘB�SB�RB�cBғB�sB�iB�bB�JB�MB�GB�HB�=B�=B�3B�0B�2B�3B��B��B��B��B�vB�XB�TB�<B�?B�)B�+B�%B�$B�$B�'B�"B�B�B��B��Bw}B{�B�[B�+B�=B��B�|B�MB�$B�tB}�B�B��B�B��B�2BM�B;#B	B�B�rB�@Bv�B~�B]BS�BP�B>^B(�B�B B�B�1B�B��BɳBơB��B�gB�TB�@B�AB�2B�(B�%B�B�B�B�B�B�B�B�B�B�#B�4B�6B�FB�HB�WB�^3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C���C���C��C�}�C�b�C�MC�8�C�jC��fC���C��iC~K�CtECp� Cpx/CpR�Cn��CmuCl� CluCk�7Cl5�Cm[�Cn��Cm^ Cl
)Ck+�Ck0�Cj��Ck.�Cj�ACi4�Cii5Ch<+ChF�ChK�Ch�cCg�,Cf�Cf��CiHpCk�Cm@8Cm\�Cm/�Cl�}Cl6�Ck�CCk�"Ck��Cl-�Cly�Cl�FCl��ClYLCk�KCk#�Cj�ChG�Cd$�C\}�CW��CG��CIPCC��C:�C3&�C1:�C0��C/vC.T$C.�C.ՀC/f�C0��C2yyC5�C9�C<�OC?i�CE^hCK*ZCN��CPMCT=KCV/1CX"/C[�AC^��Ca�Cd��Ch�CkyCl��Cn&ZCo�.Cp��Cqz*Cr�mCsZcCt$DCt��Ct�^CuojCu�mCv'@CvtKCv��Cw+�Cw�Cw',CwCv�/Cu�CuHtCt��Ct�Csv7Crz�CqSCoj3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�C�=#C�"C�*C��gC���C���C���C���C�YmC��?C�ҾC���C�چC��C��C�1<C��C�lEC�8�C��C�*&C��>C�-�C�rC���C���C��8C��0C��C��C��C�^FC�eYC�i�C��C�C�5 C���C���C��=C�5�C�F�C�/IC��C��;C�tJC�p�C�}wC���C��;C��iC��KC��6C�V~C�TC���C���C~qCu�Cp��C^�xC`D<CZH�CO�CG�CE�CE!CC^CB��CCL@CC;�CC�CEZ�CG[�CKCrCN�mCS�CU�oC\�Cc9Cf�9Ch�RCmI�Co|tCq��Cu��CyC|O�C��C��&C�|�C�s=C�B�C��C���C�)�C��5C�=jC���C���C�-�C�u�C���C���C�vC�4�C��CC�u�C���C�z6C�F�C��C��oC�,�C��C��mC��G�O�G�O�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144   G�O�G�O�A
x!A
^&A
ApA
*�A
�A	�A	�-A	��A	��A��Ac�A ��A �A m!@�h�@��,@�Qd@�U @��@���@���@��@���@�`)@�t�@�}@�BW@��@�H�@�g�@��A@�c@�p�@�x�@���@���@�(R@��v@���@���@�׍@���@��n@�E�@��3@�de@�]2@�v@���@��@�CZ@�9�@���@�+/@���@���@��6@�C�@�@@��W@��6@�K�@э�@�`n@��?@��@�>�@���@�ؑ@�|f@�l�@��@�u�@�b�@�"@�!@ʜ�@�W#@��V@���@ݚt@ߋ�@���@��@��@���@��@�7�@��6@�O@�u@�M�@��@A ��A9}A��AfHA�QA:(AzvA�@A�7A�A_�A��A�lA�A��A�RA�4A��A[8A-A��A_�A6A�G�O�G�O�