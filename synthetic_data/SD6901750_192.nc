CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   u   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:02:21Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  n\Argo synthetic profile          1.0 1.2 19500101000000  20210302050221  20210302050221  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�6;��-�1   @�6<B�s@@G���`A��/��1 1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99955 (+/- 6e-05) , vertically averaged dS =-0.017759 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @@  @�  @�  @�  A   A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cu  C�  C�� C�  C�  C�� C�  C�� C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  CԀ C�  C�  C�  C� C� C� C�� D @ D� D� D@ D  D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�� D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111       ?�  @@  @�  @�  @�  A   A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CW  Ca  Ck  Cu  C�  C�� C�  C�  C�� C�  C�� C�� C�  C�  C�  C�  C�  C�  C�  C�  C�  CԀ C�  C�  C�  C� C� C� C�� D @ D� D� D@ D  D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�� D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��-A��wA��jA��wA��jA��^A��jA���A��jA���A��An-A\5?AW
=AT �AQ�AN�`AMC�AK�AJ��AI�hAH��AH�AG"�AE��AD{AB�AA+A@�A?K�A>�HA=�mA<�yA;�^A:��A:jA9l�A8jA7�^A6�/A6 �A5?}A4�A4-A3p�A2ĜA2��A1�wA0jA/��A/l�A/%A.�A-�
A,�A,Q�A+�
A*ĜA)�7A'��A&bA$��A#t�A 1'A�A�A��A/A��AM�AA�yA��AdZA�A�RA�\A ff@�ƨ@���@�r�@�t�@�h@�{@�"�@�
=@�Z@�9X@�$�@���@�Q�@��y@��T@�&�@���@�X@��@�X@�A�@���@�E�@���@���@�$�@�hs@�ƨ@��^@��D@~ff@|(�@{�F@z��@xQ�@xb@v�@u�@t��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A��-A��wA��jA��wA��jA��^A��jA���A��jA���A��An-A\5?AW
=AT �AQ�AN�`AMC�AK�AJ��AI�hAH��AH�AG"�AE��AD{AB�AA+A@�A?K�A>�HA=�mA<�yA;�^A:��A:jA9l�A8jA7�^A6�/A6 �A5?}A4�A4-A3p�A2ĜA2��A1�wA0jA/��A/l�A/%A.�A-�
A,�A,Q�A+�
A*ĜA)�7A'��A&bA$��A#t�A 1'A�A�A��A/A��AM�AA�yA��AdZA�A�RA�\A ff@�ƨ@���@�r�@�t�@�h@�{@�"�@�
=@�Z@�9X@�$�@���@�Q�@��y@��T@�&�@���@�X@��@�X@�A�@���@�E�@���@���@�$�@�hs@�ƨ@��^@��D@~ff@|(�@{�F@z��@xQ�@xb@v�@u�@t��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBiyBcTBdZBdZBdZBe`Be`Be`Be`Be`Bx�B�}B��B�mB�/B��BǮB��B�}B�LB�!B��B��B��B�oB� Bp�Be`B]/BYBW
BO�BH�B@�B:^B7LB-B$�B�B�BhB+BB��B��B�B�B�mB�B��B��B��B��B��BĜB��BB�qB�RB�B��B��B��B�\B�bB��B�'B��B��B�BB�B�B"�B �B�B��B�mB�oB�JBq�Bw�Br�BK�B5?B2-B#�B+B��B�5BǮB��B{�BffBR�BI�BF�B1'B$�BVB��B��B��B�B�B�yB�TB�)B�
B��B��B��B��B��B�
B�
B�B�/331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       BiyBcTBQ�BQ�BQ�BR�BR�BR�BR�BR�BfkB�B�B� B��B��B�IB� B�B��B��B��B��B�NB�Bm�B^LBS	BJ�BF�BD�B=�B6cB.5B(B$�B�B�BuBIB�B��B��B�B�B�ZB�\B�,B��B��B��B��B��B��B�bB�LB�TB�9B�B��B��B��B��B}+B~/B��B��B��B��B�B�rB@B�B�B�wB��B�4B�DBz!B_�Be�B`�B9�B#'B B�B�B�B�-B��B��Bi�BTnBA B7�B4�B:B�B�nB�B��B��B��BڨBחB�tB�HB�,B�B�B�B� B�B�-B�-B�9B�P331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CZ�.CZ�cCZ�.CZ�+CZ�VCZm	CZmCZF)CZ�CX��CQ��CR�dCT��CUe�CU��CVyCCWmOCW��CW�CYu�CY�rCZ"tCZ3�CYǒCX�.CX �CWU�CWkUCW}�CX%CX(|CX�CX<CW�`CX�CY�CXkNCX�CY�CZ��C[?QC[�C[��C\�ZC]0�C]�EC[�XC[R�CZ��CY�:CW�RCT��CS CS	-CR��CT!
CV<CT��CO�WCJS�CE�CFɽC?cC8��C2��C1�C-�C,�>C+�C*JnC*C)��C++�C,eC-4C.�C/��C4�C6�C8ϰC;8�C>(CA�CE=#CF��CI+�CK��CN;�CP��CS�CCWαC\>@C^��CaZ�Cb`�Cc�WCe��Cg�'Cj�MCl�Cm� Cn[�Cn�|Co��Cp=Cq
"Cr�Crx�Cs@�Cs�Cs$�Cr��Cr�eCq�(Cq�<CpŻCo]�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�Cs�Cs��Cs�'Cs�Cs�OCs�fCsB�Cq�CjCj�$Cm&=Cn$�Cn| Co]JCpp Cp�cCqCr�lCs(�Cs�sCs�RCs!�Cr&XCqN�Cpn�Cp��Cp�Cq]OCqc�CqE�Cq~�Cp��Cq��Cs�Cq�CrKCsFvCta$Ct�Cu�GCu��CvoCw%�Cw��Cuo�Cu'Ct�YCs[UCq_kCm�Ck�Ck�Ck�mCm XCo]Cm�5Ch7eCb=VC]%C^SpCU�CN�eCG��CF&�CBUxCA3*C@C>�iC>kuC>8�C?�jC@��CA��CC�OCD�CI��CK��CO�CQ�jCU�CYR�C]BC^�Ca��Cd�[Cg@xCj7�Cm��CrGCwCCz�C|�C~CdC��C� �C��C��C�[EC���C�&KC��SC��ZC�a�C��+C�6�C��kC��tC���C�e�C�e�C��C��:C�]�C��441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�@�;�@�	@�@��@���@�ʟ@��@�M*@�/@Ⴒ@��@�a@���@��3@��
@��@�l�@��@�n�@���@���@�h@�v�@��@��@���@� @絼@��@�?@���@�9�@�7�@�fT@��@� @�R@ꚲ@�#�@���@��@쓈@�C@��@랱@�Im@��@�\@��@�m&@�m�@�s8@�*@�,@��@�R�@��@�0�@�L�@�n�@�-�@�=�@���@�9�@��}@�x�@�g@���@�͏@���@���@���@�4�@�A@��@��C@�� @�ȓ@�e3@̒�@Сx@�<i@��@؂W@�xW@� s@�م@�4]@�`�@�-_@�E@��@���@�,�@�ƣ@��:@��@�<{A �A x|A ȚA(�AgA��A��A��A2�AXoA*�A�A��A�Aw�A�A4�