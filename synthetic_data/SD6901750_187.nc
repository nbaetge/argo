CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T05:01:06Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20210302050106  20210302050106  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�)��O��1   @�)�~ܺ�@G������1�ffff`1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99956 (+/- 6e-05) , vertically averaged dS =-0.017172 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114930                                          20210205114930A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Ca  Cl  Cv  C  C�� C�  C�� C�� C�  C�� C�� C�  C�� C�� C�� C�  C�� Cŀ Cʀ Cπ CԀ Cـ C�  C� C� C� C� C�� D   D@ D� D  D@ D   D%� D,  D2@ D8� D>� DE  DK@ DQ� DX@ D^  Dd  Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    ?�  @   @@  @�  @�  @�  @�  A   A  A   Ap  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Ca  Cl  Cv  C  C�� C�  C�� C�� C�  C�� C�� C�  C�� C�� C�� C�  C�� Cŀ Cʀ Cπ CԀ Cـ C�  C� C� C� C� C�� D   D@ D� D  D@ D   D%� D,  D2@ D8� D>� DE  DK@ DQ� DX@ D^  Dd  Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�` D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��!A��-A��RA��^A��RA��^A�5?A��A�ĜA���A�7LAo|�Ah�`Ad��A\ffAS��AMdZAIdZAG�-AGG�AE�;AC�
AAhsAA�mAA�A?&�A>�jA=`BA<(�A;;dA:�A9�mA8��A8  A7�hA7�A6ZA4��A4v�A3
=A1��A0ffA/�;A/�A-�A-XA,��A+K�A*��A*ZA+A,1A+�TA+�TA+\)A*ĜA*1'A*~�A*�A)��A(�+A'A%�FA"��A ��A�AO�AƨAK�A$�Ar�A��A�RA�7A
��AXAoA	�@���@�=q@�%@�M�@�@�v�@��/@Ο�@��;@�X@�|�@�t�@��@�
=@�o@���@��m@�1'@�@�z�@�+@��7@�1'@��@�V@��#@��@��m@���@��7@���@{�m@v�@tj@q%@o�@n5?@m�@mV@m�@m�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A���A��!A��-A��RA��^A��RA��^A�5?A��A�ĜA���A�7LAo|�Ah�`Ad��A\ffAS��AMdZAIdZAG�-AGG�AE�;AC�
AAhsAA�mAA�A?&�A>�jA=`BA<(�A;;dA:�A9�mA8��A8  A7�hA7�A6ZA4��A4v�A3
=A1��A0ffA/�;A/�A-�A-XA,��A+K�A*��A*ZA+A,1A+�TA+�TA+\)A*ĜA*1'A*~�A*�A)��A(�+A'A%�FA"��A ��A�AO�AƨAK�A$�Ar�A��A�RA�7A
��AXAoA	�@���@�=q@�%@�M�@�@�v�@��/@Ο�@��;@�X@�|�@�t�@��@�
=@�o@���@��m@�1'@�@�z�@�+@��7@�1'@��@�V@��#@��@��m@���@��7@���@{�m@v�@tj@q%@o�@n5?@m�@mV@m�@m�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB?}B49B#�B0!B;dBF�BC�BB�B=qB>wB>wB=qBR�BXB\)BR�BI�BF�BC�BO�BcTBYBE�B1'BI�BR�B<jBE�B:^B1'B)�B%�B!�B�B�B�B�BhBBB�B�5B��B��BƨB�wB�^B�'B��B��B��B�BÖBĜBȴBĜB�}B�wB��BǮBƨB�wB�!B��B�7B}�Bv�BcTBgmBm�B� BhsB�bB�!B�'B�FB��BPB��B�PBffB\)BL�BA�B9XB%�B��B�NB�;B�B�1BffBR�BE�B1'B�B
=B�B�B�mB�TB�BB�5B�5B�;B�B�B�B�)B�/B��BǮBŢB�}B�qB�qB�}B��BǮBȴ333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B?}B49B#�BWB)�B4�B1�B0�B+�B,�B,�B+�BA*BFIBJcBA-B7�B4�B1�B>BQ�BGSB3�BkB7�BA2B*�B3�B(�BjBBB*BB�B�B�BB��B�`B�ZB��B̆B�@B�(B� B��B��B��B�0B�'B�B�oB��B��B�B��B��B��B�B�B� B��B�}B�@Bw�BlUBe.BQ�BU�B[�BnhBV�B~�B��B��B��B�%B��B�B{�BT�BJ�B;AB/�B'�B_B�oB��B��B��Bv�BU BA�B4?B�B1B��B�[B�2B�B�B��B��B��B��B�>B�]B�WB��B��B��B�`B�WB�4B�'B�%B�2B�9B�bB�i333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cn�0CnőCnuCn@1Cn0Cm�GCmc�CmtfClk�Cj�qCh��Ce�HCb(�C`.�C^C�C\�WC\ZoC]dC^�@C_4�C_�C_sZC`#C`L�C_@C_tC_�C]qhC]ԽC^#�C]�C\!8C\=�C\��C]L�C^�4C_��Ca@�C`�)C`�?Cb�Cb/�Cas�Ca0C`��C`�C_��C`L�C`��CaNCaͿCc�Cd�Cc��Cb�0Ca��CaCc%�Cd<�Cd<�Cd��Cc��C`��C]�VCW�ECF:�C=�.C4`�C/�9C0��C0CC.F�C,��C+��C,�TC-�C-"C-��C04~C5�qC9��C<�C?ECA��CC	CEƢCJNCMR~CO8�CSV�CV�2CZ�C]3JC_��Cb��Ce��Ch?NCkCl�oCmq.Cns�Co9Co·Cp
�Co�GCnuNCm�XCn��CqMCq��CsŢCuB�Cv�Cw;zCwm�Cw$jCvoHCu/Cr��Cp�333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�C���C�ɇC���C�~�C�O(C�X|C��+C��C�� C�wC|QCy�Cw��Cv Cu�oCv��CxOQCx�Cx��Cy0�Cy��Cz(/Cx��Cx��CxЦCw�Cws�Cw΃Cv��Cu��Cu��CvL�Cv�Cx�ECy�jC{[�Cz��Cz�.C|A"C|q2C{�}C{>XCz��Cz)�Cy��CzeKCz��C{8kC|�C~EDC~��C~a�C|�!C|sC{dLC}�C~��C~�Cb�C~��Cz�3Cw�Cq;�C]��CS�zCI��CD��CE|`CD՛CB�ICA.C@G9CA*|CB)CCA�JCBmCE*�CK�+CO��CR�QCVCX�jCZ`xC]ugCb��Ce�NChCl��Cp��Ct�Cw�PCz��C}�C��C�+�C�ǀC��bC�bC���C�%�C�viC��NC�AC�ţC�l~C���C�h8C��C��'C��sC�DC���C���C��
C�e�C��C�G�C��i441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�@�+�@��@��v@�c�@��@��@���@�g�@�T�@���@��@���@��n@�.�@�۝@��@�`�@���@��@�8�@��@�&�@��z@��x@���@�!(@��@���@���@���@��@�r�@�
�@��$@��H@�N@�@��J@�*@�X1@��@�1�@��r@�'�@���@�a=@��f@�+�@�<@��@�m@�4�@�ܛ@��1@�V@�x@���@��F@�+]@�z@���@��E@畦@ԪR@�~�@��@���@��@��@��@�O�@��L@�pw@�e@��{@��1@�G�@�h�@�r�@�%@͋�@�,�@ѤJ@ԙ�@�}�@���@��@@�EX@��7@��@��8@�@�ʔ@�56@���@�)@��`@��jA Z�A �A�A?�AA k�A <A �}A��A/�AYA)�A�A@�A_(A:�AܤA!�A�*A{F