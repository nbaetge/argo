CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   r   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:55:54Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        =���   axis      Z        �  S@   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  U   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U|   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  WD   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Y   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  Y�   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [H   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ]   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ]�   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _L   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  a   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  a�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  cP   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  e   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  e�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  gT   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  i   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  i�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  kX   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  m    DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  m�Argo synthetic profile          1.0 1.2 19500101000000  20210302045554  20210302045554  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��;�[1   @��<��� @I*�1'�/nz�G�1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99962 (+/- 4e-05) , vertically averaged dS =-0.014798 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114928                                          20210205114928A   B   B   B       ?�  @   @@  @�  @�  @�  A   A  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CD  CM  CX  Ca  Cl  Cv  C  C�� C�  C�  C�  C�� C�� C�� C�� C�  C�  C�  C�  C�  Cŀ Cʀ Cπ CԀ C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DQ� D^  Dj� Dw  D�� D�� D�  D�  D�  D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�  D�� D�� D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�@ D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      ?�  @   @@  @�  @�  @�  A   A  Ap  A�  B  B8  B`  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C:  CD  CM  CX  Ca  Cl  Cv  C  C�� C�  C�  C�  C�� C�� C�� C�� C�  C�  C�  C�  C�  Cŀ Cʀ Cπ CԀ C�  C�  C�  C�  C�  C�  C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DQ� D^  Dj� Dw  D�� D�� D�  D�  D�  D�` D�� D�� D�� D�� D�  D�  D�@ D�` D�� D�� D�  D�� D�� D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�@ D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A:I�A:1'A:5?A:I�A:E�A:E�A:9XA:5?A:VA:I�A:VA:r�A:��A:�\A:~�A:jA:jA:^5A:�\A:�!A:�!A:�jA:�uA:�DA:v�A:{A8�+A6��A6z�A6-A5ƨA5"�A4(�A3"�A2v�A1�^A1�PA1l�A1�A0ȴA0�A01A/��A/+A.I�A-��A-t�A,��A,A�A+�TA+��A+&�A*�A*A)�A({A&�/A&1A%t�A#A!��AK�Av�A�A-AbNA�A5?AE�AO�AQ�A@���@��#@���@�J@�V@Լj@�E�@���@�^5@��m@�j@�n�@���@���@�"�@��D@��D@��@���@�M�@�-@�S�@��P@��\@��u@��!@���@��h@�&�@�z�@|�@{dZ@{ƨ@s��@p  @nȴ@m��@m`B@nv�@o\)@k�m@g
=331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A:I�A:1'A:5?A:I�A:E�A:E�A:9XA:5?A:VA:I�A:VA:r�A:��A:�\A:~�A:jA:jA:^5A:�\A:�!A:�!A:�jA:�uA:�DA:v�A:{A8�+A6��A6z�A6-A5ƨA5"�A4(�A3"�A2v�A1�^A1�PA1l�A1�A0ȴA0�A01A/��A/+A.I�A-��A-t�A,��A,A�A+�TA+��A+&�A*�A*A)�A({A&�/A&1A%t�A#A!��AK�Av�A�A-AbNA�A5?AE�AO�AQ�A@���@��#@���@�J@�V@Լj@�E�@���@�^5@��m@�j@�n�@���@���@�"�@��D@��D@��@���@�M�@�-@�S�@��P@��\@��u@��!@���@��h@�&�@�z�@|�@{dZ@{ƨ@s��@p  @nȴ@m��@m`B@nv�@o\)@k�m@g
=331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhB�B�BoB{B{B{B�B{B{B{B{B{B{BuBoBoBoB{B{B{B{BuBuBhBPB��B�B�B�B�B�fB�/B��B��B��B��B��B��BɺBɺB��BȴBƨB��B��B��B�}B�XB�LB�LB�?B�-B�B��B��B��B��B��B��B��B�oB��B�bB�JB�bB�uB��B�BiyBcTB�+B=qBB�B2-B-B;dB�B�TB��B\)BG�B?}B?}B&�B�BuB
=B��B�B�B  BBbB  B��B��B�B�B�/B�;B�5B�B�B�#BȴBBÖBÖBƨB��B�B�B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          BhB�B�B(B0B0B0B7B1B0B.B1B-B/B,B(B(B'B.B/B.B/B+B+BB�B�B�rB�]B�SB�>B�#B��BžB��B��B��B��B��B�zB�{B��B�uB�jB�IB�CB�HB�@B�B�B�B�B��B��B��B��B�|B�qB��B��B��B�7B�jB�+B}B�-B�BB�_Bu�BZNBT+Bw�B.KB3lB#B�B,@BhB�=B��BM%B8�B0|B0{B�B�BzB�BB��BߤBۋB�
B�BjB�
B�B��B�BݗB�>B�IB�DB�B�B�2B��B��B��B��B��B��B�,B�"B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Ch3�ChAhCh�Ch1Ch�Ch�Ch#.Ch'7ChDCh+Ch�Cg�Cg�Cg�iCg٥Cg��Cg��Cg��Cg�Cg�8Cg�~Cg��CgcNCg0>CfcFCc�
C^1C\1�CZ�=CX��CW��CW�CX�sCYo�CZ�CZ�sCZ��CZ��CZ��C[�JC\�?C[��CWR�CU"8CT&!CTTICT-�CT�CS��CR�CQ/CPk�CO��CNv{CMf�CL$CK��CK7�CK��CFl!CA<�C>��C;TtC8��C5��C3̳C1P�C/�C.ӕC/kwC21�C3��C9�'C=�OCA�CCU�CD��CH�}CL��CR�PCY[C\�XC^�<C`x�CcNjCe"<CfƘCh�YCk!�Cl��Cm��Ck|�Ck��Ck:CluwCmCmƉCn�[Cp8Cq��Cq`�Cq�-Cr��Crt�Crl�Cu�Cv#�Cu��Cu�)CtU�CrCo�aCpUCqG�333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444                                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�C��pC�ڀC�ܤC���C��uC���C��C���C��5C��C��XC��C��GC��C���C���C�ƈC��iC���C���C��C�q'C� vC}!nCv�Ct�ECr�Cp�kCo�6Co�NCp��Cq�Cr\Cs:�Csn@Csq�Cs>Ct,�Cu��Ct,�Cop#CmCk� Cl"�Ck��Ck�Ck��CjiCh�Cg�6Cg�Ce�tCd��Cc�Cbn�Cb�Cb��C\�QCW,CT9�CP��CMs�CJ�1CH5\CEvCC�CB��CCh�CF�CHk�CO7 CSz(CXCY�cC[h�C_�bCd`ACk5�Cr��Cv�Cx�mCz�\C}��C�MC�эC��qC�F�C�DGC��C��uC��PC�OOC��C�x�C�ۚC�fC�)�C�C��C�+�C��ZC��.C��!C�M�C��C�`+C�mlG�O�G�O�G�O�G�O�G�O�441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444  G�O�G�O�@�m�@�f@�j1@�l�@�y3@�}�@�m@���@�kH@�N @�N�@�B�@�9}@�@���@�@@�?�@�@�4=@��@�σ@���@��r@�_@�
D@��v@�$4@�Gq@�`�@�d�@�Z�@�@�Z@�@鱅@�@�^�@�hJ@�@�hF@��J@�
@�}=@��@�6@�r�@�[h@�	@�`c@ޒ@��;@�z@�_�@�	@�`
@�4@٧.@��/@�u�@˼[@�0�@�<@�}�@�3:@��@��@��@���@��
@�g�@��=@��@�oo@��@ҡ�@��@@�=�@��@��\@�<�@��@���@�@��!@�T�@�4�@��@��@���@���@��3@�d@���@�X�@�FA A ��A��A�AÈAOaA$�A#�AϢA)YA�qA�*G�O�G�O�G�O�G�O�G�O�