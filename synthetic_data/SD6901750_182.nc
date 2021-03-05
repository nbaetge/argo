CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   r   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:59:51Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  m�Argo synthetic profile          1.0 1.2 19500101000000  20210302045951  20210302045951  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @�:�l�1   @�;�F��@G�V��1W�O�;`1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99958 (+/- 5e-05) , vertically averaged dS =-0.016618 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114929                                          20210205114929A   B   B   B       ?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Ck  Cu  C  C�� C�  C�  C�  C�  C�� C�� C�� C�  C�  C�  C�  C�  C�  Cʀ C�  C�  C�  C�  C�  C� C� C� C�� D @ D� D@ D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�  D�` D�� D�� D�� D�� D�  D�  D�  D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D߀ D�� D�  D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      ?�  @   @@  @�  @�  @�  @�  A   A  A   A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Cb  Ck  Cu  C  C�� C�  C�  C�  C�  C�� C�� C�� C�  C�  C�  C�  C�  C�  Cʀ C�  C�  C�  C�  C�  C� C� C� C�� D @ D� D@ D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�  D�` D�� D�� D�� D�� D�  D�  D�  D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ D�� D�� D�� D�  D�  D�@ D߀ D�� D�  D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Ab�Ab$�Ab �Ab(�Ab{Aa��AbAbA�AbM�Aa�Aa�A`AZ�+AV��AT�HAP��AG��AE�mADAC�AA�#A?��A<n�A;x�A:�A9VA81'A8bA77LA6bA5S�A4v�A3�wA2�/A2��A29XA1�A/G�A.��A.ĜA/oA.�RA-��A-|�A-l�A-"�A-/A,�A,n�A,(�A+�#A+��A+�A+�A*��A*=qA)��A)33A(��A(�jA'hsA$ffAp�A�HA�`A�/A�
AȴAO�A��A�A=qA�A�A��AdZ@�33@��@�J@�@�%@ߝ�@�I�@�33@���@�Z@�O�@�t�@�=q@�I�@��F@��!@��+@�5?@�=q@��
@�bN@���@�p�@���@�@���@K�@}/@{�F@{ƨ@{�m@zJ@w;d@vE�@r�!@o��@m�h@kt�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Ab�Ab$�Ab �Ab(�Ab{Aa��AbAbA�AbM�Aa�Aa�A`AZ�+AV��AT�HAP��AG��AE�mADAC�AA�#A?��A<n�A;x�A:�A9VA81'A8bA77LA6bA5S�A4v�A3�wA2�/A2��A29XA1�A/G�A.��A.ĜA/oA.�RA-��A-|�A-l�A-"�A-/A,�A,n�A,(�A+�#A+��A+�A+�A*��A*=qA)��A)33A(��A(�jA'hsA$ffAp�A�HA�`A�/A�
AȴAO�A��A�A=qA�A�A��AdZ@�33@��@�J@�@�%@ߝ�@�I�@�33@���@�Z@�O�@�t�@�=q@�I�@��F@��!@��+@�5?@�=q@��
@�bN@���@�p�@���@�@���@K�@}/@{�F@{ƨ@{�m@zJ@w;d@vE�@r�!@o��@m�h@kt�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBr�Bk�Bm�BgmBl�Bp�Bo�Bs�Br�Bp�Bm�B�DB��B�}B�^B��B2-B49B.B,B-B)�B&�B7LB8RB+B$�B'�B!�B�BuBJB%B��B��B��B�B�B��B�B�TB�`B�)B�/B�HB�;B�HB�BB�#B�B�B�
B�
B��B��B��BǮBƨBƨBƨB�dB��Bx�B�hB�{B�\B��B�B�3B�RB��B�3B�B�fB��B��B�BXBH�BW
B9XB>wB'�B��B�BĜB��B�bB{�Be`BS�B<jB)�B�BDBB��B�;B�B��B��B��B��BȴBȴB��B��B��B��B��B��B��B��B��333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Br�Bk�Bm�BV8B[WB_nB^hBbBazB_pB\\Bz
B��B�=B�!B�XB!B#B�B�B�B�B�B&(B'+B�B�B�B�BmBTB�-B�B��B��B��B߉B�B��B�B�:B�FB�B�B�,B�!B�0B�(B�
B�B��B��B��B��B��B��B��B��B��B��B�MB��Bg�B�\B�pB~PB��B��B�#B�DB�tB�'B�{B�TB��B��BpBGB7�BFB(^B-B�B�B�B��B�B�BkBT�BC'B+�B.B�B�zB�MB� B�{B�]B�AB� B�B�B��B��B�B�)B�B�B�B�B�B�$B�5333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CxVCx�Cx]Cx�Cx!�Cx\�Cx�Cx�3Cx�!CyKCz"C{$�Cp3CiCh �Ck��Cp�hCn/8Cm�Ck�LCi��Cg mCc�7C`��C^Q�C]�ZC_{uC`�~C`�lCa��Ca��Cb�CbNvCbnCb]~Cb�C`�C^E�C^�FCa��Ce�Ce��Cg5@Cf�(Ce4�Ces�CdK^CcQ`Cc�Cbk�Ca�C_w=C^�C\�<C[�[CZUCX�CY�HCY@QCV[�CN��CMe�CEv�C9��C4fpC3-C1ӸC0e2C/�eC.�oC-�BC.]�C.�C/�CC2^.C4V�C6��C:H�C<��C>�>CA"�CB�?CE�xCJ�CM��CP�cCT,CW�CY��C\�,C_Z8Cb:Cd]kCf�fCi[Cjo�Cm�Cp4Cq^CrGCsduCs��CtxYCtړCt��Ct�Cs�#Ct�CtK�Cs�Cs�bCsZ�CrerCqBo333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�C�$jC�$�C�+�C�L�C�dCC��C��fC���C�AkC�چC��.C��C�G�C�m�C��_C��ZC�UC�C�C�!C��HC}��Cz[0Cw��Cv�Cy�Cz6�Cz�C{�C{�7C|��C|BC|g�C|W�C|!Cz�aCwϡCxv�C{�{CZ�C��C��dC���C��C�C~��C}�}C}IUC|��C{�CyG�Cw�Cv Cu)zCs��CrVCs#!Crj2Co2Cf��Ce<�C\g�COB�CIe�CH5CF��CD�tCD�CC�CBGfCB�4CC0CD�CGS�CI��CLB�CP<�CSI�CU_zCW��CY�C]\�Ca��Cf�Cia,CmM!Cp��Cs�lCwNCy�,C}	�C�yC�A�C�{�C�DKC���C���C��C��
C�YgC��(C���C�5sC�K�C��nC���C�ؔC���C��@C��oC���C��`C�a�441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�A��A�A��A�kA�!A��A/AD[A�qABlA [@�={@�7\@�W�A ��@���@���@�|@��(@�@�@�W�@���@�S@��@�4�@�.@�p*@�?@�o�@�*�@�N�@�?�@��@�@��@�M@��@�#�@��F@���@��=@�cF@��R@�nt@�eE@�'�@�ts@���@�O-@���@�9f@�[@��f@�T)@�ih@��@堻@�tz@�4@Ӗ�@���@�W�@��@��e@�Q@�*�@�6�@���@�z@�a�@�E�@�Z�@�|�@�E@��\@��@�֊@�R3@��W@Ԃ@���@��d@�]@��*@�� @��4@�i�@��4@���@�n�@�+�@��O@�=@��wA$?A�TAH\A�/A5�A�AA�nA��AR�A�A_FA|MADAO�A"A��A��