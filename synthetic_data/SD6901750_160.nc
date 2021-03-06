CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   v   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2021-03-02T04:54:22Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        :�o     �  n�Argo synthetic profile          1.0 1.2 19500101000000  20210302045422  20210302045422  6901750 RREX                                                            Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDPROVOR                          OIN-14DO-S31-01                 5817A08                         841 @��;UUUU1   @��<��� @ID�/���1NV�p1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL  + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                     DOXY1=DOXY*(1+C*PRES/1000)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PPOX_DOXY = f(DOXY1); PPOX_DOXY_ADJUSTED=OFFSET+(SLOPE*(1+DRIFT/100.*(profile_date_juld-launch_date_juld)/365)+INCLINE_T*TEMP)*PPOX_DOXY; DOXY_ADJUSTED=f(PPOX_DOXY_ADJUSTED)                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 0.99964 (+/- 4e-05) , vertically averaged dS =-0.014133 (+/- 0.01)                                                                                                                                                                                           C=0.0040                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        INCLINE_T=0; SLOPE=1.077270, DRIFT=0.680596, OFFSET=0.000000                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OWC fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OWC Method, 2.01,  -CTD2019V01 & ARGO2019V03 -                                                                                                         Pressure effect correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Adjustment done on PPOX_DOXY;Temporal drift estimated from WOA climatology at surface - Gain based on comparison between Argo cycle 62 and reference profile rr17_1                                                                                             20210119192818202101191928182021011919281820210205114928                                          20210205114928A   B   B   B       ?�  @   @@  @�  @�  @�  A   A  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CX  Cb  Ck  Cu  C  C�� C�  C�� C�� C�  C�  C�� C�  C�  C�  C�  C�� C�� Cŀ Cʀ C�  C�  Cـ Cހ C�  C�  C� C�  C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd� Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111      ?�  @   @@  @�  @�  @�  A   A  A�  A�  B  B4  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C%  C/  C9  CC  CM  CX  Cb  Ck  Cu  C  C�� C�  C�� C�� C�  C�  C�� C�  C�  C�  C�  C�� C�� Cŀ Cʀ C�  C�  Cـ Cހ C�  C�  C� C�  C�  D @ D� D� D  D@ D� D%� D,  D2@ D8� D>� DE  DK@ DQ� DW� D^  Dd� Dj� Dp� Dw  D}@ D�� D�  D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�� D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�  D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�  D�@ D�` D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A^�yA^�yA^�A^�A^�A^�A^�A^�A^�A^�A^��A^��A^��A^�yA^�jA^r�A^9XA]x�AO��AB�A?VA;�A:�uA9�7A8ĜA8=qA7��A6�/A6�\A5��A5K�A4~�A3�mA3O�A2�A1�#A0��A0n�A/�wA/;dA.�+A.bA.(�A.9XA.ZA.I�A-�A,�jA,�A,JA*�A*�jA*A(�DA'��A&��A&��A&M�A%33A"�HA ffA   A��Ap�AƨA��A�A�yAS�A�A�7Av�A\)A V@��@�&�@�o@�(�@⟾@�\)@���@�%@��@��@�h@�$�@��y@Ӆ@��@�J@���@���@��`@�Z@��w@��F@��7@�~�@�^5@���@�\)@��@��`@�b@�7L@��H@���@�hs@�I�@~5?@zn�@x  @u�@t1@q��@o|�@mp�@k"�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A^�yA^�yA^�A^�A^�A^�A^�A^�A^�A^�A^��A^��A^��A^�yA^�jA^r�A^9XA]x�AO��AB�A?VA;�A:�uA9�7A8ĜA8=qA7��A6�/A6�\A5��A5K�A4~�A3�mA3O�A2�A1�#A0��A0n�A/�wA/;dA.�+A.bA.(�A.9XA.ZA.I�A-�A,�jA,�A,JA*�A*�jA*A(�DA'��A&��A&��A&M�A%33A"�HA ffA   A��Ap�AƨA��A�A�yAS�A�A�7Av�A\)A V@��@�&�@�o@�(�@⟾@�\)@���@�%@��@��@�h@�$�@��y@Ӆ@��@�J@���@���@��`@�Z@��w@��F@��7@�~�@�^5@���@�\)@��@��`@�b@�7L@��H@���@�hs@�I�@~5?@zn�@x  @u�@t1@q��@o|�@mp�@k"�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=BDB
=BDBDBDBDBDBJBDBDBDBJBVB{B�B�B�B!�B'�B�B
=B  B��B�B�B�yB�TB�HB�B�B��BƨB��B�jB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�bB�{B�\B�Bz�Bq�Br�Bp�BjBXBC�BN�BM�BI�BD�B8RB)�B�B#�B�B�B��B�B�ZB�B��B��B�TB�;B�ZB�5B�B%�BiyBo�BhsBH�B%�BDB�B��B�B�%Bp�B]/BN�BI�B?}B/B{B��B�B�ZB�mB�HB�#B�B�B�B�B��B��B��B��B��B��B��B��3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B
=BDB
=B��B��B��B��B��B��B��B��B��B��B��B�B�B�B	�B/BSBB��B�kB�?B�"B�B��B��BҳBˌB�rB�=B�B��B��B��B��B�wB�MB�?B�B�B�8B�FB�eB�lB�=B� B�HB�B��B��B��Bs�Bl\Bc#Bd/BbB[�BI�B5B@[B?TB;<B6!B)�B�B.B^B@B,B�B�B��B�5B�yBĈB��B��B��B��B�BqBZ�Ba#BY�B:?BvB��B�BÈB��Bw�BbJBN�B@�B;fB1-B �B.B�B�IB�B�%B�B��B��B��B��BǿBêB��B��B��B��B��B��B��3331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cd�CdGCc��CdRCd rCc��Cc� Cc�	Cd�Cc�\Cc�Cc�Cc��Cc��Cc{�Cb��Ca�C\�iCVy7CXS�CYO�CZ�DC[uC\�C\��C\��C]@�C]��C]��C^ɱC^۲C^Y�C^,�C]�C]�C]��C]�C]�NC]��C^C^J1C^�+C_a�C_�jC`(�C_�C^$�C]C[��CZ�gCY��CX�CW��CV�2CVػCV�CVj�CU"?CRh(CO��CM��CJL�CEn$C@;C>!iC;c�C6>C2��C1��C0h`C/�C/�UC06�C1,gC2�C5�hC8��C:�{C=��C?�CB�CC�<CBqC@��CB�CC��CF��CI�[CL��CO�CRlCU�rCYkJC[x{C]��C`�Ca-�Cb��Ce*�ChN�Ck��Cm��Cn�	Co�Co�cCp�Cq�Cp��CqCqn�Cr;=CrZ:Cr�Cq�#Cq�/Cq�XCp�_CpO23331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�C|��C}
�C}�C|�C|�iC|�C|�2C|�C|�C|�>C|��C|�C|f�C{�)Cz�?CtܐCnCp*Cq,�Cr��Cs�$CtMkCt�YCuG^Cu�?Cu�qCvp�CwW(Cwm�Cv߸Cv�ECvV�CvXJCvN�Cvq`Cve�CvH�Cv��Cv�Cwr�Cx �Cx��Cy CxQ�Cv�mCu�WCtQCr�>Cq�Cp�	Co�hCn��Cn�(Cn�CnM�Cl�Ci�KCg�Cd�C`��C[�9CUƒCSvCPo#CJ�|CGUCE��CDK�CC��CC�WCD#�CE:1CF��CJ<�CM�*CP<�CSEgCU��CX՛CZ3�CXE�CV��CX5�CZC]|�C`޹Cc�7Cgc�CjACn�WCr|�CṫCw��Cy�aC{=zC|�RC��C���C���C��4C�dC�{�C��C�� C��C�wqC���C�ܵC�RTC�gC�F�C�=C��C��C��gG�O�4411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  G�O�G�O�@�v@���@��V@��7@�˘@���@��1@�@��w@�O@�@�q�@�M�@�@�<@�.@�{�@�w�@��@���@���@��@�@�w�@��<@�'w@�D@�rd@�@���@��.@�|e@�}�@�t�@��@슟@�n�@�ӷ@��@��@�4@��@�=@�c@��J@�@�A@� �@�1d@�S�@��@��@�6;@�E�@�Ś@�ie@��q@�م@۽�@��@��-@�9�@� �@��@�@��@�ڦ@�q�@�Κ@��]@�K@�V�@��@�%�@�[
@��@���@��@�)8@�y�@Ϟ�@��@Ϗ�@�X�@Ԡ�@��@�ߑ@�"h@��Z@�|@�ɸ@��@���@���@�0�@���@���@��@�{�@���A A %A �!A.A3�A�A;2Aw�A�A��A�YA�OA�8A�_AR�G�O�