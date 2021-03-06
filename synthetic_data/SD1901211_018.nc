CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   x   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-28T14:47:18Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        >��	4E�        6\   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        :�o     �  oArgo synthetic profile          1.0 1.2 19500101000000  20201028144718  20201028144718  1901211 CORIOLIS_OVIDE                                                  Virginie THIERRY                                                PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               A   IF  DDDDPROVOR                          OIN-10-S3-DO-007                5817A00                         841 @����O�1   @��DDD@@G�+I��2bM��1   ARGOS      PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAT=f(DOXY); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0004 (+/- 6e-05) , vertically averaged dS =0.014259 (+/- 0.01)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A=0.886; B=15.563                                                                                                                                                                                                                                               No significant pressure drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                    No significant temperature drift detected -Calibration error is manufacturer specified accuracy                                                                                                                                                                 Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config 129 -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to a single reference profile (isobaric match as in Takeshita et al. (2013)) on cycle 0; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                          201510151559272015101515592720151015155927                                                        20161116115859A   B   B   B       @   @@  @�  @�  A   A  A0  A`  A�  A�  A�  A�  B  B<  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Ca  Cl  Cv  C  C�� C�  C�� C�� C�  C�  C�  C�� C�� C�� C�� C�� C�� Cŀ Cʀ C�  C�  Cـ Cހ C� C�  C�  C� C�  D @ D� D� D  D@ D� D%� D,  D2@ D9  D>� DE  DK@ DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�@ D�@ D�` D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @   @@  @�  @�  A   A  A0  A`  A�  A�  A�  A�  B  B<  B\  B�  B�  B�  B�  B�  B�  B�  C  C  C  C&  C0  C:  CD  CN  CX  Ca  Cl  Cv  C  C�� C�  C�� C�� C�  C�  C�  C�� C�� C�� C�� C�� C�� Cŀ Cʀ C�  C�  Cـ Cހ C� C�  C�  C� C�  D @ D� D� D  D@ D� D%� D,  D2@ D9  D>� DE  DK@ DQ@ DW� D^  Dd@ Dj� Dp� Dw  D}@ D�� D�� D�  D�  D�@ D�` D�� D�� D�� D�� D�  D�@ D�@ D�` D�� D�� D�� D�� D�  D�  D�@ D�` Dƀ Dɠ D�� D�� D�  D�  D�@ D�` D߀ D� D�� D�� D�  D�@ D�@ D�` D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AJbNAJffAJ�+AJ�DAJ�+AJ�+AJ�DAJ�DAJ�\AJ�\AJ�DAJ�\AJ�+AJ�+AJ�+AJ�AJZAJ9XAJ5?AJ9XAJ-AI��AIVAHȴAHVAH �AHA�AH�DAH1'AF�HAC�ABJAAXAAdZA@�A?|�A=hsA<E�A;�A:VA9G�A9&�A933A9O�A8ȴA8jA6�+A5;dA3��A3K�A3�-A5��A5S�A4��A4jA3�;A4bA3p�A1VA0��A/\)A+��A!�
Al�A�RA�A
n�AO�A33A�u@�{@�r�@�l�@��@���@��`@�bN@��@ߥ�@ڧ�@��@��m@���@��7@���@���@�I�@�z�@��j@�J@��`@�-@�ff@�z�@��@�@�bN@��@���@��P@�
=@�9X@z^5@xA�@u�-@u/@v��@sdZ@p��@m�@k��@j~�@h��@f�y@e/@c�F@bJ@` �@^5?@]�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�AJ�+AJ�+AJ�+AJ�DAJ�DAJ�\AJ�\AJ�DAJ�\AJ�+AJ�+AJ�+AJ�AJZAJ9XAJ5?AJ9XAJ-AI��AIVAHȴAHVAH �AHA�AH�DAH1'AF�HAC�ABJAAXAAdZA@�A?|�A=hsA<E�A;�A:VA9G�A9&�A933A9O�A8ȴA8jA6�+A5;dA3��A3K�A3�-A5��A5S�A4��A4jA3�;A4bA3p�A1VA0��A/\)A+��A!�
Al�A�RA�A
n�AO�A33A�u@�{@�r�@�l�@��@���@��`@�bN@��@ߥ�@ڧ�@��@��m@���@��7@���@���@�I�@�z�@��j@�J@��`@�-@�ff@�z�@��@�@�bN@��@���@��P@�
=@�9X@z^5@xA�@u�-@u/@v��@sdZ@p��@m�@k��@j~�@h��@f�y@e/@c�F@bJ@` �@^5?@]�444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBy�Bz�Bv�Bv�Bv�Bv�Bw�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bu�Bt�Br�Bq�Bq�Bp�Bl�BbNB`BB]/B\)BaHBiyBv�B|�BbNBM�BH�BP�BC�B?}B%�B�B{B+B��B  BB
=B+BB�B�NB��B��B�#BB  B��B��B��BB��B�`B�B�B��BZB(�B�B�5B�B��B�B�B�B��B%B-B33B �B�B�BoBB  B�B��B�9B�\Bp�B[#B6FB{B��B�NB�B�;B�B�B��B��BǮB�}B�qB�wB�-B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�{B�uB�uB�o333311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�qB�qBkB{SBqBo	Bk�Bj�BpBx?B��B��BqB\�BWvB_�BRWBN<B4�B)ZB#6B�B�B�B�B�B�B�B�TB�B�BݏB��B�B�B�B	�BrB�B
�B�B�LB�@B�qBh�B7�BKB��B�B�B�B 8B�,B�B�B;�BA�B/[B,EB%B!B�B�B�B�mB¼B��BBi�BD�B"�B+B�B�}B��B�wB�uB�IB�*B�B��B��B��B��B�BB�9B�.B�@B�zB�\B�GB�.B�B�B��B��B��B��B��B��B��B��444811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Co,1Co0ICo52G�O�Co;�Co@2CoBCoF�CoM�CoRCoVSCoZ�Coe�Co}�Co�Co��Co�RCo3Co!>Co7LCoMHCocPCp>�CpSwCo��Cn��Ck�]CfwC_uC\Z�C[�,C[�NC\�C\�BC\�C[�$C[/�C[A�C\�C\*2C[r�CZ�DCZ�,CYJ�CY[�CW�CW�CVc�CVr�CV��CV�lCU�BCT]PCTk�CS��CS�!CSiCRQ`CQ�RCP��CM�CG��C<�C3ߖC/��C,�C-�FC.�dC08bC2��C5"/C8b3C9NHC;��C>JUC@�|CCE�CEƆCHI?CJ�{CMN,CP��CS!5CW=jC[Y�C^��Ca5�CeW2CjFCm�ZCp+^Cq#xCqU�CrN�Cr~�Csz Ctv#Cur�Cvo*Cv��Cv̒CyfCz`~Cz�uCz��Cz�Cy~�Cy��Cz�Cz��C{�VC|C|=dC|m�C}n�C}�.C}�C}�;C~,�C~\333 33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333            G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                C|kC|�C|�G�O�C|tC|[C|�C|!7C|&�C|*�C|.�C|2|C|<zC|Q�C|gXC|w^C|�iC{�C|�C|�C|,5C|B�C}�C}(C|�6C{��Cy=yCto(Cn>�Ck��Ck�Ck7�Cl `ClJCl1Ck��Ck^Ck.�Ck�Cl�Ck�Cj��Cj��Ci��Ci�bChe�Cg��CgE�Cgg�CgCg��Cf�Cew2Ce��Cd�'Ce�Cd]�Cc�Cc9�Cb�VC_��CZ��CQ~CJ:SCG/�CD�_CE�4CF�CH[OCJ��CM�CPIdCP�=CS.CU�(CW��CZK�C\�6C_�Cak�Cc̊Cf��Cih�CmO�CqBFCtwCv��Cz۵C�NC�]�C���C��C�'�C���C��FC�0C���C��C��.C��C�ʱC���C�}�C���C��cC�mC�"@C�?�C���C��yC�H�C�bKC�|CC��=C�3C�%QC�?�G�O�G�O�G�O�111 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444@@�@�G�O�@@�@ @#&@'t@*E@-@/�@6�@FH@U�@a<@p4@�5@	@&@'8@5R@��@�@[]@�F@�Z@]@�@�@�E@�Q@d@+�@5�@��@G�@S@�4@��@r�@��@	@6@@
'�@	�(@	5�@	?O@	J7@	Ud@�@��@�@|�@��@�@�{@I@��@�'?�k,?�L�?�<�?�>{?�A[?�P�?�o�?��?�@?���?��?�1 ?�_�?�X?���?��?�'@ .�@ʌ@e;@��@~@	��@b>@�d@"J@�*@�p@m@�[@T$@t\@�@2`@�H@t�@F@��@��@�@�@ =�@ [�@ y}@ �@�Z@��@ n�@ ��@!0�@!P@!n�@!�@"2v@"Qe@"qVG�O�G�O�G�O�