CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB          	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       f2020-12-03T07:48:22Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
references        (http://www.argodatamgt.org/Documentation   user_manual_version       1.0    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      software_version      51.10 (version 30.06.2020 for ARGO_simplified_profile)         V   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                     Lt   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    L�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    L�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    L�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    L�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    L�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    L�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  L�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  M   STATION_PARAMETERS                       	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                    MT   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        OT   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    OX   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    O\   PARAMETER_DATA_MODE                   	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    O`   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     Oh   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     O�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     O�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    O�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       axis      T      
resolution        >Ey��0�:        O�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    O�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >Ey��0�:        O�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           O�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           O�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    O�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    O�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        O�   	PARAMETER            
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                    P    SCIENTIFIC_CALIB_EQUATION            
               	long_name         'Calibration equation for this parameter    
_FillValue                    R    SCIENTIFIC_CALIB_COEFFICIENT         
               	long_name         *Calibration coefficients for this equation     
_FillValue                    Z    SCIENTIFIC_CALIB_COMMENT         
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    b    SCIENTIFIC_CALIB_DATE            
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  p  j    PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    jp   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    jt   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    jx   PROFILE_DOXY_QC                	long_name         #Global quality flag of DOXY profile    conventions       Argo reference table 2a    
_FillValue                    j|   PROFILE_CHLA_QC                	long_name         #Global quality flag of CHLA profile    conventions       Argo reference table 2a    
_FillValue                    j�   PROFILE_BBP700_QC                  	long_name         %Global quality flag of BBP700 profile      conventions       Argo reference table 2a    
_FillValue                    j�   PROFILE_CDOM_QC                	long_name         #Global quality flag of CDOM profile    conventions       Argo reference table 2a    
_FillValue                    j�   PROFILE_NITRATE_QC                 	long_name         &Global quality flag of NITRATE profile     conventions       Argo reference table 2a    
_FillValue                    j�   PRES         	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  j�   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  rT   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  tH   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ~    TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  �|   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  �t   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ˴   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  �l   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   CHLA         	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  �   CHLA_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   
CHLA_dPRES           	         	long_name         6CHLA pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  �d   CHLA_ADJUSTED            	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  (   CHLA_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   CHLA_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     � 	�   BBP700           	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � �   	BBP700_QC            	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � h   BBP700_dPRES         	         	long_name         8BBP700 pressure displacement from original sampled value   
_FillValue        G�O�   units         decibar      � \   BBP700_ADJUSTED          	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � #    BBP700_ADJUSTED_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � *�   BBP700_ADJUSTED_ERROR            	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � ,�   CDOM         	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � 4�   CDOM_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � <`   
CDOM_dPRES           	         	long_name         6CDOM pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      � >T   CDOM_ADJUSTED            	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � F   CDOM_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � M�   CDOM_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � O�   NITRATE          	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � W�   
NITRATE_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _X   NITRATE_dPRES            	         	long_name         9NITRATE pressure displacement from original sampled value      
_FillValue        G�O�   units         decibar      � aL   NITRATE_ADJUSTED         	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � i   NITRATE_ADJUSTED_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � p�   NITRATE_ADJUSTED_ERROR           	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � r�Argo synthetic profile          1.0 1.2 19500101000000  20201203074822  20201203074822  5904479 UW, SOCCOM, Argo equivalent                                     STEPHEN RISER , KENNETH JOHNSON                                 PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                            zA   AO  DDDDARRDNAVIS_A                         0276                            110713                          863 @ׄz��1   @ׄ'҆�@I^��O�;�1#��$�1   GPS        zPRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         DOXY_ADJUSTED=DOXY*G                                                                                                                                                                                                                                            CHLA_ADJUSTED=CHLA/A, NPQ corrected (Xing et al., 2012), spike profile added back in                                                                                                                                                                            not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  NITRATE_ADJUSTED=[NITRATE-SUM(OFFSET(S)+DRIFT(S))]/GAIN                                                                                                                                                                                                         dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            G=1.0391                                                                                                                                                                                                                                                        A=2                                                                                                                                                                                                                                                             not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  OFFSET(S) and DRIFT(S) from climatology comparisons at 1000m or 1500m. GAIN from surface/deep comparison where surface values are known                                                                                                                         Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        G determined by surface measurement comparison to World Ocean Atlas 2009.See Takeshita et al.2013,doi:10.1002/jgrc.20399                                                                                                                                        A is best estimate from Roesler et al., 2017, doi: 10.1002/lom3.10185                                                                                                                                                                                           not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  Contact Tanya Maurer (tmaurer@mbari.org) or Josh Plant (jplant@mbari.org) for more information                                                                                                                                                                  2017062616081320170626160813201706261608132020120212402820201202124028202012021240282020120212402820201202124028A   A   A   B   A   F   F   A   @���@�  A��AffAA��A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D	��D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE�fDFfDF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�3D�` D��fD�l�D���D�ffD��3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��HA
>A�
AC
>Aap�A��RA��RA��RA��RA��A��A�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@BH\)BO��BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C�pC
C
C
C

C
C
C
C0�C
C
C
C
C
C
C 
C"
C$
C&
C(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CN
CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~0�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ]D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D	�]D
��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE)DE�)DF)DF�)DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK]DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\]D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dl�]Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�]Dy��D�b�D��GD�o�D���D�iGD��D�r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AG�mAH1AH�AH�AH{AH�AH1'AH1'AG�AHbAH5?AH-AHA�AHE�AHM�AHQ�AHQ�AHQ�AHVAHVAHZAHZAHZAH^5AHZAH^5AH^5AH^5AH^5AH^5AH^5AHffAHffAHn�AHn�AHjAHn�AHn�AHr�AHn�AHn�AHr�AHr�AHn�AHr�AHv�AHv�AHv�AHz�AHz�AHz�AH~�AH~�AH�AH�+AH�+AH�AH~�AH�AH~�AH�+AH�\AH�\AH~�AH~�AHn�AG�TAG�AF��AE��AE�ADffAC��AB�yAB  AA�FAA`BAAS�AAC�AAC�AA+A@1'A?�wA?�hA?|�A?t�A?
=A>��A>Q�A>E�A>9XA=A=��A=�A=�PA=�7A=�PA=t�A=hsA=l�A=oA<��A<z�A<�+A<�\A<z�A<VA<VA<=qA<�A<1A;��A;�mA;ƨA;��A;t�A;\)A;K�A;/A;�A:�/A:��A:��A:�uA:��A:~�A:Q�A:9XA:-A9�;A9��A9G�A8ȴA9?}A9/A9%A8�/A8�jA8��A8ZA8A�A8=qA8I�A89XA7��A7��A7l�A7%A6��A6��A6��A6�!A6��A6I�A5�#A5�^A5�wA5A5ƨA5�#A5��A5XA5K�A5C�A5?}A5;dA5/A4�`A4r�A3�TA3��A3�FA3�A3dZA3?}A333A3oA2�RA2ZA2^5A2�DA2�A3
=A2��A2��A2��A29XA1�-A1�A2(�A2E�A2JA2M�A2{A1�A1��A1�wA1��A1p�A1A0�A0��A0n�A0JA/��A/�A/�A/�;A/�A/G�A/+A/VA/VA/oA.��A.�\A-��A-�FA-��A-hsA-O�A-|�A-�A-�A-�^A-�^A-�FA-��A-�A-`BA-O�A-C�A-33A-/A-+A-&�A-�A-oA,��A,�/A,ȴA,�RA,��A,�+A,n�A,^5A,A�A,$�A,  A+��A+|�A+\)A+
=A*�A*r�A*jA*n�A*~�A*z�A*n�A*n�A*VA*bA)�^A)�A)`BA)"�A(�yA(�A(A'�TA'�-A'�hA'�A'dZA'oA&�HA&��A&��A&ZA%�;A%��A%p�A%p�A%p�A%p�A%O�A$��A$ȴA$��A$bA#dZA"��A"�A"�RA"E�A!�wA!�7A!t�A!?}A ��A �DA �+A jA�mA�A��A��A�A��A�PA��A�^A��A�PA|�A\)AG�AXAG�A�/A�A~�A=qA��A�AƨAC�A��A��A?}AXA�Al�A��At�A��AbNA�uAQ�A9XAI�AM�AA�A�A�A�FA��AdZA��AI�A{AJA��A�^A��AG�A%A��A�uA�DAbNA9XA=qAE�AQ�A^5A~�A�DA��A��A�!A�jA�`A��AA$�A-A=qA9XA-AJA�A�^Al�AG�A%A�`AC�A��A�uAQ�A�A��AbA
=A
�\A
VA
JA	��A	�-A	x�A��A�\AE�A�FA�HA��Ar�A1A��A
=A�HA��A��A��A�RA�A=qA�^AO�AC�Al�A
=A�A�AbNA �A��A��A�wA�A`BA;dA&�A�A �`A �RA v�A A�A -A �@��@��P@�;d@��H@�J@��#@���@�@��`@��;@�|�@�=q@�=q@�-@��@�/@��9@�Z@�  @�9X@��w@���@�|�@�t�@�dZ@��@�r�@���@��@���@���@�Z@��9@��9@��@��@���@��`@�9X@��
@��!@�O�@��@�=q@��T@�@�O�@�;d@ۮ@��w@��u@��@���@��!@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       AG�mAH1AH�AH�AH{AH�AH1'AH1'AG�AHbAH5?AH-AHA�AHE�AHM�AHQ�AHQ�AHQ�AHVAHVAHZAHZAHZAH^5AHZAH^5AH^5AH^5AH^5AH^5AH^5AHffAHffAHn�AHn�AHjAHn�AHn�AHr�AHn�AHn�AHr�AHr�AHn�AHr�AHv�AHv�AHv�AHz�AHz�AHz�AH~�AH~�AH�AH�+AH�+AH�AH~�AH�AH~�AH�+AH�\AH�\AH~�AH~�AHn�AG�TAG�AF��AE��AE�ADffAC��AB�yAB  AA�FAA`BAAS�AAC�AAC�AA+A@1'A?�wA?�hA?|�A?t�A?
=A>��A>Q�A>E�A>9XA=A=��A=�A=�PA=�7A=�PA=t�A=hsA=l�A=oA<��A<z�A<�+A<�\A<z�A<VA<VA<=qA<�A<1A;��A;�mA;ƨA;��A;t�A;\)A;K�A;/A;�A:�/A:��A:��A:�uA:��A:~�A:Q�A:9XA:-A9�;A9��A9G�A8ȴA9?}A9/A9%A8�/A8�jA8��A8ZA8A�A8=qA8I�A89XA7��A7��A7l�A7%A6��A6��A6��A6�!A6��A6I�A5�#A5�^A5�wA5A5ƨA5�#A5��A5XA5K�A5C�A5?}A5;dA5/A4�`A4r�A3�TA3��A3�FA3�A3dZA3?}A333A3oA2�RA2ZA2^5A2�DA2�A3
=A2��A2��A2��A29XA1�-A1�A2(�A2E�A2JA2M�A2{A1�A1��A1�wA1��A1p�A1A0�A0��A0n�A0JA/��A/�A/�A/�;A/�A/G�A/+A/VA/VA/oA.��A.�\A-��A-�FA-��A-hsA-O�A-|�A-�A-�A-�^A-�^A-�FA-��A-�A-`BA-O�A-C�A-33A-/A-+A-&�A-�A-oA,��A,�/A,ȴA,�RA,��A,�+A,n�A,^5A,A�A,$�A,  A+��A+|�A+\)A+
=A*�A*r�A*jA*n�A*~�A*z�A*n�A*n�A*VA*bA)�^A)�A)`BA)"�A(�yA(�A(A'�TA'�-A'�hA'�A'dZA'oA&�HA&��A&��A&ZA%�;A%��A%p�A%p�A%p�A%p�A%O�A$��A$ȴA$��A$bA#dZA"��A"�A"�RA"E�A!�wA!�7A!t�A!?}A ��A �DA �+A jA�mA�A��A��A�A��A�PA��A�^A��A�PA|�A\)AG�AXAG�A�/A�A~�A=qA��A�AƨAC�A��A��A?}AXA�Al�A��At�A��AbNA�uAQ�A9XAI�AM�AA�A�A�A�FA��AdZA��AI�A{AJA��A�^A��AG�A%A��A�uA�DAbNA9XA=qAE�AQ�A^5A~�A�DA��A��A�!A�jA�`A��AA$�A-A=qA9XA-AJA�A�^Al�AG�A%A�`AC�A��A�uAQ�A�A��AbA
=A
�\A
VA
JA	��A	�-A	x�A��A�\AE�A�FA�HA��Ar�A1A��A
=A�HA��A��A��A�RA�A=qA�^AO�AC�Al�A
=A�A�AbNA �A��A��A�wA�A`BA;dA&�A�A �`A �RA v�A A�A -A �@��@��P@�;d@��H@�J@��#@���@�@��`@��;@�|�@�=q@�=q@�-@��@�/@��9@�Z@�  @�9X@��w@���@�|�@�t�@�dZ@��@�r�@���@��@���@���@�Z@��9@��9@��@��@���@��`@�9X@��
@��!@�O�@��@�=q@��T@�@�O�@�;d@ۮ@��w@��u@��@���@��!@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�TB�B��BB\B�B#�B+B5?B8RB;dB<jB<jB<jB;dB:^B;dB;dB;dB<jB9XB49B2-B1'B1'B-B-B0!B0!B0!B0!B0!B1'B33B2-B.B,B-B-B.B-B.B.B-B,B+B+B)�B'�B%�B%�B&�B%�B%�B#�B"�B!�B!�B"�B!�B�B�B�B�B�B�B�B�B"�B#�B#�B"�B!�B�B�B�B �B�B�B�B�B�B�B{B{BuBoB\BDBDBJBPBPB\BbBVBVBVBVBPBPB
=B%BB  B  B��B��B��B��B��B��B��B��B��BBBBB+BB��B��BB	7B+BJBDB
=B
=B	7B1B%BBB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�ZB�TB�TB�NB�HB�ZB�mB�mB�sB�sB�sB�sB�mB�mB�mB�fB�fB�mB�sB�yB�yB�sB�sB�mB�mB�fB�fB�`B�`B�`B�`B�`B�fB�TB�HB�;B�)B�B�B�B�
B�B�B�B�B�#B�B�B�
B��B��B��B��BȴBǮBǮBƨBƨBŢBÖBB��B��B�wB�jB�^B�RB�RB�RB�RB�LB�?B�3B�!B�B��B��B��B��B��B�{B�oB�hB�hB�VB�PB�PB�JB�+B�B�B�%B�=B�DB�DB�PB�hB�oB�{B�{B�uB�uB��B��B��B��B�uB�bB�7B� Bw�Bq�Bm�BcTB_;BaHB]/BO�BR�BS�BP�BL�BP�BP�BS�BW
BYB[#BZBZBYBXBZBYBW
BVBT�BVBW
BXBVBS�BR�BR�BR�BT�BZB_;BaHBcTBe`BiyBjBl�Bm�Bn�Bp�Bv�B�1B�hB��B��B��B��B��B��B��B��B��B��B�oB~�Bk�Be`BcTBaHBXBR�BO�BG�BA�B>wB;dB9XB7LB49B0!B-B)�B%�B �B�B�B�B�B�B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoBhBoB�B�B�B�B�B�B�B!�B&�B(�B-B-B,B2-B49B5?B7LB@�BE�BC�B@�B9XB1'B%�B#�B#�B1'B=qBF�BVB��B)�B�5BĜB�9B�911111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�QB�B��BB\B�B#�B+B5?B8QB;cB<lB<hB<hB;eB:[B;bB;eB;eB<iB9YB49B2*B1&B1(B-B-B0B0B0B0B0B1%B33B2+B.B,B-B-B.B-
B.B.B-
B,B+B+ B)�B'�B%�B%�B&�B%�B%�B#�B"�B!�B!�B"�B!�B�B�B�B�B�B�B�B�B"�B#�B#�B"�B!�B�B�B�B �B�B�B�B�B�B~BvBvBsBmBWB@BCBIBNBNBYBaBVBVBVBUBNBKB
8B$BB��B��B��B��B��B��B��B��B��B��B��BBBBB(BB��B��BB	6B'BGBBB
;B
;B	5B/B%BBB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�UB�PB�PB�KB�DB�UB�hB�hB�nB�nB�lB�nB�jB�iB�kB�eB�eB�hB�qB�uB�uB�pB�pB�kB�gB�bB�bB�\B�[B�]B�_B�[B�eB�PB�FB�7B�'B�
B�B�B�	B�B�B�B�B�"B�B�B�B��B��B��B��BȰBǬBǪBƣBƥBşBÑBB��B�}B�rB�gB�YB�NB�NB�NB�KB�IB�;B�.B�B��B��B��B��B��B��B�rB�iB�aB�cB�OB�MB�MB�CB�&B�B�B�!B�5B�?B�=B�MB�`B�gB�wB�tB�lB�nB�|B��B��B�~B�mB�ZB�/B�Bw�Bq�Bm�BcOB_3Ba?B]&BO�BR�BS�BP�BL�BP�BP�BS�BWBYB[BZBZBYBX	BZBYBWBU�BT�BU�BW BX
BU�BS�BR�BR�BR�BT�BZB_5Ba@BcLBe\BisBjzBl�Bm�Bn�Bp�Bv�B�*B�_B��B��B��B��B��B��B��B��B��B��B�jB~�Bk|BeYBcMBa?BXBR�BO�BG�BA�B>oB;\B9RB7DB40B0B-B)�B%�B �B�B�B�B�BzBqBpB�B�B�B�B�B�B|B~BvB}BzB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BBB}BvBsBhB]BfB~B�B�B}B�B�B�B!�B&�B(�B-B-B, B2'B4/B55B7FB@yBE�BC�B@{B9RB1B%�B#�B#�B1B=gBF�BLB��B)�B�)BđB�-B�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cu�4Cu��Cu��Cu��Cu��Cu�=Cu��Cu��Cu��Cu��Cu�XCu�RCu��Cu��Cu��Cu�qCu�Cu�xCux.Cut�Cun�CuuCuq>Cu�2CuphCus�Cup�Cuk�Cup~CuTrCuaHCu@#Cu@Cu>)CuJ?CuJ�Cu9�Cu=�CuTGCupGCus�Cus�Cu]CuR�CuD�CuK;CuH�CuG�Cu>�CuJ�Cu6�Cu5�CuaCuHCu�Cu$�Cu�Cu �Ct�0CtܛCt�Ct]6Ct2CsW�Cr9�Co��CmxCj߿Ch�Ce�CcaCaK�C_v�C^\�C]�=C^C^8�C^@�C^\SC^m�C^��C_1�C_EC_LhC_AC_lC^�QC^^%C^R�C^B�C^�C^,%C^PzC^>C^2�C^A�C^<�C^(fC^�C^	�C]�C]�WC]��C]ӾC]кC]��C]�@C]��C]��C]�C]��C]�C]��C]�sC]��C]�C]�|C]��C]�C]�?C]��C]�^C]��C]��C]xC]hUC]|�C]x�C]�:C]��C]��C]�ZC^4VC^)�C^R"C^YC^fC^q�C^r^C^{nC^��C^��C^JC^��C^zWC^g�C^z C^��C^�C^��C^��C^��C^�"C^��C^�	C^�wC^��C^�C^�C^�+C^��C^�aC^�C^�C^�
C^�C^�-C^��C^��C^��C^��C^�}C^�fC^�fC^�EC^q>C^m�C^wYC^�C^�.C^v�C^b�C^GzC^R;C^G^C^28C^3zC^RQC^_�C^J�C^D�C^YjC^C�C^E<C^` C^@6C^4|C^+�C^&�C^)�C^AyC^Y�C^iuC^�_C^�RC^xDC^D%C^
�C]�,C]��C]�-C]��C]��C]�?C]bC\�PC\�)C\��C\��C\��C\��C\�3C\�C\��C\��C\�xC\=�C[�+C[�xC[h-C[C�C[/DC[7pC[[tC[�IC[�[C[��C[��C[��C[�jC[�?C[�@C[��C\3�C\�<C\��C\�)C]KBC]�C\�C\6�C[�@C[1CZ�CZ�gCZ��CZ�C[6C[�C[=�CZ�eCZBcCY�CXA�CX�CW�9CWmbCW'�CWpCW�CV�$CVʒCV�CV��CVw�CVf�CVECV4�CU��CU�jCU�JCUқCU�YCU�&CU�CU��CUe|CU8�CU�CT��CT��CT�CTcnCT/kCT%�CT	�CS��CS�CSrjCR�ZCR��CRw/CR[�CRI�CRX�CR<CQ�CP�}CO�fCO�CO1MCNqCM�GCMh�CL�jCL��CK�CKN CJjCIY�CH�CH�mCH��CHz�CHx�CH��CH�TCH�kCH��CH��CH�CG�CG��CG�SCG�RCG^�CF�MCFTDCFX�CFq�CFa�CF1CFTCF#%CF,�CF:8CF'�CE�rCD��CDzCC��CCh�CB��CB�zCBQ�CA�CA�+CAq�CA$�C@�C@��C@/C?ؙC?�7C?}WC?[C?NsC?&DC?}C?C?C?'C?�C?p!C?<�C?$C>�C>��C>��C>d>C>)"C> mC=��C=�#C=�DC=a�C=�C=StC=�^C=�C=t�C=V�C=~�C=t�C=��C=�NC=ϓC=�XC=�cC=�]C=��C=�<C=�oC=��C=�C>�C>?#C><�C>JpC>e�C>|�C>�FC>��C>��C>e�C>N7C>T4C>c�C>n�C>��C>�C>��C>~|C>�NC>}�C>�C>�C>�C>��C>�#C>��C>�TC>��C>�'C>��C>��C>�C?�C?%RC?/C?8\C?LC?^�C?{�C?��C?��C?�wC?�~C?ژC?�^C@<�C@ohC@�C@��C@�C@��CA&CA?�CAH�CAQ�CA\DCAR2CAo�CArCAx�CAc�CAL\CA�C@�ZC@��C@�*C@��C@�C@�{C@��C@�C@�7C@��C@��C@�?CAJ�CA~9CA�_CBuRCCCC7�CC#�CB��CBVjCDo.CP�C]oECk��Cv�vC{ԳC~�C}��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       C[C]�CH�CP@CXqCB!CPVCC�CXCD�C/�C?C5C,�C&vCC+C.�C;C�C�C
C
C�C	'C�C	SCVC	?C~�C~�pC~��C~�_C~��C~�C~��C~�^C~ԸC~��C	C�C�C~�C~�fC~��C~�C~�4C~�C~ՙC~�C~�?C~�qC~��C~��C~�!C~��C~��C~�%C~�ZC~o�C~0�C}�2C}�5C|�oC{�{CybDCvY�Ct�CqQCm��Ck�[CjCh3�Cg�Cf�/Cf�aCf��Cf�ECgCg ICgc�Cg��Cg��Ch�Cg��Cg��CgN{Cg�CgUCf�qCf�%Cf� Cg�Cf�Cf��Cf�iCf� Cf�Cf�ICf�Cf��CfoCf�"Cf�$Cf}CfT�CfVCfl3CfR�CfC"CfYsCfh�Cf�ECf�mCf��Cf��CfqSCfO,CfKyCfJ�Cf1)Cf.�Cf0�Cf-BCf �Cf�Cf%�Cf!�Cf4�CfGBCfk�Cf�{Cf�CfـCgyCg
�Cg+Cg$OCg$�Cg.cCgA�CgK�Cg2eCg<bCg-ACg,Cg,�CgAcCg4@CgK�Cg:!Cg6)CgZ�Cgu%Cg�eCg��Cg�VCg��Cg��Cg�Cg�uCg��Cg��Cg�iCg�0Cg�VCgn�CgX�Cg\ICgN�CgQ�CgS�CgV�Cg[	CgZ�Cg#�Cg kCg*%Cg2�Cg4]Cg)eCg�Cf�gCg�Cf�JCf�PCf�Cg�Cg�Cf��Cf��Cg
Cf�wCf�CgCf��Cf�Cf۔Cf�gCfٻCf�*CggCg�CgACgY�Cg+Cf��Cf�%Cf��Cf�?Cf��Cf��Cf�YCfh�Cf	�Ce�xCef�CeoCeDRCe4�Cel�Ce��Ce��CetKCeK�Ce$�Cd�0Cdk�Cd�Cc�ZCcրCc�7CcɵCc�"CdGGCdo�Cd�<Cd�(Cdl_CdcyCd��Cds�Cd}�Cd�Ce8MCeZ�Ce��Ce�SCe��CeOnCd�@Cd#�Cc�#CcXCcC�CcL/Cc��Cc�Cc�%Cc�-Cc�3Cb�Ca��C`�CC`v�C` 8C_ٺC_�~C_i�C_��C_F�C_0�C_6C^��C^�]C^��C^��C^��C^S.C^3�C^,zC^.�C^>4C^�C]��C]�3C]�C]��C]]FC]	�C\�MC\�7C\�ZC\{OC\q C\T^C\E`C\�C[��C[!8CZ��CZ��CZ�lCZ��CZ�CZ>�CY�MCX�ECX�CW��CWK7CV�CU�^CUp�CU >CT�%CS��CSA!CRcCQ9PCP��CP�CP{�CPQUCPO�CP]CP�9CP��CP��CP��CO�CO��CO{YCO��CO��CO*HCNT�CNvCN]CN4NCN#}CM��CM�zCM�kCM�CM�eCM�CM�7CL�kCKȃCKe�CK�CJ�KCJ-CI��CI��CI(�CI�CH�CHo�CH!ZCG��CGX�CG/�CF�CF�tCF�YCF��CF�\CF�fCF��CF�*CF��CF�YCF��CF��CFPxCF CE��CE��CE��CEnDCE9�CE%:CEmCD�~CDwsCD��CD��CD�*CD��CD��CD�XCD�>CD�CE3�CE;�CEJ�CENCED�CEKcCE4�CEXtCEepCE`CE��CE�mCE�CE�,CEמCE�gCE��CE��CE��CEדCE�CE�RCEջCE�"CE��CF�CE��CE�ACF�CE�ACE��CF�CF�CF�CF"�CF�CF,RCF'fCF--CF5KCFN7CFh�CF��CF��CF��CF�fCF��CF�WCF�TCGYCG((CGN_CG`CGZ�CG�&CG��CG��CH%CHkxCHaTCH�CH�mCH�	CH�}CH�CH�CH�CCI 0CI`CI	gCH�\CH�2CH��CHN�CH2JCH2�CH8�CH=�CHPCH@�CHM�CHY)CHhWCHbCH��CHَCICIwCJ�CJ�CJ��CJ��CJHCI�CLiCX&
Cf�Ct�C�8�C�ֶC�G=G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114   @#mu@#o@#a�@#f�@#k�@#]�@#f�@#^m@#k�@#_I@#Q�@#[�@#U#@#O�@#K�@#G@#N�@#Q
@#>:@#<@#8@#</@#9�@#F�@#9@#;a@#9+@#5�@#9@#&w@#/ @#�@#2@#�@#�@#�@#�@#�@#&Z@#8�@#;;@#;_@#,4@#%`@#@# V@#�@#@#@# @#�@#4@"��@# s@"�@#�@"�|@"��@"�@"��@"��@"�@"O~@!�@!@�@��@2e@M@L�@m@��@��@�w@�z@�@�8@͝@�@�@�@n
@z�@�@x&@O�@	D@�A@��@� @�7@� @�)@��@�q@�X@��@��@��@��@�(@zN@�I@�6@�5@iK@jH@xr@h@^*@ls@v+@�6@��@�@�@{�@e�@c�@b�@R�@Q@R�@P*@H;@=�@KY@H�@T�@`�@x@��@�s@�g@�C@��@�@�G@�@��@ �@v@�K@��@�@��@��@ �@�{@�@�>@��@ @"@?�@?�@F�@BS@?�@7�@4"@C�@=b@-�@+ @,`@�@�@@	�@v@�@�@N@7@��@��@�@��@��@�@�C@�-@�U@�@�
@��@�c@�t@�Y@Ё@�@ϩ@а@�@�X@Ō@��@�k@��@�/@�W@��@ �@�@�@��@��@�@��@��@��@��@v@@9�@��@�E@�r@�@�@�@��@�o@��@��@��@w)@0W@� @�/@��@�V@��@�@#@3@?�@>.@0�@+/@@Z@5�@<@p�@�d@ɑ@�2@*r@"@�2@r�@�@Ē@�@r�@xp@��@��@��@��@��@%�@Ru@��@�+@p�@C�@o@��@
�@�@�c@�`@�D@�<@�@~�@s�@I�@5�@0�@2}@<J@�@6@�@��@�@�V@w@n�@S�@>N@�@3@�@�3@ק@�@>8@:@��@��@��@�@�+@U�@
ƒ@
J'@
@	ɽ@	I�@�@�<@R@�@�F@3�@gI@�=@��@��@m�@R�@Q�@ZE@w�@{�@v�@uy@�@�R@��@��@��@��@]@��@��@��@��@�c@��@�@ʖ@�t@�@�u@0@k�@,�@�@��@d@:2@@ �
@ �@ q�@ G�@ X?��#?�*?���?���?��#?�r^?�<�?�-B?�c?�"?�2"?�1�?��*?�Z�?�+x?�פ?���?�d�?�:�?��>?��?�r�?�X�?�8�?��1?�z-?��
?�e?��?��?��"?�	f?��y?��?�ku?�u ?���?���?���?��u?�l�?��/?���?���?��?�	�?�k?��?�<�?�[e?�n�?�l�?�d�?�<�?��?�%�?�:�?�I"?�k[?���?�oB?�]�?�uw?�\|?�a@?�{�?�w�?���?��%?���?��^?��?��w?���?���?���?��?�;�?�H�?�T�?�o4?��?���?��?��?��?�3I?�,�?�\Y?��?��@ �@ D�@ >J@ S�@ r�@ ��@ ��@ ��@ ��@ �+@ ��@ �\@ ��@ ��@ �I@ k\@ 2�@  /@  l@ $-@ '�@ 3A@ )�@ 1�@ 9@ B�@ >�@ S�@ �<@ �r@ �@Q�@�U@�$@ŝ@u�@=8@�/@
U�@Be@@$�@'y;@)P�G�O�>څ�>�>���>��>�h
>�D�>т�>�bN>��>څ�>��?>��>�h
>ң>�bN>ۥ�>�!�>��>т�>т�>�e,>��>�e,>��a>�'R>�>�$t>�$t>��>�D�>څ�>��>��?>���>��>��>ۥ�>�A�>�A�>�!�>��?>ۥ�>�bN>��>��?>��?>���>���>�bN>�bN>�$t>��>�!�>ɠ'>�$t>�D�>���>���>�e,>څ�>��>ң>��>�e,>�f>��>�>�k�>���>\�?>$tT=��=�3�=49X<�-�<49X<XD�<49X<XD�<�9X<49X<49X<49X<49X<|PH<49X<XD�<-�<-�<XD�<49X<-�<�-�<|PH;�D�<-�<-�<XD�<49X<XD�<|PH<-�<XD�;�D�<|PH<XD�<-�<-�<|PH<49X<XD�<49X<49X<XD�<49X<-�;�D�<49X<XD�<49X<XD�<XD�<XD�;�D�;�-�<49X;�D�<XD�<49X<XD�<XD�;�D�<-�<49X<-�<-�<�PH<-�<-�<�-�<-�<XD�<XD�;�D�<49X<-�<|PH<-�<49X<-�<-�<XD�<|PH<XD�<|PH<-�<XD�>|PH;�D�;�-�<49X<-�<49X<49X<XD�<-�<XD�<-�<|PH<49X<-�<XD�<49X<-�;�D�<-�<49X;�D�<-�<49X<49X<49X<49X<49X<XD�<-�;�D�<49X<49X<-�<-�<49X<49X<49X;�D�;�-�<-�;�D�<-�<49X<49X<49X<-�<49X<|PH<-�;�D�;�D�<49X<�-�<XD�<-�<XD�;�D�<49X<49X<-�<-�<XD�<49X;�-�<XD�<XD�<-�<-�<-�<49X<-�<|PH;�-�<-�<-�<49X<�-�<XD�<49X<XD�<-�<-�;�D�<XD�<XD�<-�<-�<XD�<XD�<-�<-�<49X<49X<-�<-�<49X<-�<49X<XD�<49X<-�;�D�<-�;�D�<-�<-�;�D�<49X<49X<49X;�D�<XD�<-�;�D�;�D�<-�<-�<-�;�D�<49X<49X<49X<XD�;�-�<-�<49X;�D�<49X<|PH;�D�<|PH<-�<-�<XD�<49X<|PH<|PH<49X<49X<-�<-�<XD�<XD�<|PH<XD�<-�<49X<49X;�D�<49X<�-�<XD�<XD�<�-�<49X<49X<XD�<XD�<-�<49X<49X<XD�<XD�<|PH<-�<XD�<XD�<�3�<XD�<XD�<|PH<49X<XD�<49X<|PH<�3�<�-�<XD�<49X<XD�<XD�<XD�<49X<XD�<XD�<-�<�-�<|PH<XD�<|PH<|PH<XD�<�-�<-�<XD�<XD�<�9X<XD�<|PH<49X<XD�<�-�<�-�<�-�<|PH<�?<�-�<|PH<�-�<�-�<XD�<|PH<|PH<�3�<�9X<49X<�-�<|PH<|PH<XD�<|PH<49X<|PH<|PH<|PH<-�<XD�<�-�<�-�<49X<XD�<�-�<�-�<XD�<�9X<�-�<XD�<|PH<XD�<|PH<�3�<XD�<�-�<�3�<49X<49X<�9X<|PH<�-�<�-�<�3�<|PH<XD�<�-�<XD�<�-�<49X<|PH<�-�<|PH<|PH<XD�<XD�<�-�<XD�<�?<�3�<XD�<XD�<�-�<�-�<|PH<�-�<|PH<�-�<�-�<�9X<�3�<|PH<XD�<|PH<�-�<�-�<�-�<�-�<�3�<�3�<XD�<XD�<|PH<|PH<XD�<XD�<|PH<|PH<�-�<�3�<�3�<|PH<XD�<�-�<|PH<|PH<�-�<�-�<�-�<49X<|PH<�-�<XD�<�-�<XD�<�-�<�-�<�-�<|PH<�-�<�-�<|PH<�-�<�-�<�-�<49X<�-�<�3�<�3�<�9X<|PH<|PH<|PH<�-�<�3�<�9X<�3�<�-�<�9X<49X<�-�<|PH<XD�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       >T�>PbN>E�>OA�>\�?>R�>K��>J��>XD�>T�>W$t>OA�>\�?>M;>J��>V>H�>OA�>K��>K��>S�a>OA�>S�a>N!�>Z��>PbN>Q��>Q��>_�>R�>T�>Ye,>W$t>F?>OA�>_�>V>I�'>I�'>H�>W$t>V>J��>Ye,>W$t>W$t>r->F?>J��>J��>Q��>Ye,>H�>C�]>Q��>R�>F?>h	�>S�a>T�>B�>M;>Ye,>S�a>]�>5Y�>$tT>��=�PH=т�=�0�=OA�=�q<�+;XD�    :�-�    :�-�;�9X                ;-�    :�-ຐ-ຐ-�:�-�    ��-�;XD�;-�-ຐ-ຐ-�:�-�    :�-�;-ຐ-�:�-�-�;-�:�-ຐ-ຐ-�;-�    :�-�        :�-�    ��-�-�    :�-�    :�-�:�-�:�-�-�XD�    �-�:�-�    :�-�:�-�-ຐ-�    ��-ຐ-�<"3���-ຐ-�;XDк�-�:�-�:�-�-�    ��-�;-ຐ-�    ��-ຐ-�:�-�;-�:�-�;-ຐ-�:�-�=���-�XD�    ��-�        :�-ຐ-�:�-ຐ-�;-�    ��-�:�-�    ��-�-ຐ-�    �-ຐ-�                    :�-ຐ-�-�        ��-ຐ-�            �-�XDк�-�-ຐ-�            ��-�    ;-ຐ-�-�-�    ;XD�:�-ຐ-�:�-�-�        ��-ຐ-�:�-�    �XD�:�-�:�-ຐ-ຐ-ຐ-�    ��-�;-�XDк�-ຐ-�    ;XD�:�-�    :�-ຐ-ຐ-�-�:�-�:�-ຐ-ຐ-�:�-�:�-ຐ-ຐ-�        ��-ຐ-�    ��-�    :�-�    ��-�-ຐ-�-ຐ-ຐ-�-�            �-�:�-ຐ-�-�-ຐ-ຐ-ຐ-�-�            :�-�XDк�-�    �-�    ;-�-�;-ຐ-ຐ-�:�-�    ;-�;-�        ��-ຐ-�:�-�:�-�;-�:�-ຐ-�        �-�    ;XD�:�-�:�-�;XD�        :�-�:�-ຐ-�        :�-�:�-�;-ຐ-�:�-�:�-�;�-�:�-�:�-�;-�    :�-�    ;-�;�-�;XD�:�-�    :�-�:�-�:�-�    :�-�:�-ຐ-�;XD�;-�:�-�;-�;-�:�-�;XDк�-�:�-�:�-�;�9X:�-�;-�    :�-�;XD�;XD�;XD�;-�;�D�;XD�;-�;XD�;XD�:�-�;-�;-�;�-�;�9X    ;XD�;-�;-�:�-�;-�    ;-�;-�;-ຐ-�:�-�;XD�;XD�    :�-�;XD�;XD�:�-�;�9X;XD�:�-�;-�:�-�;-�;�-�:�-�;XD�;�-�        ;�9X;-�;XD�;XD�;�-�;-�:�-�;XD�:�-�;XD�    ;-�;XD�;-�;-�:�-�:�-�;XD�:�-�;�D�;�-�:�-�:�-�;XD�;XD�;-�;XD�;-�;XD�;XD�;�9X;�-�;-�:�-�;-�;XD�;XD�;XD�;XD�;�-�;�-�:�-�:�-�;-�;-�:�-�:�-�;-�;-�;XD�;�-�;�-�;-�:�-�;XD�;-�;-�;XD�;XD�;XD�    ;-�;XD�:�-�;XD�:�-�;XD�;XD�;XD�;-�;XD�;XD�;-�;XD�;XD�;XD�    ;XD�;�-�;�-�;�9X;-�;-�;-�;XD�;�-�;�9X;�-�;XD�;�9X    ;XD�;-�:�-�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   >��>�bN>��>�A�>��?>ң>���>���>�D�>��>�$t>�A�>��?>�;>���>�>��>�A�>���>���>��a>�A�>��a>�!�>څ�>�bN>т�>т�>��>ң>��>�e,>�$t>�?>�A�>��>�>ɠ'>ɠ'>��>�$t>�>���>�e,>�$t>�$t>�->�?>���>���>т�>�e,>��>��]>т�>ң>�?>�	�>��a>��>��>�;>�e,>��a>��>�Y�>�tT>���>|PH>Q��>0�=�A�=��q=+;�D�    ;-�    ;-�<49X                ;�-�    ;-�;-�;-�;-�    ;-�;�D�;�-�;�-�;-�;-�;-�    ;-�;�-�;-�;-�;�-�;�-�;-�;-�;-�;�-�    ;-�        ;-�    ;-�;�-�    ;-�    ;-�;-�;-�;�-�;�D�    ;�-�;-�    ;-�;-�;�-�;-�    ;-�;-�<�3�;-�;-�;�D�;-�;-�;-�;�-�    ;-�;�-�;-�    ;-�;-�;-�;�-�;-�;�-�;-�;-�>q�;�-�;�D�    ;-�        ;-�;-�;-�;-�;�-�    ;-�;-�    ;-�;�-�;-�    ;�-�;-�                    ;-�;-�;�-�        ;-�;-�            ;�-�;�D�;-�;�-�;-�            ;-�    ;�-�;-�;�-�;�-�    ;�D�;-�;-�;-�;�-�        ;-�;-�;-�    ;�D�;-�;-�;-�;-�;-�    ;-�;�-�;�D�;-�;-�    ;�D�;-�    ;-�;-�;-�;�-�;-�;-�;-�;-�;-�;-�;-�;-�        ;-�;-�    ;-�    ;-�    ;-�;�-�;-�;�-�;-�;-�;�-�            ;�-�;-�;-�;�-�;�-�;-�;-�;-�;�-�            ;-�;�D�;-�    ;�-�    ;�-�;�-�;�-�;-�;-�;-�    ;�-�;�-�        ;-�;-�;-�;-�;�-�;-�;-�        ;�-�    ;�D�;-�;-�;�D�        ;-�;-�;-�        ;-�;-�;�-�;-�;-�;-�<-�;-�;-�;�-�    ;-�    ;�-�<-�;�D�;-�    ;-�;-�;-�    ;-�;-�;-�;�D�;�-�;-�;�-�;�-�;-�;�D�;-�;-�;-�<49X;-�;�-�    ;-�;�D�;�D�;�D�;�-�<XD�;�D�;�-�;�D�;�D�;-�;�-�;�-�<-�<49X    ;�D�;�-�;�-�;-�;�-�    ;�-�;�-�;�-�;-�;-�;�D�;�D�    ;-�;�D�;�D�;-�<49X;�D�;-�;�-�;-�;�-�<-�;-�;�D�<-�        <49X;�-�;�D�;�D�<-�;�-�;-�;�D�;-�;�D�    ;�-�;�D�;�-�;�-�;-�;-�;�D�;-�<XD�<-�;-�;-�;�D�;�D�;�-�;�D�;�-�;�D�;�D�<49X<-�;�-�;-�;�-�;�D�;�D�;�D�;�D�<-�<-�;-�;-�;�-�;�-�;-�;-�;�-�;�-�;�D�<-�<-�;�-�;-�;�D�;�-�;�-�;�D�;�D�;�D�    ;�-�;�D�;-�;�D�;-�;�D�;�D�;�D�;�-�;�D�;�D�;�-�;�D�;�D�;�D�    ;�D�<-�<-�<49X;�-�;�-�;�-�;�D�<-�<49X<-�;�D�<49X    ;�D�;�-�;-�:S��:N�:S��:N�f:J�:Dզ:RU1:Xw�:a<:N�=:W=�:Xw�:Tɸ:Mm-:T��:S��:T�
:N��:T�:RU�:J�_:F�:RU�:Y��:L3y:Mm~:O�:Q�:J�H:GJ�:Mm�:\&�:J�q:RV::J��:RV%:RV::T�o:L3�:Y��:Mm�:O�D:V�:T�o:Mn;�'�:L3�:Xx�:A):L3�:W>�:RV�:T��:Q[:]ay:V:L3�:F�:Y�:J��:J��:Xy:S��:RVa:QG:F0:Dх:>��:2a�:)ċ:�s:�E:�:���:*t9��9��;9�c19�b�9�:%$9�o�:b�:�9��9�O�9�9�F9�G99�49���9�9��9�t9�9�n�9���9���9��N9���9�
u9�	9�69��9�L�9�,9�b�9��E9��9�`9�_�9��J9�u�9��z9���9�Z�9��9�p�9��29酃9��^9��{9��9�=�9�	9�Q�9��`9�Ć9�9��Y9��>9���9��9�F]9ߠ�9䇞9ڵ�9���9�n9��9�89��9ߙ�9��9ߔ�9�.9ߏ�9�9�u�9ߍX9�uf9�u
9��9�ˡ9�m�9�mr<���9ڜ�9�S9�-:�9��9�	-:o9�9�Ø9�M<9��s9�-�9�\b9�/9���9�o�9�V�9�V9�=~9���9���9��):
��9��9���9�÷9��9���9��9溬9뢒9�`.9��9��9� 9���9�C�9�Zk9�*�9��9�W|9�K9���9�P�9�6H9�9�39�3C9�9��9�G.9��9�Di9ᷱ9��9���9��9��9�ż9�!l9��Y9�cY9�5$9�6!9��9ᫀ9᫵9�f�9��9�6i9ᩁ9�:�m9�:E�9��	9��9�>9���9�A9�/#9ܹ�9��{9���9�+�9�*�9�)�9�)	9�'�9��9�9�	�9� S9ᓾ9�� 9�_
9�/�9ܣ�9׺�9�/99���9�D9��9�T9��I9�ɿ:
��9�R�9�	�9��9�KS9܌'9�E�9ҷ�9ן39܇9��9Ҳ9��9܀W9ז�9ד�9�2 9�0,9Ң�9��<9�.g9�[�9�D�9͵�9ʹ*9׃�9�g�9���9�u�9��b9�sT9���9҂�9�R9͗�9�f-9�yi9�x9�^�9�F9�A�9��9�n�9�(�9�+9���9�$�9�ȶ9��9��-9�9/9���9�N�9��39��9�ד9�Gn9��9ȋ9��9�Ȩ9�J@9�Y�9�(&9��9�2�9���9��9Ԝ�9�x:�9�_�9���9�T�9ϜN9�RW9�8�9�gx9�9�'9�H9��9��9�W9�Yw9�#�9��9��9�� :�>9�/�9��9�*A9�O9��9���9�!�9�29ۧ;9�v�9��{9�G9�v :\�9���9��	9��9���9�9��:�v:S/:��:#�$:&F:0I:7r�:-�":2��:0|:2�r:4�:4�O:'o�:�n:��:Z�:��:��:�O:��:��:��:	�}:	�:	��:,):+2:z/9��z:��:9�:n�9��:2u:��:.�9��'9�:-9� �9��9�	:�-9���:��9�lB9��f9��9��9�!E9���:��9�r�9韔9�Vl9��,9��9�
s:
��9�{?9��9���9�F�9���9��u9��n9�=�:):��9�f�9�5�9��r:�e9��?9��R9�@�:��9�N�9��9��:: ��:
�~9��h9�="9��9�e�9��19�59�9��99�9�՞9�J9�6$9��v9�y:39�9��9�I�9�y9�/�9���9���:!9�V9�?9��=9���9���: ��9�{�9���:��9��v:�r9�w�9��w9�f9�L�:�9���33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�q?�q?�?�?�q?�e�?��?��?�q?�q?�?�q?�?�q?�q?�q?�q?��?�?�?�?ͫ�?�q?�q?ɷ?�q?��?�|�?��?�q?�?��?�?�q?��?�q?��?�@8�u?��?�?�+?�?�q?�?��?�?�q?�q?�q?�|�?��?�q?��?�|�?�?�|�?�q?�?��?�?�?�+?�?�?��?�e�?�?�Z?ɷ?ɷ?ͫ�?ͫ�?�N�?��?�N�?��"?�C-?��?��?��?Ѡ'?ͫ�?�N�?�C-?Ѡ'?�C-?Ѡ'?��"?�N�?��2?��"?��?��?�Z?��?�C-?Ѡ'?Ѡ'?��2?�7�?�C-?Ѡ'?��?Ѡ'?�C-?��?��2?��?Ѡ'?Ք�?�C-?��2?�C-?Ք�?��?��?��?�N�?Ѡ'?�N�?�C-?��"?��?�C-?ͫ�?Ѡ'?��?ͫ�?��2?Ք�?��?Ѡ'?��?�7�?Ѡ'?�N�?Ѡ'?��?Ѡ'?��?Ѡ'?Ք�?Ѡ'?��?�N�?Ք�?Ѡ'?�C-?Ѡ'?Ѡ'?��"?��2?�C-?�C-?ى7?��?��?Ք�?�N�?��?�N�?�N�?�7�?��2?Ք�?��?Ѡ'?��"?��?�C-?��2?�N�?��?Ք�?Ѡ'?�N�?��?Ք�?��?Ք�?��?Ք�?Ք�?�N�?Ք�?Ք�?Ք�?��?Ѡ'?�C-?Ք�?�C-?��2?�7�?�C-?Ք�?�7�?��2?��?�C-?��?Ք�?��2?Ք�?Ք�?��?Ѡ'?Ք�?��2?ى7?��2?��2?��?�7�?�C-?��2?��2?ى7?Ք�?�7�?Ք�?�C-?�7�?�C-?�C-?��2?ى7?��2?�7�?��2?ى7?ى7?�7�?Ք�?ى7?ى7?��2?�7�?�C-?�ں?�,=?�7�?�7�?��2?ى7?Ք�?�7�?�7�?��2?�ں?�7�?��2?Ք�?�}�?�ں?ى7?�7�?�ں?�7�?Ք�?��B?ى7?�,=?�,=?�ں?��B?��B?�7�?��B?ى7?ى7?��B?�rG?�,=?� �?�rG?��B?��B?�rG?� �?� �?��B?��B?���?�f�?�M?�rG?� �?���?�}�?��B?���?�R?�R?�M?�rG?���?��B?�	�?�rG?�M?�R?�f�?���?�R?�	�?�R?�M?�[W?�R?��]?��?�[W?�[W?�	�?�R?��?��]?�[W?�b?�b?��]?���?��?���?���?���?��?�b?�Dg?��?�Dg?��?�Dg?�Dg?�8�?��?��r?��m?��?��m?��?�8�?��m?�Dg?���?��?��m?��r?�8�?���?��?�8�?�-w?��m?���?��}?���?��}?���?�~�?��r?�~�?�-w?�~�?�-w?��}?��r?��}?��}?���?�!�?��}?�!�?�-w?��r?�~�?��r?�-w?�-w?�-w?��}?�-w@��?���?�-w?���?�!�?�~�?�-w?�~�?�!�?��@ �D?��?�!�@��?�!�?�!�?�!�?��@��?�!�?��?��@ �D@ �D?��@4?��}@ �D@��?��@ �D?�!�?�s�?��?��}@��@4@ �D@4@ �D?��@4@ �D@��@.I@��?�s�?��@4@ �D?�!�@4?��@ �D@4@�
?��@ �D@��@��?��@4@4@ �D@��@��@.I@ �D@��@�
@ �D@��@ �D?��?��@��@ �D@4@��?��@4@4@ �D@ �D@ �D?��?��@��@ �D?��@ �D?��@��@��@ �D@��@4@ �D@��@�
?�!�@��@��@��?��?��?�!�?�-w?�!�?��?�-w?�~�?�!�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@��UG�O�@�zG�O�@�dG�O�G�O�@�SBG�O�@��G�O�@�LG�O�G�O�@�-�G�O�@�a�G�O�G�O�@�9`G�O�@�m�G�O�G�O�@�,�G�O�@�LSG�O�G�O�@��jG�O�@��CG�O�G�O�@�#LG�O�@�G�O�G�O�@��G�O�@��G�O�G�O�@��?G�O�G�O�@��nG�O�G�O�G�O�G�O�@��*G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�@�P�G�O�G�O�G�O�G�O�AWG�O�G�O�G�O�G�O�A8 G�O�G�O�G�O�G�O�A6r�G�O�G�O�G�O�G�O�A<A�G�O�G�O�G�O�G�O�A?�kG�O�G�O�G�O�G�O�A?�wG�O�G�O�G�O�G�O�A@7�G�O�G�O�G�O�G�O�AA�
G�O�G�O�G�O�G�O�AB�G�O�G�O�G�O�G�O�AD�G�O�G�O�G�O�AEeVG�O�G�O�G�O�G�O�G�O�AG�GG�O�G�O�G�O�G�O�AI)�G�O�G�O�G�O�AJ�BG�O�G�O�G�O�G�O�G�O�AKwoG�O�G�O�G�O�G�O�AJ0�G�O�G�O�G�O�G�O�AJ�G�O�G�O�G�O�G�O�ANs>G�O�G�O�G�O�G�O�AM�}G�O�G�O�G�O�G�O�AOC�G�O�G�O�G�O�G�O�AR�0G�O�G�O�G�O�G�O�ARîG�O�G�O�G�O�AUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AX#�AX@(G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AWݤAX}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A`��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A`�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�An�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A}q7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jWG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=~A�p#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��-A���A���A��]A�}�A�z7A�*LA�@�  3 3 3  3 3 3  3 3  3 3  3 3  3 3  3 3  3 3  3  3    3    3    3    3    3    3    3    3    3    3    3    3    3   3     3    3   3     3    3    3    3    3    3    3    3   3         33        33                        3                       3                         3                        3                       33                        3                        3                       33                        3                        3                        3              33333333   G�O�G�O�����G�O�����G�O�?   G�O�G�O��L��G�O�>���G�O�?L��G�O�G�O�?   G�O�?L��G�O�G�O�    G�O�?333G�O�G�O��   G�O�?��G�O�G�O��   G�O�?   G�O�G�O�>���G�O�?fffG�O�G�O�����G�O�?��G�O�G�O�?��G�O�G�O��L��G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O��333G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�����G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�����G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                G�O�G�O�@��G�O�@�s=G�O�@�6BG�O�G�O�@�%G�O�@��IG�O�@�uG�O�G�O�@��}G�O�@�3ZG�O�G�O�@�#G�O�@�?dG�O�G�O�@���G�O�@�G�O�G�O�@�z-G�O�@�`G�O�G�O�@��G�O�@��BG�O�G�O�@��nG�O�@��G�O�G�O�@ũG�O�G�O�@Ó0G�O�G�O�G�O�G�O�@Ģ�G�O�G�O�G�O�G�O�@�S�G�O�G�O�G�O�G�O�@�"uG�O�G�O�G�O�G�O�@��G�O�G�O�G�O�G�O�A�`G�O�G�O�G�O�G�O�AۋG�O�G�O�G�O�G�O�A#��G�O�G�O�G�O�G�O�A'LG�O�G�O�G�O�G�O�A'YG�O�G�O�G�O�G�O�A'��G�O�G�O�G�O�G�O�A)7�G�O�G�O�G�O�G�O�A*&�G�O�G�O�G�O�G�O�A+o�G�O�G�O�G�O�A,�8G�O�G�O�G�O�G�O�G�O�A/K(G�O�G�O�G�O�G�O�A0�lG�O�G�O�G�O�A1�#G�O�G�O�G�O�G�O�G�O�A2�PG�O�G�O�G�O�G�O�A1�gG�O�G�O�G�O�G�O�A2Q�G�O�G�O�G�O�G�O�A5�G�O�G�O�G�O�G�O�A5^G�O�G�O�G�O�G�O�A6�uG�O�G�O�G�O�G�O�A:JG�O�G�O�G�O�G�O�A:,�G�O�G�O�G�O�A<��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A?��A?�	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A?F�A?n^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AH/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AGz�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AVNbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ad�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�At��Au��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� 	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�=#A�\cA�A�A�2XA�.�A�޽A��n  1 1 1  1 1 1  1 1  1 1  1 1  1 1  1 1  1 1  1  1    1    1    1    1    1    1    1    1    1    1    1    1    1   1     1    1   1     1    1    1    1    1    1    1    1   1         11        11                        1                       1                         1                        1                       11                        1                        1                       11                        1                        1                        1              11111111   G�O�G�O�?'X1G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�?'X1G�O�G�O�?'X1G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?'X1?'X1?'X1?'X1?'X1?'X1?'X1?'X1