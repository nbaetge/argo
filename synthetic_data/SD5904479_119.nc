CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB          	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       f2020-12-03T07:47:38Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                 �  r\   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  tP   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ~   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ݘ   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   CHLA         	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  �$   CHLA_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
CHLA_dPRES           	         	long_name         6CHLA pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   CHLA_ADJUSTED            	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  �   CHLA_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � |   CHLA_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     � 
p   BBP700           	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � <   	BBP700_QC            	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �    BBP700_dPRES         	         	long_name         8BBP700 pressure displacement from original sampled value   
_FillValue        G�O�   units         decibar      � �   BBP700_ADJUSTED          	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � #�   BBP700_ADJUSTED_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � +�   BBP700_ADJUSTED_ERROR            	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � -�   CDOM         	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � 5T   CDOM_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � =    
CDOM_dPRES           	         	long_name         6CDOM pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      � ?   CDOM_ADJUSTED            	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � F�   CDOM_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � N�   CDOM_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � P�   NITRATE          	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � Xl   
NITRATE_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `8   NITRATE_dPRES            	         	long_name         9NITRATE pressure displacement from original sampled value      
_FillValue        G�O�   units         decibar      � b,   NITRATE_ADJUSTED         	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � i�   NITRATE_ADJUSTED_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � q�   NITRATE_ADJUSTED_ERROR           	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � s�Argo synthetic profile          1.0 1.2 19500101000000  20201203074738  20201203074738  5904479 UW, SOCCOM, Argo equivalent                                     STEPHEN RISER , KENNETH JOHNSON                                 PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                            wA   AO  DDDDARRDNAVIS_A                         0276                            110713                          863 @�|���� 1   @�|�l�#@Iz�1'�0���+1   GPS        wPRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         DOXY_ADJUSTED=DOXY*G                                                                                                                                                                                                                                            CHLA_ADJUSTED=CHLA/A, NPQ corrected (Xing et al., 2012), spike profile added back in                                                                                                                                                                            not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  NITRATE_ADJUSTED=[NITRATE-SUM(OFFSET(S)+DRIFT(S))]/GAIN                                                                                                                                                                                                         dP =-0.02 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            G=1.0391                                                                                                                                                                                                                                                        A=2                                                                                                                                                                                                                                                             not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  OFFSET(S) and DRIFT(S) from climatology comparisons at 1000m or 1500m. GAIN from surface/deep comparison where surface values are known                                                                                                                         Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        G determined by surface measurement comparison to World Ocean Atlas 2009.See Takeshita et al.2013,doi:10.1002/jgrc.20399                                                                                                                                        A is best estimate from Roesler et al., 2017, doi: 10.1002/lom3.10185                                                                                                                                                                                           not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  Contact Tanya Maurer (tmaurer@mbari.org) or Josh Plant (jplant@mbari.org) for more information                                                                                                                                                                  2017062616081220170626160812201706261608122020120212402720201202124027202012021240272020120212402720201202124027A   A   A   B   A   F   F   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�33A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2�fD3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy��D�\�D��fD�S3D��fD�ffD��fD�|�D�� D�c3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�p�A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A���A�(�A�\)A�(�B {B{Bz�B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bo�Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�\D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD ��D!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(�D(�HD)�D)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD0��D1�HD2HD2��D3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ��DKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDtT{Dy�D�]qD��
D�S�D��
D�g
D��
D�}qD��D�c�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A]\)A]`BA]x�A]�7A]�7A]�PA]��A]��A]�hA]�PA]�hA]�hA]�hA]��A]��A]�hA]��A]�PA]l�A]hsA]O�A]K�A]33A]"�A]%A\��A\��A\v�A\5?A[��AZz�AY��AY&�AX�AX�HAX��AX��AXȴAX�9AXr�AXQ�AXM�AXA�AW�mAV=qAQ"�AM��AJ1'AH�DAF��AD�AC��ABȴAB�9AA��AA;dA@JA?G�A>�RA>bNA>E�A>9XA>v�A>��A>�A>ĜA>ZA=�mA=�TA=�#A=��A=�FA=�7A=+A<��A<{A<1'A<{A<A<  A;�A;A;�A:�!A:bNA:�A9A9�7A9hsA9;dA9&�A9oA8��A8��A8��A97LA97LA9VA8�A8ȴA8�DA8��A8�A8ZA8$�A8 �A8  A7��A6��A61'A5�^A5�PA5�A4�/A4�jA4=qA3�;A3��A3��A3�#A3��A4  A4bNA4bNA4 �A3�;A3�A3l�A3dZA3��A3C�A2��A2r�A1�A2{A1�wA0�uA/��A0Q�A09XA/��A0bNA1�A/�;A0�`A2�+A2ȴA1��A0��A0ȴA0�!A0��A0��A0�A1ƨA1��A1ƨA1�A2A1|�A1S�A2E�A25?A1�A1�
A1t�A0��A0ȴA0�yA0�A0^5A0�9A0��A05?A0A�A/�A.ĜA.9XA.bNA.��A.�A.=qA. �A.5?A-��A-p�A-S�A-S�A-G�A-"�A-t�A-�
A-��A,��A,jA,��A-��A-�A.1A.bA.{A.A.  A-��A-�TA-��A-�^A-��A-x�A-hsA-�A,��A-�A,��A+��A+K�A*��A*�9A*�RA+
=A+dZA+��A,I�A,VA,(�A+�TA+��A+��A*��A*�uA+K�A+�TA,bA,A�A,ffA,Q�A, �A+�;A+�FA+��A+x�A+`BA+��A,-A,ffA,M�A+�^A+��A+�A,(�A+�#A+�FA+�^A+�A+��A+33A*ĜA)�TA)�^A)��A*A*{A*1A(�A(M�A(VA(M�A(9XA(�A'�
A'|�A'��A'��A'p�A'x�A&�9A&��A&v�A&bA%�A&��A&�A&��A&Q�A&  A%��A%�A%&�A%XA%�FA%7LA$ĜA$�A$bNA$M�A#��A#;dA"~�A"r�A"(�A"��A"��A"�A"{A!C�A!�A!&�A"1A"bNA!�TA 9XA�hAx�AdZA`BAC�A�HA��A�RA�A��A�;A7LA�A�AZA�A�
AA�A��Al�A�/AG�A��A�
AbA �A�wA��A��A�
A�A1A�A��A�A�wA��A�uA=qA�-A��A��A�A��A�A�`A�yA�A�HA�A�A��A��AZA��A��A"�A��An�A
=AS�AdZAC�A&�A�`A�HAn�A9XA��A�A�uA�AA�A��AȴAȴA�!A�AA�A�AhsAl�A��A��A�uAĜA�jA�!AI�A��A�PA�`A��A��A
=AAhsA|�A��A�FAA��A{A1'An�AQ�A��A�A�uA�#A��A"�A�jA
��A�PA�#A�!AAVA��A%AffAdZA j@��@�o@�1@�~�@��u@�$�@�J@�C�@���@���A �A~�A�AVAK�A�A ��@��H@�M�@�{@��@���@���@�~�@�\@��y@�ƨ@�t�@��T@��#@��@��@��^@��y@��m@��
@�{@�p�@�O�@�A�@���@�I�@�@�&�@���@@���@�7L@�X@�X@��@�p�@���@��@��/@���@�A�@�{@���@�b@x��@tZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A]\)A]`BA]x�A]�7A]�7A]�PA]��A]��A]�hA]�PA]�hA]�hA]�hA]��A]��A]�hA]��A]�PA]l�A]hsA]O�A]K�A]33A]"�A]%A\��A\��A\v�A\5?A[��AZz�AY��AY&�AX�AX�HAX��AX��AXȴAX�9AXr�AXQ�AXM�AXA�AW�mAV=qAQ"�AM��AJ1'AH�DAF��AD�AC��ABȴAB�9AA��AA;dA@JA?G�A>�RA>bNA>E�A>9XA>v�A>��A>�A>ĜA>ZA=�mA=�TA=�#A=��A=�FA=�7A=+A<��A<{A<1'A<{A<A<  A;�A;A;�A:�!A:bNA:�A9A9�7A9hsA9;dA9&�A9oA8��A8��A8��A97LA97LA9VA8�A8ȴA8�DA8��A8�A8ZA8$�A8 �A8  A7��A6��A61'A5�^A5�PA5�A4�/A4�jA4=qA3�;A3��A3��A3�#A3��A4  A4bNA4bNA4 �A3�;A3�A3l�A3dZA3��A3C�A2��A2r�A1�A2{A1�wA0�uA/��A0Q�A09XA/��A0bNA1�A/�;A0�`A2�+A2ȴA1��A0��A0ȴA0�!A0��A0��A0�A1ƨA1��A1ƨA1�A2A1|�A1S�A2E�A25?A1�A1�
A1t�A0��A0ȴA0�yA0�A0^5A0�9A0��A05?A0A�A/�A.ĜA.9XA.bNA.��A.�A.=qA. �A.5?A-��A-p�A-S�A-S�A-G�A-"�A-t�A-�
A-��A,��A,jA,��A-��A-�A.1A.bA.{A.A.  A-��A-�TA-��A-�^A-��A-x�A-hsA-�A,��A-�A,��A+��A+K�A*��A*�9A*�RA+
=A+dZA+��A,I�A,VA,(�A+�TA+��A+��A*��A*�uA+K�A+�TA,bA,A�A,ffA,Q�A, �A+�;A+�FA+��A+x�A+`BA+��A,-A,ffA,M�A+�^A+��A+�A,(�A+�#A+�FA+�^A+�A+��A+33A*ĜA)�TA)�^A)��A*A*{A*1A(�A(M�A(VA(M�A(9XA(�A'�
A'|�A'��A'��A'p�A'x�A&�9A&��A&v�A&bA%�A&��A&�A&��A&Q�A&  A%��A%�A%&�A%XA%�FA%7LA$ĜA$�A$bNA$M�A#��A#;dA"~�A"r�A"(�A"��A"��A"�A"{A!C�A!�A!&�A"1A"bNA!�TA 9XA�hAx�AdZA`BAC�A�HA��A�RA�A��A�;A7LA�A�AZA�A�
AA�A��Al�A�/AG�A��A�
AbA �A�wA��A��A�
A�A1A�A��A�A�wA��A�uA=qA�-A��A��A�A��A�A�`A�yA�A�HA�A�A��A��AZA��A��A"�A��An�A
=AS�AdZAC�A&�A�`A�HAn�A9XA��A�A�uA�AA�A��AȴAȴA�!A�AA�A�AhsAl�A��A��A�uAĜA�jA�!AI�A��A�PA�`A��A��A
=AAhsA|�A��A�FAA��A{A1'An�AQ�A��A�A�uA�#A��A"�A�jA
��A�PA�#A�!AAVA��A%AffAdZA j@��@�o@�1@�~�@��u@�$�@�J@�C�@���@���A �A~�A�AVAK�A�A ��@��H@�M�@�{@��@���@���@�~�@�\@��y@�ƨ@�t�@��T@��#@��@��@��^@��y@��m@��
@�{@�p�@�O�@�A�@���@�I�@�@�&�@���@@���@�7L@�X@�X@��@�p�@���@��@��/@���@�A�@�{@���@�b@x��@tZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�9B�FB�XB�}BɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�BB��BB{B�BuBJBB  B��B��B�B�yB�NB�BB�5B�5B�5B�ZB�B�B�B�B�B�B�B��B�B�B�B�B�sB�B�B�B�B�B�B�B�sB�fB�ZB�TB�HB�BB�BB�BB�NB�TB�TB�fB�B�B�B�B�B�B��B��B�B�B��B��B�B�sB�;B�B�
B��B��B��B��B��B��B��B�B�B�)B�ZB�`B�TB�NB�NB�HB�TB�sB�fB�HB�)B�B�)B�
B��BÖBȴB��BȴB��B�ZB��B�NB��BB��B�B�B�B�B�B�BBBB	7BJB%BB{B{BhB\BDBBB%BBB%B	7BBB��B�B�B�B�B��B�B�B�B�B�mB�fB�fB�fB�ZB�yB�B�B�`B�5B�HB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�BB�B�B��B��B�B�BB�fB�B�B�B�yB�fB�`B�)B�)B�`B�B��B��B��B��B��B��B��B�B�B�B��B��BBB��B��B  BBB��B  B��B��B��B�B�mB�ZB�fB�yB�B�yB�/B��B�B��B��B��B��B��B��B��B��B��BƨBǮBÖB�}B�wBȴB��B��BȴBŢB��B�dB�jB��BǮBB�qB�qB�XB�RB�3B�B��B��B��B��B�B�B��B��B��B��B��B�B��B�uB�=B�7B�1B�1B�1B�B�B�B�B�B{�Bt�Bq�Bp�Bn�Bl�BjBiyBiyBjBiyBgmBn�Bw�By�B}�B~�Bz�Bz�B}�B�B�B�+B�%B�+B�+B�B}�Bw�Bs�Bm�BcTBcTBe`BffBffBffBiyBl�Bm�Bo�Bq�Bq�Bo�Bm�BhsBiyBe`BaHBbNBm�Bs�Bv�Bw�Bz�B|�B}�By�By�Bt�Bm�BgmBbNB]/B]/B^5BbNBcTBdZBdZBcTBbNB]/B]/B[#BZB]/BdZBgmBgmBdZBbNB_;B[#BZBbNBffBhsBo�Bq�Bt�Bz�B|�B�B�7B�VB�uB��B�uB�VB�+B�B�+B�By�BbNBA�BE�BP�BM�B@�B:^B(�BPB��B�B�#B�qB�B��B�XBŢBǮB��B�B��B �B;dBB�BA�BC�B=qB&�B\B�yB��B�qB�B��B��B��B�B�!B�9B�B�B�B�wB��B��B�/B��BJB$�B$�B�BbB	7BB��B��B�B�B�yB�B�yB�sB��BBjBH�B+B�;BǮB�^B�-B�B�'1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�*B�>B�GB�]B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�FB��B(B�B�B|BSB#B B��B��B�B�|B�RB�FB�6B�9B�8B�^B�B�B�B�B�B�B�B��B�B�B�B�B�vB�B�B�B�B�B�B�B�xB�kB�]B�XB�MB�GB�FB�FB�OB�YB�XB�lB�B�B�B�B�B�B��B��B�B�B��B��B�B�tB�CB�B�B��B��B��B��B��B��B��B�B�B�.B�^B�cB�XB�QB�SB�LB�WB�vB�mB�MB�,B�B�,B�B��BÚBȸB��BȶB��B�\B��B�UB��BB��B�B�B�B�B�B�BB$B$B	=BOB)BB�B�BjB`BIBBB(BBB,B	<BBB��B�B�B�B�B��B�B�B�B�B�rB�jB�jB�jB�]B�|B�B�B�cB�:B�MB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�EB�&B�B��B�B�!B�BB�kB�B�B�B�B�kB�cB�.B�-B�fB�B��B��B��B��B��B��B��B�B�B�B��B�BBB��B��B B BB��B B��B��B��B�B�qB�`B�jB�~B�B�zB�2B�B�B�B��B��B��B��B��B��B��B��BƬBǳBÛB��B�zBȷB��B��BȶBŧB��B�gB�oB��BǲBB�wB�vB�[B�WB�6B�B��B��B��B�B�B�B��B��B��B��B��B�
B��B�uB�AB�:B�4B�6B�3B�"B�B�B�B�B{�Bt�Bq�Bp�Bn�Bl�Bj�BiyBixBj~Bi|BgmBn�Bw�By�B}�B~�Bz�Bz�B}�B�
B�!B�.B�(B�0B�-B�#B}�Bw�Bs�Bm�BcXBcUBebBfhBfhBfhBi{Bl�Bm�Bo�Bq�Bq�Bo�Bm�BhsBizBeeBaJBbOBm�Bs�Bv�Bw�Bz�B|�B}�By�By�Bt�Bm�BgmBbNB]2B]-B^4BbPBcSBdXBdXBcUBbNB]1B]3B["BZB]0BdZBgmBgmBdZBbNB_=B[#BZBbOBffBhtBo�Bq�Bt�Bz�B|�B�B�:B�UB�uB��B�uB�UB�,B�B�1B�
By�BbOBA�BE�BP�BM�B@�B:_B(�BLB��B�B�"B�qB� B��B�XBşBǫB��B�B��B �B;dBB�BA�BC�B=pB&�B\B�yB��B�oB�B��B��B��B�B�B�6B�B�B�B�vBʿB��B�-B��BHB$�B$�B�B_B	5BB��B��B�B�B�zB�B�wB�tB��BBj~BH�B#B�9BǫB�\B�+B�B�$1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
CnCm��Cm�PCm�Cm�)Cm��Cn'Cm�aCm�CCm�Cm�NCm��Cm�"Cm�KCm��Cm��Cm�2Cm�lCm�.Cm�3Cm��Cm�Cm�CmuFCm]�CmICm4�CmrCl�MCl��Cl��Cl�ICl�UCl��Cl��Cl��Cl��Cl�jCljBClK>Ck�BCk�cCj�HChc@Cc�C^~C[��CZ��CZ�CZ�
C[i�C[�<C\$�C\MC\8C[�TC\7C\q�C\��C\z�C\!�C[y�CZ�*CZ/CZ#uCZ:�CZJ�CZ58CZ�CZeCZ?UCZcCZ��C[`-C[��C[�KC[�7C[��C\fC\��C\��C\�6C\��C\g�C\nC[�AC[��C[T�C[|QC[}=C[�C\�C\rFC]�C^R�C_I�C_-�C_"XC_KC_}C_p�C_��C_�$C_Y4C_qC_JC_�C^^C^]�C^1�C^6�C^K�C^wfC^��C^��C^�dC_=C_�C_SC_b9C_�3C`�C`"C`�C_�C`~C`IEC`�{C`�C`�aC`��C`�C`��CaAC`�9C`��C`��CaB�Ca\Ca��CbP�Cb��Cb�LCc�pCd\�Cd={Cc�2Cc�HCc��Cd�Cd6�CdZhCd��CeE�Ce?�CeIyCe��Ce��CedfCe�Cf3Ce��Ce�
Ce�!Ce�5CeS�CeW
CeX-CeCewCe-�Ce�Cd�;Cds�Cc��Ccv�Cch�Cc��Cc�Cc�Cc�nCc��Cc��Cc��Cb�}Cb�4Cc�Cc<�Ccx�Cd�Cd��Cd-�Cc��Cc��Cd�Cfh�Cg�CgDCg>7CgO�CgP�Cg[�Cgu�Cg�|Cg�Cg�Cg�NCg�VCg��Cgl�Cg>�Cf��CfCd�ZCdFCd KCdQCd��CemKCf2CgA�Cg�Cg�Cg�%Cg6Cf�)Cf�,Cf8�Cf��Ch^XCi�Cj�Ck iCk]�Ckm�Cj��Cj�Cj@�CjU�Cju�Cj�Ck�LCl�rCmeCmL�Cm�CmY�Cm��CmߕCm��CmTCmSaCm8�ClѱCl"�Ck8�Cj6Ci��Ci�Ci��Cj
,Ci�ACh��Cg��Cg�%Cg��Cg�Cg�JCgy�CgL�Cg_tCgPiCg"�Cf�YCf��Cf>�Cf	=Ce�cCe��Cf��Cg�Cfs�Cf �Ce�$CerCd�7CdՒCd�hCd��CdyKCc�'Cc��Cc��CcM=Cb��Cb�Ca}]Ca��Ca��Ca��Ca�?Ca*]C`c�C_��C_�_C`	VC`e<C`M�C_��C^�C^�C^	cC]ҞC]�mC\�RC\`C\C�C\(C\cC\(�C[�yC\C\2C[�"C[}C[&ZCZ��CZ̝CZ�`CZ)CY�xCYf�CY-�CX�CX8�CX�CW��CW��CWDCV��CV�>CV$WCU��CUZ�CU$CT̵CT��CT[CT[_CT�CT��CT��CU
�CU�CT��CTs.CTDCSn�CR�CRwtCQ�CQNPCP��CPv"CO��CN�bCN#�CM��CMbSCL�ECL�CKZ�CK	+CJb�CI��CI�CHx�CG�CGBCF�vCF��CFr�CEƃCE?�CE�CD��CD'�CC�GCC��CC3
CBݾCB�vCBX�CB$�CA�
CA��CA((C@�\C@-�C?�nC?��C?��C?��C?vhC?TrC?�C>��C>UC=�eC=�KC=qLC=�C<�TC<*�C;žC;5�C:�0C:OC::�C:X�C:p*C:��C:�hC:��C;LC;�C<�rC=יC=}�C=4gC=�C>,cC>ѹC?�DCA6;CA��CB��CC�TCEI4CE�:CE�KCD�uCD��CD?CC�VCB�`CA&CC?EWC>��C>fRC>��C>��C@9CB#�CD	{CF��CH)CI�CJ��CJ��CJ�VCJ�CJQxCI��CJ9CJb|CJ�CI��CHgCGr&CF��CE�PCD%{CC$�CB��CB�CC~zCDR�CD�CER�CE�CF?NCF�"CF®CF�ZCFڪCFԃCG�CF��CLZ}C\X�Cf��Cq�Cw�`C{�C~	C~�C~�;Ccy�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             CwZ�CwM/CwEBCwE	CwJLCw@�CwR�Cw6�CwC!Cw9CwE@Cw;fCw;�Cw2�Cw$�Cw<Cv��Cv�ICv�Cv�cCv�xCv�
Cv�Cv�#Cv��Cv�&Cvz�CvY�Cv2�CvgCv#�Cv�Cv3Cu��Cu�!Cu��Cu��Cu��Cu��Cu�sCu6`Ct��Cs�Cqy\Ck�JCf�XCd8Cc5BCc�Cc?Cc�'Cd�Cd�:Cd�Cd��Cd�jCd�ICexCe3�Ce�Cd�FCd�Cc!Cb��Cb��Cb�JCb��Cb�dCb�Cb�!Cb��Cb��CcfpCc�
CdXDCdh�CdF+Cd�=Ce/CeG�CetQCev�Ce|aCeCd��CdT�Cd!�Cc�*CdGCd=Cd]�Cd�$Ce�Ce��Cg]Ch�Cg��Cg��Cg��Cg�xCh-?ChBCh=KCh�Ch-�ChCg�iCg2zCgRCf��Cf�Cf��Cg*3CgJ�Cg^�Cg�ZCg��Cg��CheCh4ChuvCh�3Ch�Ch׸Ch�Ch޸CiICiu`Ci�TCis0Ci�CiyCi}�Ci΀Ci�Ci�
Ci��Cj�Cj+�Cj{�Ck*JCk��Ckv�Cl|CmJgCm*ClغCl�TCl��Cl�eCm"�CmH"Cm�YCn<�Cn6jCn@�Cn��Cn�Cn\�Cn��Co3SCn��Cn��Cn�cCn�zCnKACnN�CnO�Cn8Cm��Cn#�Cn
�Cm�UCmb�Cl��Cl[xClL�ClwwCl�FClx�ClvBCl��Cl��ClqSCk�}Ck˛Ck��ClCl]�Cl�;Cmz&Cm�Clw�Clu�CmˮCoj�CpcCpN�CpH�Cp[DCp\!Cpg�Cp��Cp�XCp�?Cp�.CpÒCp�[Cp�Cpy-CpI�Co�Co�Cm�#Cm3qCl�~Cl�Cm��Cne�Co25CpL\CqCq�Cp�Cp@[Co�VCo�@Co9CpCqtCCsCs��CtQ�Ct��Ct�"CtLCs��CsieCs}Cs�Ct�Cu
�Cv$�Cv�JCv�
Cva�Cv��CwgCw,�Cv��Cv��Cv��CvCv(Cu^#CtkCsC�Cr�5Cr��Cs�Cs0�Cr��Cq�
CpшCp�:Cp�6Cp�,Cp�HCp��CpXCpkgCp[�Cp,_Co�Co�%Co?GCo�Cn��Cn�}Co�Cp	�Cov�Co cCn�fCnj�Cm��Cm�Cm�Cm��Cmh9Cl��Cl��Clw�Cl0pCk�Cj��CjNlCjcfCj��Cj̐Cj��Ci�.Ci)�Ch�-Ch�SCh��Ci+XCiCh`6Cgh*Cf�6Cf��Cf~�Cf+�Ce�VCd��Cd�pCd��Cd��Cd�BCd��Cd��Cd��Cd_CdCc��CcxCcZ�Cc�Cb��Cb@�Ca�Ca��Ca&�C`�C`yC`rC_��C_��C_^�C_%�C^��C^ 7C]�PC]y�C]�C\��C\��C\��C\�wC\��C]@IC]_C]e�C]@KC\��C\]C[�HC[3�CZ�#CZ(<CY}aCX��CX��CW�&CV�PCV3FCU��CUj#CT��CT�CSN%CR�yCRL�CQ�%CP��CPO8CO�}CO�CN��CNy�CN5CM�+CL�-CL�"CL_�CK�1CKsCK-�CJ��CJ|FCJ;�CI�CI�CIi9CI=CH��CH<7CG�^CGlxCG@CG�CG�CF��CFϔCF�]CF"�CE�6CEa�CE
`CDًCDh�CC�ICC�zCCECB��CBCA��CA��CA�-CA�VCA�)CB�CB
CB��CC�CDY�CEC�CD�9CD�DCE�CE��CFG�CGu�CH�4CI�+CJR�CK�{CL��CM�<CM�6CL��CLI�CK�sCK;CJ{�CH��CF��CFCE�%CF6CFvyCG�CI�CK��CN��CPVCQ�2CR��CR�HCR�CR�CR:�CQ�CR!JCRLFCR�CQa�CP=CO>�CN{�CMd�CK��CJ�"CJ?�CJ��CK#KCL CL�CM	�CM�SCM��CNgiCN�1CN��CN�CN��CN�CN~�CTW�Cd�LCp�Cz�}C�ӫC���C���C�OC�:�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114 @N�@E�@@�@@�@D@=�@IX@7�@?q@8�@@�@:@:�@4�@+�@$�@+@W@2@ �@��@�w@�,@�T@ڧ@��@�V@��@�;@q�@�g@z�@sT@ix@n�@[�@VS@J&@8�@$!@�@�8@��@�@��@��@��@i�@V&@p@�V@?�@f�@X�@W�@M�@r�@��@�\@��@d�@��@\�@�@=@ �@+b@@
�@�@#�@;�@�=@��@$@.m@m@F�@�	@�*@��@�G@��@�:@U�@!�@ �@�D@��@�1@'�@a_@�%@@��@}�@k`@c�@U�@[.@��@�!@�@�7@�@~(@R/@�X@��@ô@�@��@�@�@�@I�@W@[f@�@�6@�@	@�@�@�@	l@'�@i�@��@hq@sF@l7@o0@��@��@t�@��@��@ޔ@�@�w@�.@�=@Y�@ݲ@�@��@{�@�{@�~@�z@�?@�@x�@t�@{:@�-@��@�"@�@�@�@ܳ@�s@��@�@�@@�@[�@O�@h�@X�@�@�5@�$@D�@;v@V�@nA@W�@U�@|�@��@R�@��@�@�@ @FB@��@�A@��@V�@U�@0o@:!@�@�@�<@�@Ԑ@��@�(@ �@�@�@�@�@�@�'@Ȭ@�@�E@CI@�@�Q@��@�@�@�@�y@E�@C#@ @��@k@X{@E@��@��@�S@�j@]f@�L@��@9�@�@ȓ@ַ@��@5@Ӳ@�@ߋ@�c@�R@�@�@1@�@�?@��@��@}�@	@m�@�\@j`@u�@��@�]@[D@��@�@$�@)@�@@��@��@�V@�V@��@��@K�@B@��@�S@߬@^�@��@A�@
}@�u@�C@C1@.&@/_@F3@��@�H@~�@V�@)=@��@V�@��@-@�@E{@/2@��@9i@��@��@�X@:v@*�@�t@�@��@��@�w@O#@��@� @{)@V�@O�@i @C)@V&@R�@(U@�@�i@��@��@U�@�@�j@��@m�@�@��@��@k�@Q�@('@��@Ѕ@h�@�@��@�n@�Q@hB@8�@8�@V�@p�@��@��@��@��@H�@T@��@J@�!@��@1�@
�&@
��@
(k@	��@	�@��@��@-g@��@<A@@�_@)@��@Ql@��@��@L�@%@��@��@,�@�@̝@r�@5.@�@��@�7@m�@>�@=@ �/@ ��@ t6@ &�?��Z?�C)?�
??��O?��S?���?�zW?�)m?���?�&�?��?�6=?���?�g�?���?�E�?��?���?�S9?���?���?���?���?�B�?�j?�^�?�'?���?�TV?��?��?���?�L�?��?��z?�N�@ }�@ �m@|�@K�@3,@��@�@�%@�h@�5@O@��@ r�?�f@?�xd?�=�?�x�?�I?��]@�@^�@VG@U�@7*@��@�M@�?@��@��@T;@{�@�7@h@
@E�@��@& @s�@q+@�|@p�@��@@�k@��@9z@��@��@9@.4@=�@>'@:@l*@( @�d@�'@�D@ Y�@$��@'�z@(�@)Z�@)A!G�O�?�ߤ?��M?�
=?�a?���?�*�?��?��?��'?�J�?��h?��h?���?���?��#?�z?�Ɇ?��?���?���?�w2?�@�?���?���?�l�?��?��-?�I�?�%?���?^v�?F??7�k?2��?/'�?/��?2��?3�*?,V�?҉?��?ϫ?�>�PH>ɠ'>~� >��=���=jJ�=+6z<�D�<�9X<�?<�D�<�3�<�D�<�9X<�3�<�3�<�-�<|PH<|PH<�-�<�-�<|PH<-�<|PH<|PH<�9X<XD�<XD�<�3�<XD�<XD�<XD�<|PH<49X<-�<|PH<49X<XD�<XD�<XD�<XD�<49X<XD�<XD�<|PH<|PH<49X<49X<XD�;�D�<49X<49X<|PH<49X<49X<XD�<-�<XD�<�-�<49X<49X<|PH<XD�<XD�<XD�<XD�<XD�<-�<XD�<XD�<49X<49X<-�<-�<|PH<49X<XD�<|PH<XD�<�3�<49X<-�<XD�<XD�<-�<49X<XD�<49X<49X<-�<49X<|PH<XD�<-�<49X<|PH<49X<|PH<XD�<XD�<49X<-�<-�<49X<49X<49X<49X<XD�;�D�<49X<-�<49X<49X<49X;�D�<49X<-�<XD�<XD�<49X;�-�<-�<-�<XD�<49X<-�;�-�<XD�<|PH<49X<49X<49X<-�<-�<|PH<XD�<-�<-�;�D�<49X<XD�<49X<49X<|PH<XD�<-�<49X<49X<|PH<49X<-�<49X<XD�<XD�<49X<49X<-�<49X<49X<�-�<XD�<XD�<XD�<49X<-�<|PH<49X<|PH;�-�<49X<XD�<-�<-�<49X<|PH;�D�<XD�<-�<�9X<49X<|PH<-�<-�<49X;�D�<XD�<49X;�D�<-�;�-�<49X<-�<-�<XD�;�D�<�-�<-�<XD�;�-�<-�<-�;�D�<49X<|PH<49X<-�;�-�;�-�<-�;�D�;�D�;�D�;-�;�D�<XD�<XD�<XD�;�D�<49X<-�;�D�;�D�<|PH<49X<49X;�D�<-�<-�;�D�;�D�<XD�<-�<|PH<-�<|PH<49X<49X;�D�<49X<XD�<-�<-�<XD�<XD�;�-�<49X<49X<XD�<XD�<-�<49X<|PH<49X<49X<-�;�D�<49X<49X<XD�<|PH<49X;�D�<XD�<49X<XD�<49X<XD�<�-�<|PH<XD�<XD�<XD�<XD�<XD�<-�<49X;�D�<|PH<XD�<|PH<�-�<49X<|PH<�-�<�-�<49X<XD�<�-�<|PH<49X<�-�;�D�<-�<�-�<49X<�-�<-�<XD�<49X<|PH<49X<�-�<XD�<XD�<|PH<�3�<�-�<�-�<XD�<�3�<�-�<XD�<�9X<XD�<�-�<49X<�-�<�-�<49X<|PH<XD�<�-�<49X<|PH<�-�<49X<|PH<|PH<49X<49X<XD�<�3�<|PH<|PH<|PH<�-�<49X<|PH<�?<XD�<|PH<�-�<�-�<|PH<|PH<�3�<�3�<|PH<49X<XD�<�-�<�-�<|PH<|PH<|PH<|PH<�3�<49X<|PH<�3�<|PH<49X<�-�<�3�<|PH<�-�<XD�<�3�<XD�<�-�<|PH<�-�<�-�<-�<|PH<�-�<�-�<�-�<�3�<XD�<�3�<|PH<�3�<�3�<|PH<�-�<XD�<XD�<|PH<�3�<�9X<�3�<�-�<�-�<�3�<|PH<XD�<�3�<|PH<�-�<�3�<�-�<�-�<�9X<�3�<�-�<XD�<�-�<�3�<�3�<�3�<�-�<�-�<�-�<�3�<�-�<XD�<�3�<�-�<|PH<�3�<�3�<|PH<�-�<�3�<�-�<|PH<�9X<�9X<�3�<�3�<�3�<�-�<�-�<49X<�?<XD�<�9X<|PH<49X<�3�<|PH<�-�<�-�<�3�<|PH<|PH<�-�<�-�<|PH<�-�<XD�<|PH<�-�<�-�<XD�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?-w2?<�?5��?1��?0H?6�'?3�*?3�*?5Y�?7�?9J�?9J�?:kQ?4�o?8r�?5�?3a?8*�?+6z?-/?,�?0�E?'E9?'E9?%�?�-?I�?�H?��?A >ۥ�>�n/>�Ɇ>���>�V�>��>���>��E>���>��>��>���>�<�>v�}>C�]=�Mj=��O="3�<�<6<|PH;�PH;�9X;�D�;�PH;�-�;�PH;�9X;�-�;�-�;XD�;-�;-�;XD�;XD�;-ຐ-�;-�;-�;�9X:�-�:�-�;�-�:�-�:�-�:�-�;-�    ��-�;-�    :�-�:�-�:�-�:�-�    :�-�:�-�;-�;-�        :�-�-�        ;-�        :�-ຐ-�:�-�;XD�        ;-�:�-�:�-�:�-�:�-�:�-ຐ-�:�-�:�-�        ��-ຐ-�;-�    :�-�;-�:�-�;�-�    ��-�:�-�:�-ຐ-�    :�-�        ��-�    ;-�:�-ຐ-�    ;-�    ;-�:�-�:�-�    ��-ຐ-�                :�-�-�    ��-�            �-�    ��-�:�-�:�-�    �XDк�-ຐ-�:�-�    ��-�XD�:�-�;-�            ��-ຐ-�;-�:�-ຐ-ຐ-�-�    :�-�        ;-�:�-ຐ-�        ;-�    ��-�    :�-�:�-�        ��-�        ;XD�:�-�:�-�:�-�    ��-�;-�    ;-�XD�    :�-ຐ-ຐ-�    ;-�-�:�-ຐ-�;�9X    ;-ຐ-ຐ-�    �-�:�-�    �-ຐ-�XD�    ��-ຐ-�:�-�-�;XDк�-�:�-�XDк�-ຐ-�-�    ;-�    ��-�XDлXDк�-�-�-�-໐-�-�:�-�:�-�:�-�-�    ��-�-�-�;-�        �-ຐ-ຐ-�-�-�:�-ຐ-�;-ຐ-�;-�        �-�    :�-ຐ-ຐ-�:�-�:�-�XD�        :�-�:�-ຐ-�    ;-�        ��-�-�        :�-�;-�    �-�:�-�    :�-�    :�-�;XD�;-�:�-�:�-�:�-�:�-�:�-ຐ-�    �-�;-�:�-�;-�;XD�    ;-�;XD�;XD�    :�-�;XD�;-�    ;XDл-ຐ-�;XD�    ;XDк�-�:�-�    ;-�    ;XD�:�-�:�-�;-�;�-�;XD�;XD�:�-�;�-�;XD�:�-�;�9X:�-�;XD�    ;XD�;XD�    ;-�:�-�;XD�    ;-�;XD�    ;-�;-�        :�-�;�-�;-�;-�;-�;XD�    ;-�;�D�:�-�;-�;XD�;XD�;-�;-�;�-�;�-�;-�    :�-�;XD�;XD�;-�;-�;-�;-�;�-�    ;-�;�-�;-�    ;XD�;�-�;-�;XD�:�-�;�-�:�-�;XD�;-�;XD�;XDк�-�;-�;XD�;XD�;XD�;�-�:�-�;�-�;-�;�-�;�-�;-�;XD�:�-�:�-�;-�;�-�;�9X;�-�;XD�;XD�;�-�;-�:�-�;�-�;-�;XD�;�-�;XD�;XD�;�9X;�-�;XD�:�-�;XD�;�-�;�-�;�-�;XD�;XD�;XD�;�-�;XD�:�-�;�-�;XD�;-�;�-�;�-�;-�;XD�;�-�;XD�;-�;�9X;�9X;�-�;�-�;�-�;XD�;XD�    ;�D�:�-�;�9X;-�    ;�-�;-�;XD�;XD�;�-�;-�;-�;XD�;XD�;-�;XD�:�-�;-�;XD�;XD�:�-�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ?�w2?��?���?���?�H?��'?��*?��*?�Y�?��?�J�?�J�?�kQ?��o?�r�?��?�a?�*�?�6z?�/?��?��E?�E9?�E9?��?��-?�I�?��H?���?�A ?[��?Cn/?4Ɇ?/��?,V�?,�?/��?0�E?)��?�?�?��?<�>��}>��]>sMj>�O=�3�==<6<�PH<|PH<49X<XD�<|PH<-�<|PH<49X<-�<-�;�D�;�-�;�-�;�D�;�D�;�-�;-�;�-�;�-�<49X;-�;-�<-�;-�;-�;-�;�-�    ;-�;�-�    ;-�;-�;-�;-�    ;-�;-�;�-�;�-�        ;-�;�-�        ;�-�        ;-�;-�;-�;�D�        ;�-�;-�;-�;-�;-�;-�;-�;-�;-�        ;-�;-�;�-�    ;-�;�-�;-�<-�    ;-�;-�;-�;-�    ;-�        ;-�    ;�-�;-�;-�    ;�-�    ;�-�;-�;-�    ;-�;-�                ;-�;�-�    ;-�            ;�-�    ;-�;-�;-�    ;�D�;-�;-�;-�    ;-�;�D�;-�;�-�            ;-�;-�;�-�;-�;-�;-�;�-�    ;-�        ;�-�;-�;-�        ;�-�    ;-�    ;-�;-�        ;-�        ;�D�;-�;-�;-�    ;-�;�-�    ;�-�;�D�    ;-�;-�;-�    ;�-�;�-�;-�;-�<49X    ;�-�;-�;-�    ;�-�;-�    ;�-�;-�;�D�    ;-�;-�;-�;�-�;�D�;-�;-�;�D�;-�;-�;�-�    ;�-�    ;-�;�D�;�D�;-�;�-�;�-�;�-�<-�;�-�;-�;-�;-�;�-�    ;-�;�-�;�-�;�-�        ;�-�;-�;-�;�-�;�-�;-�;-�;�-�;-�;�-�        ;�-�    ;-�;-�;-�;-�;-�;�D�        ;-�;-�;-�    ;�-�        ;-�;�-�        ;-�;�-�    ;�-�;-�    ;-�    ;-�;�D�;�-�;-�;-�;-�;-�;-�;-�    ;�-�;�-�;-�;�-�;�D�    ;�-�;�D�;�D�    ;-�;�D�;�-�    ;�D�;�-�;-�;�D�    ;�D�;-�;-�    ;�-�    ;�D�;-�;-�;�-�<-�;�D�;�D�;-�<-�;�D�;-�<49X;-�;�D�    ;�D�;�D�    ;�-�;-�;�D�    ;�-�;�D�    ;�-�;�-�        ;-�<-�;�-�;�-�;�-�;�D�    ;�-�<XD�;-�;�-�;�D�;�D�;�-�;�-�<-�<-�;�-�    ;-�;�D�;�D�;�-�;�-�;�-�;�-�<-�    ;�-�<-�;�-�    ;�D�<-�;�-�;�D�;-�<-�;-�;�D�;�-�;�D�;�D�;-�;�-�;�D�;�D�;�D�<-�;-�<-�;�-�<-�<-�;�-�;�D�;-�;-�;�-�<-�<49X<-�;�D�;�D�<-�;�-�;-�<-�;�-�;�D�<-�;�D�;�D�<49X<-�;�D�;-�;�D�<-�<-�<-�;�D�;�D�;�D�<-�;�D�;-�<-�;�D�;�-�<-�<-�;�-�;�D�<-�;�D�;�-�<49X<49X<-�<-�<-�;�D�;�D�    <XD�;-�<49X;�-�    <-�;�-�;�D�;�D�<-�;�-�;�-�;�D�;�D�;�-�;�D�;-�;�-�;�D�;�D�;-�:���:���:�f�:��>:�m	:�s:���:�!�:���:���:��G:�m1:�J�:�J�:�[�:��:�m:�Jc:�ե:��p:��:��:��4:��M:�o:�ӝ:�vO:�gj:���:�~:�g:�?�:��:�x@:��:�,�:���:�H�:�:��:���:�6.;��:�%a:�8Z:S��:^��;]JP:f�;n�:�:�Z:B!:�]:y:<K:��:	��:�:�y;nj�:%:G�:&:%�:��::e�:Bh:Y�:Y<9�U,9���:B:=V9��d:�>:":P�:�9�D:O�9�S�9�ď9��e9��':F�9�]�:-�:,�:�9���9�W>9��P9�$�:�:�:
��:	�\:�E:%��:=�:�":<K:�:
Ѥ:g�:!f::�u: ��:T�:�:�89�:[�9�o�: ��:4c9��%9�h):b
9��1:cG:�7:b
9�X:: �H9��9���: �:��: �9�j�9���:�:A9��e9��G9�r�:�j9�β9�t�9�T�9�s9��9�}�: �c9���9��V9�>�9��c:
��;���9�)9��.9�qp: ۋ:
�:9�ʃ9�l�9���9���9��49���:��:�y:	l�9�gt:�9�c9��U:�9�IT9�I:.�9�>�9���9�R�9��i9�S�9�ܙ9���9�g�9��|9�Jp9�I�9�� 9�x :+�'9�a�: ��9�b�9�]�9�Yh9��n9�1�9��59�3�: �h:v�9���:$�:: �=: ��:��9�G^9�=9��9�C9�*:��:��9�O�9�}9�3.9���:��9�79�C:2�:4_: �9���9��G9��$: ��9�b9���9�x9���9�p9��:�_: ��:��: �D:/�9�1�9��99� �:��: ��:��:�e:��:��: �39�8: �{:.4:. :��9���:�.:��9��9� �9�	�9��e:�:b49�v9�-9�-!9�N9�,+: �<9�� 9��v9��	9�9�<�9��9�/9�-9�z9���9�:S�9���9�b:�%: �]:
q�9�!9�l�9휶9�k9�k�:
mM:9�|�9�{�9�b9�.�9��9���9�:��:�9�W�9��9��79菬9��:EE�9���9�Mc9�?�9��q9�ik9�Q59���9��
9�c�9ީ,9�y�9�2G9�x�9㉤9�lv9�i�9�89�� 9���9�/�9��: s�9�T9��9�i�:��:Z�:��:�8:!;:��:n�:�0:��:{:�:L8:-ք: V�:��:�{:'��:"q:�{:ʓ:m�:
4M:�9:nU:ʨ:V	:>
:U(:��:
2@:��:��:ݫ::�:z�:��:ӏ:I�:&`V:JZ:k�:0�:�D:.�:N�: 5�:@�:@�:uU:��:m:��:��:��:0�:�d:��:��:ٺ:��:�:�C:G:��:�H:g�:g�:B�:�:V�:�-:+�:��::�[:�(::�: D:R�::��:R�:��:�:�b:q
:��:.:�]P:�:ˬ:��:�:�:f7:,&s:;�:�w:(jR:i�:	(:
�c:
�=:�^:	�T:0�:	��:P:��:/k:j�:
�`:\ :W�:��:	�F:
.:qs:

:JX:	��:�z:�9�2:	g:	�:7: �G9�0�:D�:�r9���9�<�:�
: ��:m:��:� :ڟ:"�:�:#F�:�:�E:]0:-s:�:
�~:�]:��: :�C:L�:(
: ��9�!�:	�v9�.:z�9�V^9�E�9�9��M9Ծ�3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?�e�?�Z?�Z?�?��?ɷ?�e�?�e�?�?�?ɷ?ɷ?��?�e�?�?�?�e�?�e�?��?�e�?�?�e�?�?�?�e�?ɷ?��?�?ɷ?�e�?��?ͫ�?ͫ�?ɷ?�e�?��?�?��?�e�?�?��?ͫ�?��?ɷ?ͫ�?Ք�@R�@;�;?��?�C-?��?Ѡ'?�C-?�C-?��?�ں?ى7?�ں?�ں?Ք�?��?�7�?��2?��2?�ں?Ք�?��2?�7�?�7�?��2?�,=?�7�?�C-?�ں?�7�?ى7?�7�?�7�?��2?��2?�,=?ى7?�C-?�}�?��2?�,=?�ں?��2?��?�ں?ى7?��2?�7�?�ں?�C-?��2?Ք�?�ں?��?Ѡ'?Ք�?Ѡ'?��?Ք�?�,=?Ք�?Ք�?�ں?Ք�?�7�?ى7?�ں?��2?�7�?�7�?�C-?�ں?��2?��?�7�?�7�?��2?Ѡ'?��2?��?��2?��?�}�?Ք�?Ք�?��2?Ք�?Ք�?�C-?�C-?Ք�?Ք�?Ք�?Ѡ'?Ք�?�C-?��2?�C-?�N�?�Z?�N�?Ѡ'?ͫ�?��?Ѡ'?Ѡ'?Ѡ'?��"?��"?�Z?Ѡ'?��?Ѡ'?�C-?Ք�?�Z?��"?Ѡ'?Ѡ'?Ѡ'?�N�?ͫ�?��"?�N�?Ѡ'?��"?Ѡ'?Ѡ'?��?�C-?Ѡ'?��?Ѡ'?�N�?��2?�N�?�N�?�Z?��"?��2?�C-?Ѡ'?ى7?��2?��"?ͫ�?��?��2?Ք�?�N�?��"?�N�?Ѡ'?�N�?Ѡ'?ͫ�?��?ͫ�?��?��?��"?�N�?�N�?��?�Z?Ѡ'?�N�?ͫ�?��2?�C-?Ѡ'?��2?��2?��?�C-?�N�?��?��"?�N�?��?ɷ?�N�?Ք�?��"?�N�?��?�e�?�Z?ɷ?��"?�N�?��?Ѡ'?��"?ɷ?ɷ?�Z?ɷ?�e�?�Z?ɷ?��"?ɷ?ɷ?ɷ?ɷ?�e�?��?�e�?��"?ͫ�?Ѡ'?ɷ?ͫ�?��"?�Z?ͫ�?�N�?��2?��?��?��?ͫ�?Ք�?��?Ѡ'?��?��?��?��?Ք�?�7�?�7�?�ں?�C-?�C-?��?�7�?��?�ں?ى7?ى7?Ք�?��2?Ѡ'?ى7?ى7?��2?�ں?�}�?�ں?�,=?�}�?��B?ى7?��2?ى7?��B?� �?��B?�}�?Ք�?��B?��B?��B?���?�}�?�rG?�M?�rG?���?�rG?�M?�rG?�f�?� �?��B?�M?�f�?�rG?�R?�f�?�	�?�rG?�	�?�	�?�R?�R?�	�?�	�?�O�?�b?���?��?��]?�R?�[W?�R?��]?��]?��]?�O�?���?�Dg?�b?�[W?��?�b?�O�?�Dg?�O�?�Dg?�Dg?�b?�8�?��?�b?�O�?���?��?�Dg?�8�?��?���?�Dg?��m?��?�Dg?��m?��m?��?�Dg?��r?�8�?�~�?���?�-w?�-w?�-w?�s�?�~�?��?�~�?�~�?�!�?��}?�!�?��}?��?�s�?�s�?�~�?�~�@4?�s�?��?�s�@ �D?��@4@4@4?��@��@��@.I@��@4@��?�s�@ �D?��@��?��@4@ �D@��@��@4@4@.I@�
@�@4@4@�
@��@4@.I@��@��@(�@�@�@.I@(�@�@�
@�
@(�@.I@�
@�@.I@�
@�@�@�@(�@(�@.I@��@.I@.I@.I@(�@(�@.I@(�@�
@.I@.I@�N@�
@�
@��@(�@(�@��@�@�@�N@��@��@.I@�@�N@�
@.I@�
@(�@��@�
@��@�
@�
@4@��@ �D@ �D@ �D@.I@��3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@~""G�O�@{g�G�O�@�:G�O�G�O�@|�qG�O�@��G�O�G�O�@�eG�O�@�/$G�O�G�O�@�OjG�O�@�v�G�O�G�O�@��G�O�@���G�O�G�O�@���G�O�@��=G�O�G�O�@���G�O�@��!G�O�G�O�@��G�O�@��|G�O�G�O�@��G�O�A@�G�O�G�O�A0z�G�O�G�O�G�O�G�O�A80G�O�G�O�G�O�A=�G�O�G�O�G�O�G�O�G�O�AB��G�O�G�O�G�O�G�O�AG��G�O�G�O�G�O�G�O�AF�G�O�G�O�G�O�G�O�AFOG�O�G�O�G�O�G�O�AHCG�O�G�O�G�O�AL �AL�$G�O�G�O�G�O�G�O�AMqTG�O�G�O�G�O�G�O�AGG�O�G�O�G�O�G�O�AIp7G�O�G�O�G�O�AK�LG�O�G�O�G�O�G�O�G�O�AP� G�O�G�O�G�O�G�O�AP�yG�O�G�O�G�O�G�O�AM�oG�O�G�O�G�O�AO��G�O�G�O�G�O�G�O�G�O�AR�G�O�G�O�G�O�G�O�AT�EG�O�G�O�G�O�G�O�AR�G�O�G�O�G�O�G�O�AO��G�O�G�O�G�O�G�O�AO�WG�O�G�O�G�O�G�O�ANk�G�O�G�O�G�O�AL~ AL�OG�O�G�O�G�O�G�O�AP�-G�O�G�O�G�O�G�O�ARi&G�O�G�O�G�O�G�O�AU�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AX��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AR�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AR�vG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AMG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A[�`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ak��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ax��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ynA��MG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�<<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��aA��oA��NA��%A�7A�~\A�V�A�=A�oA�U�  3 3 3  3 3  3 3  3 3  3 3  3 3  3 3  3 3  3 3  3    3   3     3    3    3    3    3   33    3    3    3   3     3    3    3   3     3    3    3    3    3    3   33    3    3    3        3          3                        3                       3                         3                        3                        3                       3                         3                        3                       33                        3                        3             33333333333 G�O�G�O�����G�O�����G�O�>���G�O�G�O��   G�O�?fffG�O�G�O�����G�O�>���G�O�G�O�����G�O�?333G�O�G�O��L��G�O�?��G�O�G�O�����G�O�?   G�O�G�O�����G�O�>���G�O�G�O�����G�O�>���G�O�G�O�����G�O�?333G�O�G�O��fffG�O�G�O�G�O�G�O��   G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�����G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�����G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O����G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                            G�O�G�O�@k�G�O�@��G�O�@!�G�O�G�O�@D�G�O�@"k�G�O�G�O�@$nG�O�@#��G�O�G�O�@'�5G�O�@(6�G�O�G�O�@)l�G�O�@0�[G�O�G�O�@;G�O�@J��G�O�G�O�@R_/G�O�@JݤG�O�G�O�@V�iG�O�@YZG�O�G�O�@�ĞG�O�@�&XG�O�G�O�A��G�O�G�O�G�O�G�O�A �[G�O�G�O�G�O�A%d�G�O�G�O�G�O�G�O�G�O�A+BG�O�G�O�G�O�G�O�A/�'G�O�G�O�G�O�G�O�A.kG�O�G�O�G�O�G�O�A.�ZG�O�G�O�G�O�G�O�A0a�G�O�G�O�G�O�A4r�A5}G�O�G�O�G�O�G�O�A5íG�O�G�O�G�O�G�O�A/cqG�O�G�O�G�O�G�O�A1G�O�G�O�G�O�A3إG�O�G�O�G�O�G�O�G�O�A9YG�O�G�O�G�O�G�O�A8��G�O�G�O�G�O�G�O�A6�G�O�G�O�G�O�A84G�O�G�O�G�O�G�O�G�O�A:k�G�O�G�O�G�O�G�O�A=E�G�O�G�O�G�O�G�O�A:�vG�O�G�O�G�O�G�O�A7�(G�O�G�O�G�O�G�O�A7�G�O�G�O�G�O�G�O�A6��G�O�G�O�G�O�A4�YA5 �G�O�G�O�G�O�G�O�A9.�G�O�G�O�G�O�G�O�A:�G�O�G�O�G�O�G�O�A>?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A@�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A;K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A;�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A5�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AD$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AS�<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AaQG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Akb�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A{�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��QG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��zG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�eiG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7�A���A��A��zA��QA�=cA���A��A�fDA�;�A�~�  1 1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  1    1   1     1    1    1    1    1   11    1    1    1   1     1    1    1   1     1    1    1    1    1    1   11    1    1    1        1          1                        1                       1                         1                        1                        1                       1                         1                        1                       11                        1                        1             11111111111 G�O�G�O�?%�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�?%�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�?%�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�?%�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%�?%�?%�?%�?%�?%�?%�?%�?%�?%�?%�