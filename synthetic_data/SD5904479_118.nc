CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB          	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       f2020-12-03T07:47:24Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                 �  r<   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  t(   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {�   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  }�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  �   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  �   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   CHLA         	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  �   CHLA_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
CHLA_dPRES           	         	long_name         6CHLA pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   CHLA_ADJUSTED            	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  �H   CHLA_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   CHLA_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     � �   BBP700           	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � �   	BBP700_QC            	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8   BBP700_dPRES         	         	long_name         8BBP700 pressure displacement from original sampled value   
_FillValue        G�O�   units         decibar      � $   BBP700_ADJUSTED          	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     �  �   BBP700_ADJUSTED_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � (|   BBP700_ADJUSTED_ERROR            	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � *h   CDOM         	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � 2   CDOM_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   
CDOM_dPRES           	         	long_name         6CDOM pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      � ;�   CDOM_ADJUSTED            	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � CX   CDOM_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � K   CDOM_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � L�   NITRATE          	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � T�   
NITRATE_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \H   NITRATE_dPRES            	         	long_name         9NITRATE pressure displacement from original sampled value      
_FillValue        G�O�   units         decibar      � ^4   NITRATE_ADJUSTED         	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � e�   NITRATE_ADJUSTED_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � m�   NITRATE_ADJUSTED_ERROR           	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � oxArgo synthetic profile          1.0 1.2 19500101000000  20201203074724  20201203074724  5904479 UW, SOCCOM, Argo equivalent                                     STEPHEN RISER , KENNETH JOHNSON                                 PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                            vA   AO  DDDDARRDNAVIS_A                         0276                            110713                          863 @�z�OOg1   @�z33?:@Il������/g-1   GPS        vPRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         DOXY_ADJUSTED=DOXY*G                                                                                                                                                                                                                                            CHLA_ADJUSTED=CHLA/A, NPQ corrected (Xing et al., 2012), spike profile added back in                                                                                                                                                                            not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  NITRATE_ADJUSTED=[NITRATE-SUM(OFFSET(S)+DRIFT(S))]/GAIN                                                                                                                                                                                                         dP =-0.23 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            G=1.0391                                                                                                                                                                                                                                                        A=2                                                                                                                                                                                                                                                             not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  OFFSET(S) and DRIFT(S) from climatology comparisons at 1000m or 1500m. GAIN from surface/deep comparison where surface values are known                                                                                                                         Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        G determined by surface measurement comparison to World Ocean Atlas 2009.See Takeshita et al.2013,doi:10.1002/jgrc.20399                                                                                                                                        A is best estimate from Roesler et al., 2017, doi: 10.1002/lom3.10185                                                                                                                                                                                           not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  Contact Tanya Maurer (tmaurer@mbari.org) or Josh Plant (jplant@mbari.org) for more information                                                                                                                                                                  2017062616081120170626160811201706261608112020120212402620201202124026202012021240262020120212402620201202124026A   A   A   B   A   F   F   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @A�@�\)@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B���B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�B�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CHT{CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl:�Cn:�Cp:�Cr:�Ct:�Cv:�Cx:�Cz:�C|:�C~:�C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RDRD��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�RD&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQRDQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�DzR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A^�HA^�`A^�HA^�`A^�A^�A^�A^��A^��A^��A_A_A_A_%A_%A_%A_
=A_
=A_
=A_VA_VA_VA_VA_VA_VA_
=A_VA_
=A_
=A_
=A^ȴA^�+A^  A]hsA\�AY�hAT9XAN�HALz�AKXAKVAI�AG�hAF�uAFAE�7AEK�AD�HAD�DADbACO�AC�AB��AA�A@�`A@�+A>��A=�#A=�#A=�
A=��A=��A=x�A<�jA<=qA< �A;�#A;?}A:��A:�\A: �A:  A9A9t�A97LA9K�A9?}A8��A8��A8M�A7�mA7p�A7;dA6�A6�A6ĜA6��A6�A5ƨA5�hA57LA4^5A41A3�mA3��A3%A2��A2z�A2E�A1��A0��A1�A1�FA1�PA1;dA1VA0�/A0ĜA0��A0��A0�uA0~�A0�A/�TA/�TA/�A/��A0(�A1&�A0jA0�uA0�/A0��A1;dA1K�A0�HA0�!A0�A1K�A1�-A1�FA1��A1�;A1ƨA1�A1�A1�FA1A1A1��A1��A1�hA1�7A1��A1�7A1��A1�-A1�^A1�^A1�-A1�FA1��A1��A1�PA1�A1|�A1t�A1O�A1?}A0�A0�yA1
=A0�A1
=A0ȴA0��A0��A0�\A0�+A0�A/p�A/`BA/K�A/��A/�A/�TA/�mA/�#A/A/��A/�wA/�A/��A/�hA/�PA/O�A.�A.�A.^5A.1'A.-A.5?A.Q�A.��A/33A/+A/"�A/�A.�HA.�RA.ĜA.��A.I�A.JA.1A.  A.E�A.��A.n�A.�DA.��A/�A/�A/�A/A.��A.�A.�yA.�`A.�/A.�/A.��A.��A.ȴA.ĜA.��A.�!A.��A.��A.��A.��A.��A.�uA.�\A.�\A.�DA.�+A.�+A.�A.z�A.r�A.n�A.n�A.n�A.n�A.ffA.^5A.VA.VA.VA.Q�A.Q�A.Q�A.M�A.I�A.E�A.E�A.A�A.=qA.9XA.1'A.-A. �A.�A.�A.{A.1A.  A-��A-�A-�mA-�#A-��A-A-�^A-�-A-��A-�hA-�A-p�A-dZA-XA-K�A-K�A-G�A-C�A-?}A-33A-/A-"�A-�A-%A,��A,��A,�A,�/A,��A,��A,ĜA,�9A,��A,�DA,v�A,bNA,VA,-A+��A+��A+��A+��A+|�A+t�A+hsA+S�A+VA*��A*r�A*-A*bA)��A)dZA);dA)oA(��A(�jA(�!A(��A(��A(�DA(~�A(n�A'��A'�
A'�wA'�A'��A'G�A'�A&��A&��A&$�A%��A%�PA%33A%�A%oA%�A%
=A%
=A%%A$��A$��A$�!A$��A$r�A$VA$1A#��A#�A#�A#�A#A#��A#t�A#O�A#;dA#33A#�A#A"�A"�/A"��A"bNA"=qA"�A!�mA!�A!�PA!�hA!p�A!;dA!33A!7LA!K�A!`BA!XA!;dA!�A ��A ĜA ��A bNA r�A ȴA ȴA ȴA|�A�^A;dA
=A��A��A��A�A^5A�;A�A&�A��A��AM�A$�A(�A9XAbNA�A��AXAS�A��A��A?}A%A�hA�/A��AA��AM�A�mA
=A{A�A1'AoA+A%A�A�AffA�A�-A�A��AVA�PA7LAƨA
�A
��A
�A`BA�#A��Az�A�9AC�A��A��A�hA��A��A$�Av�A�A�AK�A�yA=qA�A�TAK�A�A�A�/AȴA�DA �A&�A��A^5A7LA�A
I�A&�A~�A��A�FA�Ao@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A^�HA^�`A^�HA^�`A^�A^�A^�A^��A^��A^��A_A_A_A_%A_%A_%A_
=A_
=A_
=A_VA_VA_VA_VA_VA_VA_
=A_VA_
=A_
=A_
=A^ȴA^�+A^  A]hsA\�AY�hAT9XAN�HALz�AKXAKVAI�AG�hAF�uAFAE�7AEK�AD�HAD�DADbACO�AC�AB��AA�A@�`A@�+A>��A=�#A=�#A=�
A=��A=��A=x�A<�jA<=qA< �A;�#A;?}A:��A:�\A: �A:  A9A9t�A97LA9K�A9?}A8��A8��A8M�A7�mA7p�A7;dA6�A6�A6ĜA6��A6�A5ƨA5�hA57LA4^5A41A3�mA3��A3%A2��A2z�A2E�A1��A0��A1�A1�FA1�PA1;dA1VA0�/A0ĜA0��A0��A0�uA0~�A0�A/�TA/�TA/�A/��A0(�A1&�A0jA0�uA0�/A0��A1;dA1K�A0�HA0�!A0�A1K�A1�-A1�FA1��A1�;A1ƨA1�A1�A1�FA1A1A1��A1��A1�hA1�7A1��A1�7A1��A1�-A1�^A1�^A1�-A1�FA1��A1��A1�PA1�A1|�A1t�A1O�A1?}A0�A0�yA1
=A0�A1
=A0ȴA0��A0��A0�\A0�+A0�A/p�A/`BA/K�A/��A/�A/�TA/�mA/�#A/A/��A/�wA/�A/��A/�hA/�PA/O�A.�A.�A.^5A.1'A.-A.5?A.Q�A.��A/33A/+A/"�A/�A.�HA.�RA.ĜA.��A.I�A.JA.1A.  A.E�A.��A.n�A.�DA.��A/�A/�A/�A/A.��A.�A.�yA.�`A.�/A.�/A.��A.��A.ȴA.ĜA.��A.�!A.��A.��A.��A.��A.��A.�uA.�\A.�\A.�DA.�+A.�+A.�A.z�A.r�A.n�A.n�A.n�A.n�A.ffA.^5A.VA.VA.VA.Q�A.Q�A.Q�A.M�A.I�A.E�A.E�A.A�A.=qA.9XA.1'A.-A. �A.�A.�A.{A.1A.  A-��A-�A-�mA-�#A-��A-A-�^A-�-A-��A-�hA-�A-p�A-dZA-XA-K�A-K�A-G�A-C�A-?}A-33A-/A-"�A-�A-%A,��A,��A,�A,�/A,��A,��A,ĜA,�9A,��A,�DA,v�A,bNA,VA,-A+��A+��A+��A+��A+|�A+t�A+hsA+S�A+VA*��A*r�A*-A*bA)��A)dZA);dA)oA(��A(�jA(�!A(��A(��A(�DA(~�A(n�A'��A'�
A'�wA'�A'��A'G�A'�A&��A&��A&$�A%��A%�PA%33A%�A%oA%�A%
=A%
=A%%A$��A$��A$�!A$��A$r�A$VA$1A#��A#�A#�A#�A#A#��A#t�A#O�A#;dA#33A#�A#A"�A"�/A"��A"bNA"=qA"�A!�mA!�A!�PA!�hA!p�A!;dA!33A!7LA!K�A!`BA!XA!;dA!�A ��A ĜA ��A bNA r�A ȴA ȴA ȴA|�A�^A;dA
=A��A��A��A�A^5A�;A�A&�A��A��AM�A$�A(�A9XAbNA�A��AXAS�A��A��A?}A%A�hA�/A��AA��AM�A�mA
=A{A�A1'AoA+A%A�A�AffA�A�-A�A��AVA�PA7LAƨA
�A
��A
�A`BA�#A��Az�A�9AC�A��A��A�hA��A��A$�Av�A�A�AK�A�yA=qA�A�TAK�A�A�A�/AȴA�DA �A&�A��A^5A7LA�A
I�A&�A~�A��A�FA�Ao@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�RB�XB�XB�XB�^B�^B�^B�^B�^B�^B�^B�dB�dB�dB�dB�^B�^B�dB�jB�dB�dB�dB�dB�jB�jB�qB�qB�wB��B��BǮB��B��B�)B�BB)�BE�BJ�BB�B=qB2-B!�B�B�B�B{BhB\BJB+BBB��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�fB�ZB�mB�yB�B�yB�sB�mB�fB�ZB�TB�`B�fB�yB�sB�mB�`B�TB�5B�)B�#B�B��B��B��B��BɺBĜBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�sB�NB�fB�B�B��B��B��B�B��B��BB%B1B
=B
=B
=B
=BJBVB\B\BbBbBbBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuBDBDBDBoB�B�B�B�B�B�B�B�B�B�B�B�B\B\B
=B1B1B	7BDBbB�B�B�B�B�B{B�BuB\BPBJBJBbB�B{B�B�B �B �B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"�B!�B!�B"�B"�B"�B"�B"�B!�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B"�B"�B"�B"�B"�B"�B"�B!�B!�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBuBoBbBVBJB
=B
=B1B1B1B+BB��B��B��B��B��B�B�B�B�B�yB�yB�sB�sB�mB�mB�fB�NB�BB�;B�5B�/B�B�B�
B��B��BɺBƨBÖBBBĜBĜBĜBĜBĜBBB��B��B��B�wB�qB�qB�jB�jB�jB�dB�^B�^B�XB�^B�XB�XB�^B�XB�RB�LB�FB�?B�9B�3B�9B�9B�?B�?B�FB�LB�XB�dB�jB�dB�^B�^B�RB�LB�LB�dB��B��B�wB�'B��B�{B�hB�bB�VB�PB�JB�DB�7B�%B�B�B�B}�B}�B�B�+B�\B�uB�}BĜBĜB�qB�-B�B��B��B�VB� Bx�Br�Bn�BiyB`BBT�BK�B?}B�B�B�B�B�B�B�BhBoB�B)�B0!B-B%B��B��BBPB�B6FBffB�bB��B��B��B��B��B��B�LB�wB��B��B��B��B��BȴBǮBɺB��B��B��BɺBɺBƨB�jB�B��B�JB~�Bk�BK�BD�B?}B>wB>wB:^Bo11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B�=B�EB�EB�BB�IB�IB�IB�IB�IB�IB�JB�OB�OB�PB�PB�JB�JB�PB�TB�PB�SB�PB�PB�YB�YB�_B�[B�cB�rB�rBǚB̺B��B�B�nB B)�BE�BJ�BBB=aB2B!�B�B�BvBgBSBJB9BBB�B��B��B��B�B�vB�vB�|B�|B�vB�yB�xB�sB�kB�pB�rB�vB�}B�B�wB�mB�RB�FB�ZB�cB�tB�cB�_B�YB�RB�GB�?B�MB�QB�cB�^B�]B�LB�AB�"B�B�B��B��B��B��BͿBɥBąBțB��B��B��B��B��B��B��B��B��B��B��B̹B��B��B��B��B�\B�8B�QB�wB�{B��B��B��B�B��B��BBBB
)B
(B
+B
-B8BCBGBKBPBPBPB\BiBtB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BaB1B0B0BYBtByByBzB}B|B�B�B�B�B�BuBGBGB
)BBB	#B1BMB�B�B�B�BuBgBmBbBFB>B6B6BNBuBiBzB�B �B �B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"�B!�B!�B"�B"�B"�B"�B"�B!�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B"�B"�B"�B"�B"�B"�B"�B!�B!�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~B�B�ByByBsBsBsBnBhBgBcB_B_BOBCB7B
)B
)BB BBB�B��B��B��B��B��B�B�|B�wB�jB�eB�dB�bB�bB�YB�YB�OB�:B�-B�&B�"B�B�B��B��B��B��BɨBƒBÂB�zB�xBćBĆBĆBĉBĉB�yB�xB�uB�nB�nB�cB�\B�[B�TB�TB�SB�NB�GB�IB�FB�HB�BB�EB�FB�FB�<B�6B�1B�(B�#B�B�!B�#B�(B�'B�0B�:B�EB�LB�VB�MB�GB�GB�=B�5B�9B�NB�nB�nB�cB�B��B�bB�PB�LB�AB�:B�4B�-B�B�B��B��B��B}�B}�B��B�B�EB�[B�gBćBĄB�\B�B��B��B�~B�@B�Bx�Br�Bn�BiaB`,BT�BK�B?eB�B�B�B�B�B�BmBMBTByB)�B0B,�BB��B��B B9BoB6-BfPB�HB��B��B��B��B��B��B�6B�bB�rB��B��B��B˰BȟBǛBɥBʫBʯBʫBɥBɥBƐB�UB��B��B�2B~�BkoBK�BD�B?gB>_B>^B:FBV11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cn�nCn�(Cn��CnvJCn�0Cn|KCn��Cne�CngCnT�CnU�CnV�CnG�Cn=�CnDCn=<Cn(�Cn%YCn Cm��Cm�}Cm��Cm�iCm��Cm��Cmo�Cm-�Cl�XClE2Ck�0Cj��Ch��Cf�SCc��C_��C[�CWj�CV-xCVt�CV��CWz�CW�#CW��CV۝CV��CV�CV�4CV�?CV�rCV��CWQ]CWe�CW;�CWS�CW�*CXy�CYǧCZ?�CZY,CZ�0CZ��C[o�C\F�C]QxC^&�C^�"C_�C`C`7�C`1�C`3�C`;C_��C_�hC`C_�iC_��C`�C_�jC_�FC_>gC^�C^�aC^�C_#�C_@C^�C^�BC^U�C]��C]	C\óC\u�C[ȔC[B�C[�C[)CZƌCZD_CZ�C[�C\��C]ϨC^��C_ImC_y�C_L�C^��C^үC^��C]�C]�C\��C\��C]C]�hC_f�Ca�bCb�Cc'�Cd��CeB�CfhCf��CfALCfCf��Cg�Ch��Ch�ChڣCi\Ci�Ci�Ci1CiY2Cih�CicCiX!CiVCi: Ci:_CiP�Ci��Ci�Cj4CjJ0Cjd�CjgCj�*Cj�Cj��Cjp�Cj��Cj�vCj�tCj��Cj��Cj�Cj�%Cj��Cj�YCk�Cj�sCkUCj��Cj�aCju!Cj�Ci�^Ci��CjT�Ck~Ckz�Ck��Ck�UCk��Ck�Cl)�Cl�CltFCl��Cl�Cl\6Cl�Ck��CkS�CkCj�CkO|Ck�Cl7~Cl��Cm��Cm��Cm��Cm��Cm��Cmt\Cm��CmQCm	$Cl�+Cl��Cm'�Cm�mCn�Cn��Co`ICp]�Cp��Cp�Cq(�Cq�YCq��Cq�[Cq�xCq��Cr�CrdCr%CrE{Cr\qCrvmCr��Cr��Cr�rCr��CrәCrǶCr�)Cr�
Cr��Cs#�Cs�CsB�CsR�CsY�CsSsCsp�CszJCs�ICs��Cs��Cs��Cs�)Cs��Cs�Cs�OCs��Cs��Cs�WCtCs��CtsCt0CtDCt�CtICt$�Ct( Ct�Ct�CtCt Ct'�Ct4Ct�Ct[Ct�Ct�Cs�)Cs�]Cs��Cs�QCs�_Cs�qCss1Csa Csj�Csa�CsH�CsF~Cs'<Cs@sCs>QCs6Cs
<Cs�Cr��Cr��Cr��Cr�nCr�zCr�WCr�Cr��Cr��Cr{�Cr]�Cr73Cr/�Cr�Cq�)Cq}Cq(#Cp�WCp�YCpv@Cp;9CpCo�YCo��Co �Cno�Cm�lCmxwCmaCl��Cl:�Cl
CkŠCk��Ck�NCk��CkxCkG
Ck4�Ck�Cj�Cj&qCi�CiĦCirWCi5@Ch�	Ch� Ch"�Cg�JCf�.Cf��Cf8�Ce��Ce�YCe��Ce�_Ce�3Ce�"Ce�BCevCe�Cd�rCd��Cdb�Cd�Cc�XCc�uCc��Cc�NCco�Cc�Cb��Cb��CbW^Cb�Ca��Ca��Ca9�C`�-C`�AC`1C_��C_NC^�tC^x�C^�C]� C]F�C\��C\k�C\8�C\'"C\7$C\M�C\JEC\	C[�mC[mmC[rCZ�GCZw�CZ	�CYЬCYUCX�PCW�UCVЂCV�GCV�}CVG�CV)CU��CU��CU&�CT��CTx�CTQ�CT.�CS�^CSR�CR��CR�CQl�CQMCP�.CQW:CP=7CN��CMD�CK�VCKn�CJ�9CI��CH��CG��CG�XCG(�CF�|CFMCECDQCD	�CCq�CB��CB�CBX�CB#CA��CA�ACA�CA�1CA��CA��CAd�C@�C@��C@K�C@�hCATC@ؙC@y�C@+�C?��C>-�C<��C;��C;�C;h5C;n�C;y�C;��C<�C<�C<�C;�gC:ӤC:
�C9��C9?�C9�C8�C8��C8��C8��C8��C8��C8�hC8͍C9-%C9ŎC:;�C:��C;S�C<�C=�C=W�C=��C=��C=�C>[C>�533333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Cw�Cw��Cw�Cw�3CwخCw�pCw�Cw�Cw�eCw�SCw�xCw�\Cw��Cw��Cw�Cw��Cwx�CwuCw_
CwH�CwBSCw0Cw�Cv��Cv�.Cv�&Cvs�Cv!MCu�*Ct�<Cs��Cr~Co��CllChX&Cc��C_��C^�MC^�[C_D�C_�C`q.C`>�C_BAC^�C_ /C_�C_�C_"�C_b�C_��C_��C_�C_�BC`;,C`�eCbK�Cb�&Cb��Cc�CcnCd^Cd�Ce��Cf�YCg�ChUCh� Ch��Ch��Ch��Ch�Ch��Ch�qChΤCh�Ch��Ch��Ch�#ChH�Cg��Cg��CgwCg��Cg�SCg�uCg�QCg_Cg;Cfe*Ce�ZCeevCeCCd`�Cc�;Cc�yCc��CcTgCb�#Cb�@Cc��Ce��Cf{�CgfChpCh6kCh�CgDiCg�CgK�Cf��Ce�Ce-yCeU�Ce�=Cf�RCh"�Cj��Ck]_Cl	LCm�OCn9nCo�Co�~CoBCoCo�Cp�kCq�Cq�PCq�jCr8�Cr9(Cr5CrO@Crx�Cr��Cr�6Crw�Cru�CrX�CrX�Crp7Cr�Cr��Cs@pCssVCs��Cs�QCs�fCs��Cs��Cs�;Cs�ZCs�Cs�Cs��Cs��Cs�`Cs�Cs�\Ct�Ct7Ct'_Ct4�Ct'�Cs�vCs��Cs=Cr�lCr��Cs~/Ct1�Ct��Ct�Cu�CuCu$�Cue�CuÈCu�CuєCu�LCu�CuR�Ct�Ct�CtHACt�Ct��Ct�hCus�Cv*=Cv�}Cw qCwB�Cw#lCv�2Cv�0Cv� Cv�~CvM�Cv>7CvC�CvmkCwCw��Cx=&Cx�XCyíCzF�Cz[7Cz��Cz��C{/�C{,�C{M?C{u�C{��C{C{��C{��C{�aC{�aC|[C|?C|.�C|8�C|R2C|E�C|^6C|xC|~C|��C|��C|ſC|�-C|ݧC|�C|��C|�gC}C}�C}.�C}:�C}PQC}`�C}l�C}jsC}~�C}��C}��C}�~C}}�C}��C}��C}�HC}�5C}�NC}�YC}�	C}�hC}��C}�8C}�oC}��C}�.C}��C}��C}�QC}��C}r�C}ePC}N�C}LSC}#�C},>C|�C|�AC|�zC|��C|�,C|ɕC|�C|�MC|�C|�}C|��C|�LC|o�C|_�C|\�C|D�C|H�C| C|�C|&�C|?C{��C{��C{��C{�C{|[C{;BCz�ICz�CzACz �Cy�-Cy��Cy|\Cy\nCyCxY"CwCwL�Cv�tCv[�CvdCuwCuD�Ct��CtýCt��Ct�\Ct�CtzCtf�Ct7KCs��CsN1Cs�Cr�Cr�CrS�Cq�4Cq��Cq6ACp��Co��Co�?Co9Cn�dCn�%Cn�Cn��Cn��Cn��Cn��Cnn�CnCm��Cm�#CmQCl��Cl�jCl�aCl��Cl��ClT|Ck��Ck�uCkh_Ck0�Cj�]Cj�;Cj]mCj	Ci�CizVCh�"Chx Ch	2Cg��Cg+�Cf�PCf>
Ce��CepCe
Cd�CCd��Cd�iCd�Cd�JCd��CdGmCd�Cc��CcD�Cc�Cb�iCbT�Ca�jCaHtC`?�C_6�C_�C^��C^��C^��C^B�C]�DC]| C\��C\�`C\��C\zqC\IaC[��C[1<CZL�CY��CYACY#CY��CXa�CV��CUK[CS�'CSb�CRĉCQl�CP�CO�QCOZ�CN�}CNr,CM�hCL�CK�CK�;CKCJY�CJp0CI��CI�RCIPCI#�CI CI �CI*�CI�CH�sCH��CH9CG��CH�CH�dCHb�CH .CG�CG CE�rCD,�CC>�CB�"CB�CB��CB�CB�'CC]�CCu�CC]�CC�CB!�CAQ$C@�*C@~C@R#C@�C?�C?�C?�C?�<C?�C?�"C@ZC@j�CA	CA��CB~CB��CC]�CD|vCD��CD��CE!�CE<CEv�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114 @�D@� @��@�?@�(@�=@��@�I@�"@~�@�@�;@v8@o�@s�@oN@a�@_l@QN@C@>�@3C@(D@)@�@�@��@��@ @�d@ �@��@�@On@�L@��@A�@n�@�O@�M@L�@��@�s@�@��@�p@�3@��@Ω@�]@1@>�@"�@2�@�@�@�0@#�@4�@V�@�<@�P@}F@.�@�b@8]@�[@{@$@@�@�@��@�@�!@�n@�S@�1@��@��@vd@DA@#?@H>@d�@_@Cg@�@۫@s�@��@�L@�T@)K@�&@�@��@}�@'!@ @�8@��@�@U@}�@��@�@�@.�@�@��@�@�w@�2@
e@��@�:@>�@�(@1@�@v�@.@b<@ 
@�@n'@�c@�@��@ځ@�@�@D@ @.�@8�@5A@-�@,�@@+@)@JK@�b@�\@��@��@�@t@�@��@�x@��@�Y@�@��@ d@\@j@(�@8@L0@B(@J�@Bb@"�@�}@�.@gO@��@��@H�@�b@�f@ϻ@�!@�*@�@I�@?j@R�@W�@/j@�@��@j@W4@9�@|�@��@�@��@�@)>@?4@+'@	@�@�G@�<@�j@�u@�@��@�@�R@�t@0�@�e@ -R@ :a@ `T@ � @ �?@ ��@ �G@ �Y@ ��@ �&@!-@!�@!-@!>S@!H�@!Y@!e�@!k�@!|I@!ta@!��@!��@!�S@!��@!�I@!�<@!��@!Չ@!�O@!��@!�#@!��@!�@"	x@"8@"�@")h@"10@"/�@"<�@"@}@"@�@"Eo@";�@"TQ@"Uz@"O�@"G,@"O�@"\b@"^�@"W�@"Rk@"R@"V�@"^n@"N�@"IK@"FJ@"I�@"E@"4�@",\@"@"^@"x@"�@!�k@!�g@!��@!��@!�Z@!ȱ@!��@!Ĭ@!�A@!��@!��@!�x@!�@!�@!�+@!s�@!v8@!\1@!Y]@!`o@!Xz@!A�@!.@!G@!l@ �m@ ��@ ��@ `@ )�@  d@�@�v@��@�Q@d�@�]@��@E�@�s@�8@q�@
@��@�E@�<@�}@�Z@��@w@j�@LY@�@�*@�V@v!@?d@�@�!@�L@`*@�(@��@Jf@>@�@˝@�P@՛@ǆ@�$@��@��@_f@>@?@��@��@�@h�@h�@]�@@O@�@ډ@�3@��@V�@,�@�Z@Ǵ@�.@m@�@��@��@?:@�@��@Z�@'�@�@��@t@h-@r�@��@�@Tx@;@�@��@s�@I`@ C@�0@��@._@��@�V@��@�p@�b@l@?X@�@�@e�@Lg@2�@*@��@��@Hy@�@E�@
�@
�@7t@
{�@	��@�&@��@I�@�/@A@�@��@��@r<@ @�C@�@�&@^�@��@�/@�{@>�@	*@ �@ ��@ ��@ ��@ �@ ��@ �r@ TB@ N?�Ù@ Q�@ ]@ ?N@  ?��r?�� ?��~?�l?���?���?�B�?�Kp?�Y�?���?��?�0^?�K?���?�}?�r?�ϑ?�c�?�+�?���?���?���?���?���?�}�?��J?���?�K?��?���?�l&?�'/?�r?���?�գ?��?�TK?�u�?���G�O�?�~?�~?��c?��?�*�?��?�9X?� \?���?�ߤ?�1�?���?��*?�9X?�
=?�RT?�RT?��?ǧ�?��#?�<6?���?��?öF?�x?��?�C�?��?Ď�?�{?��E?��?��-?�n�?��7?<�>��a=�D�=+<�PH=-�==<6=0�<�J�<�?<�?<�3�<�J�<�-�<XD�<�3�<�9X<�?<�3�<|PH<�-�<|PH<�9X<�?<�?<�?<�9X<�?<�9X<�9X<�-�<|PH<49X<|PH<|PH<�3�<|PH<�-�<49X<�-�<�-�<XD�<XD�<|PH<�-�<�?<�-�<XD�<XD�<|PH<|PH<�9X<49X<49X<|PH<49X<XD�<XD�<�3�<-�<49X<-�<XD�<49X<|PH<|PH<49X<XD�<-�<49X<XD�<|PH<XD�<-�<XD�<49X<XD�<49X<XD�<XD�<XD�<XD�<XD�<XD�<XD�<-�<XD�<49X<49X<49X<-�<-�<-�<-�;�D�<XD�<49X<-�;�D�<49X<|PH<-�;�D�<-�;�-�<-�;�D�<-�;�D�<49X<-�<-�<-�<-�<49X<49X<49X<49X;�D�<-�<-�<-�<-�<XD�<49X<49X<49X<49X<-�<49X<XD�;�-�;�D�;�-�;�D�<49X;�-�;�D�<49X<-�;�D�<49X<49X<49X<XD�<-�<-�;�D�<49X<XD�<49X<-�<49X<-�<-�;�-�<-�;�D�<-�<-�<-�<-�<|PH<49X<XD�;�D�;�D�<-�;�D�;�D�<-�<49X<-�<-�;�D�<-�<-�<XD�<49X;�-�<-�<-�;�D�;�D�<-�<-�<-�<XD�;�D�;�-�<-�;�-�;�D�<-�<-�;�D�<49X<-�<XD�;�D�<-�;�D�;�D�<-�;�D�;�D�<49X<-�<-�;�D�<-�;�D�;�D�;-�<-�;�-�<XD�;�D�<-�;�-�;�D�;�-�;�D�;�D�<-�<-�<-�;-�;�-�;�-�;�-�<-�;�-�;�-�;�D�;�D�;�-�<-�;�D�;�D�<-�<-�;�-�;-�<-�;�-�<-�;�D�;-�;�-�<-�;-�;�-�<-�;-�;�-�;�D�<-�<-�;�-�;�D�;-�;�-�;�D�<-�<49X<-�<-�;�D�;�D�;�-�;�D�;-�;�D�;�-�;�D�<-�;-�;�-�<49X<-�;�-�;�D�;�D�;�D�<-�;�-�<-�<-�;�D�<XD�;�D�;�D�<-�;�D�;-�<-�;�D�;�D�<-�<-�<-�<XD�<XD�<49X<49X<49X<-�;-�;�D�<49X<49X<-�<-�;�D�<49X<-�<-�<XD�<-�<-�<-�<49X<-�<-�;�D�<|PH<49X<-�<-�;�D�<-�<-�<49X<XD�<-�<XD�<-�<-�<-�<XD�<XD�<49X;�D�<-�<|PH<XD�<-�<-�<-�<-�<-�<49X;�D�;�D�<-�<XD�<49X<XD�<-�<-�<XD�<-�<49X<-�<|PH<XD�<49X<XD�<XD�<-�<XD�<|PH<49X<-�<|PH<XD�<|PH<49X<|PH<XD�<XD�<49X<�-�;�D�<49X<|PH<XD�<49X<�-�<�-�<�-�<�-�<49X<|PH<|PH<XD�<|PH<�3�<�3�<�-�<XD�<|PH<|PH<-�<�-�<�-�<|PH<XD�<|PH<49X<�-�<-�<�9X<XD�<|PH<|PH<�-�<XD�<�-�<�-�<�9X<�-�<XD�<|PH<XD�<�3�<�3�<49X<|PH<|PH<�-�<�-�<|PH<|PH<|PH<XD�<�-�<|PH<�3�<XD�<49X<49X<|PH<|PH<|PH<XD�<XD�<�-�<�-�<�-�<�-�<�-�<�-�<�-�<|PH33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ?���                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?BM�?BM�?C�F?@�`?O�	?61�?BM�?BM�?C�F?;��?B�?C�F?@U2?@�`?BM�?BM�?BM�?Cn/?N��?>��?@�`?BM�?G��?BM�?R�?:�h?9�#?:�h?C&?<�?/o�?*�L?I�?%?  �>�J�>N!�=A��<49X<"3�<F?<�-�<XD�<-�;�D�;�D�;�-�<-�;XD�:�-�;�-�;�9X;�D�;�-�;-�;XD�;-�;�9X;�D�;�D�;�D�;�9X;�D�;�9X;�9X;XD�;-�    ;-�;-�;�-�;-�;XD�    ;XD�;XD�:�-�:�-�;-�;XD�;�D�;XD�:�-�:�-�;-�;-�;�9X        ;-�    :�-�:�-�;�-ຐ-�    ��-�:�-�    ;-�;-�    :�-ຐ-�    :�-�;-�:�-ຐ-�:�-�    :�-�    :�-�:�-�:�-�:�-�:�-�:�-�:�-ຐ-�:�-�            ��-ຐ-ຐ-ຐ-�-�:�-�    ��-�-�    ;-ຐ-�-ຐ-�XDк�-�-ຐ-�-�    ��-ຐ-ຐ-ຐ-�                �-ຐ-ຐ-ຐ-ຐ-�:�-�                ��-�    :�-�XDл-�XDл-�    �XDл-�    ��-�-�            :�-ຐ-ຐ-�-�    :�-�    ��-�    ��-ຐ-�XDк�-�-ຐ-ຐ-ຐ-ຐ-�;-�    :�-�-�-ຐ-�-�-ຐ-�    ��-ຐ-�-ຐ-ຐ-�:�-�    �XDк�-ຐ-�-�-ຐ-ຐ-ຐ-�:�-�-�XDк�-�XDл-ຐ-ຐ-�-�    ��-�:�-�-ຐ-�-�-ຐ-�-�-�    ��-ຐ-�-ຐ-�-�-໐-ຐ-�XD�:�-�-ຐ-�XDл-�XDл-�-ຐ-ຐ-ຐ-໐-�XDлXDлXDк�-�XDлXDл-�-�XDк�-�-�-ຐ-ຐ-�XDл�-ຐ-�XDк�-�-໐-�XDк�-໐-�XDк�-໐-�XDл-ຐ-ຐ-�XDл-໐-�XDл-ຐ-�    ��-ຐ-�-�-�XDл-໐-�-�XDл-ຐ-໐-�XD�    ��-�XDл-�-�-ຐ-�XDк�-ຐ-�-�:�-�-�-ຐ-�-໐-ຐ-�-�-ຐ-ຐ-ຐ-�:�-�:�-�            ��-໐-�-�        ��-ຐ-�-�    ��-ຐ-�:�-ຐ-ຐ-ຐ-�    ��-ຐ-�-�;-�    ��-ຐ-�-ຐ-ຐ-�    :�-ຐ-�:�-ຐ-ຐ-ຐ-�:�-�:�-�    �-ຐ-�;-�:�-ຐ-ຐ-ຐ-ຐ-ຐ-�    �-�-ຐ-�:�-�    :�-ຐ-ຐ-�:�-ຐ-�    ��-�;-�:�-�    :�-�:�-ຐ-�:�-�;-�    ��-�;-�:�-�;-�    ;-�:�-�:�-�    ;XDл-�    ;-�:�-�    ;XD�;XD�;XD�;XD�    ;-�;-�:�-�;-�;�-�;�-�;XD�:�-�;-�;-ຐ-�;XD�;XD�;-�:�-�;-�    ;XDк�-�;�9X:�-�;-�;-�;XD�:�-�;XD�;XD�;�9X;XD�:�-�;-�:�-�;�-�;�-�    ;-�;-�;XD�;XD�;-�;-�;-�:�-�;XD�;-�;�-�:�-�        ;-�;-�;-�:�-�:�-�;XD�;XD�;XD�;XD�;XD�;XD�;XD�;-�85555555555555555555555552222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ?�M�?�M�?öF?��`?ϊ	?�1�?�M�?�M�?öF?���?��?öF?�U2?��`?�M�?�M�?�M�?�n/?���?���?��`?�M�?ǧ�?�M�?ң?��h?��#?��h?�&?��?�o�?��L?�I�?�%?� �?9J�>�!�=���<�9X<�3�<�?=-�<�D�<�-�<XD�<XD�<-�<�-�;�D�;-�<-�<49X<XD�<-�;�-�;�D�;�-�<49X<XD�<XD�<XD�<49X<XD�<49X<49X;�D�;�-�    ;�-�;�-�<-�;�-�;�D�    ;�D�;�D�;-�;-�;�-�;�D�<XD�;�D�;-�;-�;�-�;�-�<49X        ;�-�    ;-�;-�<-�;-�    ;-�;-�    ;�-�;�-�    ;-�;-�    ;-�;�-�;-�;-�;-�    ;-�    ;-�;-�;-�;-�;-�;-�;-�;-�;-�            ;-�;-�;-�;-�;�-�;-�    ;-�;�-�    ;�-�;-�;�-�;-�;�D�;-�;�-�;-�;�-�    ;-�;-�;-�;-�                ;�-�;-�;-�;-�;-�;-�                ;-�    ;-�;�D�;�-�;�D�;�-�    ;�D�;�-�    ;-�;�-�            ;-�;-�;-�;�-�    ;-�    ;-�    ;-�;-�;�D�;-�;�-�;-�;-�;-�;-�;�-�    ;-�;�-�;�-�;-�;�-�;�-�;-�    ;-�;-�;�-�;-�;-�;-�    ;�D�;-�;-�;�-�;�-�;-�;-�;-�;-�;�-�;�D�;-�;�D�;�-�;-�;-�;�-�    ;-�;-�;�-�;-�;�-�;�-�;-�;�-�;�-�    ;-�;-�;�-�;-�;�-�;�-�<-�;-�;�D�;-�;�-�;-�;�D�;�-�;�D�;�-�;�-�;-�;-�;-�<-�;�D�;�D�;�D�;-�;�D�;�D�;�-�;�-�;�D�;-�;�-�;�-�;-�;-�;�D�<-�;-�;�D�;-�;�-�<-�;�D�;-�<-�;�D�;-�<-�;�D�;�-�;-�;-�;�D�;�-�<-�;�D�;�-�;-�    ;-�;-�;�-�;�-�;�D�;�-�<-�;�-�;�D�;�-�;-�<-�;�D�    ;-�;�D�;�-�;�-�;�-�;-�;�D�;-�;-�;�-�;-�;�-�;�-�;-�;�-�<-�;-�;�-�;�-�;-�;-�;-�;-�;-�            ;-�<-�;�-�        ;-�;-�;�-�    ;-�;-�;-�;-�;-�;-�    ;-�;-�;�-�;�-�    ;-�;-�;�-�;-�;-�    ;-�;-�;-�;-�;-�;-�;-�;-�    ;�-�;-�;�-�;-�;-�;-�;-�;-�;-�    ;�-�;�-�;-�;-�    ;-�;-�;-�;-�;-�    ;-�;�-�;-�    ;-�;-�;-�;-�;�-�    ;-�;�-�;-�;�-�    ;�-�;-�;-�    ;�D�;�-�    ;�-�;-�    ;�D�;�D�;�D�;�D�    ;�-�;�-�;-�;�-�<-�<-�;�D�;-�;�-�;�-�;-�;�D�;�D�;�-�;-�;�-�    ;�D�;-�<49X;-�;�-�;�-�;�D�;-�;�D�;�D�<49X;�D�;-�;�-�;-�<-�<-�    ;�-�;�-�;�D�;�D�;�-�;�-�;�-�;-�;�D�;�-�<-�;-�        ;�-�;�-�;�-�;-�;-�;�D�;�D�;�D�;�D�;�D�;�D�;�D�;�-�:�-�:�-�:�:�yF:���:�r�:�n]:�t:�[m:�nf:��Z:�4R:��E:��@:�ye;:�:�E�:��:��R:�.:�� :�n�:�]E:���:�,N:���:�yM:Ѭ�:��J:��:�	�:�k�:�j:�]B:�6�:��$:` `:~.:f�:J�:�:�.:wx:�T:	�>:��:):':=�:$+:��:7�:	��:^�:~�:��:F�:C?:��:��:}:em:	�n:�:]d:\�:r�:��:
�:
��:4��:
�:Ĉ:	�:Nh::	�m:	��:	��:H�:�:q�:}�:�:�:��u:J�:�:]�:�:��:
��:3�:v: ��:��:-�:!�:[O:�9�Nj::�i: ��:'9��q:�:�D9�_�9�^+:U:	t	9�\�:�9��: ��9�n�:�n:�"�:P�: �_: ٘:6�:�(9�AB:�: ��9�</:��9��:	q�:�!u:�":��: �e9�W:�,:�:��9�m�9�T�9�T�9�<=:-�9���:��9�i�:�9�:�:�:�U:K9�PV:3:��:�:�+: Փ: ��9�5L9��:%:0:G:[:F]:�:.�:	g�:�:	�:�9�%Z:��:	c�:*�:*B:�?:��:A :) :��:(:&��:J�:փ:�:	_�:wp:	]�:#9:��:t�:��:�:	`':�.:H5:�g:
��:�:F�:�r:��:�u:!�:��:
�:E�:-:�h:��:�(:/C:�~:�a:P�:�i:�R:h:��:�:�.:g{:�S:�:g5:f�:f:��:�:��:,3:�@::�:��:p�:e�:N:6U:��:�U:ie:M�:�:��:�~:�O:%��:&�:%��:A�:�:A�:A�:$W�:�:{�:�j:	Y:AZ:�:�:�:�:�):�:	X:��:	W�:�:�: ��:�:�:�:�:��:��: ��:m)9��:��9���:��9��+:��:2#:
��:l:�: �C:kM:�h9�&9�49�Ka:�n9��9�2�9��N9�1^9��j9��9�9���9�,�9�:0��9�AZ9�#9��9�(m9�9��I9���9�	�9�µ9��9�459�9�GQ9�ј9��9���9�29��:��9�49��9�@i:Wn: ��9��p9�h9���9�9�z�9�[9�_.9��~:�9�9�=�9�!9��:'�9�#�9�9�!9�"�9譋9� n9�99�5�9��9�9�9�89�G9�F�9�t9�E{9��e9��9�	9텻9���:��9�Ǻ9��I9㮺9蔟9�L~9㩤:�9�ҝ9�о9�\�9��;9�@�:��9�W~9�o[9�@w:A�9�ʈ9��+9�<9���9���9�5�9��9� 9�7�9�8X9��9��2:	�9�1�9��9�t�:��9��o:��:b�:�[:1�g:,�C:"�
:'�4:1{:B��:K>0:X��:e:�&�:��Y:r�Q:i�c:b�]:c�m:VHk:K7;:G�C:=��:9�$:,~l:0+�:2�*:3�^:�b:!|:1:��:k�:
j:
 s: .�9�9��}9�/:)�x:�:Q9���9�-:Y�:UV9���9�6v9��|:˫:/�:ڋ:@:�: 8>: 7�:��:��:oo:T��:S��:X��:�:}`�:a&�:D��:!T_:#�r:�:~:�z:(��:"��:(�:�:#��:(�Y:$�:.�i:�h:�:-�:�:�6:J�:��:K��:)�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ?���                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?��?Ѡ'?�?ɷ?�e�?�q?�?��?ͫ�?ɷ?�?ɷ?�e�?ͫ�?�q?�Z?��?�?�Z?��?�?��?�Z?�?ɷ?�?�?�e�?ɷ?�q?ɷ?��?�e�?��?�e�?Ѡ'?�N�?��?�N�?Ѡ'?�C-?�C-?�N�?Ѡ'?Ѡ'?Ѡ'?��?�,=?��?�C-?Ք�?��?�7�?�7�?Ք�?�C-?��2?��2?ى7?��2?�C-?Ք�?ى7?��?Ѡ'?Ѡ'?Ѡ'?��?ɷ?�N�?��"?�N�?��?��"?��?Ѡ'?��?��?�C-?��"?ͫ�?�N�?��?Ѡ'?��"?Ք�?�N�?Ք�?ͫ�?��2?�N�?��2?��2?��2?�C-?Ѡ'?��2?ى7?Ք�?�}�?��2?��2?��2?�C-?Ք�?���?�N�?Ք�?Ք�?�C-?��2?Ք�?��2?Ք�?�7�?��2?Ѡ'?Ѡ'?��"?�C-?��"?�Z?�Z?ɷ?�e�?�N�?�Z?�Z?�?�?�e�?��?ɷ?�q?�?ɷ?ɷ?�e�?�e�?�?�e�?�e�?�e�?�q?�q?ɷ?�q?�?��?�q?ɷ?�e�?�?��?�q?�?�?��?�e�?�?�e�?�?�?��?�|�?��?�?��?�?�?�q?�q?�?�?�?�q?�|�?�?�?��?�?�q?��?��?�|�?��?�q?�?�?�?�e�?��?�?�?��@�IR?�?�q?��?�+?��?�|�?�?�?��?�q?��?�|�?��?��?���?��?���?�|�?���?�|�?���?��?�6z?�+?���?�|�?�+?�6z?��?���?��?��?���?���?��?���?�A�?�6z?�6z?���?��u?���?�+?��?�|�?��?��?�6z?�|�?��?��?���?�6z?���?���?���?�+?�|�?��?���?���?�6z?��?��?�6z?�+?��?�6z?���?���?�+?��?���?��?��?��?��?��?�|�?�6z?���?�|�?�+?���?��?�6z?��?�|�?�|�?�6z?�|�?��?�6z?���?�|�?��?��?���?�+?��?��?��?�+?�6z?�+?�+?��?��?��?�+?��?�?��?��?��?�q?�q?�+?�?�?�?��"?�Z?�?�?�e�?ɷ?�e�?ͫ�?�Z?�e�?ɷ?�e�?�e�?�N�?ɷ?�Z?�Z?�Z?�e�?��"?ͫ�?�N�?�N�?ͫ�?�C-?Ք�?ͫ�?�C-?��"?��"?Ѡ'?�N�?�N�?Ѡ'?��?��2?Ѡ'?�N�?Ѡ'?��2?Ѡ'?Ք�?Ѡ'?�C-?Ѡ'?�C-?Ք�?�N�?��2?��2?Ք�?Ք�?Ք�?Ք�?Ք�?�C-?��2?ى7?�ں?�ں?��2?�}�?�ں?�}�?ى7?� �?ى7?��B?�ں?�ں?�}�?���?��B?�7�?� �?��B?��B?��B?��B?�rG?��B?�M?�rG?� �?���?�f�?�M?���?�R?�f�?���?�[W?�M?�rG?�	�?�	�?�[W?�R?��?�f�?�M?�	�?��]?�b?��]?�Dg?��]?��m?���?���?�b?�Dg?�b?�8�?�Dg?��r?��m?�8�?�8�?��r?��r?�-w?���?�-w?�-w?��}?���?���?��r?�!�?�s�?��r?�-w?�-w?��r?��}?�-w?��m?�8�?�-w?�~�?��}?�-w?�-w?�8�?���?�-w?�~�?�-w?�-w?�-w?�-w?��m?�-w?��r?�~�?�~�?�~�?���?�-w?�~�?�s�?�!�?��?�!�@ �D?��?�!�?�s�?�s�?��@ �D?�!�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 ?���                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@nH<G�O�@qO�G�O�@m
�G�O�@m5�G�O�G�O�@oGG�O�@pv�G�O�G�O�@k�!G�O�G�O�@t�nG�O�@ni]G�O�G�O�@o8�G�O�@w�tG�O�G�O�@�4�G�O�@��G�O�G�O�@�a�G�O�A>�G�O�G�O�A2�G�O�A3�IG�O�G�O�A=��G�O�A=A?G�O�G�O�A@��G�O�G�O�G�O�G�O�AC')G�O�G�O�G�O�AD�G�O�G�O�G�O�G�O�AB��G�O�G�O�G�O�G�O�G�O�AAy�G�O�G�O�G�O�G�O�AC��G�O�G�O�G�O�G�O�AF��G�O�G�O�G�O�G�O�AI�&G�O�G�O�G�O�G�O�ANL�G�O�G�O�G�O�AT��AV3G�O�G�O�G�O�A\xG�O�G�O�G�O�G�O�G�O�AU�WG�O�G�O�G�O�G�O�AY/^G�O�G�O�G�O�G�O�A\c-G�O�G�O�G�O�AV6AT�=G�O�G�O�G�O�G�O�AP9�G�O�G�O�G�O�G�O�AI�G�O�G�O�G�O�G�O�AI�$G�O�G�O�G�O�G�O�AI;G�O�G�O�G�O�G�O�AI��G�O�G�O�G�O�AHTG�O�G�O�G�O�G�O�G�O�AH��G�O�G�O�G�O�G�O�AI�@G�O�G�O�G�O�G�O�AJ+9G�O�G�O�G�O�G�O�AK�G�O�G�O�G�O�G�O�AH�G�O�G�O�G�O�G�O�AIpHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AN/.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AHsBAH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AC�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AC��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AB�MAB�gG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AE��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AS�^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ab�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Am�wG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A~��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�'   3 3 3 3  3 3  3  3 3  3 3  3 3  3 3  3 3  3 3  3    3   3    3     3    3    3    3    3   33   3     3    3    3   33    3    3    3    3    3   3     3    3    3    3    3    3        3         33                        3                        3                       33                       3                         3                        3                        3                        3                        3                        3                        3              3 G�O�G�O�G�O�����G�O����G�O�>���G�O�?L��G�O�G�O�?333G�O�?L��G�O�G�O�?��G�O�G�O��333G�O�>L��G�O�G�O��   G�O�����G�O�G�O��   G�O�����G�O�G�O��   G�O�=���G�O�G�O�����G�O�����G�O�G�O��333G�O�>���G�O�G�O��   G�O�G�O�G�O�G�O����G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�?   G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��   G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O����G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��   G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�    G�O�G�O�G�O�@��G�O�@�5G�O�@�|G�O�@��G�O�G�O�@��G�O�@��G�O�G�O�@[�G�O�G�O�@JG�O�@�G�O�G�O�@�NG�O�@W#G�O�G�O�@0�qG�O�@I��G�O�G�O�@���G�O�A޽G�O�G�O�A�
G�O�A+4G�O�G�O�A&[�G�O�A%�+G�O�G�O�A)+�G�O�G�O�G�O�G�O�A+�G�O�G�O�G�O�A,�mG�O�G�O�G�O�G�O�A+��G�O�G�O�G�O�G�O�G�O�A*�G�O�G�O�G�O�G�O�A,~�G�O�G�O�G�O�G�O�A/O�G�O�G�O�G�O�G�O�A2�G�O�G�O�G�O�G�O�A6��G�O�G�O�G�O�A=N�A>��G�O�G�O�G�O�AD�dG�O�G�O�G�O�G�O�G�O�A>[CG�O�G�O�G�O�G�O�AA�JG�O�G�O�G�O�G�O�AEG�O�G�O�G�O�A>�"A=})G�O�G�O�G�O�G�O�A8١G�O�G�O�G�O�G�O�A2CG�O�G�O�G�O�G�O�A25G�O�G�O�G�O�G�O�A1��G�O�G�O�G�O�G�O�A2��G�O�G�O�G�O�A0�G�O�G�O�G�O�G�O�G�O�A1(�G�O�G�O�G�O�G�O�A2k,G�O�G�O�G�O�G�O�A2�%G�O�G�O�G�O�G�O�A4��G�O�G�O�G�O�G�O�A1��G�O�G�O�G�O�G�O�A24G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A6�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A1.A0��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A,M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A,��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A+_9A+hSG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A.T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A<�JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AK~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AV�cG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AgJ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A|#)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�a   1 1 1 1  1 1  1  1 1  1 1  1 1  1 1  1 1  1 1  1    1   1    1     1    1    1    1    1   11   1     1    1    1   11    1    1    1    1    1   1     1    1    1    1    1    1        1         11                        1                        1                       11                       1                         1                        1                        1                        1                        1                        1                        1              1 G�O�G�O�G�O�?%f�G�O�?%f�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�?%f�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�?%f�?%f�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�?%f�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?%f�