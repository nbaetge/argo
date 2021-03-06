CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB          	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       f2020-12-03T07:47:52Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
_FillValue                 �  r`   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  tT   PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |$   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ~   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  ��   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   CHLA         	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  �`   CHLA_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   
CHLA_dPRES           	         	long_name         6CHLA pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      �  �$   CHLA_ADJUSTED            	         	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     �  �   CHLA_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   CHLA_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         mg/m3      C_format      %.3f   FORTRAN_format        F.3    
resolution        <���     � 
�   BBP700           	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � �   	BBP700_QC            	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � X   BBP700_dPRES         	         	long_name         8BBP700 pressure displacement from original sampled value   
_FillValue        G�O�   units         decibar      � L   BBP700_ADJUSTED          	         	long_name         )Particle backscattering at 700 nanometers      
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � $   BBP700_ADJUSTED_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � +�   BBP700_ADJUSTED_ERROR            	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         m-1    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � -�   CDOM         	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � 5�   CDOM_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � =�   
CDOM_dPRES           	         	long_name         6CDOM pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      � ?t   CDOM_ADJUSTED            	         	long_name         ?Concentration of coloured dissolved organic matter in sea water    
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � GD   CDOM_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � O   CDOM_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         ppb    C_format      %.3f   FORTRAN_format        F.3    
resolution        :�o     � Q   NITRATE          	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � X�   
NITRATE_QC           	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   NITRATE_dPRES            	         	long_name         9NITRATE pressure displacement from original sampled value      
_FillValue        G�O�   units         decibar      � b�   NITRATE_ADJUSTED         	         	long_name         Nitrate    standard_name         +moles_of_nitrate_per_unit_mass_in_sea_water    
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � jl   NITRATE_ADJUSTED_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � r<   NITRATE_ADJUSTED_ERROR           	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %.2f   FORTRAN_format        F.2    
resolution        <#�
     � t0Argo synthetic profile          1.0 1.2 19500101000000  20201203074752  20201203074752  5904479 UW, SOCCOM, Argo equivalent                                     STEPHEN RISER , KENNETH JOHNSON                                 PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                            xA   AO  DDDDARRDNAVIS_A                         0276                            110713                          863 @�03�1   @���(@I_�;dZ�1!�7Kƨ1   GPS        xPRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         DOXY_ADJUSTED=DOXY*G                                                                                                                                                                                                                                            CHLA_ADJUSTED=CHLA/A, NPQ corrected (Xing et al., 2012), spike profile added back in                                                                                                                                                                            not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  NITRATE_ADJUSTED=[NITRATE-SUM(OFFSET(S)+DRIFT(S))]/GAIN                                                                                                                                                                                                         dP =-0.06 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            G=1.0391                                                                                                                                                                                                                                                        A=2                                                                                                                                                                                                                                                             not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  OFFSET(S) and DRIFT(S) from climatology comparisons at 1000m or 1500m. GAIN from surface/deep comparison where surface values are known                                                                                                                         Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        G determined by surface measurement comparison to World Ocean Atlas 2009.See Takeshita et al.2013,doi:10.1002/jgrc.20399                                                                                                                                        A is best estimate from Roesler et al., 2017, doi: 10.1002/lom3.10185                                                                                                                                                                                           not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  Contact Tanya Maurer (tmaurer@mbari.org) or Josh Plant (jplant@mbari.org) for more information                                                                                                                                                                  2017062616081220170626160812201706261608122020120212402720201202124027202012021240272020120212402720201202124027A   A   A   B   A   F   F   A   @���@�  A   A!��A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�\�D�� D�|�D�� D�p D�� DԀ D���D�l�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A ��A"�]A@��A_\)A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB�B(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6}qD7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI
=DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW}qDX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^�=D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�
DyФD�^�D���D�~�D���D�q�D���Dԁ�D��D�n�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AO�TAO�mAO�mAO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO��AO��AO��AO��AP  AP  AO��AO��AP  AP  AP  AP  APAPAP1AP1AP1AO�AO�AO�AO�AO��AO��AO��AO��AO��AO��AO��AO��AO��AO��AP  AP  AO��AO��AO�FAO�AO\)AN��AN  ALJAH�AF�AFI�AD$�AB�/AB(�A@ĜA>ȴA=��A="�A<  A;�^A;�hA;�A:�\A:5?A:JA9A9A9?}A8��A8�!A8�HA8�DA8z�A8jA8v�A8�+A8��A8�A8�A8��A8�yA9
=A933A9&�A9VA9%A8~�A7�A7O�A7�A7�^A7l�A6�!A69XA6�A5�A5��A4�+A3�#A3&�A3
=A1x�A0Q�A/�A//A/O�A/�;A0jA/�-A/\)A/&�A/7LA/S�A/XA/`BA/7LA.��A.�A.��A.r�A.5?A-S�A,��A,�/A,�A,n�A,ZA,�RA-C�A-O�A-��A.�A. �A.�A.9XA/?}A/��A/�7A/�hA/��A/�A/�^A/�A/�
A/O�A/O�A/\)A.~�A/
=A/�7A/&�A.z�A.Q�A.1'A.��A.��A.�A/7LA/hsA/�hA.�yA.^5A.z�A-�;A,��A,��A,�jA,�A,M�A,M�A,z�A,��A-�A-VA-%A-/A-�A-;dA,�A,v�A,v�A,�\A,��A,��A,�A,~�A,-A,A+��A+G�A+oA*��A*��A*�`A*�jA*��A*^5A*A�A*E�A*A�A*JA)�^A)��A)p�A)hsA)dZA)XA)/A)A(�DA(5?A(A�A'�;A'�mA(=qA(ĜA(��A(��A(�+A(Q�A(�A'�A'�^A';dA&�`A&��A&^5A%�A%�;A%�wA%p�A%dZA%�A%�FA%ƨA%��A%�^A%��A%`BA%�A$�yA$��A$v�A$JA#|�A#\)A#�A#VA"ȴA"��A"�uA"�\A"��A"�\A!�mA!t�A!
=A ��A -A��AO�AoA5?A�AoA��AVA+Ax�A�A��AĜA�!AZA|�AO�A"�A��A  A  AI�A%A��A�yA�/Av�A��A�hA��A��AA�A��Al�A�A�FA�;AoA��A�DAt�A�A�
A�7A;dA
=A�/A��A��A
=A�AbAt�AA�A�jA��A�yA/Ax�A�7A��A��A��A��A`BAoA�jAv�An�A��A��A��A��A^5AM�AE�A-A�7A��A��A�AoA��AZA�^AĜA��A�+A�A�FA��A��A�7A�7A|�A33AĜAbNA1'AJA��A?}Av�AE�A{At�A�uA�A�-AC�A
�!A
$�A	�;A	�;A	A	|�A	hsA	O�A�At�Ap�A�A�`A�uA��A��A�DAffAȴA��A7LAC�A`BAK�A�`A�uA�A%AbA&�A�RA�A��A�A/AM�A�PAoA9XAv�A�FA�7Ap�AS�AO�Ax�A��An�A��Ar�AA�A��A&�A��A��AĜAXA ��A �A �@���@���@�@���@�&�@���@�=q@�M�@�@�K�@�ƨ@�p�@�Ĝ@��/@���@���@���@��D@�(�@��w@�^5@���@�G�@��P@���@���@���@�v�@�@�&�@�j@�1'@�+@�O�@�X@�X@�|�@�~�@���@�Z@�@�@�\)@�33@�K�@��@�+@��@�r�@܃@ͺ^@�/@�l�@���@�n�@�  @���@{33@t��@n�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                AO�TAO�mAO�mAO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO�AO��AO��AO��AO��AP  AP  AO��AO��AP  AP  AP  AP  APAPAP1AP1AP1AO�AO�AO�AO�AO��AO��AO��AO��AO��AO��AO��AO��AO��AO��AP  AP  AO��AO��AO�FAO�AO\)AN��AN  ALJAH�AF�AFI�AD$�AB�/AB(�A@ĜA>ȴA=��A="�A<  A;�^A;�hA;�A:�\A:5?A:JA9A9A9?}A8��A8�!A8�HA8�DA8z�A8jA8v�A8�+A8��A8�A8�A8��A8�yA9
=A933A9&�A9VA9%A8~�A7�A7O�A7�A7�^A7l�A6�!A69XA6�A5�A5��A4�+A3�#A3&�A3
=A1x�A0Q�A/�A//A/O�A/�;A0jA/�-A/\)A/&�A/7LA/S�A/XA/`BA/7LA.��A.�A.��A.r�A.5?A-S�A,��A,�/A,�A,n�A,ZA,�RA-C�A-O�A-��A.�A. �A.�A.9XA/?}A/��A/�7A/�hA/��A/�A/�^A/�A/�
A/O�A/O�A/\)A.~�A/
=A/�7A/&�A.z�A.Q�A.1'A.��A.��A.�A/7LA/hsA/�hA.�yA.^5A.z�A-�;A,��A,��A,�jA,�A,M�A,M�A,z�A,��A-�A-VA-%A-/A-�A-;dA,�A,v�A,v�A,�\A,��A,��A,�A,~�A,-A,A+��A+G�A+oA*��A*��A*�`A*�jA*��A*^5A*A�A*E�A*A�A*JA)�^A)��A)p�A)hsA)dZA)XA)/A)A(�DA(5?A(A�A'�;A'�mA(=qA(ĜA(��A(��A(�+A(Q�A(�A'�A'�^A';dA&�`A&��A&^5A%�A%�;A%�wA%p�A%dZA%�A%�FA%ƨA%��A%�^A%��A%`BA%�A$�yA$��A$v�A$JA#|�A#\)A#�A#VA"ȴA"��A"�uA"�\A"��A"�\A!�mA!t�A!
=A ��A -A��AO�AoA5?A�AoA��AVA+Ax�A�A��AĜA�!AZA|�AO�A"�A��A  A  AI�A%A��A�yA�/Av�A��A�hA��A��AA�A��Al�A�A�FA�;AoA��A�DAt�A�A�
A�7A;dA
=A�/A��A��A
=A�AbAt�AA�A�jA��A�yA/Ax�A�7A��A��A��A��A`BAoA�jAv�An�A��A��A��A��A^5AM�AE�A-A�7A��A��A�AoA��AZA�^AĜA��A�+A�A�FA��A��A�7A�7A|�A33AĜAbNA1'AJA��A?}Av�AE�A{At�A�uA�A�-AC�A
�!A
$�A	�;A	�;A	A	|�A	hsA	O�A�At�Ap�A�A�`A�uA��A��A�DAffAȴA��A7LAC�A`BAK�A�`A�uA�A%AbA&�A�RA�A��A�A/AM�A�PAoA9XAv�A�FA�7Ap�AS�AO�Ax�A��An�A��Ar�AA�A��A&�A��A��AĜAXA ��A �A �@���@���@�@���@�&�@���@�=q@�M�@�@�K�@�ƨ@�p�@�Ĝ@��/@���@���@���@��D@�(�@��w@�^5@���@�G�@��P@���@���@���@�v�@�@�&�@�j@�1'@�+@�O�@�X@�X@�|�@�~�@���@�Z@�@�@�\)@�33@�K�@��@�+@��@�r�@܃@ͺ^@�/@�l�@���@�n�@�  @���@{33@t��@n�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�BB�B��B��B��BB��B�B�TB�B��B��B��B��BǮBĜBŢBǮBƨBɺBƨBĜBŢB��B��B��B��B��B��B�)B�;B�NB�TB�mB�B�B�B�B�B�B�NB�HB�mB�yB�fB�;B�)B�#B�B��B��BB�dB�RB��B��B�uB�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�JB�7B�1B�7B�1B�7B�\B��B��B��B��B�B�B�!BBɺB��B��B��B��B��B��B��B��B��B��BȴB��B�B��B��B��B��B��B�B�B�;B�NB�ZB�5B�B�B��BĜBŢBĜBÖBBBŢB��B��B��B��B��B��B�B��B��B��B��B��B�B�B��B��B��B��BɺBǮBƨBƨBŢBĜBÖB��B��BÖBŢBÖB��B��B��B��B��B��B�}B�wB�^B�LB�RB�?B�FB�qBǮBǮBŢBĜBB��B�qB�^B�?B�'B�B�B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�{B�{B�uB�hB�\B�\B�VB�VB�PB�+B�B|�Bv�Bq�Bk�BgmBcTB\)BT�BP�BO�BQ�BT�BZBffBv�Bs�Bq�Bm�BbNB_;B[#BJ�B5?B5?B;dBJ�BK�BJ�BJ�BC�B/B�B�BVB�B!�B,B2-B/B�BbBhB+BVBk�By�Bw�Bv�Bv�Bv�Bx�By�By�B{�Br�Bm�BjBiyBhsBjBn�Bt�By�B{�B|�B~�B� B�B�B� B}�B|�B�B�7B�=B�=B�DB�=B�7B�1B�%B~�Bp�Bl�Bk�BhsBcTBaHB[#BS�BR�BR�BR�BP�BQ�BS�BS�BT�BVBT�BS�BQ�BQ�BQ�BQ�BN�BJ�BH�BE�B?}B5?B.B-B,B(�B#�B!�B!�B �B�B�B�B{BhB{B�B�B�B�B�B�B�B&�B,B2-B6FB:^B;dB8RB5?B.B#�B�B�B�B�B7LB9XBC�BS�BhsBcTB]/BaHB\)B[#B\)B]/B]/BaHBhsBt�Bz�Bx�Bv�Bq�BiyBcTB\)BN�BA�B=qB;dB5?B1'B33B0!B2-B49B.B&�B+B1'B5?B:^B1'B0!B2-B>wBR�BXBZBYBW
BS�BQ�BN�BG�BC�BD�BD�BE�BC�BA�B?}B>wB9XB33B33B2-B,B%�B!�B�B�B�B�B�B�B!�B�B1BB�NB��BJ�BJB�NB��B�FB�-B�-B�9B�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�CB�B��B��B��BB��B�B�WB�B��B��B��BʾBǭBėBŢBǭBƪBɹBƧBęBšB��B��B��B��B��B��B�)B�<B�NB�SB�oB�B�B�B�B�B�B�NB�GB�lB�|B�eB�;B�&B�#B�B��B��BB�cB�RB��B��B�rB�OB�fB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�xB�JB�3B�.B�3B�0B�4B�YB��B��B��B��B��B�B� BBɹB��B��B��B��B��B��B��B��B��B��BȲB��B�B��B��B��BʿB��B�B�B�<B�PB�ZB�5B�B�B��BĘBŠBĞBÕBBBšBʿB��B��B��B��B��B�B��B��B��B��B��B�B�B��B��B��B��BɹBǮBƧBƧBŤBĚBÔB��B��BÔBŠB×B��B�B��B��B��B��B�|B�wB�^B�GB�QB�<B�FB�nBǭBǭBŠBĚBB��B�nB�_B�;B�%B�B�B��B��B��B��B��B��B��B��B�B� B��B��B��B��B��B��B��B�B�xB�wB�qB�dB�XB�\B�TB�TB�NB�+B�
B|�Bv�Bq�Bk�BgiBcUB\$BT�BP�BO�BQ�BT�BZBfdBv�Bs�Bq�Bm�BbMB_6B[BJ�B57B5;B;^BJ�BK�BJ�BJ�BC�B/B�B�BTB�B!�B,B2&B/B�B^B`B*�BVBk�By�Bw�Bv�Bv�Bv�Bx�By�By�B{�Br�Bm�Bj{BivBhqBj~Bn�Bt�By�B{�B|�B~�B�B�B�B�B}�B|�B�B�6B�:B�:B�BB�<B�4B�-B�&B~�Bp�Bl�Bk�BhoBcPBaCB[!BS�BR�BR�BR�BP�BQ�BS�BS�BT�BVBT�BS�BQ�BQ�BQ�BQ�BN�BJ�BH�BE�B?xB5:B.B-B,B(�B#�B!�B!�B �B�B�B�BvBbBuB}B|B}B�B�B�B�B&�B,B2)B6?B:YB;_B8NB5;B.B#�B�B�B�B�B7LB9UBC�BS�BhoBcOB]-BaDB\$B[B\&B]-B]+BaCBhnBt�Bz�Bx�Bv�Bq�BitBcRB\%BN�BA�B=lB;_B5<B1!B3-B0B2'B44B.B&�B*�B1#B5;B:YB1 B0B2)B>sBR�BX	BZBYBWBS�BQ�BN�BG�BC�BD�BD�BE�BC�BA�B?yB>uB9UB3.B3.B2(B,B%�B!�B�B�B�B�B�B�B!�B�B*BB�HB�BJ�BAB�HBʺB�@B�%B�&B�2B�&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Cp��Cp��Cp��Cp��Cp�;Cp�.Cpu�Cpm7Cpi8Cp^�Cp�Cpx�Cpn�Cp[�Cpu�Cp^�Cpm�Cpl�CppCpi�CpZ�Cph�CpX�Cpb�CpM<CpRRCpK�CpN�CpKqCp9�CpN�Cp6�Cp?�Cp0�Cp'Cp�Cp�CpACp(0Cp�Co�[Co��Co��Co�yCoĶCo��Co�Co�tCoCo4�Cn�?CnE�CmX{ClL�CjfCg�&Cc�VC_��C\�uC[��CZ��CZRCZ$�CY��CZ1CZsUCZǩC[Y�C[�nC\*GC\j�C\��C]*�C]�C]��C]��C]��C]�C^�C^\�C^hC^�]C^�cC^��C^�TC^��C^�ZC^�C_�C_N�C_RC_n C_�C_{C_NpC_�C_�C_Y+C_vC_�C^�/C^��C^�)C^�gC^�kC^j�C^�C]�pC]�PC]b�C\�C\��C\��C\ʑC]3C]w�C]��C]}�C]�C]��C^/&C^��C^�C^��C^ʴC^ӭC^�C^��C^�dC^�C^�&C^��C_*C_w[C_�oC` �C`E C`��C`�"Ca�Ca�CaF�Ca��Ca�wCb�CblCa��Ca�Ca�Ca\�CaH�CacC`�C`�C`�C`��C`�aCa�Ca$<C`րC`��CahCaUjCa٢Cb_Cb#JCbN�CbP�Cb.�Ca�4Ca��CaճCaw�Ca,�Cae�Ca��Ca�CCa�{Ca̟Cb�Cb<�Cb]:CbiCbzmCb��Cb��Cb��Cb|dCbj/Cbb Cb�Cb��Cb�ICb��Cb@Ca�6Ca��CaWCa$Ca"�Ca,�Ca�Ca	-CaCaXCaCaJ=CaZkCas�Ca��CauBCa��Ca�Ca��Ca��Ca�YCa�]Ca��Ca�Ca�LCaҷCbCb�tCcG�Cd7�Cd!CdCc�4Cc��Cc�gCc�Ccb�CcR8Cc]"CcA�CcG�Cc�Cc+�Cc(�Cc�Cc)|Cc<�CcaPCcm�CcW�CcO�Cc8�Cc4�CcpCb�jCb�UCb�]Cb�Cbo�Cba�Cb>:CbH:CbASCbv�CbX�Cbc�Cb7ZCb�Ca�KCa.�C`�C`byC`jC_�C_p�C_�C^��C^9pC^$�C^C]�C]ټC]��C]�C]�6C]�C\�C\L<C[��C[DCZ��CY��CX��CY/cCYu�CY��CY�TCX�CX;CW\�CV_�CU�1CU 3CS�CSo�CSCR�CRH�CQ�^CQ9,CQH�CQ��CQ��CQ�)CP�YCO�XCN�ECN;%CM�WCM�CMp5CM+�CL��CLzCJ�@CJv�CJ�(CJ�CK	�CK1�CKd�CK�CK��CK�GCK�DCK�;CKzjCKqTCKW�CK,�CK'^CK.CJ�FCJ��CJq(CJrXCJe�CJN#CJIACJR�CJ7�CJ:�CJ@CJ3CI�CI��CID�CI�CH�GCH;�CH�CG�CF�CF8�CE�rCEvCE,CD��CD�CD9XCC�CC��CCvCB�<CB�CA��CA7jC@�C@�sC@��C@��C@�uCA�C@�tC@�hC@f:C@]�C@9:C@:C?�DC?�hC?t�C>�qC>��C>�C>��C? �C>��C?=�C?@C?,�C?-3C?'C>�C>�C>�C>t�C>VsC>0�C>JyC>K�C>��C>��C>�OC?=QC?H�C>��C><�C>#�C=֮C=D5C= �C=;�C=n�C=Q�C=��C=�C=c C=Z�C=:C<�dC<�C<g�C<S�C<i�C<��C<ώC=4�C={�C>C>͙C?_IC?�[C?�zC@	�C@5�C@b+C@��C@��CACA�wCA�CAx�CA:MCA�CA��CB�CA��CAy�C@q�C@&�C@:�C@dQC@�}C@�ICA�CAW�CA��CB+�CBV@CBjCB��CB��CB�zCC[�CC��CC�HCD:�CD�0CD��CD��CEf�CE�`CF'�CFgyCF�CF��CF��CFԈCF��CGRmCH=�CIm�CI�DCN6�CU��Cd�4Co4Cv��C{�CC~��C~FC}�lCd6t33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333334                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Cy�XCy�Cy��Cy�MCy��Cy�Cy��Cy��CyϣCy��Cy�CyߨCyՁCy��CyܽCyēCy�,Cy�wCyֻCy�Cy��Cy�?Cy��Cy�Cy��Cy��Cy�Cy��Cy��Cy�OCy�5Cy�eCy�VCy��Cy��Cy|�Cy�=CyxdCy�CyhCySCyLJCyE$CyR,Cy$�Cy*Cx��CyACx��Cx�	Cx1Cw��Cv�7Cu��Cs�YCp�tCl��ChW�Ce�lCd�=Ccq�Cb�eCb�SCbw{Cb��Cb��CcU�Cc�TCd~�Cd�
Ce�CeV�CeЈCf-ICfk�Cf�\Cf~Cf� Cf��CgSCg:Cg=�CgbLCg�CgeZCg��Cg��Cg�dCgȽCh	�Ch\Ch*�Ch<�Ch8Ch	�Cg�-Cg�Ch�CgҙCgӼCg�3CgocCg�<Cg��Cgr�Cg;Cf��Cf{�CfN�Cf
�Ce�oCeS�Ces Cel�Ce�*Cf �CfQzCf&�Cfu	Cf�6Cf�Cg4�Cg]�Cgq?Cg��Cg�Cg�?Cg�xCg�`Cg��Cgo�Cg��Cg��Ch4)Ch�Ch�*Ci	�CiS�Cir�Ci�Ci�CjkCjU3Cj��Cj�CjٹCj� Cj~�CjRACj,�Cj�Ci�:Ci��CiyCi~LCi�KCie�Ci��Ci��Ci�	Ci��CiϴCj$�Cj�NCjڶCj��Ck'�Ck)�Ck�Cj��Cj��Cj�7CjH^Ci��Cj5�CjQ�Cj]�Cj��Cj��Cj�6CkZCk7CkCWCkUbCky�Ck{�Ck�LCkWmCkD�Ck<!Ck[=Ck}&Ck��Ck� Ck�Cj�eCjaTCj&�Ci�Ci�:Ci�oCi��CiղCi�Ci�Ci��CjNCj*CjD�CjR%CjFCjR�CjVrCjiCjn,CjeECj`CjsCjy�Cj��Cj�Cj�CCkb�Cl*Cm$,Cm�Cl��Cl��Cl�sClf�Cle7ClF�Cl5�Cl@�Cl$LCl*�Ck�8CliCl
bCk�ClIClwClELClRCl;BCl3BClCl�Ck��Ck�BCk�ZCk��Ckc�CkJuCk;�Ck�Ck!8CkCkQ�Ck2�Ck=�Ck�Cj�Cje7Ci��Ci~>Ci(yChƽChp/Ch-/Cg�"Cgc�Cf��Cf�\Cf��Cf��Cf�^Cf��Cf�Cf\CCe��CeQCd�SCd>�Cc��CcR�CbD$CaICa�SCa�5Cb�Cb�Car�C`�pC_ȁC^��C^HC]TAC\�C[��C[ZC[�CZ�jCY�mCYgiCYwcCY�+CZ�CY�CX�CW�CV��CVKoCU�eCU��CUx�CU1VCT��CS��CR��CRa�CR�RCRԓCR��CS#�CSX�CS�1CS�xCS��CS�{CS��CSo&CSe�CSKJCS7CS�CR��CR�MCRzuCR[�CR\�CRO�CR7"CR2CR;�CR�CR"�CQ�ZCR	CQ�0CQ}�CQ#2CP�CP�CP8CO�!COT3CN��CM�CM�6CM8VCL�CL��CLB�CK�wCK��CK3UCJ�|CJUMCI��CI9�CH�oCHy�CHT6CH.&CHD�CHx�CH�CHpDCH�CG�CG�LCG�OCG�CGc�CG@�CF�CFsCFO8CFkwCFn	CFx�CFtYCF��CF�fCF��CF��CF�wCFq�CF[aCF%�CE��CEǧCE�[CE�5CE��CE��CF/�CFs�CF��CF�pCFY�CE��CE��CEB�CD��CDd�CD��CD��CD��CD�CD��CD��CD�!CD�'CDR�CD{CC��CC��CC��CC��CD1yCD�zCD�WCE}nCFCuCF��CG�CGA�CG��CG��CG��CH?�CHKXCH�!CI& CI[�CI	"CH�oCH��CI�CI�HCI�^CI
4CG�CG�;CG�CG�CH"_CHAUCH�vCH�CI=mCIÑCI�}CJCJ8�CJS�CJ��CJ�"CKJCK}~CK�CLHCLO�CL��CM�CM��CM�CN)kCN[�CN_�CN|�CN��CN�\CO�CPCQM�CQ�/CVF�C^7[Cm��Cx�tC�.+C��#C���C�]yC��C��}G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114@�C@�N@�@�e@�V@�@�y@�@�@�&@�C@�M@��@�3@�o@��@��@�@�@�V@ׇ@��@�@��@�p@��@�q@�Q@�?@�z@�~@��@�V@�Z@� @�@��@�5@��@��@�V@��@�i@��@s�@j�@[�@_�@E�@�@׹@t�@�-@%@�@�i@`�@�@��@G1@��@0@@ @�N@�@F\@~p@ߒ@<{@jD@�@��@�@P/@x@�@��@�q@��@�5@��@��@�@(�@�@3�@1�@?�@W�@�?@�n@� @��@��@�@_y@`@�1@]�@^�@9�@T@)�@,�@ �@�@��@�Y@e�@:@��@��@�
@��@Y@H
@gX@L%@~@�V@� @��@@�@)s@/j@<Q@7�@93@6L@p@J�@h�@�C@��@��@%@T7@hG@�2@��@�D@�@-Y@`�@M�@;�@�@�4@�@��@��@�n@l7@o�@t�@_�@�@�{@��@x@��@�0@2@N�@c@�@�>@j�@C�@/�@/@��@�"@�@��@��@�@)v@P�@t@��@�@�@�n@��@��@�Z@�?@��@��@�@�y@��@v2@)�@ �@�B@�T@�w@� @��@�|@��@�=@�\@��@݃@�l@�"@�]@��@��@�@	@_@ @=@|@E@-�@\�@��@%p@�;@�@�z@�Q@r�@K�@K@7�@,�@3�@!x@%�@�b@�@�@?@w@a@6�@>�@0+@+@�@�@��@�*@�%@�:@�@�@��@u@{�@w@��@��@��@pp@N@V@��@o�@8�@�@®@��@b@ @��@�@�]@�f@�2@�1@�	@n?@
�@�=@��@�@�2@|�@�s@.�@n�@��@��@��@Iy@�q@8�@�z@C@��@צ@�@b�@='@��@ue@#w@-�@hm@��@z�@
�t@
4�@	�0@	&
@��@��@�@q@'�@�1@��@��@��@�s@W@!*@B�@hq@�M@r�@j�@_8@Q`@KU@:m@�@&@g@�E@��@��@��@��@��@�p@��@z�@|�@X�@w�@M�@ @�@�O@py@)@
�@��@E�@Ҡ@�u@WA@�@� @�@~a@T�@_@��@~F@@ ��@ ~\@ M�@ 5�@ �@ ,	@ M,@ b�@ G�@ �?��}?��C?���?��?�8M?�E?��:?��?��
?��2?��|?�:?��?�[�?�_:?�E�?�F$?�>
?�6?��?���?�P�?�(�?��7?��?�s?�bi?��?�?�[�?�j�?��8?�_?���?�~�?���?�bP?���?��M?�ͧ?�x?�?���?���?��G?�J�?��#?���?�{�?��U?��w?� �?��?��?�ɂ?���?���?��`?��?�k;?���?��@ (�@ 08@ \>@ �=@ ގ@ ��@ �G@ k7@ ��@a@ <@ �^?���?��7?���?���@  @ )�@ j�@ ��@ �'@!@=@JL@l"@}R@� @��@�@;�@i@�t@�p@�k@F�@��@�@�@�@w@'@:@Q�@��@*B@�m@3Q@	"�@7�@
�@}@$#@'^�@)�@)mh@)@(ہG�O�?C�]?7�k?=�d?8��?@?9J�?F�B?=�d??|�?7
=?<�?K��?A��?G�?C�]?A-w?;��?7
=?@?:�?I�'?:kQ?:�??|�?8��?A-w?B�?:�?OA�?E��?Lq?G_p?C�]?TS�?A-w?2��?A-w?@�I?9J�?=�d?4Ɇ?A-w?=�d?F�B?E��?A��?@?<�?>��?<�?=�d?B�?9J�?1��?)��?N<?K^>�z>?|�>�q=�G�=XD�=+<|PH<�3�<�9X<49X<�-�<�-�<XD�<|PH<�3�<|PH<|PH<XD�<49X<49X<�-�<XD�<49X<|PH<|PH<49X<XD�<|PH<|PH<�-�<49X<XD�<-�<49X<-�<-�<-�<49X<XD�<�-�;�D�<49X<49X<XD�<49X<XD�<|PH<|PH<|PH<XD�<49X<XD�<49X<�9X<|PH<49X<|PH<XD�<|PH<XD�<|PH<|PH<|PH<49X<49X<�-�<|PH<XD�<-�<|PH<|PH<|PH<|PH<|PH<XD�<�-�<49X<XD�;�D�<|PH<|PH<XD�<�-�<XD�<49X<49X;�D�<-�<49X<XD�<49X<49X<-�<|PH<�-�<XD�<XD�<49X<|PH<XD�<49X<49X<-�<49X;�D�<-�<-�<-�<49X;�D�<49X<49X;�D�<49X<-�;�D�<49X<-�<49X<XD�<49X<XD�<|PH<|PH<-�<49X<49X<|PH<|PH<49X<�-�<-�<|PH<XD�<XD�<49X<49X<XD�<XD�<XD�<-�<XD�<XD�<49X<XD�<|PH<|PH;�D�<XD�<49X<-�<-�<-�<XD�<-�<49X<XD�<49X<|PH<�-�;�D�<49X<49X<-�<XD�<XD�<-�<49X<-�<49X<-�<-�<49X<XD�<49X;�D�<49X<49X<49X<49X<49X<-�<-�<-�<49X<49X<XD�;�D�<-�<-�;�D�<-�<XD�;�D�<49X;�D�<|PH<|PH<49X<-�<-�<49X<49X<|PH<-�<XD�<XD�<|PH<XD�<|PH<|PH<XD�<49X<49X;�D�<-�<|PH<-�<49X<49X<|PH<49X<49X<49X<XD�<-�<XD�<|PH<XD�<49X<|PH<49X<49X<XD�<49X<XD�<|PH<49X<|PH<XD�<XD�<-�<|PH<XD�<49X<�-�<XD�<|PH<�-�<49X<XD�<�-�<XD�<XD�<|PH<XD�<|PH<XD�<XD�<|PH<|PH<XD�<XD�<�-�<XD�<|PH<�-�<XD�<|PH<|PH<|PH<XD�<49X<49X<49X;�D�<XD�<|PH<XD�<|PH<49X<|PH<49X<�-�<XD�<|PH<XD�<|PH<XD�<|PH<XD�<-�<�-�<XD�<|PH<XD�<XD�<XD�<�-�<49X<|PH<�3�<�-�<|PH<�-�<XD�<�3�<|PH<XD�<|PH<XD�<XD�<XD�<|PH<�-�<�-�<�3�<�-�<�-�<XD�<XD�<|PH<|PH<|PH<|PH<�9X<�-�<�-�<�-�<49X<|PH<XD�<�3�<�-�<�3�<�-�<�3�<|PH<�-�<�3�<�-�<|PH<�-�<|PH<�-�<�-�<|PH<�3�<�-�<�-�<|PH<XD�<|PH<�3�<�3�<|PH<�-�<�9X<|PH<�?<|PH<�-�<�-�<�-�<�3�<�-�<�?<|PH<�3�<�-�<|PH<�3�<�-�<XD�<|PH<XD�<|PH<�9X<|PH<�9X<|PH<�3�<�9X<XD�<|PH<|PH<�-�<�3�<�-�<�3�<|PH<�3�<�-�<�3�<|PH<�-�<�9X<�3�<�-�<|PH<�-�<�-�<XD�<49X<|PH<�-�<�-�<�3�<�3�<49X<|PH<�-�<|PH<�9X<�9X<�-�<�-�<XD�<�-�<|PH<�-�<�3�<�-�<�-�<�3�<�9X<�3�<�9X<�-�<�-�<�-�<�c�<�9X<�3�<�-�<�-�<XD�<�9X<49X<�-�<49X<�3�<XD�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ��ff                                            >�-w>�Ɇ>��>���>�<6>�z>��]>��>��>�9X>��#>��>���>��>�-w>�\�>���>�9X>�<6>�*�>��B>��k>�*�>��>���>�\�>�>�*�>�q>��>ɠ'>Ď�>�-w>т�>�\�>���>�\�>��d>�z>��>���>�\�>��>��]>��>���>�<6>��#>��>��#>��>�>�z>�'�>��>�}V>�zx>0�E=�9X=�h�=J��<�6z<49X;-�;�-�;�9X    ;XD�;XD�:�-�;-�;�-�;-�;-�:�-�        ;XD�:�-�    ;-�;-�    :�-�;-�;-�;XD�    :�-ຐ-�    ��-ຐ-ຐ-�    :�-�;XDл-�        :�-�    :�-�;-�;-�;-�:�-�    :�-�    ;�9X;-�    ;-�:�-�;-�:�-�;-�;-�;-�        ;XD�;-�:�-ຐ-�;-�;-�;-�;-�;-�:�-�;XD�    :�-�-�;-�;-�:�-�;XD�:�-�        �-ຐ-�    :�-�        ��-�;-�;XD�:�-�:�-�    ;-�:�-�        ��-�    �-ຐ-ຐ-ຐ-�    �-�        �-�    ��-�-�    ��-�    :�-�    :�-�;-�;-ຐ-�        ;-�;-�    ;XDк�-�;-�:�-�:�-�        :�-�:�-�:�-ຐ-�:�-�:�-�    :�-�;-�;-�-�:�-�    ��-ຐ-ຐ-�:�-ຐ-�    :�-�    ;-�;XDл-�        ��-�:�-�:�-ຐ-�    ��-�    ��-ຐ-�    :�-�    �-�                    ��-ຐ-ຐ-�        :�-�-ຐ-ຐ-�-ຐ-�:�-�-�    �-�;-�;-�    ��-ຐ-�        ;-ຐ-�:�-�:�-�;-�:�-�;-�;-�:�-�        �-ຐ-�;-ຐ-�        ;-�            :�-ຐ-�:�-�;-�:�-�    ;-�        :�-�    :�-�;-�    ;-�:�-�:�-ຐ-�;-�:�-�    ;XD�:�-�;-�;XD�    :�-�;XD�:�-�:�-�;-�:�-�;-�:�-�:�-�;-�;-�:�-�:�-�;XD�:�-�;-�;XD�:�-�;-�;-�;-�:�-�            �-�:�-�;-�:�-�;-�    ;-�    ;XD�:�-�;-�:�-�;-�:�-�;-�:�-ຐ-�;XD�:�-�;-�:�-�:�-�:�-�;XD�    ;-�;�-�;XD�;-�;XD�:�-�;�-�;-�:�-�;-�:�-�:�-�:�-�;-�;XD�;XD�;�-�;XD�;XD�:�-�:�-�;-�;-�;-�;-�;�9X;XD�;XD�;XD�    ;-�:�-�;�-�;XD�;�-�;XD�;�-�;-�;XD�;�-�;XD�;-�;XD�;-�;XD�;XD�;-�;�-�;XD�;XD�;-�:�-�;-�;�-�;�-�;-�;XD�;�9X;-�;�D�;-�;XD�;XD�;XD�;�-�;XD�;�D�;-�;�-�;XD�;-�;�-�;XD�:�-�;-�:�-�;-�;�9X;-�;�9X;-�;�-�;�9X:�-�;-�;-�;XD�;�-�;XD�;�-�;-�;�-�;XD�;�-�;-�;XD�;�9X;�-�;XD�;-�;XD�;XD�:�-�    ;-�;XD�;XD�;�-�;�-�    ;-�;XD�;-�;�9X;�9X;XD�;XD�:�-�;XD�;-�;XD�;�-�;XD�;XD�;�-�;�9X;�-�;�9X;XD�;XD�;XD�;a';�9X;�-�;XD�;XD�:�-�;�9X    ;XD�    ;�-�:�-�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222822222222222?A-w?4Ɇ?:�?5��?=<6?6z?C�]?:�?<�?49X?9�#?I�?>��?E�?A-w?>\�?8��?49X?=<6?8*�?F�B?7�k?8*�?<�?5��?>\�?@?8*�?Lq?B�?I�'?D��?A-w?Q��?>\�?/��?>\�?=�d?6z?:�?1��?>\�?:�?C�]?B�?>��?=<6?9�#?<�?9�#?:�?@?6z?/'�?&�?}V?zx>��E>49X> h�=���=+6z<�9X;�-�<-�<49X    ;�D�;�D�;-�;�-�<-�;�-�;�-�;-�        ;�D�;-�    ;�-�;�-�    ;-�;�-�;�-�;�D�    ;-�;-�    ;-�;-�;-�    ;-�;�D�;�-�        ;-�    ;-�;�-�;�-�;�-�;-�    ;-�    <49X;�-�    ;�-�;-�;�-�;-�;�-�;�-�;�-�        ;�D�;�-�;-�;-�;�-�;�-�;�-�;�-�;�-�;-�;�D�    ;-�;�-�;�-�;�-�;-�;�D�;-�        ;�-�;-�    ;-�        ;-�;�-�;�D�;-�;-�    ;�-�;-�        ;-�    ;�-�;-�;-�;-�    ;�-�        ;�-�    ;-�;�-�    ;-�    ;-�    ;-�;�-�;�-�;-�        ;�-�;�-�    ;�D�;-�;�-�;-�;-�        ;-�;-�;-�;-�;-�;-�    ;-�;�-�;�-�;�-�;-�    ;-�;-�;-�;-�;-�    ;-�    ;�-�;�D�;�-�        ;-�;-�;-�;-�    ;-�    ;-�;-�    ;-�    ;�-�                    ;-�;-�;-�        ;-�;�-�;-�;-�;�-�;-�;-�;�-�    ;�-�;�-�;�-�    ;-�;-�        ;�-�;-�;-�;-�;�-�;-�;�-�;�-�;-�        ;�-�;-�;�-�;-�        ;�-�            ;-�;-�;-�;�-�;-�    ;�-�        ;-�    ;-�;�-�    ;�-�;-�;-�;-�;�-�;-�    ;�D�;-�;�-�;�D�    ;-�;�D�;-�;-�;�-�;-�;�-�;-�;-�;�-�;�-�;-�;-�;�D�;-�;�-�;�D�;-�;�-�;�-�;�-�;-�            ;�-�;-�;�-�;-�;�-�    ;�-�    ;�D�;-�;�-�;-�;�-�;-�;�-�;-�;-�;�D�;-�;�-�;-�;-�;-�;�D�    ;�-�<-�;�D�;�-�;�D�;-�<-�;�-�;-�;�-�;-�;-�;-�;�-�;�D�;�D�<-�;�D�;�D�;-�;-�;�-�;�-�;�-�;�-�<49X;�D�;�D�;�D�    ;�-�;-�<-�;�D�<-�;�D�<-�;�-�;�D�<-�;�D�;�-�;�D�;�-�;�D�;�D�;�-�<-�;�D�;�D�;�-�;-�;�-�<-�<-�;�-�;�D�<49X;�-�<XD�;�-�;�D�;�D�;�D�<-�;�D�<XD�;�-�<-�;�D�;�-�<-�;�D�;-�;�-�;-�;�-�<49X;�-�<49X;�-�<-�<49X;-�;�-�;�-�;�D�<-�;�D�<-�;�-�<-�;�D�<-�;�-�;�D�<49X<-�;�D�;�-�;�D�;�D�;-�    ;�-�;�D�;�D�<-�<-�    ;�-�;�D�;�-�<49X<49X;�D�;�D�;-�;�D�;�-�;�D�<-�;�D�;�D�<-�<49X<-�<49X;�D�;�D�;�D�;�'<49X<-�;�D�;�D�;-�<49X    ;�D�    <-�;-�:���:��:�ݽ:��(:��:�G:�o�:�G!:���:��q:��$:�?:���:��:���:���:�I:���:�K�:�̬:��B:�vj:�G?:�]:��':���:�G?:�$:�G?:��;:���:��w:�':���:��.:��g:��:�̙:� j<l��:��j:�:�S:�Xm:�:�/�:�@�:�� :��	:���:��E:��:�W�:��j:��9:�(5:rW�:K	3:3�D:#�z:$��:k�:|z:�9��:I�:FY:[e:��9���:<�9��*:O�:�:W�:KE:��9���9���:E�9���:,"9��}9���:�|9�W:�=:z�9���:��9��j9��H9�&9��x9�#�:�9���9���9��9�ӯ9���9�-�:6�:	�::�_9�	�9�M9���9��i9��h;�GY9�v-9�V:m�9�L�9�؞9�g�:�9���9�b>9��s9�w�:�9�``9�xR:9�C�9�Z�:��9�X�:�9�O�:��9�z�9��9��9�E�9�v�: Ӗ9�y�9�f9�7S9�}�9��t9��^9���9��:k9���9�x9��r9�l9��9���9��9�h9�L9�x 9�{�9�~�9�L�9뎄9��9�c9��9��9��9��q9��9�4W9�u�9�q�9�Z�9�n9��69�}�9��M9��9��9��w9�y�9��9�e�9��99��9��9���9� ^9�b�9�vF9��O9��%9���9��b9��9�t�9�Cz9�*�9�?!9��b9���9�!�9�hJ9��U9�N�9���:j�9�S9�� 9��#:�9�s�:>�9���9��9��9�W�:�
9��:�-9�Me9�q9�yU9��9�׆9�f9���:��9��9���9�I:9�Hb9�/5:��9�(�9���9�:I9�7�9� 9�cS9�3�9��9�d9���9��<9�4|9�9�H�9�r9�D�:%oe9�9�e9�č9��9���9��F9��9褾9�]j9��[: �c9��9�@�9��9�#�9�}9��|9�0a9��9���9耴9�z�9�9�c9��9ٽ49�3h9��:��9��9�"�9� 9�9�a9㋙9��9�q�9�Z19���9�w79�$9�^A9��t9��9��49�9��Y:��9�Lr9�$9�ly9�Yj9�9�~9��9�!9�p9��S9�W9�Y�9��F9ނ�9ހP:��9�gB:�B9� �9��69��F9��w9�/9�"�9�9t: a�9�~�9�h]9��9��:6`,9�RQ9���:� :�B:;:��:��:
,D:�]:"�p:q�:�:�U:�: @H:W�: =<:��:K�:��:#��:�:E�: +D:o:� :~: "\:�:qA:NJ:�L:e:5q:��:�:�:�0:��:��:?�:�;:R :�:�:l�:;:	�Z:+�:��:l~:k:��:	ޯ:��:h�9�/9��c: (:  �:	�9���9�;�:�y:o:m�:��:I:��:��:	�:>:7/�:�G:��:�V:2�:"�:W�:��:�:~K:Db:h�:>7:$�@:�d:T�Q:!�:��:g:�O:΍:�F:Z�:��:�O:!
�:h$:	��:y:�z:
�J:	��:	�:�!:o�:��:��:,�:�p:�Q9�:>&:�	:�5:�:��9��f: �O:F;H5::b�9�Z59�*i9�9��w:b�9�X9�G9�q�:"(9�؈:$u: ߡ9��9�^�9�P:�E9��p:T�:D:�:Z@: ʤ:vx: :�n:�:��:&�:�BV:	T�:��:]e:�t:i�:@�:v:C�a9�6j9��9���:�;9�|9�N�9��S9�.�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ��ff                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?ɷ?ɷ?�e�?��?�e�?�e�?��?�e�?�e�?�?ɷ?�Z?�?�?�?�e�?�e�?ɷ?ͫ�?��?ɷ?�q?�?�?�e�?�e�?�q?�?�e�?�?ͫ�?�e�?�?�|�?�e�?�e�?�e�?��?�?�e�?ɷ?ɷ?�?��?�?�e�?�q?ɷ?��?�e�?�?�?��?�?�e�?�Z?ɷ?�Z?��"?Ք�?�C-?Ք�?�C-?��2?��2?�ں?ى7?�ں?�7�?�ں?�7�?�7�?�ں?�,=?�ں?�ں?�7�?Ք�?�7�?�ں?�C-?Ք�?Ք�?Ք�?��2?��2?Ք�?ى7?��?��2?��2?�7�?Ѡ'?�C-?��?�,=?Ք�?ى7?��2?��"?��?�C-?�7�?Ք�?�7�?��2?�7�?�ں?��2?�7�?�7�?��B?�,=?�}�?�,=?�ں?�ں?�}�?��B?��B?ى7?ى7?ى7?��B?�ں?�7�?�7�?�ں?�7�?��2?�7�?�ں?�ں?�}�?ى7?�,=?ى7?ى7?��2?Ք�?��2?�,=?�C-?�7�?�ں?��?��2?ى7?Ѡ'?�C-?��2?Ѡ'?Ք�?�C-?�C-?�,=?Ք�?Ք�?��?�ں?�7�?Ք�?Ք�?ى7?Ք�?Ѡ'?��?�C-?ͫ�?�C-?�C-?Ѡ'?�C-?��2?Ք�?Ѡ'?��2?�C-?��?��2?ى7?Ք�?�7�?Ք�?��?Ք�?Ք�?Ѡ'?Ք�?��?Ѡ'?Ք�?�N�?Ք�?��2?Ք�?��2?�7�?�C-?�ں?Ѡ'?��2?�7�?�ں?��2?�,=?��2?Ք�?ى7?�7�?�7�?ى7?�C-?Ք�?�7�?Ք�?�,=?��2?ى7?��2?ى7?ى7?ى7?�ں?�7�?Ք�?Ѡ'?Ք�?Ք�?ى7?Ք�?��2?�7�?Ք�?�7�?�7�?��2?�C-?Ք�?Ք�?ى7?�ں?�7�?��2?�}�?��2?Ք�?�ں?�}�?�,=?��2?ى7?�7�?�}�?�ں?�7�?�}�?ى7?ى7?ى7?�ں?ى7?�7�?�}�?��B?�rG?�,=?��B?�rG?�,=?���?� �?��B?� �?�M?�	�?���?� �?�	�?� �?�M?���?�}�?���?�	�?�M?�rG?�f�?�	�?�R?��?�M?�[W?�M?�f�?�	�?��?�M?�[W?�b?�b?�O�?��?�O�?��?�b?�b?�b?�O�?���?��?���?�b?�Dg?�b?�b?�Dg?�Dg?��m?�O�?��?��?��?���?�8�?���?�Dg?��?��?��r?��r?��m?���?�8�?��m?�Dg?��r?�8�?�8�?�8�?��?�8�?��m?�8�?�8�?��}?��?�8�?�-w?�-w?���?�-w?�8�?�-w?��r?�8�?���?���?��}?���?��?��}?���?�!�?�!�?�!�?�s�?�!�?��}?�~�?�!�@ �D@ �D?��?�s�?�s�?�s�@4@��?��?�s�@��?�s�?��@��@4@�
@�@��@��@�@��@.I@.I@4@ �D@��@��@4@��@.I@.I@��@�
@.I@.I@��@��@.I@�
@.I@��@��@�
@.I@��@.I@(�@(�@�N@(�@��@.I@�
@��@(�@.I@��@ �D@.I@��@��@.I@.I@��@�@(�@�
@�
@�@�
@�@(�@�@(�@�
@(�@.I@�
@�
@(�@z@.I@�
@�
@�@�
@(�@�
@�
@(�@�N@�N@.I@.I@�@�N@��@.I@�@�
@(�@�@�@.I@�N@�@(�@�N@�@z@z@z@.I@��@(�@(�@�
@�c@�@�@�
@��?��@.I@4@.I?��@�?��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ��ff                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�SG�O�@̒G�O�@���G�O�G�O�@�s�G�O�@�ѕG�O�G�O�@М�G�O�@��G�O�G�O�@�C�G�O�@��G�O�G�O�@�XEG�O�@�s�G�O�G�O�@���G�O�@�L]G�O�G�O�@�G�G�O�@�<�G�O�G�O�@Έ�G�O�@���G�O�G�O�@��qG�O�@�DPG�O�G�O�@ӂ�G�O�G�O�G�O�@�#w@�'G�O�G�O�G�O�G�O�A,�7G�O�G�O�G�O�G�O�A;�ZG�O�G�O�G�O�G�O�AD��G�O�G�O�G�O�G�O�AE;�G�O�G�O�G�O�AF�BAF��G�O�G�O�G�O�G�O�AE�G�O�G�O�G�O�G�O�AD��G�O�G�O�G�O�AD�AD�PG�O�G�O�G�O�G�O�AH�VG�O�G�O�G�O�ALl�AMIXG�O�G�O�G�O�G�O�AQ0$G�O�G�O�G�O�G�O�AYS�G�O�G�O�G�O�G�O�AW*9G�O�G�O�G�O�G�O�AW��G�O�G�O�G�O�G�O�AX�/G�O�G�O�G�O�G�O�A[?�G�O�G�O�G�O�G�O�AY,�G�O�G�O�G�O�G�O�AW�G�O�G�O�G�O�AWǮG�O�G�O�G�O�G�O�G�O�AV�G�O�G�O�G�O�G�O�AV� G�O�G�O�G�O�G�O�AXn~G�O�G�O�G�O�G�O�AV-�G�O�G�O�G�O�AY�	AZ_G�O�G�O�G�O�G�O�AZ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AY�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A]�	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aa��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ae��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AwRsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�!�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ޚA��A��kA��+A���A�+IA�;�A�uHA���A��TA�L  3 3 3  3 3  3 3  3 3  3 3  3 3  3 3  3 3  3 3  3   33    3    3    3    3   33    3    3   33    3   33    3    3    3    3    3    3    3    3   3     3    3    3    3   33    3         3         3                        3                        3                        3                        3                       3                        33                        3                        3                       3                         3                        3              33333333333G�O�G�O�����G�O�����G�O�?   G�O�G�O�����G�O�?333G�O�G�O��fffG�O�>L��G�O�G�O��   G�O�=���G�O�G�O��333G�O�?L��G�O�G�O�����G�O�    G�O�G�O��L��G�O�    G�O�G�O��L��G�O�?��G�O�G�O��333G�O�?��G�O�G�O��333G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�����G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��   G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�����G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�����G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                            G�O�G�O�@�\LG�O�@��_G�O�@�9G�O�G�O�@�|�G�O�@���G�O�G�O�@���G�O�@��eG�O�G�O�@�MG�O�@� G�O�G�O�@�a�G�O�@�}<G�O�G�O�@�G�O�@�U�G�O�G�O�@�P�G�O�@�FG�O�G�O�@��(G�O�@���G�O�G�O�@��G�O�@�M�G�O�G�O�@���G�O�G�O�G�O�@�,�@��oG�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�A#��G�O�G�O�G�O�G�O�A,�|G�O�G�O�G�O�G�O�A-@[G�O�G�O�G�O�A.��A.�SG�O�G�O�G�O�G�O�A-�KG�O�G�O�G�O�G�O�A,�fG�O�G�O�G�O�A,��A,��G�O�G�O�G�O�G�O�A0��G�O�G�O�G�O�A4qrA5M�G�O�G�O�G�O�G�O�A94�G�O�G�O�G�O�G�O�AAX/G�O�G�O�G�O�G�O�A?.�G�O�G�O�G�O�G�O�A?�fG�O�G�O�G�O�G�O�A@��G�O�G�O�G�O�G�O�ACDDG�O�G�O�G�O�G�O�AA18G�O�G�O�G�O�G�O�A?�&G�O�G�O�G�O�A?�RG�O�G�O�G�O�G�O�G�O�A>��G�O�G�O�G�O�G�O�A>ΤG�O�G�O�G�O�G�O�A@s!G�O�G�O�G�O�G�O�A>2$G�O�G�O�G�O�AAܭABc�G�O�G�O�G�O�G�O�AB�cG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AE��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AI�lG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AM�]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A_WG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AtHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ay�WG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A}�eA~c�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�w�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�CG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��TA���A��}A���A�-�A�>"A�w�A��"A���A��  1 1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  1   11    1    1    1    1   11    1    1   11    1   11    1    1    1    1    1    1    1    1   1     1    1    1    1   11    1         1         1                        1                        1                        1                        1                       1                        11                        1                        1                       1                         1                        1              11111111111G�O�G�O�?&^�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�?&^�G�O�G�O�?&^�G�O�G�O�G�O�?&^�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�?&^�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�?&^�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�?&^�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�?&^�?&^�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&^�?&^�?&^�?&^�?&^�?&^�?&^�?&^�?&^�?&^�?&^�