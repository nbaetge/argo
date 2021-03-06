CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB          	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       f2020-12-03T07:48:07Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
     � r�Argo synthetic profile          1.0 1.2 19500101000000  20201203074807  20201203074807  5904479 UW, SOCCOM, Argo equivalent                                     STEPHEN RISER , KENNETH JOHNSON                                 PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                            yA   AO  DDDDARRDNAVIS_A                         0276                            110713                          863 @ׁ�QY��1   @ׁ���R�@It�/���0������1   GPS        yPRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          CDOM                                                            NITRATE                                                         PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         DOXY_ADJUSTED=DOXY*G                                                                                                                                                                                                                                            CHLA_ADJUSTED=CHLA/A, NPQ corrected (Xing et al., 2012), spike profile added back in                                                                                                                                                                            not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  NITRATE_ADJUSTED=[NITRATE-SUM(OFFSET(S)+DRIFT(S))]/GAIN                                                                                                                                                                                                         dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            G=1.0391                                                                                                                                                                                                                                                        A=2                                                                                                                                                                                                                                                             not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  OFFSET(S) and DRIFT(S) from climatology comparisons at 1000m or 1500m. GAIN from surface/deep comparison where surface values are known                                                                                                                         Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        G determined by surface measurement comparison to World Ocean Atlas 2009.See Takeshita et al.2013,doi:10.1002/jgrc.20399                                                                                                                                        A is best estimate from Roesler et al., 2017, doi: 10.1002/lom3.10185                                                                                                                                                                                           not applicable                                                                                                                                                                                                                                                  not applicable                                                                                                                                                                                                                                                  Contact Tanya Maurer (tmaurer@mbari.org) or Josh Plant (jplant@mbari.org) for more information                                                                                                                                                                  2017062616081320170626160813201706261608132020120212402720201202124027202012021240272020120212402720201202124027A   A   A   B   A   F   F   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�3D�y�D�ٚD�i�D�  D�s3D��fD�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s�
@��@��A��A<��A\��A|��A�z�A�z�A��A�z�A�z�A�z�A�z�A�G�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B���BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Dz=D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D �=D!s�D!��D"s�D"��D#s�D#��D$z=D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+�qD,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^mqD^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt�=Dy�
D�s�D�ӆD�c�D���D�mD��RD�s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AM?}AM7LAM?}AM7LAM7LAM?}AM?}AM?}AMC�AM?}AMG�AMC�AM?}AM;dAM;dAM?}AMG�AMC�AMC�AM?}AM?}AMC�AMC�AMC�AMC�AMG�AMK�AMK�AMG�AMG�AMG�AMG�AMG�AMK�AMK�AMK�AMK�AMK�AMK�AMO�AMK�AMG�AMG�AMG�AMG�AMG�AMG�AMC�AM?}AM;dAM;dAM?}AM?}AM?}AMC�AM;dAM/AM/AM�AL��AL�+AK�AJ��AIp�AFz�AB�9A@bNA>�yA>  A=��A=�7A<�A<Q�A;��A;`BA:��A:ffA: �A9�mA9�7A9`BA8�/A8ZA89XA8{A8Q�A8�A8^5A85?A8$�A8�A7�A7�;A7��A7C�A7/A6�\A6�A5�A65?A6=qA69XA5�wA57LA4ȴA4��A4��A4I�A4�A4M�A4-A3�^A3�A3p�A3�A3�FA3�7A2�yA3&�A3\)A3XA3��A3�mA3A3O�A3
=A2��A2n�A2r�A2��A3�hA3��A3�A3�mA3�FA3��A3�PA3x�A3�A3�;A4M�A3�wA3�-A3��A3|�A3dZA3`BA37LA2��A2��A2�9A2�HA3&�A3�A3�
A3�hA3dZA3&�A2��A2r�A2A1A17LA0�A0��A0�A0~�A0�A0n�A0n�A0bNA0Q�A01'A0bA/�A/�#A/A/�A/��A/��A/�^A/��A/�
A/�#A/�#A/�;A/��A/7LA/;dA.��A.�A.�/A.�yA.��A/�A.�A.��A.~�A.I�A.-A.5?A.1'A.$�A. �A.�A.A-�TA-�
A-�
A-A-��A-�A-�7A-l�A-\)A-l�A-O�A-+A-�A-�A-�A-"�A,��A,�HA,ȴA,��A,ZA, �A+��A+�A+hsA+`BA+S�A+S�A+XA+S�A+K�A+O�A+XA+G�A+;dA+7LA+�A+&�A+"�A+oA*��A*��A*��A*r�A*I�A*jA*v�A*bNA*Q�A*I�A*{A)��A)��A)t�A)x�A)`BA)A)+A)+A)A)VA(��A(��A(�A(bNA(=qA( �A'��A'��A'�A'x�A'O�A&�!A&z�A&ffA&9XA%��A%��A%l�A%`BA%S�A%?}A%/A%"�A%�A%/A%7LA%oA%VA%C�A%p�A%��A%x�A%/A$�`A$ȴA$��A$z�A$bNA$r�A$�A$M�A#�A#��A#t�A#/A"��A"�\A"=qA"�A"A"A!�A!�#A!A!��A!��A!��A!��A!��A!��A!|�A!dZA!p�A"=qA"��A!�hA ��Ap�A��A�jA�yA�Ar�AI�A9XAA��A�7A�AĜA�`A�9A�`A��A�A;dA?}A
=AoA�A�A�A�AoA%A��A5?AAAA�TA�wAx�A��A��AE�AK�A
=A��A�A��A\)A�yA�+A=qA{A�-A��A��AZA��A`BA|�A�`A�`AG�A��A�A��A��A��A�hA�7A�A�A�Ax�Ap�AVAM�A|�A"�A$�AA~�A�A  A�A-AM�A9XA��A�hA&�A
=A�/A�RA��A�AM�AjAA\)A��A`BAA��A�mAA��AAO�A�+A�A�^A�-A��AK�A��A��AĜAĜA��A��AVAQ�A�A��A�jA��A�\A��A�\A&�AdZA
=A��A�DA1'A�HA�!A��A�^A�A�PA
=A~�AbA"�A
9XA	��A	�A	�A	�7A	"�A	
=A�A��A��A�`A��A�!A�@٩�@�-@�@�33@�|�@��-@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       AM?}AM7LAM?}AM7LAM7LAM?}AM?}AM?}AMC�AM?}AMG�AMC�AM?}AM;dAM;dAM?}AMG�AMC�AMC�AM?}AM?}AMC�AMC�AMC�AMC�AMG�AMK�AMK�AMG�AMG�AMG�AMG�AMG�AMK�AMK�AMK�AMK�AMK�AMK�AMO�AMK�AMG�AMG�AMG�AMG�AMG�AMG�AMC�AM?}AM;dAM;dAM?}AM?}AM?}AMC�AM;dAM/AM/AM�AL��AL�+AK�AJ��AIp�AFz�AB�9A@bNA>�yA>  A=��A=�7A<�A<Q�A;��A;`BA:��A:ffA: �A9�mA9�7A9`BA8�/A8ZA89XA8{A8Q�A8�A8^5A85?A8$�A8�A7�A7�;A7��A7C�A7/A6�\A6�A5�A65?A6=qA69XA5�wA57LA4ȴA4��A4��A4I�A4�A4M�A4-A3�^A3�A3p�A3�A3�FA3�7A2�yA3&�A3\)A3XA3��A3�mA3A3O�A3
=A2��A2n�A2r�A2��A3�hA3��A3�A3�mA3�FA3��A3�PA3x�A3�A3�;A4M�A3�wA3�-A3��A3|�A3dZA3`BA37LA2��A2��A2�9A2�HA3&�A3�A3�
A3�hA3dZA3&�A2��A2r�A2A1A17LA0�A0��A0�A0~�A0�A0n�A0n�A0bNA0Q�A01'A0bA/�A/�#A/A/�A/��A/��A/�^A/��A/�
A/�#A/�#A/�;A/��A/7LA/;dA.��A.�A.�/A.�yA.��A/�A.�A.��A.~�A.I�A.-A.5?A.1'A.$�A. �A.�A.A-�TA-�
A-�
A-A-��A-�A-�7A-l�A-\)A-l�A-O�A-+A-�A-�A-�A-"�A,��A,�HA,ȴA,��A,ZA, �A+��A+�A+hsA+`BA+S�A+S�A+XA+S�A+K�A+O�A+XA+G�A+;dA+7LA+�A+&�A+"�A+oA*��A*��A*��A*r�A*I�A*jA*v�A*bNA*Q�A*I�A*{A)��A)��A)t�A)x�A)`BA)A)+A)+A)A)VA(��A(��A(�A(bNA(=qA( �A'��A'��A'�A'x�A'O�A&�!A&z�A&ffA&9XA%��A%��A%l�A%`BA%S�A%?}A%/A%"�A%�A%/A%7LA%oA%VA%C�A%p�A%��A%x�A%/A$�`A$ȴA$��A$z�A$bNA$r�A$�A$M�A#�A#��A#t�A#/A"��A"�\A"=qA"�A"A"A!�A!�#A!A!��A!��A!��A!��A!��A!��A!|�A!dZA!p�A"=qA"��A!�hA ��Ap�A��A�jA�yA�Ar�AI�A9XAA��A�7A�AĜA�`A�9A�`A��A�A;dA?}A
=AoA�A�A�A�AoA%A��A5?AAAA�TA�wAx�A��A��AE�AK�A
=A��A�A��A\)A�yA�+A=qA{A�-A��A��AZA��A`BA|�A�`A�`AG�A��A�A��A��A��A�hA�7A�A�A�Ax�Ap�AVAM�A|�A"�A$�AA~�A�A  A�A-AM�A9XA��A�hA&�A
=A�/A�RA��A�AM�AjAA\)A��A`BAA��A�mAA��AAO�A�+A�A�^A�-A��AK�A��A��AĜAĜA��A��AVAQ�A�A��A�jA��A�\A��A�\A&�AdZA
=A��A�DA1'A�HA�!A��A�^A�A�PA
=A~�AbA"�A
9XA	��A	�A	�A	�7A	"�A	
=A�A��A��A�`A��A�!A�@٩�@�-@�@�33@�|�@��-@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBȴBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺBɺB��B��B��B��B��B��B��B�5B�NB�TB�ZB�`B�fB�sB�mB�yB�yB�B�B�B�B�yB�mB�ZB�NB�NB�TB�yB�B�B�B�B�B�B�B�B�B�B�B�mB�sB�B�B�B�B�B�fB�fB�B�mB�`B�yB�yB�fB�`B�`B�mB�B�B�mB�B�B�B��B��B��B��B��B�B�B�B��B+BDBVBbB\B\B\B\BuB�B"�B�B�B�B�B�B�B�BuBhBoB{B�B"�B%�B"�B �B�B�B�B\BPB+BBBBBBBBBBBB  B  B  B��B  BB%B+B1B1B	7B1B%BBBBBBBBBBBB  BBBBBBB  B��B  B  B  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�yB�mB�fB�mB�fB�TB�fB�sB�fB�mB�`B�TB�NB�HB�;B�5B�#B�B�B�
B�B��B��B��BɺBǮBÖB��B��B��B�}B�wB�qB�qB�wB�}B�wB�wBBĜBȴBƨBĜBBB��B��B��BÖBĜBÖB�}B�qB�dB�XB�?B�-B�B�B�B�!B�B�B�!B�'B�3B�?B�?B�?B�?B�?B�?B�LBĜB��B��B�9B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B�XB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴBƨBB�^B�FB�!B��B�Bt�Bq�Bl�BjBhsBhsBl�Bn�Bz�B�%B�oB��B�-B�B�5B�NB�`B�fB�mB�mB�sB�sB�sB�yB�yB�yB�sB�`B�5B�B��B��BȴB�qB�XB�RB�RB�}BBÖB��B�}B�qB�qB�qB�qB�}BĜBĜB��BɺBB�jB��B�
B�)B�/B�BB�fB�ZB�;B�B�B��B��B�B�
B�B�B�
B�B�;B�NB�HB�fB�B�B�B�B�B��B��BB1B%BBB��B�B�B�BB+BBB��B��B�B�B�yB�B�B�B�B�B�B�B�B�B�B�B��B�B�7B+B�TB��B�jB�'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�NB�jB�rB�{B�}B�B�B�B�B�B��B�B�B�B�B�B�xB�iB�hB�pB�B�B��B��B��B��B��B��B��B�B��B�B�B�B�B��B��B��B�B�B�B�B�B�}B�B�B�B�|B�}B�B�B�B�B�B�B��B��B�B�B��B��B��B�B��B�BJBbBtB�BxByByBxB�B�B"�B�B�B�B�B�B�B�B�B�B�B�B�B"�B& B"�B �B�B�B�ByBpBJB<B9B1B(B2B2B2B2B1B+B"B B B B�B B"BCBHBNBNB	VBPBDB1B>B0B#B#B*B0B=B1B)B"B B#B$B#B#B#B#B B�B B B B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�B�B�B�B�B�B�B�qB�B�B�B�B�|B�oB�jB�fB�XB�RB�@B�2B�,B�&B�#B��B��B��B��B��BôB��B��B��B��B��B��B��B��B��B��B��B¬BķB��B��BĻBªB©B��B��B��BòBķBóB��B��B�B�uB�YB�HB�7B�/B�5B�<B�5B�6B�@B�@B�NB�[B�[B�[B�[B�ZB�YB�jBĶB��B��B�UB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�pB��B��B��B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B¬B�zB�dB�>B��B�=Bt�Bq�Bl�Bj�Bh�Bh�Bl�Bn�Bz�B�@B��B��B�HB� B�OB�lB�}B�B�B�B�B�B�B�B�B�B�B�|B�PB�#B�B��B��B��B�qB�mB�lB��B¬BóB��B��B��B��B��B��B��BķBĵB��B��BªB��B��B�'B�BB�JB�]B�B�tB�YB�3B�"B�B�B� B�'B�!B� B�)B�.B�YB�jB�fB�B�B��B��B��B��B��B��B%BPBDB0B(B�B��B��B��B*BHB7B"B�B��B��B�B�B�B��B�B��B��B�B��B�B��B��B��B��B�B�NB+B�kB��B��B�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
Ct��Ct��Ct�mCt�,Ct��Ct��Ct�*Ct��Ct��Ct��Ct�?Ct�CtHtCtGCtP-CtxCt�rCtkiCt$�Ct"�Ct#Ct2�Ct%�Ct'�Ct#�Ct0�Ct�Ct/VCt�Ct.�Ct+Ct0�Ct#�Ct$ Ct&`Ct,�CtXCt.�Ct�Cs�0Cs��Cs�ECs�;Cs��Cs��Csu�CsjTCsU7CsCszCs	Cs�CrԨCr��CrO3Cq��Cqg Cp��CoӘCn��Cm�Cj��Cf�Cb�iC_�}C^�C]q�C]�4C]�fC^�C^IC^�C_1�C_�?C_�C_�<C`$C`:�C_�C_�C_3�C_�C_6�C_{NC`)�C`��C`�BC`�`C`��C`�^Ca
CaC`�HC`іC`��C`k�C`��C`۝Cai;CazCaj�Ca7�C`��C`�
C`��C`�C`�0C`�HC`��Ca�C`��C`�LCaCaE&Ca_4Ca�9Caw�Ca�Cb4-Cb^\Cb�Cb��Cb��Cb�~Cb��Cb��Cb�$CcC�Cc�8CdXCdp�Cd�.Cd��Cd�OCd��Cd��CeCeH�Ce��CeܮCe��Ce�?Ce�RCe��Ce��Ce�cCe�[Ce�NCe��Ce��Cf2Cf>Cf��Cf�|CfE�Cf:^Cf�Ce�5CeĐCe�7Ce�"CehiCe�Ce�FCe�Ce��Ce��Ce�sCe��Ce��Ce�vCe�Ce�Ce�Ce�Ce�*Ce��Ce�Cf+CfF~Cf`|CfqlCfk�Cf>�Cf5FCe�Ce��Ce��Ce��Ce��Ce�Ce�{CfzCf.�Cf�Ce�Ce��Ce�HCf 'Cf* CfGRCfl�Cff�Cf~#Cf��Cf��Cf�|Cf��Cf��Cf�?Cg�Cg/�Cg?Cg&.Cg7�Cg)�Cg@�Cg4�Cg*-CgDCgMCg#9CgZ�Cg�bCgӐCh�Chb�Cho�Chr�Ch��Ch��Ch��Ch�`Ci5�Ci^�Ci�qCi�xCi�YCi�mCj�Cj5�CjDHCj,Cj�Cj�qCj��Cj KCi��Ci��Ci�ZCi�,Ci�wCi�Ci�Ci�Ci#Ch��Ch$1Ch�Ch1Cg�TCg�Cg��Cg�Cg�FCh5yCh@�ChD	Ch�Cg��Cg��Cg�MCg:2Cg#�CgcCf��Cf*bCePCduHCd0�Cc�KCc`.Cb��Ca�&Cb�Ca��CaCa�Ca�Ca��Ca��Cb3Cb8sCb8�CbɳCc��Cd}Cd��Cd�nCd��Cd��Cdu�Cd^xCdK�Cdc�CdW�Cc�)Cct�Cb�kCbyWCb�CaK"C`��C_��C^�C^7C]�>C^�C^;C^T�C^�[C^�
C_;�C_U�C_:�C^�SC^k�C]�`C\?�CZ9�CWb#CVRaCU�CU�CTX�CSπCS��CS��CS>%CR�>CR�yCR5�CQ��CQ�CO�TCO7�CO�CN��CO�CP7vCR+�CST�CSOWCR�CRIaCRB4CR'qCRCR-CQ�<CQ��CQ��CQ7�CP��CP��CPk*CP�COY�CNe�CL�CJ��CH��CG��CG;�CF�qCFk&CFLCF@tCFECE՝CE�zCEL�CD��CC��CCG�CB��CB<�CA��C@��C@0,C?K:C>4�C=��C=�=C=H�C=�C=DC<��C<ƣC<��C<��C<|�C<`C<$�C;�ZC;�IC;ǵC;�NC;�"C;��C;��C<3�C<*qC<tC;�C;Q�C:�0C:�C:!>C9��C9�0C9h<C9CC99�C9~�C9p�C9W�C8�qC8��C8jC8&KC7k�C7AC6��C6�sC6c]C6=]C67�C6F�C6^C6��C6��C6�[C6�aC6�PC6ܡC6��C6�C6ΝC6ŮC6�hC6�yC7�C6��C6�kC6�4C6�{C6�C6�MC6��C6�C6|�C6�.C6��C6�HC7$�C7��C7�oC7y�C7"C7"C7N=C7��C7�C8//C8��C9�C94QC9(AC9�C9q"C9�C9��C9��C9�xC9��C9� C:G�O�C?�/COۧC]S�Cj��Cu��CzS�C}.jC~v$33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 33333334                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   G�O�                                C~S�C~`cC~C�C~4�C~R�C~S�C~P�C~:�C~;�C~;�C~/�C~$-C}աC}�"C}ݧC~(C~)C}��C}��C}��C}��C}�'C}��C}��C}�GC}��C}�CC}��C}��C}��C}��C}��C}�SC}��C}�8C}��C}�!C}�C}�XC}h?C}H�C}8�C})�C})�C}�C|��C|��C|��C|��C|�C|�EC|��C|SLC|C{ȞC{q�Cz�WCz*�Cy4*Cx�CvM�Cs��Co�,Ck�iCh�JCf�,CfjCf,tCf��Cf�Cf�CgM,Cg�ChNCh�JCh�Ch�Ch�JCh�JChb�Cg��Cg�rCg�Ch8DCh��Ci��Ci�~Ci�Ci�)Ci��Ci֔Ci�Ci��Ci��Ci~"Ci25CiH0Ci�ZCj9�CjJ�Cj;BCjCi�|CiV�Cif&Ci��Ci�Ci�&CiƊCiچCi��Ci�Ci�NCjCj/CjT�CjH�Cj�CkcCk88Ckj�CkէCk��Ck�JCkz.Ckh�Ck��Cl&�Cl֭CmE�Cm_fCm�3Cm�~Cm��Cm�kCm��Cn+Cn@Cn�aCnقCn�Cn�VCn�nCn�Cn��Cn�Cn��Cn~�Cn�Cn�Co2/Co>�Co�8Co�NCoF�Co:�Co�Cn�Cn�rCn��Cn�Cn`�Cnx�Cn�wCn�Cn��Cn�\Cn�,Cn٣Cn�Cn�Cn��Cn��Cn�-Cn�Cn�CnӉCn�Co�CoGtCobwCotCon5Co?7Co5�Cn�Cn�ZCn��Cn�LCn��Cn�"Cn�%Co �Co.�CoTCn�uCn�|Cn�Cn�]Co)�CoHQCoo!Coh�Co�GCo�`Co��Co��Co�@Co��Co�kCp�Cp9�Cp!jCp/�CpB Cp3�CpKZCp>�Cp4Cp&�Cp'�Cp,�Cpf�Cp�Cp�Cq1Cqx�Cq�CCq�dCq��Cq�Cr(CrPCrTXCr~�Cr�JCs]Cs�Cs`CsG9Cs^Csm2Cs�dCs�uCs�Cs�@CsG�Cr��Cr�gCr�ECr�<Cr�Cr�8Cr�}Cr��Cr)�Cq��Cq7�Cq%�Cq�Cp��Cp�NCp�mCp��Cp�/CqI�CqU�CqX�Cq,Cp�&Cp��Cp��CpD�Cp-7Cp	�Co��Co*?Cn�CmdCm�Cl�RClDCk�Cj�Cj�VCj��Cj�ZCjojCjP�Cj_�Cj�mCkJCk�Ck@Ck��Cl�Cml#Cm�iCm��Cm�Cm��Cmd�CmLZCm9!CmQ�CmECl�3ClY�Ck�?CkTACj�QCj;CifEChdVCgq�Cf�KCf��Cf�3Cf�Cg
CgR�Cg�$Cg�pCheCg�pCg�]CgCf;.Cd�0Cb��C_�
C^��C^D�C]s�C\�:C\�C\�C\>C[��C[>3CZ�
CZm�CY�<CY.CX*CWRCW8CW�CW��CX[�CZc�C[�5C[�wCZ�rCZ�DCZz�CZ_ CZN�CZI�CZ pCZ�CY�'CYe�CY	�CX�^CX�ZCX3{CWu_CVw�CT��CR�.CP�lCO�CO+CN�2CN-=CN�CN �CM�eCM��CMe	CM�CLH�CKZ�CJ�zCJ��CI��CI1�CH8dCG��CF�CE��CE\CD�kCD��CD��CDh?CDO�CD(5CD�CC�iCC۩CC��CC�CC8�CCqCCOCC>CC�CC�CC%�CC��CC��CC]eCCCB��CBI�CA��CAhTCAWC@ӮC@�C@��C@w�C@��C@��C@�%C@)�C?�
C?�C?Y�C>��C>kdC>0C=��C=��C=]�C=W�C=g4C=�C=�MC=��C=̰C=�C=�C> C>�C>@C=�qC=�(C=��C>&,C>+xC>FC>�C>�C>C>C=�xC=�JC=�5C=�!C=�IC>AC>:C>NC>ϙC>�XC>�5C>K(C>K.C>yC>�jC?�C?b�C?�C@>�C@r#C@e�C@[�C@�UC@�C@��C@�[C@��CA  CA�CAN5G�O�CGc�CW�;Ce��Cs�CR=C��C��TG�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 11111114   @"�@"�@"��@"�=@"�]@"��@"�0@"��@"��@"��@"��@"��@"t>@"sI@"ya@"��@"�w@"�}@"\�@"[I@"[a@"e�@"].@"^u@"[�@"dX@"X}@"c�@"O�@"c@"X�@"dh@"[�@"\ @"]�@"a�@"K�@"cA@"H�@".=@"@"�@"]@"@!�@!�@!��@!�|@!�q@!�p@!�"@!��@!|�@!SD@!$<@ �@ ��@ Q@}�@�@�I@�@��@��@�/@�'@D@O�@��@��@�?@n@m�@��@�:@�@@D@�@�@o%@X�@qS@��@�@q/@��@�R@��@�Q@�@�n@��@��@or@>�@L�@�/@�]@�@�|@�h@t&@VC@`@�r@y�@zU@��@��@��@�N@�	@�^@�@��@�@>&@nT@�a@��@�#@�'@�X@��@�K@��@#@��@ڧ@�#@T@y@@0D@I�@Y@z�@��@� @�P@��@��@��@��@��@�Z@�@�G@��@�@�@O�@T	@"�@n@�@�@�@�A@��@��@�Q@�L@�@��@��@�Z@�5@��@��@�@�@�@��@�k@�M@�
@�c@#~@4�@@
@<K@7@@��@�m@��@��@�2@�r@�U@�B@�@�K@�@�x@��@��@�@$@<�@8�@H@^�@h@l�@R@X�@�@��@��@��@�@@��@��@��@��@��@�R@�@�I@�6@�8@+�@\�@��@�^@�_@��@�1@�9@�H@C@2X@R�@��@��@��@��@�L@�@�+@U@�@�@�@z:@_�@T@xy@{S@i@[�@M�@�@��@a-@U�@Q�@,�@Q@��@�@2�@l�@t*@vZ@Y�@9�@#@��@Ő@��@��@ml@�@_1@�@��@�]@5�@�$@G�@T@K_@"�@	�@�H@��@.Z@m�@q+@qq@��@\:@�J@B�@&�@�@@�k@��@ҥ@�@�O@��@C�@��@�R@P�@�Y@`,@�@�@�:@�*@��@��@��@�@L@@t�@�@t@K@�J@Y@xq@�@</@�u@@a@��@7/@��@��@͘@{D@P�@@�P@hx@
��@
?w@	�(@	�}@	�B@
c@
x@��@�`@��@|@�}@Ӹ@��@��@�q@��@��@Y@"o@
�w@
��@
�w@
^c@	�@	Bq@�@��@�@�@~�@CS@��@�P@י@��@��@s�@5�@��@%}@ݾ@��@,"@ Þ@ $?���?�n?���?���?��?��?���?�f�?�G�?��?��@?��J?���?��Q?�=I?��u?��|?���?��?��??��?�ɾ?�Ql?�D�?�%?���?�$�?��T?��?���?�?�у?���?�hT?�[�?���?��?��?���?���?�G�?��?���?�?�8�?���?��?�c7?�[�?�o�?�?��%?��<?��?���?�!W?�7?�F?�<{?�$g?��?�1n?�d?�j�?�X�?�:�?�:w?�<*?�JB?�/�?��?��9?�4?��?�4�?�J�?�?�<�?�7{?��?�f?�o?��&?�2s?�?��V?��X?��?�T�?�D�?�8M?���?��v?��^?��8?�?�
g?�?�nXG�O�?�81@
;@/�@�@#g�@&yM@(_$G�O�?0�E?1��?7�k?3�*?0�E?.��?1��?E�?7
=?/'�?5Y�?49X?3�?._?5Y�?+ƨ?1hs?2��?1��?;��?6z?'E9?7�k?1��?7�k?)��?/'�?,V�?+ƨ?B�?0�E?.��?3�?3�*?+ƨ?._?3�*?,�?,V�?4Ɇ?+6z?+6z?(e�?,V�?1hs?1hs?(e�?+ƨ?-w2?#�&?%��?��?&�?)��?#S�?n�?}V?<�?e?9�>�>�?>�n�>_�>-�=�0�=jJ�=0�<�J�<�D�<�?<�-�<�-�<XD�<|PH<�9X<|PH<XD�<�-�<|PH<�-�<XD�<-�<49X<XD�<XD�<XD�<XD�<�-�<�-�<|PH<|PH<�-�<XD�<49X<XD�<|PH<�-�<XD�<49X<-�<49X<49X<|PH<|PH<-�<49X<|PH<49X<XD�<49X<-�<XD�<XD�<|PH<49X<-�<49X<|PH<XD�<49X;�D�<49X<XD�<XD�<-�<49X<49X<�-�<49X<-�<49X<49X<XD�<-�<-�<49X<-�<-�;�D�<49X<49X<49X;�D�<-�<-�<49X<49X<49X<49X<-�;�D�<49X<�9X<-�;�D�<XD�<XD�;�D�<-�;�-�<-�<49X<49X<-�<-�<49X<49X<XD�<-�<-�<49X<49X<-�<-�<XD�<XD�<|PH<XD�<XD�<49X<-�<49X;�D�;�D�<49X<-�<|PH<49X<49X<XD�<49X<49X<-�;�D�<49X;�D�<XD�<-�<XD�<-�<-�;�-�<49X<XD�<-�<49X<XD�<49X<-�;�D�;�D�;�-�;�-�<XD�<49X<XD�<49X;�D�<-�;�D�<XD�<-�;�D�<49X<XD�<-�<-�<49X;�D�;�-�;�D�;�-�;�D�;�D�<XD�<-�<-�<49X<49X;�-�<49X<XD�<-�;�-�;�D�;�-�;�D�<XD�<XD�<-�<-�<49X;�D�<-�<XD�;�-�<49X<-�<-�;�D�<-�<49X<-�<-�;�-�;�D�<XD�<|PH<-�<�-�<49X;�D�<49X<49X<|PH<-�<-�<-�<49X<XD�<-�<XD�;�-�<49X;�D�<-�;�D�;�-�<-�<XD�<49X;�D�<49X<49X<-�<XD�<49X<49X<XD�<49X<-�<-�<-�;�D�<-�<-�<49X<|PH<-�<-�<49X<XD�<49X<49X<XD�<-�<-�<49X<XD�<|PH<49X<XD�;�D�<49X<49X<XD�<49X;�D�;�-�<49X<49X<49X<-�;�D�<-�<|PH<XD�<49X<-�<|PH<XD�<XD�<|PH;�D�<-�<-�<XD�<|PH<XD�<XD�<XD�<XD�<49X<XD�<�-�<XD�<XD�<|PH<|PH<|PH<XD�<49X<XD�<XD�<�-�<�9X<XD�<XD�<XD�<XD�<|PH<XD�<49X<49X<|PH<|PH<|PH<XD�<�9X<|PH<|PH<|PH<|PH<|PH<�-�<-�<XD�<�-�<XD�<�-�<|PH<XD�<|PH<|PH<|PH<|PH<|PH<|PH<49X<|PH<�-�<|PH<|PH<49X<�3�<|PH<-�<|PH<|PH<XD�<|PH<XD�<�-�<|PH<�-�<|PH<|PH<�3�<XD�<|PH<�-�<XD�<|PH<49X<XD�<�-�<�-�<�-�<|PH<XD�<�-�<|PH<XD�<XD�<|PH<|PH<XD�<XD�<|PH<XD�<|PH<XD�<|PH<|PH<XD�<�3�<49X<XD�<�9X<|PH<�-�<|PH<�3�<�-�<|PH<�9X<XD�<�-�<�-�<�?<XD�<|PH<XD�<XD�<|PH<XD�<�-�<49X<�-�<-�<|PH<�-�<XD�<�-�<�3�<�-�<|PH<|PH<|PH<|PH<49X<|PH<�9X<XD�<XD�<|PH<XD�G�O�<�-�<XD�<|PH<�9X<|PH<XD�<�-�<|PH33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 33333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   G�O�                                >�_>�'�>�Ɇ>��E>�_>�ƨ>�'�>�M�>�9X>�V�>���>�hs>�H>�6z>���>���>���>���>�'�>���>��*>�tT>�Ɇ>�'�>�Ɇ>��>�V�>���>���>�>�_>�ƨ>�H>��E>���>�6z>��E>�>���>���>�e�>�e�>���>���>���>���>���>���>��L>�@>���>�!�>��&>��>��>���>��q>�k�>~� >�h�>PbN>@�I>��=��a=��K=��<�<6<XD�<-�;�PH;�D�;XD�;XD�:�-�;-�;�9X;-�:�-�;XD�;-�;XD�:�-ຐ-�    :�-�:�-�:�-�:�-�;XD�;XD�;-�;-�;XD�:�-�    :�-�;-�;XD�:�-�    ��-�        ;-�;-ຐ-�    ;-�    :�-�    ��-�:�-�:�-�;-�    ��-�    ;-�:�-�    �-�    :�-�:�-ຐ-�        ;XD�    ��-�        :�-ຐ-ຐ-�    ��-ຐ-�-�            �-ຐ-ຐ-�                ��-�-�    ;�9X��-�-�:�-�:�-�-ຐ-�XDк�-�        ��-ຐ-�        :�-ຐ-ຐ-�        ��-ຐ-�:�-�:�-�;-�:�-�:�-�    ��-�    �-�-�    ��-�;-�        :�-�        ��-�-�    �-�:�-ຐ-�:�-ຐ-ຐ-�XD�    :�-ຐ-�    :�-�    ��-�-�-�XDлXD�:�-�    :�-�    �-ຐ-�-�:�-ຐ-�-�    :�-ຐ-ຐ-�    �-�XDл-�XDл-�-�:�-ຐ-ຐ-�        �XD�    :�-ຐ-�XDл-�XDл-�:�-�:�-ຐ-ຐ-�    �-ຐ-�:�-�XD�    ��-ຐ-�-ຐ-�    ��-ຐ-�XDл-�:�-�;-ຐ-�;XD�    �-�        ;-ຐ-ຐ-ຐ-�    :�-ຐ-�:�-�XD�    �-ຐ-�-�XDк�-�:�-�    �-�        ��-�:�-�        :�-�    ��-ຐ-ຐ-�-ຐ-ຐ-�    ;-ຐ-ຐ-�    :�-�        :�-ຐ-ຐ-�    :�-�;-�    :�-�-�        :�-�    �-�XD�            ��-�-ຐ-�;-�:�-�    ��-�;-�:�-�:�-�;-�-ຐ-ຐ-�:�-�;-�:�-�:�-�:�-�:�-�    :�-�;XD�:�-�:�-�;-�;-�;-�:�-�    :�-�:�-�;XD�;�9X:�-�:�-�:�-�:�-�;-�:�-�        ;-�;-�;-�:�-�;�9X;-�;-�;-�;-�;-�;XDк�-�:�-�;XD�:�-�;XD�;-�:�-�;-�;-�;-�;-�;-�;-�    ;-�;XD�;-�;-�    ;�-�;-ຐ-�;-�;-�:�-�;-�:�-�;XD�;-�;XD�;-�;-�;�-�:�-�;-�;XD�:�-�;-�    :�-�;XD�;XD�;XD�;-�:�-�;XD�;-�:�-�:�-�;-�;-�:�-�:�-�;-�:�-�;-�:�-�;-�;-�:�-�;�-�    :�-�;�9X;-�;XD�;-�;�-�;XD�;-�;�9X:�-�;XD�;XD�;�D�:�-�;-�:�-�:�-�;-�:�-�;XD�    ;XDк�-�;-�;XD�:�-�;XD�;�-�;XD�;-�;-�;-�;-�    ;-�;�9X:�-�:�-�;-�:�-�G�O�;XD�:�-�;-�;�9X;-�:�-�;XD�;-�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 22222222   ?._?/'�?4Ɇ?0�E?._?+ƨ?/'�?BM�?49X?,V�?2��?1hs?0H?+6z?2��?(��?.��?/��?/'�?8��?3�*?$tT?4Ɇ?/'�?4Ɇ?&�?,V�?)��?(��?@?._?+ƨ?0H?0�E?(��?+6z?0�E?*?)��?1��?(e�?(e�?%��?)��?.��?.��?%��?(��?*�L?!@?"��?!�?#�&?&�? �?��?�q?	k�>�� ? h�>�bN>��I>���>S�a>�K=���==<6<�D�<�-�<|PH<XD�;�D�;�D�;-�;�-�<49X;�-�;-�;�D�;�-�;�D�;-�;-�    ;-�;-�;-�;-�;�D�;�D�;�-�;�-�;�D�;-�    ;-�;�-�;�D�;-�    ;-�        ;�-�;�-�;-�    ;�-�    ;-�    ;-�;-�;-�;�-�    ;-�    ;�-�;-�    ;�-�    ;-�;-�;-�        ;�D�    ;-�        ;-�;-�;-�    ;-�;-�;�-�            ;�-�;-�;-�                ;-�;�-�    <49X;-�;�-�;-�;-�;�-�;-�;�D�;-�        ;-�;-�        ;-�;-�;-�        ;-�;-�;-�;-�;�-�;-�;-�    ;-�    ;�-�;�-�    ;-�;�-�        ;-�        ;-�;�-�    ;�-�;-�;-�;-�;-�;-�;�D�    ;-�;-�    ;-�    ;-�;�-�;�-�;�D�;�D�;-�    ;-�    ;�-�;-�;�-�;-�;-�;�-�    ;-�;-�;-�    ;�-�;�D�;�-�;�D�;�-�;�-�;-�;-�;-�        ;�D�    ;-�;-�;�D�;�-�;�D�;�-�;-�;-�;-�;-�    ;�-�;-�;-�;�D�    ;-�;-�;�-�;-�    ;-�;-�;�D�;�-�;-�;�-�;-�;�D�    ;�-�        ;�-�;-�;-�;-�    ;-�;-�;-�;�D�    ;�-�;-�;�-�;�D�;-�;-�    ;�-�        ;-�;-�        ;-�    ;-�;-�;-�;�-�;-�;-�    ;�-�;-�;-�    ;-�        ;-�;-�;-�    ;-�;�-�    ;-�;�-�        ;-�    ;�-�;�D�            ;-�;�-�;-�;�-�;-�    ;-�;�-�;-�;-�;�-�;�-�;-�;-�;-�;�-�;-�;-�;-�;-�    ;-�;�D�;-�;-�;�-�;�-�;�-�;-�    ;-�;-�;�D�<49X;-�;-�;-�;-�;�-�;-�        ;�-�;�-�;�-�;-�<49X;�-�;�-�;�-�;�-�;�-�;�D�;-�;-�;�D�;-�;�D�;�-�;-�;�-�;�-�;�-�;�-�;�-�;�-�    ;�-�;�D�;�-�;�-�    <-�;�-�;-�;�-�;�-�;-�;�-�;-�;�D�;�-�;�D�;�-�;�-�<-�;-�;�-�;�D�;-�;�-�    ;-�;�D�;�D�;�D�;�-�;-�;�D�;�-�;-�;-�;�-�;�-�;-�;-�;�-�;-�;�-�;-�;�-�;�-�;-�<-�    ;-�<49X;�-�;�D�;�-�<-�;�D�;�-�<49X;-�;�D�;�D�<XD�;-�;�-�;-�;-�;�-�;-�;�D�    ;�D�;-�;�-�;�D�;-�;�D�<-�;�D�;�-�;�-�;�-�;�-�    ;�-�<49X;-�;-�;�-�;-�G�O�;�D�;-�;�-�<49X;�-�;-�;�D�;�-�:�p:�:��:���:�/�:�6:�b:���:���:xx�:�^:|&�:�^�:�":�":|&�:�4:�^�:�$�:y��:o��:}a:��:��7:�X�:���:�GX:��K:�X�:�$�:���:���:�GN:���:�$�:��	:��#:�/�:z�:�$�:z�:~�C;�g�:y��:�^:t�n:q:���:��-;��:���:xx�:��:}a:xx�:s�:t��:mmW:o��:ml(:a$�:Y�::L@�:I��:)�}:��:��:2�:8�:
�e:|�:	��:
�G:
�O:r:	��:	�:�!:�#:	��:N�:{�9��x:	�:=�:&7:�	:�:$o:��:#�:"�:t":�:Y�9�R:4:%h:�i9�z�: �:0�:!�9�n9��t9���::��: �Q9�
�:F�9�y�:^B: �:R9���:0�9�pA:!29��:!?9�s*9��Z9���9���:X�:Wq9�"�9���9�jf9�U{9�V�9���9�9�W9�j�9�j29��9�hQ9�ޒ9�T�:79�۝9�f�9�f9�|�:O9��T:��9��9���9�=9�q: ��9�}�:g9�9�x�9��: �H9�Y'9��%9��9�P�9��9�e�9��9�]9�B9�K�9���9�a�9�I&9�_�9�vL9�9��9梿9���9��9�q�9��[: ��9�CZ9�Z�9�+�9���9넖9�%�9�jK9�:y�9�I9�R�9��E9�9�9�Oo9��W9�c�: ǰ9���9���:$"9�26:;r9�1H9��`:"�9��9�E�9�-�9�ϴ9�,,9�B|9�*�9�B|9��9�(S: ��9��x9��9�?59��1:��9�$K9�
9��9�6k9�5C9���9��9�H9�_"9�k9�Gd:�D9�.�9�.m: ��9�-�9�� 9��29�9���9�+�:�9���9�?�:>(9��9�;�9��.9�&: ��9�$	9��9�ũ9�7�:&	9��9�L 9�3�9���9��9���9��M9��9�-o9���9�+9���9�a9�?�9���9�R�9���9�:Q9�i9�zH9�aH9��9��V9�-�9�*�9�p39�m�9��9��9�?99�V�9��9�9�L9�9�k+9�n9�=9�@�9�9��9训9�[9�>9풅9��9�9�zL9�J�9팓9�A9�A�9�nL9�S�9��9�ڪ:��9��9�g9�9�?9��9�.�9�]�9�E�9���9��b9���:��:��:�;9���:�q: ��9�h�9�G�9�,:��:,
:	6: {n:��:�4:
J}: x�9���9� 
:q2:�K:�:'�$:6m�:fK�:��':�ډ:�_`:�p�:��:�C:�{l:�j:��:���:ln*:/�:7�T::�:B�:C�k::�:7�0:'�':#�::!�Z:&m%:+T�:'�:&cd:*�:*!:F�::��: +E:�7:!a�:4:�:�X:"��:![:��:+B:% �:>�: 9}:<q�:"��:%!:"�~:��:��:,|�:!q�: 7�:%�: 5�:1_�:%�:)��:!a�: %�:!Y�:"�(:(�#:+'�:'y�:'z?:;;:'wF:&;�:,\4:h$:g:$��:��:)�:&3:7`:-�: 
�:�::!C�:��:\D:��:7�:
�:C�9�U�9�p9��9�.�9�79�ZT9���:��9�Y9�
-:=N9�~V9��9��9⢘9��i9�9�t�9�D9�δ9�[:�19��W9��9�B�9��d9�&R9�"�9�;�f�9�}9�bu9���9��E9�B=9�<'9�è9�s9���9�9�z79�9齯9�D�9�Z�99鴯9�,9���9�k~G�O�9�=:
W8:П9�J 9�j�9�H�9�4�9�33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 33333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   G�O�                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�q?�q?�q?�?�e�?��?�?�?��?�q?�q?�?�?�?�q?�?�?�q?�q?�|�?��?�e�?�?�q?��?�q?�+?�q?ɷ?�?��?�|�?�?��?��?��?��?�q?��?��?��?��?�q?�?�?��?�?��?�q?�?��?�?�|�?�?��?�q?�|�?��m?�?�?�q?�@IV?��?ɷ?Ք�?��2?Ք�?�C-?��2?Ѡ'?��?Ք�?��2?��?��2?��2?�C-?Ք�?�C-?�C-?Ք�?�7�?��?��2?Ք�?Ѡ'?�N�?�C-?�7�?��?�C-?Ք�?Ք�?��2?��2?�7�?��2?�7�?��?Ք�?�C-?�N�?��2?�C-?�C-?Ք�?�C-?�C-?��?�C-?��?�7�?��2?Ѡ'?Ք�?Ք�?Ք�?��?�C-?�C-?Ѡ'?��?Ք�?�C-?��?ͫ�?��"?�N�?��?Ѡ'?Ѡ'?�N�?��?��"?��"?ɷ?��"?��"?�Z?ɷ?��"?�N�?��?ͫ�?��?ͫ�?�Z?��"?ɷ?ɷ?�Z?�Z?�Z?�Z?ɷ?�Z?�?��?ͫ�?ɷ?�Z?�N�?��?ͫ�?�Z?��?ͫ�?��?�e�?ɷ?�Z?ͫ�?�Z?�Z?�Z?ͫ�?ͫ�?�C-?�N�?�Z?ͫ�?��"?��"?��"?��"?��"?�Z?Ѡ'?��"?ͫ�?�Z?��"?Ք�?�Z?��"?ͫ�?��?��"?�N�?��?��"?�Z?��?��"?��?�Z?ɷ?�N�?ɷ?��?ͫ�?��?��?��?��"?ͫ�?�N�?��"?��?�Z?ͫ�?��?��?��"?ͫ�?ͫ�?ɷ?��"?��?�Z?��?�Z?��"?ͫ�?ͫ�?ɷ?ͫ�?�Z?ɷ?�Z?�Z?��?�e�?�e�?��?��?��?ͫ�?�Z?ɷ?��?ɷ?�N�?��"?ɷ?ɷ?�Z?�Z?��"?��"?ͫ�?�N�?�C-?ͫ�?�N�?��"?Ѡ'?�N�?�Z?Ѡ'?��?��"?�N�?�C-?��?Ѡ'?��?��?Ѡ'?��2?ى7?�C-?ى7?�7�?��2?�,=?�C-?ى7?ى7?�ں?ى7?��2?�}�?��2?��?�C-?�C-?��?Ք�?��?��2?Ք�?�C-?�C-?��2?Ք�?�ں?Ք�?Ք�?Ք�?��2?�ں?�,=?�7�?�}�?�ں?�,=?�}�?�,=?�ں?� �?� �?�,=?�,=?�}�?�,=?�ں?�,=?�,=?�}�?��B?�M?���?�M?�	�?�	�?�M?�rG?�R?�[W?�[W?�M?�	�?��?�b?�b?�R?��]?�[W?���?��?�[W?�	�?��]?�R?��]?�	�?�f�?��?�[W?�R?�R?�	�?�[W?�[W?�	�?��]?�b?��?�O�?�b?��?�O�?�Dg?��?��?��m?��m?��?��?�8�?���?��?�-w?�-w?��r?�-w?��r?��}?���?���?��m?�-w?���?�!�?�-w?���?���?�~�?�-w?�!�?��}?���?�!�?��r?�~�?��}?�~�?�~�?�s�?�s�?��?��}?��}?�~�?��r?�!�?�~�?�-w?�-w?��?��?�s�?�!�?�-w@��?�~�?�s�?��}?�s�?�s�?��?�s�?��?�s�?�~�?�~�?�s�@ �D@4?�s�?��?�!�@��@��@��@ �D@ �D?�s�?��}?��@ �D@4?��?��?��}@4?�s�?��?�s�?��?��?��?�s�@��@ �D@(�@ �D?�s�?��?�s�?�s�@��@4@4@ �D@ �D?�s�?�!�@ �D?��@��?�!�@��?�s�?�s�?�!�G�O�?�!�@4@ �D@ �D?��@��?���@ �D33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 33333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   G�O�                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@Ë G�O�@�k�G�O�@�_�G�O�G�O�@� �G�O�@�G�O�G�O�@�BSG�O�@�VEG�O�@ǻ�G�O�G�O�@��!G�O�G�O�@�ĚG�O�@Ȥ�G�O�@�.G�O�G�O�@��G�O�@���@�ѸG�O�@ʨ1G�O�G�O�@��G�O�@�z�G�O�G�O�@ˤ�G�O�@���G�O�G�O�@�C�G�O�G�O�G�O�G�O�@ʞ�G�O�G�O�G�O�@�ŲG�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�A8�cG�O�G�O�G�O�G�O�G�O�A?��G�O�G�O�G�O�G�O�AB�jG�O�G�O�G�O�AFAAFXFG�O�G�O�G�O�G�O�ACmG�O�G�O�G�O�G�O�AD�G�O�G�O�G�O�AH&G�O�G�O�G�O�G�O�G�O�AJ�zG�O�G�O�G�O�G�O�AL��G�O�G�O�G�O�G�O�AMЁG�O�G�O�G�O�G�O�AMɳG�O�G�O�G�O�AL49G�O�G�O�G�O�G�O�G�O�AL;G�O�G�O�G�O�G�O�AG�G�O�G�O�G�O�G�O�AH=G�O�G�O�G�O�G�O�AJQ�G�O�G�O�G�O�AI9�G�O�G�O�G�O�G�O�AE�4G�O�G�O�G�O�AGݟG�O�G�O�G�O�G�O�G�O�G�O�AO,:G�O�G�O�G�O�G�O�AQ�G�O�G�O�G�O�AQ�rAQ~�G�O�G�O�G�O�G�O�APc�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AP��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AR��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AR.DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AUӍG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aa4eG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AlnBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�h�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�[HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�PA���A��_A�g�A�
A��OA���  3 3 3  3 3  3 3 3  3  3 3 3  3 33 3  3 3  3 3  3    3   3     3   3     3    3   33    3    3   3     3    3    3    3   3     3    3    3    3   3    3   3      3    3   33    3         3         3                        3                        3                        3                        3                        3                        3                        3                        3                       33                       3                         3              33333333   G�O�G�O����G�O��L��G�O�?L��G�O�G�O��   G�O�>���G�O�G�O��   G�O�>���G�O�?L��G�O�G�O��L��G�O�G�O��L��G�O��L��G�O�?fffG�O�G�O�����G�O�?�  ��  G�O��L��G�O�G�O�����G�O�?   G�O�G�O����G�O�>���G�O�G�O��L��G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O����G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�?��G�O�G�O�G�O�>���G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O����G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�����G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�  ��  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O��333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                G�O�G�O�@���G�O�@���G�O�@�͙G�O�G�O�@�n�G�O�@���G�O�G�O�@��2G�O�@��$G�O�@�)�G�O�G�O�@�Z G�O�G�O�@�2yG�O�@�oG�O�@���G�O�G�O�@���G�O�@�U�@�?�G�O�@�G�O�G�O�@���G�O�@��G�O�G�O�@�G�O�@�_�G�O�G�O�@���G�O�G�O�G�O�G�O�@��G�O�G�O�G�O�@�3�G�O�G�O�G�O�G�O�G�O�AN~G�O�G�O�G�O�A �RG�O�G�O�G�O�G�O�G�O�A'��G�O�G�O�G�O�G�O�A*�YG�O�G�O�G�O�A-�nA.5G�O�G�O�G�O�G�O�A+$ G�O�G�O�G�O�G�O�A,�sG�O�G�O�G�O�A/�G�O�G�O�G�O�G�O�G�O�A2�iG�O�G�O�G�O�G�O�A4��G�O�G�O�G�O�G�O�A5�pG�O�G�O�G�O�G�O�A5��G�O�G�O�G�O�A3�(G�O�G�O�G�O�G�O�G�O�A3��G�O�G�O�G�O�G�O�A/��G�O�G�O�G�O�G�O�A/��G�O�G�O�G�O�G�O�A2�G�O�G�O�G�O�A0��G�O�G�O�G�O�G�O�A-_#G�O�G�O�G�O�A/��G�O�G�O�G�O�G�O�G�O�G�O�A6�*G�O�G�O�G�O�G�O�A8�G�O�G�O�G�O�A9DbA95�G�O�G�O�G�O�G�O�A8�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A8��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A:9pG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A9�4G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A=�}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AC�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AH�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AT%1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Am��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ax�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�DLG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��\A��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��kG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�6�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�jMA���A��QA�n�A�CmA��A���A�^#  1 1 1  1 1  1 1 1  1  1 1 1  1 11 1  1 1  1 1  1    1   1     1   1     1    1   11    1    1   1     1    1    1    1   1     1    1    1    1   1    1   1      1    1   11    1         1         1                        1                        1                        1                        1                        1                        1                        1                        1                       11                       1                         1              11111111   G�O�G�O�?&�NG�O�?&�NG�O�?&�NG�O�G�O�?&�NG�O�?&�NG�O�G�O�?&�NG�O�?&�NG�O�?&�NG�O�G�O�?&�NG�O�G�O�?&�NG�O�?&�NG�O�?&�NG�O�G�O�?&�NG�O�?&�N?&�NG�O�?&�NG�O�G�O�?&�NG�O�?&�NG�O�G�O�?&�NG�O�?&�NG�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�N?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�?&�N?&�NG�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�N?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?&�N?&�N?&�N?&�N?&�N?&�N?&�N?&�N