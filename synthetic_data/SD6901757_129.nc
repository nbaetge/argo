CDF       
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB          	   title         Argo float vertical profile    institution       CORIOLIS   source        
Argo float     history       f2020-10-31T01:04:58Z creation (software version 1.10 (version 30.06.2020 for ARGO_simplified_profile))     
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
resolution        ?F�l�l        6P   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        =���   axis      Z        0  Q   PRES_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  T4   PRES_ADJUSTED            	      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        0  U    PRES_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  X0   PRES_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  X�   TEMP         	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  \,   TEMP_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  _\   
TEMP_dPRES           	         	long_name         6TEMP pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      0  `(   TEMP_ADJUSTED            	      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  cX   TEMP_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  f�   TEMP_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  gT   PSAL         	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  j�   PSAL_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  m�   
PSAL_dPRES           	         	long_name         6PSAL pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      0  n�   PSAL_ADJUSTED            	      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  q�   PSAL_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  t�   PSAL_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  u�   DOXY         	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  x�   DOXY_QC          	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  |   
DOXY_dPRES           	         	long_name         6DOXY pressure displacement from original sampled value     
_FillValue        G�O�   units         decibar      0  |�   DOXY_ADJUSTED            	      	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     
_FillValue        G�O�   units         micromole/kg   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  �   DOXY_ADJUSTED_QC         	         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  �8   DOXY_ADJUSTED_ERROR          	         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     0  �Argo synthetic profile          1.0 1.2 19500101000000  20201031010458  20201031010458  6901757 NAOS                                                            Serge LE RESTE                                                  PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                               �A   IF  DDDDARVOR_D                         OIN-15-ARDP-02                  5608A08                         838 @חS6��1   @חT���@Hk��RFf�1c>�,�q1   GPS        PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL (re-calculated by using PRES_ADJUSTED)+ Delta_S, where Delta_S is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r                                                                                DOXY1=DOXY*(1+C*PRES/1000); DOXY2= DOXY1 - drift_coef                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           PSAT=f(DOXY2); PSAT_ADJUSTED=A*PSAT+B;DOXY_ADJUSTED=f(PSAT_ADJUSTED)                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            r= 1.0008 (+/- 5e-05) , vertically averaged dS =0.032767 (+/- 0.01)                                                                                                                                                                                             C=0.0100; drift_coef= -4.93;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A=1.008; B=0                                                                                                                                                                                                                                                    No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              No adjustement was necessary -Calibration error is manufacturer specified accuracy                                                                                                                                                                              Salinity drift or offset detected - OW fit is adopted. Error = maximum [statistical uncertainty, 0.01]. OW Method(2009), config  39 -CTD2016V1 -                                                                                                                Pressure effect correction on DOXY; Temporal drift correction on DOXY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Percent saturation corrected as a linear function of PSAT; Comparison to the reference profile rr15_0 (isobaric match as in Takeshita et al. (2013)) on cycle 1; PSAT converted from DOXY and DOXY_ADJUSTED converted from PSAT_ADJUSTED                        20180605164550201806051645502018060516455020181114150056                                          20181114150056A   B   B   A   >L��?fff?�ff@333@s33@�33@���@���A��A��A&ffA�ffA�  B33B4ffB^��B�  B�ffB�ffB�ffB�  B晚B���CffC�C�C%ffC/ffC9�CCffCM��CW33Ca�CkL�CuffCffC��3C��3C��fC���C��3C���C���C�� C�ٚC���C��fC��fC���C���C��fCπ CԦfC���CަfC�fC�� C��3C��C���D 33D� D� D��D,�Ds3D%�fD+��D2FfD8s3D>�3DD��DK@ DQl�DW�fD^fDd,�Djl�Dp��Dw  D},�D��fD�� D�� D��D�C3D�c3D�|�D��3D��fD�ٚD���D�9�D�@ D�VfD�y�D�� D��3D�ٚD�fD�#3D�@ D�c3DƖfDə�D̳3D��fD��3D�fD�\�D�c3D߀ D�fD��D�� D��3D��D�<�D�\�D��fD���D���E �El�E	�E��E)�E�3E
\�E� El�E�fE��EfE�3E@ EњE^fE��E{3E�E��E )�E!�3E#Y�E$ٚE&nfE'�3E)� E+3E,�fE.>fE/�3E1l�E2� E4|�E6 E7�fE9.fE:� E<Q�E=ٚE?nfEA  EB�3ED  EE��EG<�EH�3EJY�EK�fEM|�EO EP��ER,�ES� EUFfEV� EXvfEY�fE[��E]�E^�3E`C3Ea� Ec^fEd�3Ef|�Eh�Ei� Ek+3El��EnX Eo� EqnfEr��Et� Ev  Ew� EyA�Ez��E|c3E}�3E6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222>L��?ffh?�fg@334@s34@�33@���@���A��A��A&ffA�ffA�  B33B4ffB^��B�  B�ffB�ffB�ffB�  B晚B���CffC�C�C%ffC/ffC9�CCffCM��CW33Ca�CkL�CuffCffC��3C��3C��fC���C��3C���C���C�� C�ٚC���C��fC��fC���C���C��fCπ CԦfC���CަfC�fC�� C��3C��C���D 33D� D� D��D,�Ds3D%�fD+��D2FfD8s3D>�3DD��DK@ DQl�DW�fD^fDd,�Djl�Dp��Dw  D},�D��fD�� D�� D��D�C3D�c3D�|�D��3D��fD�ٚD���D�9�D�@ D�VfD�y�D�� D��3D�ٚD�fD�#3D�@ D�c3DƖfDə�D̳3D��fD��3D�fD�\�D�c3D߀ D�fD��D�� D��3D��D�<�D�\�D��fD���D���E �El�E	�E��E)�E�3E
\�E� El�E�fE��EfE�3E@ EњE^fE��E{3E�E��E )�E!�3E#Y�E$ٚE&nfE'�3E)� E+3E,�fE.>fE/�3E1l�E2� E4|�E6 E7�fE9.fE:� E<Q�E=ٚE?nfEA  EB�3ED  EE��EG<�EH�3EJY�EK�fEM|�EO EP��ER,�ES� EUFfEV� EXvfEY�fE[��E]�E^�3E`C3Ea� Ec^fEd�3Ef|�Eh�Ei� Ek+3El��EnX Eo� EqnfEr��Et� Ev  Ew� EyA�Ez��E|c3E}�3E6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A.VA.VA.VA.ZA.VA.ZA. �A.M�A.5?A.�A.(�A.I�A.VA-ƨA. �A.~�A.z�A,Q�A+7LA*v�A*M�A*9XA*1'A*JA)�A)A)�PA)dZA)G�A)G�A)7LA)7LA)33A)+A)�A)oA)oA)VA(��A(�A(�A(�yA(�\A!%A�-AI�Ar�A�A�DA+A��A��A	�A�HAXAn�A�HA��A�HA��A��AZA�A@��`@���@ؼj@�\)@̴9@��/@��@��y@͙�@�|�@�%@�O�@�v�@��y@���@���@��D@�ȴ@� �@��@�r�@��T@�+@�r�@�`B@���@�1@���@���@���@���@���@��m@��@���@�J@�/@l�@|�@zM�@xĜ@w|�@w\)@v�R@up�@sS�@q��@pbN@n��@l��@kS�@i�#@g�;@f��@d�D@b��@a%@_�@]�T@\Z@Z�!@X�u@W\)@V��@U�-@TZ@R�@Q�7@P  @N��@M�T@LZ@Kt�@J~�@I�@I��@H��@G\)@Fv�@D�@C��@B�!@B=q@Ax�@@Ĝ@@1'@?\)@>E�@=O�@<j@;t�@:��@:�@9x�@8��@8A�@7\)@6�R@5�@4j@3��@2��@1�^@1�@0bN@/�;@/�@.V@-@-�@,�D@+��@+��@*�@*n�@)�^@)G�@(�9@(A�@'�w@';d@&�R@&5?@%�-@%O�@$��@$�@$Z@$1@#�F@#dZ@#o@"�H@"��@"��@"~�@"M�@"J@!�@!��331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�A. �A.M�A.5?A.�A.(�A.I�A.VA-ƨA. �A.~�A.z�A,Q�A+7LA*v�A*M�A*9XA*1'A*JA)�A)A)�PA)dZA)G�A)G�A)7LA)7LA)33A)+A)�A)oA)oA)VA(��A(�A(�A(�yA(�\A!%A�-AI�Ar�A�A�DA+A��A��A	�A�HAXAn�A�HA��A�HA��A��AZA�A@��`@���@ؼj@�\)@̴9@��/@��@��y@͙�@�|�@�%@�O�@�v�@��y@���@���@��D@�ȴ@� �@��@�r�@��T@�+@�r�@�`B@���@�1@���@���@���@���@���@��m@��@���@�J@�/@l�@|�@zM�@xĜ@w|�@w\)@v�R@up�@sS�@q��@pbN@n��@l��@kS�@i�#@g�;@f��@d�D@b��@a%@_�@]�T@\Z@Z�!@X�u@W\)@V��@U�-@TZ@R�@Q�7@P  @N��@M�T@LZ@Kt�@J~�@I�@I��@H��@G\)@Fv�@D�@C��@B�!@B=q@Ax�@@Ĝ@@1'@?\)@>E�@=O�@<j@;t�@:��@:�@9x�@8��@8A�@7\)@6�R@5�@4j@3��@2��@1�^@1�@0bN@/�;@/�@.V@-@-�@,�D@+��@+��@*�@*n�@)�^@)G�@(�9@(A�@'�w@';d@&�R@&5?@%�-@%O�@$��@$�@$Z@$1@#�F@#dZ@#o@"�H@"��@"��@"~�@"M�@"J@!�@!��444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB/B.B-B-B/B2-B/B1'B0!B0!B1'B1'B1'B+B/B33B1'B�BPB%BBBBB  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3Bo�Bm�BgmB[#BaHB:^BoB�B\B�NB�B��BB{B^5B�hB(�BVBM�B�#BB�`B�yB�B�mBS�B�BB��BjBx�Bu�Br�Bt�B�%BR�B��B
=B
=BB  BB)�B�B��B��B�/B��B�wB�3B�B�B��B��B��B��B��B��B��B��B�{B�uB�{B��B��B��B��B�uB�oB�hB�bB�bB�bB�\B�\B�\B�\B�\B�VB�VB�VB�PB�PB�VB�VB�\B�bB�hB�hB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�hB�hB�bB�\B�VB�PB�JB�DB�=B�7B�1B�1B�+B�%B�%B�B�B�B�B�B�B�B�B� B� B~�333333111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333333333333333333333333333333333333333333333333333333333333333333333333                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�BQBS'BR!BR BS+BS'BS+BMBQBU3BS(B;�B/FB(B&B%B$B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�gB�XB�1B|�B�B\B4B?_B1B�B��B^B#�B6$B�B�2BJ�B0BBo�B� B#�B�BB0B�Bu�BB�JB�0B��B�uB�]B�lB��Bt�B �B+�B+�B&�B!�B#�BK�B@HB �BIB��B�@B��BԤBЋB�}B�]B�;B�(B�6B�+B�"B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�&B�&B�-B�,B�3B�0B�0B�8B�8B�6B�6B�9B�9B�7B�-B�0B�*B�#B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�yB�{B�qB�mB�hB�dB�_B�aB�_B�UB�QB�RB�L444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
C~�C~�C~�C~֌C~�#G�O�C~�iC~�mC~�.C~�-C~�C~�C~�$C~�C~׏C~��C�C�IC�e{C��C�� C��HC���C��C��XC���C��sC���C���C�WC�J C�0�C��C� �C��C�eC��C�CC�vC~�C}��Cy�Cj�C[�CY-�C^��C^�ICW1~CN�CJ.CH�CH�zCItPCI��CHA�CG8<CG[CH��CH9CDߖC>yC=a�C<fQCA�CI�[CKܩCO$4CQ��CT]eCO��CL4zCT�-CX�*CZ��C\'�C^�'C_�Ca��Ch��CoдCn�9Cp�WCrJCs�Cs7�Cq�SCs��Cwu:Cy��C|��CDC�sdC�?�C�t�C���C�ZC�eC�lCC�H�C�_�C�|�C���C��C��C�gC���C��OC��C���C��;C��6C���C��bC��C�hC�$uC�/<C�=C�?�C�R�C�T�C�cvC�w:C���C���C��-C��{C�rZC�M)C�-C�^C�dC��C���C��9C���C���C�{hC�\�C�2�C��C��?C���C�>�C��#C��C�g�C�#`C�ZC\�C~ƑC~�C}N�C|s3C{k�CzF�Cx�CwQ�Cv�dCu��CsX	Cq�CqkCp�WCp �Co��Co �Cn�8CnI~Cm�FCm�dCmx5Cm5
ClރCl�^ClVCl}Ck��Ck�Ck2iCkrCj��Cj�TCj�WCjq�Cj#Ci�JCi{lCiCBCh��Ch�$Ch�sChk�Ch9lCh
�Cg�sCg��Cg��Cgw�Cg\yCg;Cg!cCg
)Cf�33333 333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333                    G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        C��C���C� �C��?C���G�O�C���C��-C��C��6C��C���C���C�C�	�C���C�&�C���C��C�D:C�H�C�S+C�lC���C��~C�w!C��IC�z�C�T�C�(�C�$C�mC��C���C��<C��"C��8C��jC���C�l�C��	C�p�Cr=QCcv�C`�xCf+�CfK�C^ʫCU��CQ�9CPt�CP,�CP��CQ��CO�HCNȭCN��CPJ�CO��CL|rCF�CD��CDCI}nCQ��CSܜCW=�CZ#�C\��CW��CTw�C]4;Ca�CcI�Cd�CgW�ChҵCj�6Cq�XCy2iCxi�CzJ"C{�C}�OC|�eC{�ZC}��C��$C��)C�}FC��MC���C��OC�� C��wC�uC��)C���C���C��C�!C�cRC��rC��C��1C��C���C�}�C��nC��hC��(C���C��C�IC�!�C�:�C�N.C�d�C�o�C��*C���C��@C��C��C��C�mC��C��qC���C��C��C��=C��+C�x�C�f�C�T�C�QC�F?C�/DC��C��5C���C��fC�1C��lC��C�k�C�-C��C��WC�~�C�"�C��kC�cC��=C�RlC���C�ٺC���C�1�C��C�%wC�CE�C~��C~$�C}�oC}LZC|�:C|��C|upC|PrC|�C{�9C{�C{_C{3ACz��Cz�xCzk�CzM-Cz@ZCzCz|Cy��Cy�&CyU�CyQCx�Cx�BCx�Cxo1CxIxCx#�Cx�Cw��Cw�uCw��Cw��Cw��Cw��Cwu�Cwl#Cw\�11111 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@��@��@�{�@�{CG�O�@�|�@�y�@���@���@��W@�y\@�w�@��@���@���@�ϟ@��@���@��@���@�R@�@8@�j*@�k/@�U_@�l�@�\8@�@��
@��n@���@�Ki@�1,@�+�@��@��v@���@��@�Vc@�x@���@��@�]�@׿,@���@��@��H@��E@�FL@�@��*@Ƞ�@�'D@ǀE@ƃ6@�bo@���@�YR@�N�@��@��@�/�@�n,@�t@�c%@Ρ�@�j3@��t@�Ul@���@�[@؆t@�2C@۽�@��@߂�@�:�@�(�@�:�@�z6@�G,@��L@�r�@��@�p�@�@�-�@�m�@�u�@��A V�A'�Ac A�4A	�Ag2AvaA[YAzVA��A�AF0AF�AX/A4�A�A�AtA*OAH�AaeA}�A��A��A�YA�,A��A��A�A�A,fAHA_ArVA�A}�Ak�AO�A7�A*SA&�A�A�A�A�@A��AȎA�A�pAw�A[�A�A�sAx�A2@AA��A�CA`SA�A �A rRA ;@�#@��@��W@�;�@��@���@�r�@�
}@���@��@�x�@��@�#@�*�@�ݩ@�2@�\C@�8�@�c@�@��@�Q@�&�@���@�,@�g�@�J@�=�@��@��@��@��@�\a@�#�@���@��s@��@�:@�[@�6�@�@��@��=@��}@���@���@�/@��@톈@�w�