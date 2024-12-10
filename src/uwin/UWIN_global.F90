MODULE UWIN_global
!===============================================================================
!
! Module containing gridded component objects, import/export fields names and 
! such.
!
!===============================================================================

USE ESMF
USE UWIN_GriddedComponent,ONLY:griddedComponent,farrayPtrType2DI4,&
                               farrayPtrType2DR4,farrayPtrType2DR8

!===============================================================================
IMPLICIT NONE

TYPE(griddedComponent),DIMENSION(:),ALLOCATABLE,TARGET :: gc
TYPE(griddedComponent),POINTER :: thisgc => NULL()

INTEGER,PARAMETER :: ngc          = 3  ! Number of gridded components
INTEGER,PARAMETER :: maxNumFields = 10 ! Maximum number of exchange fields

LOGICAL :: atmosphere,waves,ocean

! Coupling switches:
LOGICAL :: sstFromOcean
LOGICAL :: currentsFromOcean
LOGICAL :: windStressFromWavesScalar
LOGICAL :: windStressFromWavesVector
INTEGER :: vectorStressLimiter
LOGICAL :: oceanStressFromWaves
LOGICAL :: oceanAdvectsWaves
LOGICAL :: waveCurrentInteraction
REAL(KIND=ESMF_KIND_R8) :: vconvAtmosphere
REAL(KIND=ESMF_KIND_R8) :: vconvOcean

LOGICAL,DIMENSION(ngc) :: modelIsEnabled    = .FALSE.
LOGICAL,DIMENSION(ngc) :: componentIsActive = .FALSE.

CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(ngc) :: &
  gridCompName = ['WRF 3.8.1','UMWM 2.0.0','HYCOM 2.2.98']

! Model domains size:
INTEGER :: idma,jdma
INTEGER :: idmw,jdmw
INTEGER :: idmo,jdmo
INTEGER :: xgRefinementRatio  !! BK Pull refinement ratio out to uwin.nml.

INTEGER,DIMENSION(3) :: idm,jdm

! Tile dimensions for each model:
INTEGER :: isa,iea,jsa,jea ! Atmosphere
INTEGER :: isw,iew,jsw,jew ! Waves 
INTEGER :: iso,ieo,jso,jeo ! Ocean 
INTEGER :: isx,iex,jsx,jex ! Exchange grid

! Virtual machine:
TYPE(ESMF_VM) :: vm
INTEGER       :: petCount,localPet,mpicomm

! Array kind:
TYPE(ESMF_ArraySpec) :: arrspec2DI4
TYPE(ESMF_ArraySpec) :: arrspec2DR4
TYPE(ESMF_ArraySpec) :: arrspec2DR8
TYPE(ESMF_ArraySpec) :: arrspec3DR4
TYPE(ESMF_ArraySpec) :: arrspec3DR8

!===============================================================================

! ImpFields matrix that describes the number of fields to be exchanged between 
! each model component; 
!
! Obviously, diagonal elements must be zero.
!
INTEGER,DIMENSION(ngc,ngc),PARAMETER :: numImpFields = RESHAPE &
!
!                            A   W   O
!                            ^   ^   ^
!                  ImpField  |   |   |
!                            |   |   |    !  From
                         ([  0,  5, 10, & ! <---- A
                             4,  0,  2, & ! <---- W
                             3,  3,  0],& ! <---- O
!
                     [ngc,ngc]) ! Reshape to 3x3 matrix
!
INTEGER,DIMENSION(ngc,ngc) :: numExpFields

!TYPE(ESMF_Field),DIMENSION(maxNumFields,ngc,ngc) :: impField,expField

! Stokes drift import and export fields and field bundles:
TYPE(ESMF_Field)       :: sdcuImpField,sdcvImpField,sdcuExpField,sdcvExpField
TYPE(ESMF_FieldBundle) :: sdcImpFieldBundle,sdcExpFieldBundle

! State and bundle names:
CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(3),PARAMETER ::          & 
impStateName = ['ATM impState','WAV impState','OCN impState'],&
expStateName = ['ATM expState','WAV expState','OCN expState']  

CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(ngc,ngc),PARAMETER ::             & 
impBundleName = RESHAPE(['------------','WAV from ATM','OCN from ATM', &
                         'ATM from WAV','------------','OCN from WAV', &
                         'ATM from OCN','WAV from OCN','------------'],&
                        [ngc,ngc])

CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(ngc,ngc),PARAMETER ::       & 
expBundleName = RESHAPE(['----------','WAV to ATM','OCN to ATM', &
                         'ATM to WAV','----------','OCN to WAV', &
                         'ATM to OCN','WAV to OCN','----------'],&
                        [ngc,ngc])

TYPE(ESMF_State),      DIMENSION(1) :: stateList
TYPE(ESMF_FieldBundle),DIMENSION(1) :: fieldBundleList

! Fill in field names in this table whenever new fields are added to coupler communication:
CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(maxNumFields,ngc,ngc),PARAMETER :: impFieldName = RESHAPE(&
['------','------','------','------','------','------','------','------','------','------',& !    / 
 'u10   ','v10   ','rhoa  ','psim  ','rmol  ','------','------','------','------','------',& ! W from A
 'airtmp','vapmix','shwflx','radflx','precip','surtmp','wndspd','tauewd','taunwd','mslprs',& ! O from A
  
 'taux_A','tauy_A','ust   ','vst   ','------','------','------','------','------','------',& ! A from W
 '------','------','------','------','------','------','------','------','------','------',& !    / 
 'taux_O','tauy_O','------','------','------','------','------','------','------','------',& ! O from W

 'sst   ','ue    ','ve    ','------','------','------','------','------','------','------',& ! A from O
 'uc    ','vc    ','rhow  ','------','------','------','------','------','------','------',& ! W from O
 '------','------','------','------','------','------','------','------','------','------'],& !    /
[maxNumFields,ngc,ngc])

! Evaluate this at the beginning of main_driver:
CHARACTER(LEN=ESMF_MAXSTR),DIMENSION(maxNumFields,ngc,ngc) :: expFieldName

TYPE(farrayPtrType2DI4),DIMENSION(ngc) :: maskfarrayPtr

! Array pointers:
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:),  POINTER :: farrayPtr2DI4 => NULL()
INTEGER(KIND=ESMF_KIND_I4),DIMENSION(:,:,:),POINTER :: farrayPtr3DI4 => NULL()
REAL   (KIND=ESMF_KIND_R4),DIMENSION(:,:),  POINTER :: farrayPtr2DR4 => NULL()
REAL   (KIND=ESMF_KIND_R8),DIMENSION(:,:),  POINTER :: farrayPtr2DR8 => NULL()   
REAL   (KIND=ESMF_KIND_R4),DIMENSION(:,:,:),POINTER :: farrayPtr3DR4 => NULL()
REAL   (KIND=ESMF_KIND_R8),DIMENSION(:,:,:),POINTER :: farrayPtr3DR8 => NULL()   

!==============================================================================!
ENDMODULE UWIN_global
