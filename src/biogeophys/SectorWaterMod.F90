module SectorWaterMod

   #include "shr_assert.h"
   use shr_kind_mod     , only : r8 => shr_kind_r8
   use decompMod        , only : bounds_type, get_proc_global
   use decompMod        , only : subgrid_level_gridcell, subgrid_level_column, subgrid_level_patch
   use shr_log_mod      , only : errMsg => shr_log_errMsg
   use abortutils       , only : endrun
   use pftconMod        , only : pftcon
   use clm_varctl       , only : iulog
   use clm_varcon       , only : isecspday, denh2o, spval, ispval
   use clm_varpar       , only : nlevsoi, nlevgrnd
   use clm_time_manager , only : get_step_size, get_curr_date
   use SoilHydrologyMod , only : CalcSectorWaterWithdrawals
   use SoilHydrologyType, only : soilhydrology_type
   use SoilStateType    , only : soilstate_type
   use SoilWaterRetentionCurveMod, only : soil_water_retention_curve_type
   use WaterType        , only : water_type
   use WaterFluxBulkType, only : waterfluxbulk_type
   use WaterFluxType    , only : waterflux_type
   use WaterStateBulkType, only : waterstatebulk_type
   use WaterStateType   , only : waterstate_type
   use WaterTracerUtils , only : CalcTracerFromBulk, CalcTracerFromBulkFixedRatio
   use GridcellType     , only : grc
   use ColumnType       , only : col                
   use PatchType        , only : patch                
   use subgridAveMod    , only : p2c, c2g
   use filterColMod     , only : filter_col_type, col_filter_from_logical_array
   !

 implicit none
 private

 ! !PUBLIC TYPES:

 ! This type is public (and its components are public, too) to aid unit testing
 type, public :: sectorWater_params_type
      ! Time of day to start domestic and livestock water use, seconds (0 = midnight). 
      ! We start satisfying the demand in the time step FOLLOWING this time, 
      integer  :: dom_and_liv_start_time

      ! Time of day to start industrial (thermoelectric, manufacturing and mining) water use, seconds (0 = midnight). 
      ! We start satisfying the demand in the time step FOLLOWING this time, 
      integer  :: ind_start_time        

      ! Legth in seconds other which domestic and livestock water demand is satisfied. 
      ! Actual time may differ if this is not a multiple of dtime. 
      ! SectorWater module won't work properly if dtime > secsperday.
      integer  :: dom_and_liv_length

      ! Legth in seconds other which industrial water demand is satisfied. 
      integer  :: ind_length         

      ! Threshold for river water volume below which SectorWater usage is shut off, if
      ! limit_sectorWater is .true. (fraction of available river water). A threshold of 0
      ! means allow all river water to be used; a threshold of 0.1 means allow 90% of the
      ! river volume to be used; etc.
      real(r8) :: sectorWater_river_volume_threshold

      ! Whether sectorWater usage is limited based on river storage. This only applies if ROF is
      ! enabled (i.e., rof_prognostic is .true.) - otherwise we don't limit sectorWater usage,
      ! regardless of the value of this flag.
      logical :: limit_sectorWater_if_rof_enabled

      ! use groundwater supply for sectorWater usage (in addition to surface water)
      logical :: use_groundwater_sectorWater

end type sectorWater_params_type


type, public :: sectorWater_type
      private

      ! Private data members; set in initialization:
      type(sectorWater_params_type) :: params
      integer :: dtime                ! land model time step (sec)
      integer :: dom_and_liv_nsteps_per_day ! number of time steps per day in which we satisfy domestic and livestock demand
      integer :: ind_nsteps_per_day ! number of time steps per day in which we satisfy industrial demand (thermoelectric, manufacturing and mining)
   

      ! Private data members; time-varying:
      ! naming: dom = domestic, liv = livestock, elec = thermoelectric, mfc = manufacturing, min = mining
      ! naming: withd = withdrawal, cons = consumption, rf = return flow

      real(r8), pointer :: ann_dom_withd_grc       (:,:) ! annual expected withdrawal from input data
      real(r8), pointer :: ann_dom_cons_grc        (:,:) ! annual expected consumption from input data
      real(r8), pointer :: dom_withd_grc             (:) ! expected withdrawal flux for the day [mm/s]
      real(r8), pointer :: dom_cons_grc              (:) ! expected consumption flux for the day [mm/s]
      real(r8), pointer :: dom_withd_actual_grc      (:) ! actual withdrawal flux for the day [mm/s]
      real(r8), pointer :: dom_cons_actual_grc       (:) ! actual consumption flux for the day  [mm/s]
      real(r8), pointer :: dom_rf_actual_grc         (:) ! actual return flow flux for the day  [mm/s]

      real(r8), pointer :: ann_liv_withd_grc       (:,:) ! annual expected withdrawal from input data
      real(r8), pointer :: ann_liv_cons_grc        (:,:) ! annual expected consumption from input data
      real(r8), pointer :: liv_withd_grc             (:) ! expected withdrawal flux for the day [mm/s]
      real(r8), pointer :: liv_cons_grc              (:) ! expected consumption flux for the day [mm/s]
      real(r8), pointer :: liv_withd_actual_grc      (:) ! actual withdrawal flux for the day [mm/s]
      real(r8), pointer :: liv_cons_actual_grc       (:) ! actual consumption flux for the day  [mm/s]
      real(r8), pointer :: liv_rf_actual_grc         (:) ! actual return flow flux for the day  [mm/s]

      real(r8), pointer :: ann_elec_withd_grc       (:,:) ! annual expected withdrawal from input data
      real(r8), pointer :: ann_elec_cons_grc        (:,:) ! annual expected consumption from input data
      real(r8), pointer :: elec_withd_grc             (:) ! expected withdrawal flux for the day [mm/s]
      real(r8), pointer :: elec_cons_grc              (:) ! expected consumption flux for the day [mm/s]
      real(r8), pointer :: elec_withd_actual_grc      (:) ! actual withdrawal flux for the day [mm/s]
      real(r8), pointer :: elec_cons_actual_grc       (:) ! actual consumption flux for the day  [mm/s]
      real(r8), pointer :: elec_rf_actual_grc         (:) ! actual return flow flux for the day  [mm/s]

      real(r8), pointer :: ann_mfc_withd_grc       (:,:) ! annual expected withdrawal from input data
      real(r8), pointer :: ann_mfc_cons_grc        (:,:) ! annual expected consumption from input data
      real(r8), pointer :: mfc_withd_grc             (:) ! expected withdrawal flux for the day [mm/s]
      real(r8), pointer :: mfc_cons_grc              (:) ! expected consumption flux for the day [mm/s]
      real(r8), pointer :: mfc_withd_actual_grc      (:) ! actual withdrawal flux for the day [mm/s]
      real(r8), pointer :: mfc_cons_actual_grc       (:) ! actual consumption flux for the day  [mm/s]
      real(r8), pointer :: mfc_rf_actual_grc         (:) ! actual return flow flux for the day  [mm/s]

      real(r8), pointer :: ann_min_withd_grc       (:,:) ! annual expected withdrawal from input data
      real(r8), pointer :: ann_min_cons_grc        (:,:) ! annual expected consumption from input data
      real(r8), pointer :: min_withd_grc             (:) ! expected withdrawal flux for the day [mm/s]
      real(r8), pointer :: min_cons_grc              (:) ! expected consumption flux for the day [mm/s]
      real(r8), pointer :: min_withd_actual_grc      (:) ! actual withdrawal flux for the day [mm/s]
      real(r8), pointer :: min_cons_actual_grc       (:) ! actual consumption flux for the day  [mm/s]
      real(r8), pointer :: min_rf_actual_grc         (:) ! actual return flow flux for the day  [mm/s]

      integer , pointer :: n_dom_and_liv_steps_left_grc    (:) ! number of time steps for which we still need to satisfy domestic and livestock demand (if 0, ignore)
      integer , pointer :: n_ind_steps_left_grc    (:) ! number of time steps for which we still need to satisfy industrial demand (if 0, ignore)
      
      
   contains
      ! Public routines
      ! COMPILER_BUG(wjs, 2014-10-15, pgi 14.7) Add an "SectorWater" prefix to some  generic routines like "Init"
      ! (without this workaround, pgi compilation fails in restFileMod)
      procedure, public :: Init => SectorWaterInit
      procedure, public :: Restart
      procedure, public :: CalcSectorWaterFluxes
      procedure, public :: CalcSectorWaterNeeded
      procedure, public :: Clean => SectorWaterClean ! deallocate memory

      ! Private routines
      procedure, private :: ReadNamelist
      procedure, private :: CheckNamelistValidity   ! Check for validity of input parameters
      procedure, private :: InitAllocate => SectorWaterInitAllocate
      procedure, private :: InitHistory => SectorWaterInitHistory
      procedure, private :: InitCold => SectorWaterInitCold
      procedure, private :: CalcSectorWaterBulkWithdrawals      ! calculate sectoral water withdrawals for bulk water
      procedure, private :: CalcSectorWaterOneTracerWithdrawals ! calculate sectoral water withdrawals for one water tracer
      procedure, private :: CalcSectorWaterTotalGWUncon   ! calculate total sectoral water withdrawal flux from the unconfined aquifer, for either bulk or one water tracer
      procedure, private :: CalcSectorWaterApplicationFluxes    ! calculate sectoral water application fluxes for either bulk water or a single tracer
      procedure, private :: CalcNstepsPerDay_dom_and_liv    ! given dtime, calculate dom_and_liv_nsteps_per_day
      procedure, private :: CalcNstepsPerDay_ind    ! given dtime, calculate ind_nsteps_per_day
      procedure, private :: CalcSectorDemandVolrLimited   ! calculate demand limited by river volume for each patch
end type sectorWater_type

interface sectorWater_params_type
   module procedure sectorWater_params_constructor
end interface sectorWater_params_type

real(r8), parameter :: m3_over_km2_to_mm = 1.e-3_r8

character(len=*), parameter, private :: sourcefile = &
   __FILE__

contains

   ! ========================================================================
   ! Constructors
   ! ========================================================================

   !-----------------------------------------------------------------------
      function sectorWater_params_constructor(dom_and_liv_start_time, ind_start_time, &
         dom_and_liv_length, ind_length, sectorWater_river_volume_threshold, &
         limit_sectorWater_if_rof_enabled, use_groundwater_sectorWater) &
         result(this)
      !
      ! !DESCRIPTION:
      ! Create an sectorWater_params instance
      !
      ! !USES:
      !
      ! !ARGUMENTS:
      type(sectorWater_params_type) :: this  ! function result
      integer , intent(in) :: dom_and_liv_start_time
      integer , intent(in) :: ind_start_time
      integer , intent(in) :: dom_and_liv_length
      integer , intent(in) :: ind_length
      real(r8), intent(in) :: sectorWater_river_volume_threshold
      logical , intent(in) :: limit_sectorWater_if_rof_enabled
      logical , intent(in) :: use_groundwater_sectorWater
      !
      ! !LOCAL VARIABLES:
      
      character(len=*), parameter :: subname = 'sectorWater_params_constructor'
      !-----------------------------------------------------------------------

      this%dom_and_liv_start_time = dom_and_liv_start_time
      this%ind_start_time = ind_start_time
      this%dom_and_liv_length = dom_and_liv_length
      this%ind_length = ind_length
      this%sectorWater_river_volume_threshold = sectorWater_river_volume_threshold
      this%limit_sectorWater_if_rof_enabled = limit_sectorWater_if_rof_enabled
      this%use_groundwater_sectorWater = use_groundwater_sectorWater

   end function sectorWater_params_constructor

! ========================================================================
! Infrastructure routines (initialization, restart, etc.)
! ========================================================================

!------------------------------------------------------------------------
   subroutine SectorWaterInit(this, bounds, NLFilename, &
      use_aquifer_layer)

   class(sectorWater_type) , intent(inout) :: this
   type(bounds_type)      , intent(in)    :: bounds
   character(len=*)       , intent(in)    :: NLFilename ! Namelist filename
   logical                , intent(in)    :: use_aquifer_layer
   call this%ReadNamelist(NLFilename, use_aquifer_layer)
   call this%InitAllocate(bounds) ! whether an aquifer layer is used in this run

   call this%InitHistory(bounds)
   call this%InitCold(bounds)
 end subroutine SectorWaterInit

!-----------------------------------------------------------------------
 subroutine ReadNamelist(this, NLFilename, use_aquifer_layer)
   !
   ! !DESCRIPTION:
   ! Read the sectorWater namelist
   !
   ! !USES:
   use fileutils      , only : getavu, relavu, opnfil
   use shr_nl_mod     , only : shr_nl_find_group_name
   use spmdMod        , only : masterproc, mpicom
   use shr_mpi_mod    , only : shr_mpi_bcast
   use shr_infnan_mod , only : nan => shr_infnan_nan, assignment(=)
   !
   ! !ARGUMENTS:
   class(sectorWater_type) , intent(inout) :: this
   character(len=*), intent(in) :: NLFilename ! Namelist filename
   logical, intent(in) :: use_aquifer_layer    ! whether an aquifer layer is used in this run
   !
   ! !LOCAL VARIABLES:

   ! temporary variables corresponding to the components of sectorWater_params_type
   integer  :: dom_and_liv_start_time
   integer  :: ind_start_time
   integer  :: dom_and_liv_length
   integer  :: ind_length
   real(r8) :: sectorWater_river_volume_threshold
   logical  :: limit_sectorWater_if_rof_enabled
   logical  :: use_groundwater_sectorWater

   integer :: ierr                 ! error code
   integer :: unitn                ! unit for namelist file
   character(len=*), parameter :: nmlname = 'sectorWater_inparm'

   character(len=*), parameter :: subname = 'ReadNamelist'
   !-----------------------------------------------------------------------

   namelist /sectorWater_inparm/ dom_and_liv_start_time, ind_start_time, dom_and_liv_length, &
        ind_length, sectorWater_river_volume_threshold, limit_sectorWater_if_rof_enabled, &
        use_groundwater_sectorWater
   
   ! Initialize options to garbage defaults, forcing all to be specified explicitly in
   ! order to get reasonable results
   dom_and_liv_start_time = 0
   ind_start_time = 0
   dom_and_liv_length = 0
   ind_length = 0
   sectorWater_river_volume_threshold = nan
   limit_sectorWater_if_rof_enabled = .false.
   use_groundwater_sectorWater = .false.

   if (masterproc) then
      unitn = getavu()
      write(iulog,*) 'Read in '//nmlname//'  namelist'
      call opnfil (NLFilename, unitn, 'F')
      call shr_nl_find_group_name(unitn, nmlname, status=ierr)
      if (ierr == 0) then
         read(unitn, nml=sectorWater_inparm, iostat=ierr)
         if (ierr /= 0) then
            call endrun(msg="ERROR reading "//nmlname//"namelist"//errmsg(sourcefile, __LINE__))
         end if
      else
         call endrun(msg="ERROR could NOT find "//nmlname//"namelist"//errmsg(sourcefile, __LINE__))
      end if
      call relavu( unitn )
   end if

   call shr_mpi_bcast(dom_and_liv_start_time, mpicom)
   call shr_mpi_bcast(ind_start_time, mpicom)
   call shr_mpi_bcast(dom_and_liv_length, mpicom)
   call shr_mpi_bcast(ind_length, mpicom)
   call shr_mpi_bcast(sectorWater_river_volume_threshold, mpicom)
   call shr_mpi_bcast(limit_sectorWater_if_rof_enabled, mpicom)
   call shr_mpi_bcast(use_groundwater_sectorWater, mpicom)
 
   this%params = sectorWater_params_type( &
        dom_and_liv_start_time = dom_and_liv_start_time, &
        ind_start_time = ind_start_time, &
        dom_and_liv_length = dom_and_liv_length, &
        ind_length = ind_length, &
        sectorWater_river_volume_threshold = sectorWater_river_volume_threshold, &
        limit_sectorWater_if_rof_enabled = limit_sectorWater_if_rof_enabled, &
        use_groundwater_sectorWater = use_groundwater_sectorWater)

   if (masterproc) then
      write(iulog,*) ' '
      write(iulog,*) nmlname//' settings:'
      ! Write settings one-by-one rather than with a nml write because
      ! sectorWater_river_volume_threshold may be NaN
      write(iulog,*) 'dom_and_liv_start_time = ', dom_and_liv_start_time
      write(iulog,*) 'ind_start_time = ', ind_start_time
      write(iulog,*) 'dom_and_liv_length = ', dom_and_liv_length
      write(iulog,*) 'ind_length = ', ind_length
      write(iulog,*) 'limit_sectorWater_if_rof_enabled = ', limit_sectorWater_if_rof_enabled
      if (limit_sectorWater_if_rof_enabled) then
         write(iulog,*) 'sectorWater_river_volume_threshold = ', sectorWater_river_volume_threshold
      end if
      write(iulog,*) 'use_groundwater_sectorWater = ', use_groundwater_sectorWater
      write(iulog,*) ' '

      call this%CheckNamelistValidity(use_aquifer_layer)
   end if
 end subroutine ReadNamelist

!-----------------------------------------------------------------------
 subroutine CheckNamelistValidity(this, use_aquifer_layer)
   !
   ! !DESCRIPTION:
   ! Check for validity of input parameters.
   !
   ! Assumes that the inputs have already been packed into 'this%params'.
   !
   ! Only needs to be called by the master task, since parameters are the same for all
   ! tasks.
   !
   ! !ARGUMENTS:
   class(sectorWater_type), intent(in) :: this
   logical, intent(in) :: use_aquifer_layer    ! whether an aquifer layer is used in this run
   !
   ! !LOCAL VARIABLES:

   character(len=*), parameter :: subname = 'CheckNamelistValidity'
   !-----------------------------------------------------------------------

   associate( &
        dom_and_liv_start_time => this%params%dom_and_liv_start_time, &
        ind_start_time => this%params%ind_start_time, &
        dom_and_liv_length => this%params%dom_and_liv_length, &
        ind_length => this%params%ind_length, &
        sectorWater_river_volume_threshold => this%params%sectorWater_river_volume_threshold, &
        use_groundwater_sectorWater => this%params%use_groundwater_sectorWater, &
        limit_sectorWater_if_rof_enabled => this%params%limit_sectorWater_if_rof_enabled)

   if (dom_and_liv_start_time < 0 .or. dom_and_liv_start_time >= isecspday) then
      write(iulog,*) ' ERROR: dom_and_liv_start_time must be >= 0 and < ', isecspday
      write(iulog,*) ' dom_and_liv_start_time = ', dom_and_liv_start_time
      call endrun(msg=' ERROR: dom_and_liv_start_time out of bounds ' // errMsg(sourcefile, __LINE__))
   end if

   if (ind_start_time < 0 .or. ind_start_time >= isecspday) then
      write(iulog,*) ' ERROR: ind_start_time must be >= 0 and < ', isecspday
      write(iulog,*) ' ind_start_time = ', ind_start_time
      call endrun(msg=' ERROR: ind_start_time out of bounds ' // errMsg(sourcefile, __LINE__))
   end if

   if (dom_and_liv_length <= 0 .or. dom_and_liv_length > isecspday) then
      write(iulog,*) ' ERROR: dom_and_liv_length must be > 0 and <= ', isecspday
      write(iulog,*) ' dom_and_liv_length = ', dom_and_liv_length
      call endrun(msg=' ERROR: dom_and_liv_length out of bounds ' // errMsg(sourcefile, __LINE__))
   end if

   if (ind_length <= 0 .or. ind_length > isecspday) then
      write(iulog,*) ' ERROR: ind_length must be > 0 and <= ', isecspday
      write(iulog,*) ' ind_length = ', ind_length
      call endrun(msg=' ERROR: ind_length out of bounds ' // errMsg(sourcefile, __LINE__))
   end if

   if (limit_sectorWater_if_rof_enabled) then
      if (sectorWater_river_volume_threshold < 0._r8 .or. sectorWater_river_volume_threshold > 1._r8) then
         write(iulog,*) ' ERROR: sectorWater_river_volume_threshold must be between 0 and 1'
         write(iulog,*) ' sectorWater_river_volume_threshold = ', sectorWater_river_volume_threshold
         call endrun(msg=' ERROR: sectorWater_river_volume_threshold must be between 0 and 1 ' // &
              errMsg(sourcefile, __LINE__))
      end if
   end if

   if (use_groundwater_sectorWater .and. .not. limit_sectorWater_if_rof_enabled) then
      write(iulog,*) ' ERROR: use_groundwater_sectorWater only makes sense if limit_sectorWater_if_rof_enabled is set.'
      write(iulog,*) '(If limit_sectorWater_if_rof_enabled is .false., then groundwater extraction will never be invoked.)'
      call endrun(msg=' ERROR: use_groundwater_sectorWater only makes sense if limit_sectorWater_if_rof_enabled is set' // &
           errMsg(sourcefile, __LINE__))
   end if

   if (use_aquifer_layer .and. use_groundwater_sectorWater) then
         write(iulog,*) ' ERROR: use_groundwater_sectorWater and use_aquifer_layer may not be used simultaneously'
         call endrun(msg=' ERROR: use_groundwater_sectorWater and use_aquifer_layer cannot both be set to true' // &
              errMsg(sourcefile, __LINE__))
   end if

   end associate

 end subroutine CheckNamelistValidity
 
!-----------------------------------------------------------------------
 subroutine SectorWaterInitAllocate(this, bounds)
   !
   ! !DESCRIPTION:
   ! Initialize sector water data structure
   !
   ! !USES:
   use shr_infnan_mod , only : nan => shr_infnan_nan, assignment(=)
   !
   ! !ARGUMENTS:
   class(sectorWater_type) , intent(inout) :: this
   type(bounds_type)      , intent(in)    :: bounds
   !
   ! !LOCAL VARIABLES:
   integer :: begg, endg

   character(len=*), parameter :: subname = 'InitAllocate'
   !-----------------------------------------------------------------------

   begg = bounds%begg; endg= bounds%endg   

   allocate(this%ann_dom_withd_grc (begg:endg))                  ; this%ann_dom_withd_grc      (:)   = nan
   allocate(this%ann_dom_cons_grc (begg:endg))                   ; this%ann_dom_cons_grc       (:)   = nan
   allocate(this%dom_withd_grc (begg:endg))                      ; this%dom_withd_grc          (:)   = nan
   allocate(this%dom_cons_grc (begg:endg))                       ; this%dom_cons_grc           (:)   = nan
   allocate(this%dom_withd_actual_grc (begg:endg))               ; this%dom_withd_actual_grc   (:)   = nan
   allocate(this%dom_cons_actual_grc (begg:endg))                ; this%dom_cons_actual_grc    (:)   = nan
   allocate(this%dom_rf_actual_grc (begg:endg))                  ; this%dom_rf_actual_grc      (:)   = nan

   allocate(this%ann_liv_withd_grc (begg:endg))                  ; this%ann_liv_withd_grc      (:)   = nan
   allocate(this%ann_liv_cons_grc (begg:endg))                   ; this%ann_liv_cons_grc       (:)   = nan
   allocate(this%liv_withd_grc (begg:endg))                      ; this%liv_withd_grc          (:)   = nan
   allocate(this%liv_cons_grc (begg:endg))                       ; this%liv_cons_grc           (:)   = nan
   allocate(this%liv_withd_actual_grc (begg:endg))               ; this%liv_withd_actual_grc   (:)   = nan
   allocate(this%liv_cons_actual_grc (begg:endg))                ; this%liv_cons_actual_grc    (:)   = nan
   allocate(this%liv_rf_actual_grc (begg:endg))                  ; this%liv_rf_actual_grc      (:)   = nan

   allocate(this%ann_elec_withd_grc (begg:endg))                 ; this%ann_elec_withd_grc      (:)   = nan
   allocate(this%ann_elec_cons_grc (begg:endg))                  ; this%ann_elec_cons_grc       (:)   = nan
   allocate(this%elec_withd_grc (begg:endg))                     ; this%elec_withd_grc         (:)   = nan
   allocate(this%elec_cons_grc (begg:endg))                      ; this%elec_cons_grc          (:)   = nan
   allocate(this%elec_withd_actual_grc (begg:endg))              ; this%elec_withd_actual_grc  (:)   = nan
   allocate(this%elec_cons_actual_grc (begg:endg))               ; this%elec_cons_actual_grc   (:)   = nan
   allocate(this%elec_rf_actual_grc (begg:endg))                 ; this%elec_rf_actual_grc     (:)   = nan

   allocate(this%ann_mfc_withd_grc (begg:endg))                  ; this%ann_mfc_withd_grc      (:)   = nan
   allocate(this%ann_mfc_cons_grc (begg:endg))                   ; this%ann_mfc_cons_grc       (:)   = nan
   allocate(this%mfc_withd_grc (begg:endg))                      ; this%mfc_withd_grc          (:)   = nan
   allocate(this%mfc_cons_grc (begg:endg))                       ; this%mfc_cons_grc           (:)   = nan
   allocate(this%mfc_withd_actual_grc (begg:endg))               ; this%mfc_withd_actual_grc   (:)   = nan
   allocate(this%mfc_cons_actual_grc (begg:endg))                ; this%mfc_cons_actual_grc    (:)   = nan
   allocate(this%mfc_rf_actual_grc (begg:endg))                  ; this%mfc_rf_actual_grc      (:)   = nan

   allocate(this%ann_min_withd_grc (begg:endg))                  ; this%ann_min_withd_grc      (:)   = nan
   allocate(this%ann_min_cons_grc (begg:endg))                   ; this%ann_min_cons_grc       (:)   = nan
   allocate(this%min_withd_grc (begg:endg))                      ; this%min_withd_grc          (:)   = nan
   allocate(this%min_cons_grc (begg:endg))                       ; this%min_cons_grc           (:)   = nan
   allocate(this%min_withd_actual_grc (begg:endg))               ; this%min_withd_actual_grc   (:)   = nan
   allocate(this%min_cons_actual_grc (begg:endg))                ; this%min_cons_actual_grc    (:)   = nan
   allocate(this%min_rf_actual_grc (begg:endg))                  ; this%min_rf_actual_grc      (:)   = nan

   allocate(this%n_dom_and_liv_steps_left_grc    (begg:endg))  ; this%n_dom_and_liv_steps_left_grc     (:)   = 0
   allocate(this%n_ind_steps_left_grc    (begg:endg))          ; this%n_ind_steps_left_grc             (:)   = 0

 end subroutine SectorWaterInitAllocate


!-----------------------------------------------------------------------
 subroutine SectorWaterInitHistory(this, bounds)
   !
   ! !DESCRIPTION:
   ! Initialize sectoral water use history fields
   !
   ! !USES:
   use histFileMod  , only : hist_addfld1d
   !
   ! !ARGUMENTS:
   class(sectorWater_type) , intent(inout) :: this
   type(bounds_type)      , intent(in)    :: bounds
   !
   ! !LOCAL VARIABLES:
   integer :: begg, endg
   
   character(len=*), parameter :: subname = 'InitHistory'
   !-----------------------------------------------------------------------

   begg = bounds%begg; endg= bounds%endg

   this%dom_withd_actual_grc(begg:endg) = spval
   call hist_addfld1d (fname='DOM_ACTUAL_WITHD', units='mm/s', &
        avgflag='A', long_name='domestic actual withdrawal flux', &
        ptr_patch=this%dom_withd_actual_grc, default='inactive')

   this%liv_withd_actual_grc(begg:endg) = spval
   call hist_addfld1d (fname='LIV_ACTUAL_WITHD', units='mm/s', &
        avgflag='A', long_name='livestock actual withdrawal flux', &
        ptr_patch=this%liv_withd_actual_grc, default='inactive')
   
   this%elec_withd_actual_grc(begg:endg) = spval
   call hist_addfld1d (fname='ELEC_ACTUAL_WITHD', units='mm/s', &
        avgflag='A', long_name='thermoelectric actual withdrawal flux', &
        ptr_patch=this%elec_withd_actual_grc, default='inactive')      

   this%mfc_withd_actual_grc(begg:endg) = spval
   call hist_addfld1d (fname='MFC_ACTUAL_WITHD', units='mm/s', &
        avgflag='A', long_name='manufacturing actual withdrawal flux', &
        ptr_patch=this%mfc_withd_actual_grc, default='inactive')

   this%min_withd_actual_grc(begg:endg) = spval
   call hist_addfld1d (fname='MIN_ACTUAL_WITHD', units='mm/s', &
        avgflag='A', long_name='mining actual withdrawal flux', &
        ptr_patch=this%min_withd_actual_grc, default='inactive')

 end subroutine SectorWaterInitHistory

!-----------------------------------------------------------------------
 subroutine SectorWaterInitCold(this, bounds)
   !
   ! !DESCRIPTION:
   ! Do cold-start initialization for irrigation data structure
   !
   ! !ARGUMENTS:
   class(sectorWater_type) , intent(inout) :: this
   type(bounds_type)      , intent(in)    :: bounds

   character(len=*), parameter :: subname = 'InitCold'
   !-----------------------------------------------------------------------

   this%dtime = get_step_size()
   this%dom_and_liv_nsteps_per_day = this%Calc_dom_and_liv_NstepsPerDay(this%dtime)
   this%ind_nsteps_per_day = this%Calc_ind_NstepsPerDay(this%dtime)

   this%dom_withd_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%liv_withd_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%elec_withd_actual_grc(bounds%begg:bounds%endg) = 0._r8
   this%mfc_withd_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%min_withd_actual_grc(bounds%begg:bounds%endg)  = 0._r8

   this%dom_cons_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%liv_cons_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%elec_cons_actual_grc(bounds%begg:bounds%endg) = 0._r8
   this%mfc_cons_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%min_cons_actual_grc(bounds%begg:bounds%endg)  = 0._r8

   this%dom_rf_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%liv_rf_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%elec_rf_actual_grc(bounds%begg:bounds%endg) = 0._r8
   this%mfc_rf_actual_grc(bounds%begg:bounds%endg)  = 0._r8
   this%min_rf_actual_grc(bounds%begg:bounds%endg)  = 0._r8

 end subroutine SectorWaterInitCold

!-----------------------------------------------------------------------
 pure function Calc_dom_and_liv_NstepsPerDay(this, dtime) result(dom_and_liv_nsteps_per_day)
 !
 ! !DESCRIPTION:
 ! Given dtime (sec), determine number of steps per day to satisfy demand for domestic and livestock sectors
 !
 ! !USES:
 !
 ! !ARGUMENTS:
 integer :: dom_and_liv_nsteps_per_day  ! function result
 class(sectorWater_type) , intent(in) :: this
 integer                , intent(in) :: dtime ! model time step (sec)
 !
 ! !LOCAL VARIABLES:
 
 character(len=*), parameter :: subname = 'Calc_dom_and_liv_NstepsPerDay'
 !-----------------------------------------------------------------------
 
 dom_and_liv_nsteps_per_day = ((this%params%dom_and_liv_length + (dtime - 1))/dtime)  ! round up

end function Calc_dom_and_liv_NstepsPerDay


 !-----------------------------------------------------------------------
pure function Calc_ind_NstepsPerDay(this, dtime) result(ind_nsteps_per_day)
 !
 ! !DESCRIPTION:
 ! Given dtime (sec), determine number of steps per day to satisfy demand for industrial sectors
 !
 ! !USES:
 !
 ! !ARGUMENTS:
 integer :: ind_nsteps_per_day  ! function result
 class(sectorWater_type) , intent(in) :: this
 integer                , intent(in) :: dtime ! model time step (sec)
 !
 ! !LOCAL VARIABLES:
 
 character(len=*), parameter :: subname = 'Calc_ind_NstepsPerDay'
 !-----------------------------------------------------------------------
 
 ind_nsteps_per_day = ((this%params%ind_length + (dtime - 1))/dtime)  ! round up

end function Calc_ind_NstepsPerDay

!-----------------------------------------------------------------------
subroutine SectorWaterClean(this)
!
! !DESCRIPTION:
! Deallocate memory
!
! !ARGUMENTS:
class(sectorWater_type), intent(inout) :: this
!
! !LOCAL VARIABLES:

character(len=*), parameter :: subname = 'Clean'
!-----------------------------------------------------------------------

deallocate(this%ann_dom_withd_grc)
deallocate(this%ann_dom_cons_grc)
deallocate(this%dom_withd_grc)
deallocate(this%dom_cons_grc)
deallocate(this%dom_withd_actual_grc)
deallocate(this%dom_cons_actual_grc)
deallocate(this%dom_rf_actual_grc)

deallocate(this%ann_liv_withd_grc)
deallocate(this%ann_liv_cons_grc)
deallocate(this%liv_withd_grc)
deallocate(this%liv_cons_grc)
deallocate(this%liv_withd_actual_grc)
deallocate(this%liv_cons_actual_grc)
deallocate(this%liv_rf_actual_grc)

deallocate(this%ann_elec_withd_grc)
deallocate(this%ann_elec_cons_grc)
deallocate(this%elec_withd_grc)
deallocate(this%elec_cons_grc)
deallocate(this%elec_withd_actual_grc)
deallocate(this%elec_cons_actual_grc)
deallocate(this%elec_rf_actual_grc)

deallocate(this%ann_mfc_withd_grc)
deallocate(this%ann_mfc_cons_grc)
deallocate(this%mfc_withd_grc)
deallocate(this%mfc_cons_grc)
deallocate(this%mfc_withd_actual_grc)
deallocate(this%mfc_cons_actual_grc)
deallocate(this%mfc_rf_actual_grc)

deallocate(this%ann_min_withd_grc)
deallocate(this%ann_min_cons_grc)
deallocate(this%min_withd_grc)
deallocate(this%min_cons_grc)
deallocate(this%min_withd_actual_grc)
deallocate(this%min_cons_actual_grc)
deallocate(this%min_rf_actual_grc)


deallocate(this%n_dom_and_liv_steps_left_grc)
deallocate(this%n_ind_steps_left_grc)

end subroutine sectorWaterClean


! ========================================================================
! Science routines
! ========================================================================

  !==============================================================================
subroutine readSectorWaterData (bounds, sectorwater_inst)
   !
   ! !DESCRIPTION:
   ! read 12 months of sectoral water usage data
   !
   ! !USES:
   use fileutils       , only : getfil
   use clm_varctl      , only : fsurdat
   use domainMod       , only : ldomain
   use clm_varcon      , only : grlnd
   
   ! !ARGUMENTS:
   type(bounds_type), intent(in) :: bounds
   type(sectorWater_type), intent(inout) :: sectorwater_inst
   !
   ! !LOCAL VARIABLES:
   type(file_desc_t) :: ncid             ! netcdf id
   real(r8), pointer :: ann_dom_withd(:,:) ! 12 months of monthly withdrawals from input data set
   real(r8), pointer :: ann_dom_cons(:,:) ! 12 months of monthly consumption from input data set
   real(r8), pointer :: ann_liv_withd(:,:) ! 12 months of monthly withdrawals from input data set
   real(r8), pointer :: ann_liv_cons(:,:) ! 12 months of monthly consumption from input data set
   real(r8), pointer :: ann_elec_withd(:,:) ! 12 months of monthly withdrawals from input data set
   real(r8), pointer :: ann_elec_cons(:,:) ! 12 months of monthly consumption from input data set
   real(r8), pointer :: ann_mfc_withd(:,:) ! 12 months of monthly withdrawals from input data set
   real(r8), pointer :: ann_mfc_cons(:,:) ! 12 months of monthly consumption from input data set
   real(r8), pointer :: ann_min_withd(:,:) ! 12 months of monthly withdrawals from input data set
   real(r8), pointer :: ann_min_cons(:,:) ! 12 months of monthly consumption from input data set
   
   integer :: ier                        ! error code
   integer :: ni,nj,ns                   ! indices
   integer :: dimid,varid                ! input netCDF id's
   integer :: ntim                       ! number of input data time samples
   integer :: nlon_i                     ! number of input data longitudes
   integer :: nlat_i                     ! number of input data latitudes
   logical :: isgrid2d                   ! true => file is 2d
   character(len=256) :: locfn           ! local file name
   character(len=32) :: subname = 'readSectorWaterData'
   !-----------------------------------------------------------------------

   ann_dom_withd    => sectorwater_inst%ann_dom_withd_grc
   ann_dom_cons     => sectorwater_inst%ann_dom_cons_grc

   ann_liv_withd    => sectorwater_inst%ann_liv_withd_grc
   ann_liv_cons     => sectorwater_inst%ann_liv_cons_grc

   ann_elec_withd    => sectorwater_inst%ann_elec_withd_grc
   ann_elec_cons     => sectorwater_inst%ann_elec_cons_grc

   ann_mfc_withd    => sectorwater_inst%ann_mfc_withd_grc
   ann_mfc_cons     => sectorwater_inst%ann_mfc_cons_grc

   ann_min_withd    => sectorwater_inst%ann_min_withd_grc
   ann_min_cons     => sectorwater_inst%ann_min_cons_grc


   if (masterproc) then
      write (iulog,*) 'Attempting to read annual sectoral water usage data .....'
   end if

   ! Determine necessary indices
   call getfil(fsurdat, locfn, 0)
   call ncd_pio_openfile (ncid, trim(locfn), 0)
   call ncd_inqfdims (ncid, isgrid2d, ni, nj, ns)

   if (ldomain%ns /= ns .or. ldomain%ni /= ni .or. ldomain%nj /= nj) then
      write(iulog,*)trim(subname), 'ldomain and input file do not match dims '
      write(iulog,*)trim(subname), 'ldomain%ni,ni,= ',ldomain%ni,ni
      write(iulog,*)trim(subname), 'ldomain%nj,nj,= ',ldomain%nj,nj
      write(iulog,*)trim(subname), 'ldomain%ns,ns,= ',ldomain%ns,ns
      call endrun(msg=errMsg(sourcefile, __LINE__))
   end if

   call ncd_io(ncid=ncid, varname='withd_dom', flag='read', data=ann_dom_withd, &
           dim1name=grlnd)
   call ncd_io(ncid=ncid, varname='cons_dom', flag='read', data=ann_dom_cons, &
           dim1name=grlnd)

   call ncd_io(ncid=ncid, varname='withd_liv', flag='read', data=ann_liv_withd, &
           dim1name=grlnd)
   call ncd_io(ncid=ncid, varname='cons_liv', flag='read', data=ann_liv_cons, &
           dim1name=grlnd)

   call ncd_io(ncid=ncid, varname='withd_elec', flag='read', data=ann_elec_withd, &
           dim1name=grlnd)
   call ncd_io(ncid=ncid, varname='cons_elec', flag='read', data=ann_elc_cons, &
           dim1name=grlnd)     
           
   call ncd_io(ncid=ncid, varname='withd_mfc', flag='read', data=ann_mfc_withd, &
           dim1name=grlnd)
   call ncd_io(ncid=ncid, varname='cons_mfc', flag='read', data=ann_mfc_cons, &
           dim1name=grlnd)

   call ncd_io(ncid=ncid, varname='withd_min', flag='read', data=ann_min_withd, &
           dim1name=grlnd)
   call ncd_io(ncid=ncid, varname='cons_min', flag='read', data=ann_min_cons, &
           dim1name=grlnd)

   call ncd_pio_closefile(ncid)

endsubroutine readSectorWaterData

subroutine CalcSectorWaterNeeded(this, bounds, volr, rof_prognostic)

use shr_const_mod      , only : SHR_CONST_TKFRZ
use clm_time_manager , only : get_curr_date, is_end_curr_month, get_curr_days_per_year
!
! !ARGUMENTS:
class(sectorWater_type) , intent(inout) :: this
type(bounds_type)      , intent(in)    :: bounds

! river water volume (m3) (ignored if rof_prognostic is .false.)
real(r8), intent(in) :: volr( bounds%begg: )

! whether we're running with a prognostic ROF component; this is needed to determine
! whether we can limit demand based on river volume.
logical, intent(in) :: rof_prognostic

!
! !LOCAL VARIABLES:
integer :: g    ! gridcell index
integer :: year    ! year (0, ...) for nstep+1
integer :: mon     ! month (1, ..., 12) for nstep+1
integer :: day     ! day of month (1, ..., 31) for nstep+1
integer :: sec     ! seconds into current date for nstep+1
real(r8) :: dayspyr ! days per year
real(r8) :: dayspm   ! days per month
real(r8) :: secs_per_day   ! seconds per day
real(r8) :: dom_and_liv_flux_factor   ! factor to transform the demand from mm per month to mm/s for the given day
real(r8) :: ind_flux_factor   ! factor to transform the demand from mm per month to mm/s for the given day


real(r8) :: dom_demand(bounds%begg:bounds%endg)
real(r8) :: dom_demand_volr_limited(bounds%begg:bounds%endg)

real(r8) :: dom_consumption(bounds%begg:bounds%endg)
real(r8) :: dom_consumption_volr_limited(bounds%begg:bounds%endg)

real(r8) :: liv_demand(bounds%begg:bounds%endg)
real(r8) :: liv_demand_volr_limited(bounds%begg:bounds%endg)

real(r8) :: liv_consumption(bounds%begg:bounds%endg)
real(r8) :: liv_consumption_volr_limited(bounds%begg:bounds%endg)

real(r8) :: elec_demand(bounds%begg:bounds%endg)
real(r8) :: elec_demand_volr_limited(bounds%begg:bounds%endg)

real(r8) :: elec_consumption(bounds%begg:bounds%endg)
real(r8) :: elec_consumption_volr_limited(bounds%begg:bounds%endg)

real(r8) :: mfc_demand(bounds%begg:bounds%endg)
real(r8) :: mfc_demand_volr_limited(bounds%begg:bounds%endg)

real(r8) :: mfc_consumption(bounds%begg:bounds%endg)
real(r8) :: mfc_consumption_volr_limited(bounds%begg:bounds%endg)

real(r8) :: min_demand(bounds%begg:bounds%endg)
real(r8) :: min_demand_volr_limited(bounds%begg:bounds%endg)

real(r8) :: min_consumption(bounds%begg:bounds%endg)
real(r8) :: min_consumption_volr_limited(bounds%begg:bounds%endg)

! Whether we should limit deficits by available volr
logical :: limit_sectorwater

character(len=*), parameter :: subname = 'CalcSectorWaterNeeded'
!-----------------------------------------------------------------------

! Enforce expected array sizes
SHR_ASSERT_ALL_FL((ubound(volr) == (/bounds%endg/)), sourcefile, __LINE__)

! Get current date
call get_curr_date(year, mon, day, sec)
dayspyr = get_curr_days_per_year()
dayspm  = dayspyr/12_r8
!secs_per_day = 24_r8*3600_r8
!dom_and_liv_flux_factor = ((1_r8/dayspm)/secs_per_day)
!ind_flux_factor = ((1_r8/dayspm)/secs_per_day)
dom_and_liv_flux_factor = ((1_r8/dayspm)/this%params%dom_and_liv_length)
ind_flux_factor = ((1_r8/dayspm)/this%params%ind_length)

! Compute demand [mm]
! First initialize demand to 0 everywhere;
dom_demand(bounds%begg:bounds%endg) = 0._r8
dom_consumption(bounds%begg:bounds%endg) = 0._r8

liv_demand(bounds%begg:bounds%endg) = 0._r8
liv_consumption(bounds%begg:bounds%endg) = 0._r8

elec_demand(bounds%begg:bounds%endg) = 0._r8
elec_consumption(bounds%begg:bounds%endg) = 0._r8

mfc_demand(bounds%begg:bounds%endg) = 0._r8
mfc_consumption(bounds%begg:bounds%endg) = 0._r8

min_demand(bounds%begg:bounds%endg) = 0._r8
min_consumption(bounds%begg:bounds%endg) = 0._r8

do g = bounds%begg,bounds%endg
   dom_demand(g) = this%ann_dom_withd_grc(g,mon)
   dom_consumption(g) = this%ann_dom_cons_grc(g,mon)

   liv_demand(g) = this%ann_liv_withd_grc(g,mon)
   liv_consumption(g) = this%ann_liv_cons_grc(g,mon)
   
   elec_demand(g) = this%ann_elec_withd_grc(g,mon)
   elec_consumption(g) = this%ann_elec_cons_grc(g,mon)

   mfc_demand(g) = this%ann_mfc_withd_grc(g,mon)
   mfc_consumption(g) = this%ann_mfc_cons_grc(g,mon)

   min_demand(g) = this%ann_min_withd_grc(g,mon)
   min_consumption(g) = this%ann_min_cons_grc(g,mon)

end do ! end loop over gridcels

! Limit deficits by available volr, if desired. Note that we cannot do this limiting
! if running without a prognostic river model, since we need river volume for this
! limiting.
!
! NOTE(wjs, 2016-11-22) In principle we could base this on rof_present rather than
! rof_prognostic, but that would depend on the data runoff (drof) model sending river
! volume, which it currently does not.
limit_sectorwater = (this%params%limit_sectorWater_if_rof_enabled .and. rof_prognostic)
if (limit_sectorwater) then
   call this%CalcSectorDemandVolrLimited( &
        bounds = bounds, &
        dom_demand = dom_demand(bounds%begg:bounds%endg), &
        dom_consumption = dom_consumption(bounds%begg:bounds%endg), &
        liv_demand = liv_demand(bounds%begg:bounds%endg), &
        liv_consumption = liv_consumption(bounds%begg:bounds%endg), &
        elec_demand = elec_demand(bounds%begg:bounds%endg), &
        elec_consumption = elec_consumption(bounds%begg:bounds%endg), &
        mfc_demand = mfc_demand(bounds%begg:bounds%endg), &
        mfc_consumption = mfc_consumption(bounds%begg:bounds%endg), &
        min_demand = min_demand(bounds%begg:bounds%endg), &
        min_consumption = min_consumption(bounds%begg:bounds%endg), &
        volr = volr(bounds%begg:bounds%endg), &
        dom_demand_volr_limited = dom_demand_volr_limited(bounds%begg:bounds%endg), &
        dom_consumption_volr_limited = dom_consumption_volr_limited(bounds%begg:bounds%endg), &
        liv_demand_volr_limited = liv_demand_volr_limited(bounds%begg:bounds%endg), &
        liv_consumption_volr_limited = liv_consumption_volr_limited(bounds%begg:bounds%endg), &
        elec_demand_volr_limited = elec_demand_volr_limited(bounds%begg:bounds%endg), &
        elec_consumption_volr_limited = elec_consumption_volr_limited(bounds%begg:bounds%endg), &
        mfc_demand_volr_limited = mfc_demand_volr_limited(bounds%begg:bounds%endg), &
        mfc_consumption_volr_limited = mfc_consumption_volr_limited(bounds%begg:bounds%endg), &
        min_demand_volr_limited = min_demand_volr_limited(bounds%begg:bounds%endg), &
        min_consumption_volr_limited = min_consumption_volr_limited(bounds%begg:bounds%endg))
else
   dom_demand_volr_limited(bounds%begg:bounds%endg) = dom_demand(bounds%begg:bounds%endg)
   dom_consumption_volr_limited(bounds%begg:bounds%endg) = dom_consumption(bounds%begg:bounds%endg)

   liv_demand_volr_limited(bounds%begg:bounds%endg) = liv_demand(bounds%begg:bounds%endg)
   liv_consumption_volr_limited(bounds%begg:bounds%endg) = liv_consumption(bounds%begg:bounds%endg)

   elec_demand_volr_limited(bounds%begg:bounds%endg) = elec_demand(bounds%begg:bounds%endg)
   elec_consumption_volr_limited(bounds%begg:bounds%endg) = elec_consumption(bounds%begg:bounds%endg)

   mfc_demand_volr_limited(bounds%begg:bounds%endg) = mfc_demand(bounds%begg:bounds%endg)
   mfc_consumption_volr_limited(bounds%begg:bounds%endg) = mfc_consumption(bounds%begg:bounds%endg)

   min_demand_volr_limited(bounds%begg:bounds%endg) = min_demand(bounds%begg:bounds%endg)
   min_consumption_volr_limited(bounds%begg:bounds%endg) = min_consumption(bounds%begg:bounds%endg)
end if

! Convert demand to withdrawal rates [mm/s]
do g = bounds%begg,bounds%endg
   ! Domestic
   this%dom_withd_grc = dom_demand*dom_and_liv_flux_factor
   this%dom_withd_actual_grc = dom_demand_volr_limited*dom_and_liv_flux_factor

   this%dom_cons_grc = dom_consumption*dom_and_liv_flux_factor
   this%dom_cons_actual_grc = dom_consumption_volr_limited*dom_and_liv_flux_factor
   
   this%dom_rf_actual_grc = this%dom_withd_actual_grc - this%dom_cons_actual_grc

   ! Livestock
   this%liv_withd_grc = liv_demand*dom_and_liv_flux_factor
   this%liv_withd_actual_grc = liv_demand_volr_limited*dom_and_liv_flux_factor

   this%liv_cons_grc = liv_consumption*dom_and_liv_flux_factor
   this%liv_cons_actual_grc = liv_consumption_volr_limited*dom_and_liv_flux_factor
   
   this%liv_rf_actual_grc = this%liv_withd_actual_grc - this%liv_cons_actual_grc

   ! Thermoelectric
   this%elec_withd_grc = elec_demand*ind_flux_factor
   this%elec_withd_actual_grc = elec_demand_volr_limited*ind_flux_factor

   this%elec_cons_grc = elec_consumption*ind_flux_factor
   this%elec_cons_actual_grc = elec_consumption_volr_limited*ind_flux_factor
   
   this%elec_rf_actual_grc = this%elec_withd_actual_grc - this%elec_cons_actual_grc

   ! Manufacturing
   this%mfc_withd_grc = mfc_demand*ind_flux_factor
   this%mfc_withd_actual_grc = mfc_demand_volr_limited*ind_flux_factor

   this%mfc_cons_grc = mfc_consumption*ind_flux_factor
   this%mfc_cons_actual_grc = mfc_consumption_volr_limited*ind_flux_factor
   
   this%mfc_rf_actual_grc = this%mfc_withd_actual_grc - this%mfc_cons_actual_grc

   ! Mining
   this%min_withd_grc = min_demand*ind_flux_factor
   this%min_withd_actual_grc = min_demand_volr_limited*ind_flux_factor

   this%min_cons_grc = min_consumption*ind_flux_factor
   this%min_cons_actual_grc = min_consumption_volr_limited*ind_flux_factor
   
   this%min_rf_actual_grc = this%min_withd_actual_grc - this%min_cons_actual_grc
end do

end subroutine CalcSectorWaterNeeded


  !-----------------------------------------------------------------------
subroutine CalcSectorDemandVolrLimited(this, bounds, dom_demand, dom_consumption, liv_demand, liv_consumption, elec_demand, elec_consumption, &
   mfc_demand, mfc_consumption, min_demand, min_consumption, volr, dom_demand_volr_limited, dom_consumption_volr_limited, liv_demand_volr_limited, &
   liv_consumption_volr_limited, elec_demand_volr_limited, elec_consumption_volr_limited, mfc_demand_volr_limited, &
   mfc_consumption_volr_limited, min_demand_volr_limited, min_consumption_volr_limiteds)
! !USES:
!
! !ARGUMENTS:
class(sectorWater_type) , intent(in) :: this
type(bounds_type)      , intent(in) :: bounds

real(r8), intent(in) :: dom_demand( bounds%begg: )
real(r8), intent(in) :: dom_consumption( bounds%begg: )

real(r8), intent(in) :: liv_demand( bounds%begg: )
real(r8), intent(in) :: liv_consumption( bounds%begg: )

real(r8), intent(in) :: elec_demand( bounds%begg: )
real(r8), intent(in) :: elec_consumption( bounds%begg: )

real(r8), intent(in) :: mfc_demand( bounds%begg: )
real(r8), intent(in) :: mfc_consumption( bounds%begg: )

real(r8), intent(in) :: min_demand( bounds%begg: )
real(r8), intent(in) :: min_consumption( bounds%begg: )

! river water volume [m3]
real(r8), intent(in) :: volr( bounds%begg: )

real(r8), intent(out) :: dom_demand_volr_limited( bounds%begg: )
real(r8), intent(out) :: dom_consumption_volr_limited( bounds%begg: )

real(r8), intent(out) :: liv_demand_volr_limited( bounds%begg: )
real(r8), intent(out) :: liv_consumption_volr_limited( bounds%begg: )

real(r8), intent(out) :: elec_demand_volr_limited( bounds%begg: )
real(r8), intent(out) :: elec_consumption_volr_limited( bounds%begg: )

real(r8), intent(out) :: mfc_demand_volr_limited( bounds%begg: )
real(r8), intent(out) :: mfc_consumption_volr_limited( bounds%begg: )

real(r8), intent(out) :: min_demand_volr_limited( bounds%begg: )
real(r8), intent(out) :: min_consumption_volr_limited( bounds%begg: )

!
! !LOCAL VARIABLES:
integer  :: g  ! gridcell index
real(r8) :: available_volr ! volr available for withdrawal [m3]
real(r8) :: max_demand_supported_by_volr ! [kg/m2] [i.e., mm]

! ratio of demand_volr_limited to demand for each grid cell
real(r8) :: dom_demand_limited_ratio_grc(bounds%begg:bounds%endg)
real(r8) :: liv_demand_limited_ratio_grc(bounds%begg:bounds%endg)
real(r8) :: elec_demand_limited_ratio_grc(bounds%begg:bounds%endg)
real(r8) :: mfc_demand_limited_ratio_grc(bounds%begg:bounds%endg)
real(r8) :: min_demand_limited_ratio_grc(bounds%begg:bounds%endg)


character(len=*), parameter :: subname = 'CalcSectorDemandVolrLimited'
!-----------------------------------------------------------------------

SHR_ASSERT_ALL_FL((ubound(dom_demand) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(dom_consumption) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(liv_demand) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(liv_consumption) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(elec_demand) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(elec_consumption) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(mfc_demand) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(mfc_consumption) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(min_demand) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(min_consumption) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(volr) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(dom_demand_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(dom_consumption_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(liv_demand_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(liv_consumption_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(elec_demand_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(elec_consumption_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(mfc_demand_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(mfc_consumption_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(min_demand_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)
SHR_ASSERT_ALL_FL((ubound(min_consumption_volr_limited) == (/bounds%endg/)), sourcefile, __LINE__)


do g = bounds%begg, bounds%endg
   if (volr(g) > 0._r8) then
      available_volr = volr(g) * (1._r8 - this%params%sectorWater_river_volume_threshold)
      max_demand_supported_by_volr = available_volr / grc%area(g) * m3_over_km2_to_mm
   else
      ! Ensure that negative volr is treated the same as 0 volr
      max_demand_supported_by_volr = 0._r8
   end if

   if (dom_demand(g) > max_demand_supported_by_volr) then
      ! inadequate river storage, adjust irrigation demand
      dom_demand_limited_ratio_grc(g)  = max_demand_supported_by_volr / dom_demand(g)
      liv_demand_limited_ratio_grc(g)  = 0._r8
      elec_demand_limited_ratio_grc(g) = 0._r8
      mfc_demand_limited_ratio_grc(g)  = 0._r8
      min_demand_limited_ratio_grc(g)  = 0._r8

   else if (liv_demand(g) > (max_demand_supported_by_volr - dom_demand(g))) then
      dom_demand_limited_ratio_grc(g)  = 1._r8
      liv_demand_limited_ratio_grc(g)  = max_demand_supported_by_volr / liv_demand(g)
      elec_demand_limited_ratio_grc(g) = 0._r8
      mfc_demand_limited_ratio_grc(g)  = 0._r8
      min_demand_limited_ratio_grc(g)  = 0._r8
   else if (elec_demand(g) > (max_demand_supported_by_volr - dom_demand(g) - liv_demand(g))) then
      dom_demand_limited_ratio_grc(g)  = 1._r8
      liv_demand_limited_ratio_grc(g)  = 1._r8
      elec_demand_limited_ratio_grc(g) = max_demand_supported_by_volr / elec_demand(g)
      mfc_demand_limited_ratio_grc(g)  = 0._r8
      min_demand_limited_ratio_grc(g)  = 0._r8
   else if (mfc_demand(g) > (max_demand_supported_by_volr - dom_demand(g) - liv_demand(g) - elec_demand(g))) then
      dom_demand_limited_ratio_grc(g)  = 1._r8
      liv_demand_limited_ratio_grc(g)  = 1._r8
      elec_demand_limited_ratio_grc(g) = 1._r8
      mfc_demand_limited_ratio_grc(g)  = max_demand_supported_by_volr / mfc_demand(g)
      min_demand_limited_ratio_grc(g)  = 0._r8

   else if (min_demand(g) > (max_demand_supported_by_volr - dom_demand(g) - liv_demand(g) - elec_demand(g) - mfc_demand(g)))
      dom_demand_limited_ratio_grc(g)  = 1._r8
      liv_demand_limited_ratio_grc(g)  = 1._r8
      elec_demand_limited_ratio_grc(g) = 1._r8
      mfc_demand_limited_ratio_grc(g)  = 1._r8
      min_demand_limited_ratio_grc(g)  = max_demand_supported_by_volr / min_demand(g)

   else
      dom_demand_limited_ratio_grc(g)  = 1._r8
      liv_demand_limited_ratio_grc(g)  = 1._r8
      elec_demand_limited_ratio_grc(g) = 1._r8
      mfc_demand_limited_ratio_grc(g)  = 1._r8
      min_demand_limited_ratio_grc(g)  = 1._r8
   end if
end do

dom_demand_volr_limited(bounds%begg:bounds%endg) = 0._r8
dom_consumption_volr_limited(bounds%begg:bounds%endg) = 0._r8
liv_demand_volr_limited(bounds%begg:bounds%endg) = 0._r8
liv_consumption_volr_limited(bounds%begg:bounds%endg) = 0._r8
elec_demand_volr_limited(bounds%begg:bounds%endg) = 0._r8
elec_consumption_volr_limited(bounds%begg:bounds%endg) = 0._r8
mfc_demand_volr_limited(bounds%begg:bounds%endg) = 0._r8
mfc_consumption_volr_limited(bounds%begg:bounds%endg) = 0._r8
min_demand_volr_limited(bounds%begg:bounds%endg) = 0._r8
min_consumption_volr_limited(bounds%begg:bounds%endg) = 0._r8
do g = bounds%begg, bounds%endg
   dom_demand_volr_limited(c) = dom_demand(g) * dom_demand_limited_ratio_grc(g)
   dom_consumption_volr_limited(c) = dom_consumption(g) * dom_demand_limited_ratio_grc(g)

   liv_demand_volr_limited(c) = liv_demand(g) * liv_demand_limited_ratio_grc(g)
   liv_consumption_volr_limited(c) = liv_consumption(g) * liv_demand_limited_ratio_grc(g)

   elec_demand_volr_limited(c) = elec_demand(g) * elec_demand_limited_ratio_grc(g)
   elec_consumption_volr_limited(c) = elec_consumption(g) * elec_demand_limited_ratio_grc(g)

   mfc_demand_volr_limited(c) = mfc_demand(g) * mfc_demand_limited_ratio_grc(g)
   mfc_consumption_volr_limited(c) = mfc_consumption(g) * mfc_demand_limited_ratio_grc(g)

   min_demand_volr_limited(c) = min_demand(g) * min_demand_limited_ratio_grc(g)
   min_consumption_volr_limited(c) = min_consumption(g) * min_demand_limited_ratio_grc(g)

end do

end subroutine CalcSectorDemandVolrLimited




end module SectorWaterMod