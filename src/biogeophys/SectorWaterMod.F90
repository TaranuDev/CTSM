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
      use clm_time_manager , only : get_step_size
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
         ! sfc_sectorX_rate_patch = is actual withdrawal and can be lower than expected withdrawal due to surface water limitation (if activated)
         ! sectorX_rate_demand_patch = is total expected withdrawal (obtained from input data)
         ! qflx_sectorX_demand_patch = 
         real(r8), pointer :: sfc_dom_rate_patch        (:) ! current domestic withdrawal rate from surface water [mm/s]
         real(r8), pointer :: dom_rate_demand_patch     (:) ! current domestic withdrawal rate, neglecting surface water source limitation [mm/s]
         real(r8), pointer :: qflx_dom_demand_patch     (:) ! domesctic withdrawal flux neglecting surface water source limitation [mm/s]
         
         real(r8), pointer :: sfc_liv_rate_patch        (:) ! current livestock withdrawal rate from surface water [mm/s]
         real(r8), pointer :: liv_rate_demand_patch     (:) ! current livestock withdrawal rate, neglecting surface water source limitation [mm/s]
         real(r8), pointer :: qflx_liv_demand_patch     (:) ! livestock withdrawal flux neglecting surface water source limitation [mm/s]

         integer , pointer :: n_dom_and_liv_steps_left_patch    (:) ! number of time steps for which we still need to satisfy domestic and livestock demand (if 0, ignore)

         real(r8), pointer :: sfc_elec_rate_patch        (:) ! current thermoelectric withdrawal rate from surface water [mm/s]
         real(r8), pointer :: elec_rate_demand_patch     (:) ! current thermoelectric withdrawal rate, neglecting surface water source limitation [mm/s]
         real(r8), pointer :: qflx_elec_demand_patch     (:) ! domesctic thermoelectric flux neglecting surface water source limitation [mm/s]
         
         real(r8), pointer :: sfc_mfc_rate_patch        (:) ! current manufacturing withdrawal rate from surface water [mm/s]
         real(r8), pointer :: mfc_rate_demand_patch     (:) ! current manufacturing withdrawal rate, neglecting surface water source limitation [mm/s]
         real(r8), pointer :: qflx_mfc_demand_patch     (:) ! livestock manufacturing flux neglecting surface water source limitation [mm/s]
         
         real(r8), pointer :: sfc_min_rate_patch        (:) ! current mining withdrawal rate from surface water [mm/s]
         real(r8), pointer :: mfc_min_demand_patch     (:) ! current mining withdrawal rate, neglecting surface water source limitation [mm/s]
         real(r8), pointer :: qflx_min_demand_patch     (:) ! livestock mining flux neglecting surface water source limitation [mm/s]


         integer , pointer :: n_ind_steps_left_patch    (:) ! number of time steps for which we still need to satisfy industrial demand (if 0, ignore)

      contains
         ! Public routines
         ! COMPILER_BUG(wjs, 2014-10-15, pgi 14.7) Add an "SectorWater" prefix to some  generic routines like "Init"
         ! (without this workaround, pgi compilation fails in restFileMod)
         procedure, public :: Init => SectorWaterInit
         procedure, public :: Restart
         procedure, public :: CalcSectorWaterFluxes
         procedure, public :: CalcSectorWaterNeeded
         procedure, public :: UseGroundwaterSectorWater ! Returns true if groundwater usage is enabled for sectoral water usage
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
         procedure, private :: CalcSectorWaterDeficitVolrLimited   ! calculate deficit limited by river volume for each patch
   end type sectorWater_type

   interface sectorWater_params_type
      module procedure sectorWater_params_constructor
   end interface sectorWater_params_type

   ! Conversion factors
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
         soilstate_inst, soil_water_retention_curve, &
         use_aquifer_layer)
      use SoilStateType , only : soilstate_type
  
      class(sectorWater_type) , intent(inout) :: this
      type(bounds_type)      , intent(in)    :: bounds
      character(len=*)       , intent(in)    :: NLFilename ! Namelist filename
      type(soilstate_type)   , intent(in)    :: soilstate_inst
      class(soil_water_retention_curve_type), intent(in) :: soil_water_retention_curve
      logical                , intent(in)    :: use_a
      call this%ReadNamelist(NLFilename, use_aquifer_layer)
      call this%InitAllocate(bounds)quifer_layer ! whether an aquifer layer is used in this run
  
      call this%InitHistory(bounds)
      call this%InitCold(bounds, soilstate_inst, soil_water_retention_curve)
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
      integer :: begp, endp
      integer :: begc, endc
  
      character(len=*), parameter :: subname = 'InitAllocate'
      !-----------------------------------------------------------------------
  
      begp = bounds%begp; endp= bounds%endp
      begc = bounds%begc; endc= bounds%endc
  
      allocate(this%qflx_dom_demand_patch (begp:endp))              ; this%qflx_dom_demand_patch  (:)   = nan
      allocate(this%qflx_liv_demand_patch (begp:endp))              ; this%qflx_liv_demand_patch  (:)   = nan
      allocate(this%qflx_elec_demand_patch (begp:endp))              ; this%qflx_elec_demand_patch  (:)   = nan
      allocate(this%qflx_mfc_demand_patch (begp:endp))              ; this%qflx_mfc_demand_patch  (:)   = nan
      allocate(this%qflx_min_demand_patch (begp:endp))              ; this%qflx_min_demand_patch  (:)   = nan

      allocate(this%sfc_dom_rate_patch        (begp:endp))          ; this%sfc_dom_rate_patch         (:)   = nan
      allocate(this%sfc_liv_rate_patch        (begp:endp))          ; this%sfc_liv_rate_patch         (:)   = nan
      allocate(this%sfc_elec_rate_patch        (begp:endp))          ; this%sfc_elec_rate_patch         (:)   = nan
      allocate(this%sfc_mfc_rate_patch        (begp:endp))          ; this%sfc_mfc_rate_patch         (:)   = nan
      allocate(this%sfc_min_rate_patch        (begp:endp))          ; this%sfc_min_rate_patch         (:)   = nan

      allocate(this%dom_rate_demand_patch     (begp:endp))          ; this%dom_rate_demand_patch      (:)   = nan
      allocate(this%liv_rate_demand_patch     (begp:endp))          ; this%liv_rate_demand_patch      (:)   = nan
      allocate(this%elec_rate_demand_patch     (begp:endp))          ; this%elec_rate_demand_patch      (:)   = nan
      allocate(this%mfc_rate_demand_patch     (begp:endp))          ; this%mfc_rate_demand_patch      (:)   = nan
      allocate(this%min_rate_demand_patch     (begp:endp))          ; this%min_rate_demand_patch      (:)   = nan

      allocate(this%n_dom_and_liv_steps_left_patch    (begp:endp))          ; this%n_dom_and_liv_steps_left_patch     (:)   = 0
      allocate(this%n_ind_steps_left_patch    (begp:endp))          ; this%n_ind_steps_left_patch     (:)   = 0

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
      integer :: begp, endp
      
      character(len=*), parameter :: subname = 'InitHistory'
      !-----------------------------------------------------------------------
  
      begp = bounds%begp; endp= bounds%endp
  
      this%qflx_dom_demand_patch(begp:endp) = spval
      call hist_addfld1d (fname='QDOMESTIC_DEMAND', units='mm/s', &
           avgflag='A', long_name='domestic demand', &
           ptr_patch=this%qflx_dom_demand_patch, default='inactive')

      this%qflx_liv_demand_patch(begp:endp) = spval
      call hist_addfld1d (fname='QLIVESTOCK_DEMAND', units='mm/s', &
           avgflag='A', long_name='livestock demand', &
           ptr_patch=this%qflx_liv_demand_patch, default='inactive')
      
      this%qflx_elec_demand_patch(begp:endp) = spval
      call hist_addfld1d (fname='QELECTRIC_DEMAND', units='mm/s', &
           avgflag='A', long_name='thermoelectric demand', &
           ptr_patch=this%qflx_elec_demand_patch, default='inactive')      

      this%qflx_mfc_demand_patch(begp:endp) = spval
      call hist_addfld1d (fname='QMANUFACTURING_DEMAND', units='mm/s', &
           avgflag='A', long_name='manufacturing demand', &
           ptr_patch=this%qflx_mfc_demand_patch, default='inactive')

      this%qflx_min_demand_patch(begp:endp) = spval
      call hist_addfld1d (fname='QMINING_DEMAND', units='mm/s', &
           avgflag='A', long_name='mining demand', &
           ptr_patch=this%qflx_min_demand_patch, default='inactive')
  
    end subroutine SectorWaterInitHistory

  !-----------------------------------------------------------------------
    subroutine SectorWaterInitCold(this, bounds)
      !
      ! !DESCRIPTION:
      ! Do cold-start initialization for irrigation data structure
      !
      ! !ARGUMENTS:
      class(irrigation_type) , intent(inout) :: this
      type(bounds_type)      , intent(in)    :: bounds
  
      character(len=*), parameter :: subname = 'InitCold'
      !-----------------------------------------------------------------------

      this%dtime = get_step_size()
      this%dom_and_liv_nsteps_per_day = this%Calc_dom_and_liv_NstepsPerDay(this%dtime)
      this%ind_nsteps_per_day = this%Calc_ind_NstepsPerDay(this%dtime)

      this%qflx_dom_demand_patch(bounds%begp:bounds%endp) = 0._r8
      this%qflx_liv_demand_patch(bounds%begp:bounds%endp) = 0._r8
      this%qflx_elec_demand_patch(bounds%begp:bounds%endp) = 0._r8
      this%qflx_mfc_demand_patch(bounds%begp:bounds%endp) = 0._r8
      this%qflx_min_demand_patch(bounds%begp:bounds%endp) = 0._r8

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
   
   deallocate(this%qflx_dom_demand_patch)
   deallocate(this%qflx_liv_demand_patch)
   deallocate(this%qflx_elec_demand_patch)
   deallocate(this%qflx_mfc_demand_patch)
   deallocate(this%qflx_min_demand_patch)

   deallocate(this%sfc_dom_rate_patch)
   deallocate(this%sfc_liv_rate_patch)
   deallocate(this%sfc_elec_rate_patch)
   deallocate(this%sfc_mfc_rate_patch)
   deallocate(this%sfc_min_rate_patch)


   deallocate(this%dom_rate_demand_patch)
   deallocate(this%liv_rate_demand_patch)
   deallocate(this%elec_rate_demand_patch)
   deallocate(this%mfc_rate_demand_patch)
   deallocate(this%min_rate_demand_patch)


   deallocate(this%n_dom_and_liv_steps_left_patch)
   deallocate(this%n_ind_steps_left_patch)

 end subroutine sectorWaterClean

 
  ! ========================================================================
  ! Science routines
  ! ========================================================================
  
 