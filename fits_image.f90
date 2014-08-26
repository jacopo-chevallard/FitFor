module fits_image

  use fits_parameters

  implicit none

!+
  type class_image

    character(len = FF_MAX_EXT_LENGTH)                               :: name = ''

    integer                               :: n_axis = FF_DEFAULT_INTEGER
    integer,  dimension(3)                :: naxes = [ FF_DEFAULT_INTEGER, FF_DEFAULT_INTEGER, FF_DEFAULT_INTEGER ]
    integer                               :: bitpix = FF_DEFAULT_INTEGER
    real                                  :: wl_start = FF_DEFAULT_REAL
    real                                  :: wl_step = FF_DEFAULT_REAL
    real,     dimension(:), allocatable   :: wl

    integer,  dimension(:), pointer       :: data => null()

    ! Linked list pointers

    class( class_image ), pointer         :: next => null()
    class( class_image ), pointer         :: prev => null()

    class( class_image ), pointer         :: first => null()
    class( class_image ), pointer         :: last => null()

    contains
      procedure, nopass :: initialize     => initialize_image
      procedure         :: read           => read_image
      procedure         :: add            => add_image
      procedure         :: find           => find_image
      procedure         :: write_to_disk  => write_image_to_disk
      procedure         :: free           => free_image
      procedure         :: print          => print_image
      !procedure :: insert => insert_values_image
      procedure         :: go_next        => next_image
      procedure         :: go_first       => first_image
      procedure         :: go_last        => last_image

      procedure         :: get_data_image_i_2d
      procedure         :: get_data_image_j_2d
      procedure         :: get_data_image_k_2d
      procedure         :: get_data_image_sp_2d
      procedure         :: get_data_image_dp_2d

      procedure         :: get_data_image_i_3d
      procedure         :: get_data_image_j_3d
      procedure         :: get_data_image_k_3d
      procedure         :: get_data_image_sp_3d
      procedure         :: get_data_image_dp_3d

      generic           :: get            => get_data_image_i_2d, get_data_image_j_2d, get_data_image_k_2d, get_data_image_sp_2d, get_data_image_dp_2d, get_data_image_i_3d, get_data_image_j_3d, get_data_image_k_3d, get_data_image_sp_3d, get_data_image_dp_3d

  end type class_image
!+

  contains

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function first_image( this ) result( first )

    class( class_image ),         intent(in)     :: this
    class( class_image ), pointer                :: first

    first => null()
    if ( associated(this%first) ) first => this%first

  end function first_image

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function last_image( this ) result( last )

    class( class_image ), target, intent(in)  :: this
    class( class_image ), pointer             :: last, image

    last => null()

    image => this
    do while (associated(image) )
      last => image
      image => image%go_next( )
    enddo

  end function last_image

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function next_image( this ) result( next )

    class( class_image ),         intent(in)     :: this
    class( class_image ), pointer                :: next

    next => null()
    if ( associated(this%next) ) next => this%next

  end function next_image
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialize_image( this, imageName )

    class( class_image ), pointer,  intent(out)           :: this
    character(len=*),               intent(in), optional  :: imageName

    allocate( this )
    this%first => this

    if ( present(imageName) ) this%name = imageName

  end subroutine initialize_image

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Free the entire image
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine free_image( this )

    class( class_image ), target, intent(inout) :: this
    class( class_image ), pointer               :: current
    class( class_image ), pointer               :: next

    if ( FF_verbose_global > 1 ) write (FF_stdout, FF_FF_stdout_fmt), ' Freeing FITS image...'

    current => this

    do while (associated(current) )
      next => current%next
      deallocate(current)
      nullify(current)
      current => next
    enddo

  end subroutine free_image

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine  print_image( this )

    class( class_image ),           intent(in)  :: this

    class( class_image ),   pointer             :: currentImage
    integer                                     :: i

    currentImage => this%go_first( )

    do while( associated(currentImage) )
      write( FF_stdout, '(/,a,a,a,I7,a,I7,a,/)'), 'The image "', trim(adjustl(currentImage%name)), '" contains ', currentImage%n_axis, ' n_axis'
      write( FF_stdout, '(a)'), 'Axis dimension: '
      do i=1, currentImage%n_axis
        if ( currentImage%naxes(i) > 1 ) write( FF_stdout, '(a,I1,a,I7)' ), 'Axis ', i, ' dimension: ', currentImage%naxes(i)
      enddo
      currentImage => currentImage%go_next( )
    enddo

end subroutine print_image

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine  read_image( this, unit )

    class( class_image )                 :: this
    integer,              intent(in)     :: unit

    integer                              :: i, status, maxdim, group, fpixel, nelements, factor
    character(len=90)                    :: comment
    logical                              :: anyf

    integer,          dimension(:), allocatable     :: data_j
    integer(kind=2),  dimension(:), allocatable     :: data_i
    integer(kind=8),  dimension(:), allocatable     :: data_k
    real,             dimension(:), allocatable     :: data_e
    real(kind=FF_df),    dimension(:), allocatable     :: data_d

    status = 0
    group = 0                         ! (integer) sequence number of the data group (=0 for non-grouped data)
    fpixel = 1                        ! (integer) the first pixel position

    ! Get extension name
    call ftgkys( unit, 'EXTNAME', this%name, comment, status ) ; call cfitsio_print_error( status )

    if ( FF_verbose_global > 1 .and. status == 0 ) write (FF_stdout, '(/,a,a,a,/)'), ' Reading the FITS image"' , trim(adjustl(this%name)), '"'
    status = 0

    ! Get the parameters that define the type and size of the image (NAXIS, NAXES, BITPIX )
    maxdim = 2
    call ftgipr( unit, maxdim, this%bitpix, this%n_axis, this%naxes, status ) ; call cfitsio_print_error( status )

    if ( this%n_axis == 2 ) this%naxes(3) = 1

    ! Total number of pixels in the image
    nelements = product( this%naxes ) 

    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Number of axis: ', this%n_axis
    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Dimensions of axis: ', this%naxes
    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Data type of the image: ', this%bitpix

    ! Get wavelength scale
    call ftgkye( unit, 'CRVAL1', this%wl_start, comment, status ) ; call cfitsio_print_error( status )
    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Starting wavelength: ', this%wl_start

    ! Get wavelength sampling
    call ftgkye( unit, 'CD1_1', this%wl_step, comment, status ) ; call cfitsio_print_error( status )
    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Wavelength step: ', this%wl_step

    ! Create wl vector
    allocate( this%wl(this%naxes(1)) )
    this%wl(1) = this%wl_start
    do i=2, this%naxes(1)
      this%wl(i) =  this%wl(i-1) + this%wl_step 
    enddo

    ! Read the flux as a 2D image
  !  allocate( this%data(this%naxes(1), this%naxes(2)) )
  !  nullval = -99.99
  !  call ftg2de( unit, group, nullval, this%naxes(1), this%naxes(1), this%naxes(2), this%data, anyf, status ) ; call cfitsio_print_error( status )

    !!! Read the image using the appropriate routines depending on the data type (BITPIX) !!!

    select case ( this%bitpix )

      ! i - short integer (I*2, 16-bit integer)
      case ( 16 )

        if ( allocated(data_i) ) then
          if ( size(data_i) /= nelements ) deallocate( data_i )
        endif
        if ( .not. allocated(data_i) ) allocate( data_i(nelements) )
        call ftgpvi( unit, group, fpixel, nelements, FF_DEFAULT_SHORT_INTEGER, data_i, anyf, status ) ;  call cfitsio_print_error( status )
        factor = size( transfer( data_i(1:2), mold_int) ) / size(data_i(1:2))
        allocate( this%data(factor*nelements), source = transfer( data_i, mold_int ) )

      ! j - integer (I*4, 32-bit integer)
      case ( 32 )

        if ( allocated(data_j) ) then
          if ( size(data_j) /= nelements ) deallocate( data_j )
        endif
        if ( .not. allocated(data_j) ) allocate( data_j(nelements) )
        call ftgpvj( unit, group, fpixel, nelements, FF_DEFAULT_INTEGER, data_j, anyf, status ) ;  call cfitsio_print_error( status )
        factor = size( transfer( data_j(1:2), mold_int) ) / size(data_j(1:2))
        allocate( this%data(factor*nelements), source = transfer( data_j, mold_int ) )

      ! k - long integer (I*8, 64-bit integer)
      case ( 64 )

        if ( allocated(data_k) ) then
          if ( size(data_k) /= nelements ) deallocate( data_k )
        endif
        if ( .not. allocated(data_k) ) allocate( data_k(nelements) )
        call ftgpvk( unit, group, fpixel, nelements, FF_DEFAULT_LONG_INTEGER, data_k, anyf, status ) ;  call cfitsio_print_error( status )
        factor = size( transfer( data_k(1:2), mold_int) ) / size(data_k(1:2))
        allocate( this%data(factor*nelements), source = transfer( data_k, mold_int ) )

      ! e - real exponential floating point (R*4)
      case ( -32 )

        if ( allocated(data_e) ) then
          if ( size(data_e) /= nelements ) deallocate( data_e )
        endif
        if ( .not. allocated(data_e) ) allocate( data_e(nelements) )
        call ftgpve( unit, group, fpixel, nelements, FF_DEFAULT_REAL, data_e, anyf, status ) ;  call cfitsio_print_error( status )
        factor = size( transfer( data_e(1:2), mold_int) ) / size(data_e(1:2))
        allocate( this%data(factor*nelements), source = transfer( data_e, mold_int ) )

      ! d - double precision real floating-point (R*8)
      case ( -64 )

        if ( allocated(data_d) ) then
          if ( size(data_d) /= nelements ) deallocate( data_d )
        endif
        if ( .not. allocated(data_d) ) allocate( data_d(nelements) )
        call ftgpvd( unit, group, fpixel, nelements, FF_DEFAULT_DOUBLE, data_d, anyf, status ) ;  call cfitsio_print_error( status )
        factor = size( transfer( data_d(1:2), mold_int) ) / size(data_d(1:2))
        allocate( this%data(factor*nelements), source = transfer( data_d, mold_int ) )

      case default

        write( FF_stderr, * ) '[ ERROR: unrecognized BITPIX value ]'

    end select


  end subroutine read_image

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_i_2d( this, data )

    class( class_image ),                             intent(in)  :: this
    integer(kind=2),    dimension(:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2)) )
    data = reshape(transfer( this%data, data), this%naxes(1:2))

  end subroutine get_data_image_i_2d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_j_2d( this, data )

    class( class_image ),                             intent(in)  :: this
    integer,            dimension(:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2)) )
    data = reshape(transfer( this%data, data), this%naxes(1:2))

  end subroutine get_data_image_j_2d
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_k_2d( this, data )

    class( class_image ),                             intent(in)  :: this
    integer(kind=8),    dimension(:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2)) )
    data = reshape(transfer( this%data, data), this%naxes(1:2))

  end subroutine get_data_image_k_2d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_sp_2d( this, data )

    class( class_image ),                             intent(in)  :: this
    real,               dimension(:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2)) )
    data = reshape(transfer( this%data, data), this%naxes(1:2))

  end subroutine get_data_image_sp_2d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_dp_2d( this, data )

    class( class_image ),                             intent(in)  :: this
    real(kind=FF_df),      dimension(:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2)) )
    data = reshape(transfer( this%data, data), this%naxes(1:2))

  end subroutine get_data_image_dp_2d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_i_3d( this, data )

    class( class_image ),                             intent(in)  :: this
    integer(kind=2),    dimension(:,:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2), this%naxes(3)) )
    data = reshape(transfer( this%data, data), this%naxes)

  end subroutine get_data_image_i_3d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_j_3d( this, data )

    class( class_image ),                             intent(in)  :: this
    integer,            dimension(:,:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2), this%naxes(3)) )
    data = reshape(transfer( this%data, data), this%naxes)

  end subroutine get_data_image_j_3d
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_k_3d( this, data )

    class( class_image ),                             intent(in)  :: this
    integer(kind=8),    dimension(:,:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2), this%naxes(3)) )
    data = reshape(transfer( this%data, data), this%naxes)

  end subroutine get_data_image_k_3d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_sp_3d( this, data )

    class( class_image ),                             intent(in)  :: this
    real,               dimension(:,:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2), this%naxes(3)) )
    data = reshape(transfer( this%data, data), this%naxes)

  end subroutine get_data_image_sp_3d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_data_image_dp_3d( this, data )

    class( class_image ),                             intent(in)  :: this
    real(kind=FF_df),      dimension(:,:,:), allocatable,  intent(out) :: data

    allocate( data(this%naxes(1), this%naxes(2), this%naxes(3)) )
    data = reshape(transfer( this%data, data), this%naxes)

  end subroutine get_data_image_dp_3d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  recursive function find_image( this, name ) result( image )

    class( class_image ), target, intent(in)             :: this
    character(len=*),             intent(in)             :: name

    class( class_image ), pointer                        :: image

    image => null()

    if ( FF_str_eq_no_case( this%name, name)  ) then
      image => this 
    else if ( associated(this%next) ) then
      image => find_image( this%next, name )
    else
      write( FF_stderr, * ) ' [ ERROR: cannot find an image with name: ', trim(adjustl(name)), ' ] '
    endif

  end function find_image


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine add_image( this, imageName )

    class( class_image ), target                :: this
    character(len=*),     intent(in), optional  :: imageName

    class( class_image ), pointer               :: newImage

    ! Allocate new image
    allocate( newImage )

    newImage%next => this%next
    this%next => newImage
    !newImage%prev => this
    newImage%first => this%first

    if ( present(imageName) ) newImage%name = imageName

  end subroutine add_image


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_image_to_disk( this, unit )

    class( class_image ), intent(in)  :: this
    integer,              intent(in)  :: unit

    class( class_image ),   pointer   :: image
    integer                           :: status, group, fpixel, nelements

    status = 0 
    group = 0                         ! (integer) sequence number of the data group (=0 for non-grouped data)
    fpixel = 1                        ! (integer) the first pixel position
    nelements = product( this%naxes ) ! nelements - (integer) number of data elements to read or write

    ! Point to the first image
    image => this%go_first( )

    do while ( associated(image) )

      ! Append/create a new empty HDU onto the end of the file and move to it.
      call ftcrhd( unit, status ) ; call cfitsio_print_error( status )

      ! FTPHBN writes all the required header keywords which define the structure of the binary image. NROWS and TFIELDS gives the number of rows and columns in the image, 
      if ( this%n_axis == 3 ) then
        call ftphps( unit, this%bitpix, this%n_axis, this%naxes, status ) ; call cfitsio_print_error( status )
      else
        call ftphps( unit, this%bitpix, this%n_axis, this%naxes(1:2), status ) ; call cfitsio_print_error( status )
      endif
      
      ! Put elements into the data array

      ! i - short integer (I*2, 16-bit integer)
      if ( this%bitpix == 16 ) then
        if ( this%naxes(3) > 1 ) then 
          call ftppri ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_short_int),this%naxes), status ) ; call cfitsio_print_error( status )
        else
          call ftppri ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_short_int),this%naxes(1:2)), status ) ; call cfitsio_print_error( status )
        endif

      ! j - integer (I*4, 32-bit integer)
      elseif ( this%bitpix == 32 ) then
        if ( this%naxes(3) > 1 ) then 
          call ftpprj ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_int),this%naxes), status ) ; call cfitsio_print_error( status )
        else
          call ftpprj ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_int),this%naxes(1:2)), status ) ; call cfitsio_print_error( status )
        endif

      ! k - long integer (I*8, 64-bit integer)
      elseif ( this%bitpix == 64 ) then
        if ( this%naxes(3) > 1 ) then 
          call ftpprk ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_long_int),this%naxes), status ) ; call cfitsio_print_error( status )
        else
          call ftpprk ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_long_int),this%naxes(1:2)), status ) ; call cfitsio_print_error( status )
        endif

      ! e - real exponential floating point (R*4)
      elseif ( this%bitpix == -32 ) then
        if ( this%naxes(3) > 1 ) then 
          call ftppre ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_float),this%naxes), status ) ; call cfitsio_print_error( status )
        else
          call ftppre ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_float),this%naxes(1:2)), status ) ; call cfitsio_print_error( status )
        endif

      ! d - double precision real floating-point (R*8)
      elseif ( this%bitpix == -64 ) then
        if ( this%naxes(3) > 1 ) then 
          call ftpprd ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_double),this%naxes), status ) ; call cfitsio_print_error( status )
        else
          call ftpprd ( unit, group, fpixel, nelements, reshape(transfer(this%data, mold_double),this%naxes(1:2)), status ) ; call cfitsio_print_error( status )
        endif
      else
        write( FF_stderr, * ) '[ ERROR: unrecognized BITPIX value ]'
      endif

      image => image%go_next( )
    enddo

  end subroutine write_image_to_disk


end module fits_image
