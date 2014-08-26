module manipulate_fits

  use fits_parameters
  use fits_column
  use fits_table
  use fits_image

  implicit none

!+
  type class_fits

!    integer          :: kind = selected_real_kind( 6 )
    integer                             :: unit = FF_DEFAULT_INTEGER
    character(len=FF_MAX_FILENAME_LENGTH)  :: name = ''
    logical                             :: overwrite = .true.
    integer                             :: n_hdu = FF_DEFAULT_INTEGER
    integer                             :: n_table = FF_DEFAULT_INTEGER
    integer                             :: n_image = FF_DEFAULT_INTEGER

    class( class_table ), pointer   :: table

   ! type( class_bin_table ),    dimension(:), allocatable  :: binTable
    class( class_image ), pointer   :: image

    contains

      procedure, nopass :: initialize     => initialize_fits_file
      procedure         :: read           => read_fits_file
      procedure         :: close          => close_fits_file
      procedure         :: delete         => delete_fits_file
      procedure         :: write_to_disk  => write_fits_file_to_disk
      procedure         :: free           => free_fits_file
      procedure         :: print          => print_fits_file

  end type class_fits
!+

  contains


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine close_fits_file( this )

    class( class_fits ), intent(in) :: this
    integer                         :: status

    ! The FITS file must always be closed before exiting the program. 
    ! Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
    status = 0
    call ftclos( this%unit, status ) ; call cfitsio_print_error( status )

    call ftfiou( this%unit, status ) ; call cfitsio_print_error( status )

  end subroutine close_fits_file


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine delete_fits_file( this )

    ! A simple little routine to delete a FITS file

    class( class_fits ), intent(in)   :: this
    logical                           :: is_exist

    inquire( file = this%name, exist = is_exist )

    if ( is_exist ) call system( 'rm ' // this%name )
   
  end subroutine delete_fits_file
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialize_fits_file( this, name, overwrite )

    class( class_fits ), intent(out), pointer   :: this
    character(len=*),    intent(in)             :: name
    logical,             intent(in), optional   :: overwrite

    allocate( this )

    this%name = trim(adjustl(name))

    if ( present(overwrite) ) this%overwrite = overwrite

  end subroutine initialize_fits_file

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine print_fits_file( this )

    class ( class_fits ), intent(in)            :: this

    if ( associated(this%table) ) then
      write( FF_stdout, '(/,a,a,a,I2,a,/)' ) 'The FITS file "', trim(adjustl(this%name)), '" contains ', this%n_table, ' tables.'
      call this%table%print( )
    endif

    if ( associated(this%image) ) then
      write( FF_stdout, '(/,a,a,a,I2,a,/)' ) 'The FITS file "', trim(adjustl(this%name)), '" contains ', this%n_image, ' images.'
      call this%image%print( )
    endif

  end subroutine print_fits_file

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine free_fits_file( this )

    class ( class_fits ), intent(in)            :: this

    if ( associated(this%table) ) call this%table%free( )
    if ( associated(this%image) ) call this%image%free( )

  end subroutine free_fits_file

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_fits_file( this, name, extension_name, extension_number, n_skip_extension, columnName )

    class ( class_fits )                                  :: this
    character(len=*),               intent(in)            :: name
    character(len=*), dimension(:), intent(in), optional  :: extension_name
    integer,          dimension(:), intent(in), optional  :: extension_number
    integer,                        intent(in), optional  :: n_skip_extension
    character(len=*), dimension(:), intent(in), optional  :: columnName

    integer,          dimension(:), allocatable           :: extNumbers
    integer                                               :: nhdu, i, rwmode, n_Table, n_image, n_skip
    integer                                               :: status, hdutype, nMove
    class( class_table ), pointer                         :: currentTable
    class( class_image ), pointer                         :: currentImage
    
    if ( FF_verbose_global > 1 ) write (FF_stdout, FF_FF_stdout_fmt), ' Reading the FITS file: ', trim(name)

    this%name = name

    ! The STATUS parameter must always be initialized.
    status = 0

    ! Get an unused Logical Unit Number to use to open the FITS file.
    call ftgiou( this%unit, status ) ; call cfitsio_print_error( status )

    ! Open file in readonly mode (this routine automatically skips the presence
    ! of an empty primary array)
    rwmode = 0
    call ftdopn( this%unit, this%name, rwmode, status ) ; call cfitsio_print_error( status )

    ! Get current HDU, if it's > 1 it means that the first nhdu-1 extensions are empty
    call ftghdn( this%unit, nhdu )    
    n_skip = 0   
    if ( nhdu > 1 ) n_skip = nhdu-1

    ! Check if the user specified some extension to skipe
    if ( present(n_skip_extension) ) then
      n_skip = n_skip_extension
    endif

    ! Check if user specified a set of extension_name or extension_number, otherwise read the whole FITS file
    if ( present(extension_name) ) then
      this%n_hdu = size( extension_name )
      extNumbers = extension_name_to_number( this%unit, extension_name )
    elseif ( present(extension_number) ) then
      this%n_hdu = size( extension_number )
      allocate( extNumbers(this%n_hdu) )
      extNumbers(:) = extension_number
    else
      ! Get total number of HDU (extensions)
      call ftthdu( this%unit, this%n_hdu, status ) ; call cfitsio_print_error( status )
      allocate( extNumbers(this%n_hdu-n_skip) )
      extNumbers(:) = [ (i, i=n_skip+1,this%n_hdu) ]
      this%n_hdu = this%n_hdu - n_skip
    endif

    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Total number of HDU to be read: ', this%n_hdu

    ! Go through each HDU to determine if is an ASCII or binary table, or an
    ! image
    ! Move to the first HDU skipping skip_hdu extensions
    n_table = 0 ; n_image = 0
    nMove = 1 ; hduType = -1

    do i=1, this%n_hdu
      ! Move to a specified (absolute) HDU in the FITS file
      call ftmahd( this%unit, extNumbers(i), hduType, status ) ; call cfitsio_print_error( status )
      ! Get type of HDU
      call ftghdt( this%unit, hdutype, status) ; call cfitsio_print_error( status )

      select case( hdutype )
        case ( 0 ) 
          if ( .not. associated(this%image) ) then
            call initialize_image( currentImage )
            this%image => currentImage
          else
            call currentImage%add( )
            currentImage => currentImage%go_last( )
          endif
          call read_image( currentImage, this%unit )
          n_image = n_image + 1
        case ( 1:2 ) 
          if ( .not. associated(this%table) ) then
            call initialize_table( currentTable )
            this%table => currentTable
          else
            call currentTable%add( )
            currentTable => currentTable%go_last( )
          endif
          if ( present(columnName) ) then 
            call read_table( currentTable, this%unit, columnName = columnName )
          else
            call read_table( currentTable, this%unit )
          endif
          n_table = n_table + 1
      end select

    enddo

    if ( FF_verbose_global > 1 ) write (FF_stdout, '(/,a,I3)'), ' Total number of tables extensions read: ', n_table
    this%n_table = n_table

    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Total number of image extensions read: ', n_image
    this%n_image = n_image

    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' FITS file: ', trim(name), ' read'

  end subroutine read_fits_file


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function extension_name_to_number( unit, extension_name ) result( extension_number )

    integer,                        intent(in)    :: unit
    character(len=*), dimension(:), intent(in)    :: extension_name
    integer,          dimension(:), allocatable   :: extension_number

    integer                                       :: i, n, hdutype, extver, status

    n = size( extension_name )
    allocate( extension_number(n) )

    hdutype = -1 ! =  any type of HDU

    do i=1, n
      ! Move to the (first) HDU which has the specified extension type and EXTNAME (or HDUNAME)
      ! and EXTVER keyword values
      call ftmnhd( unit, hduType, trim(adjustl(extension_name(i))), extver, status ) ; call cfitsio_print_error( status )
      ! Get the number of the current HDU in the FITS file (primary array = 1)
      call ftghdn( unit, extension_number(i) )
    enddo

  end function extension_name_to_number

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_fits_file_to_disk( this, write_tables, write_images, logical_unit )

    class( class_fits ),  intent(in)            :: this
    logical,              intent(in),  optional :: write_tables
    logical,              intent(in),  optional :: write_images
    integer,              intent(out), optional :: logical_unit

    logical                         :: extend, simple
    integer                         :: status, bitpix, naxis, pcount, gcount, blocksize
    integer, dimension(2)           :: naxes

    ! The STATUS parameter must always be initialized.
    status = 0

    ! Check if the file already exist, in which case delete it
    if ( this%overwrite ) call this%delete

    ! Get an unused Logical Unit Number to use to open the FITS file.
    call ftgiou( this%unit, status ) ; call cfitsio_print_error( status )

    ! Create the new empty FITS file.  The blocksize parameter is a historical artifact and the value is ignored by FITSIO.
    blocksize = 1
    call ftinit( this%unit, this%name, blocksize, status ) ; call cfitsio_print_error( status )

    simple = .true.  ! does the FITS file conform to all the FITS standards?
    extend = .true.  ! true if there may be extensions following the primary data
    bitpix = 8 ; naxis = 0 
    naxes(1) = 0 ; naxes(2) = 0
    pcount = 0 ; gcount = 1

    ! Write the required header keywords to the file
    call ftphpr( this%unit, simple, bitpix, naxis, naxes, pcount, gcount, extend, status ) ; call cfitsio_print_error( status )

    ! Write the FITS tables to the disk
    if ( present( write_tables) ) then
      if ( write_tables ) call this%table%write_to_disk( this%unit )
    endif

    ! Write the FITS images to the disk
    if ( present( write_images) ) then
      if ( write_images ) call this%image%write_to_disk( this%unit )
    endif

    ! Output the logical unit, so as to be re-used keeping the file open
    if ( present(logical_unit) ) logical_unit = this%unit


  end subroutine write_fits_file_to_disk

end module manipulate_fits
