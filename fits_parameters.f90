module fits_parameters

use, intrinsic :: iso_fortran_env, only : input_unit, &
                                          output_unit, &
                                          error_unit
  implicit none

  private :: ch_cap                                    

  logical,       parameter        :: FF_run_check_global = .true.        
  integer,       parameter        :: FF_verbose_global  = 0        

  ! Identification of module alternative
  character (len=*), parameter :: module_physical_constants = "default"

  ! "Double precision" numbers precision
  integer,       parameter        :: FF_df = selected_real_kind(8,300)

  integer, parameter  :: FF_MAX_COL_LENGTH = 64
  integer, parameter  :: FF_MAX_UNIT_LENGTH = 32
  integer, parameter  :: FF_MAX_FORMAT_LENGTH = 16

  integer, parameter  :: FF_MAX_FITS_COLUMNS = 999

! A public variable to use as a MOLD for transfer()
  integer,          dimension(:), allocatable :: mold_int
  integer(kind=2),  dimension(:), allocatable :: mold_short_int
  integer(kind=8),  dimension(:), allocatable :: mold_long_int
  real,             dimension(:), allocatable :: mold_float
  real(kind=FF_df),    dimension(:), allocatable :: mold_double
  character(len=FF_MAX_COL_LENGTH),    dimension(:), allocatable :: mold_string

  integer, parameter              :: FF_stdin = input_unit

  integer, parameter              :: FF_stdout = output_unit
  character(len=16)               :: FF_FF_stdout_fmt = '(/,a,/)'

  integer, parameter               :: FF_stderr = error_unit
  character(len=16)                :: FF_FF_stderr_fmt = '(/,a,/)'

  real,          parameter        :: FF_DEFAULT_REAL = -99.99
  real(kind=FF_df), parameter        :: FF_DEFAULT_DOUBLE = -99.99999999
  real,          parameter        :: FF_NULLVAL_FITSIO = -99.99999
  integer,       parameter        :: FF_DEFAULT_INTEGER = -99
  integer,       parameter        :: FF_DEFAULT_SHORT_INTEGER = -99
  integer,       parameter        :: FF_DEFAULT_LONG_INTEGER = -99
  character(len=1), parameter     :: FF_DEFAULT_CHAR = ''

  integer,       parameter        :: FF_MAX_EXT_LENGTH = 64
  integer,       parameter        :: FF_MAX_FILENAME_LENGTH = 1024
  integer,       parameter        :: FF_MAX_COMMENT_LENGTH = 512

contains

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cfitsio_print_error( status )


    integer, intent(in)   :: status
    character(len=200)        :: err_message

    if ( status /= 0 ) then 
      call ftgerr( status, err_message ) 
      write( FF_stderr, *), ' [ CFITSIO ERROR: ', trim(adjustl(err_message)), ' ] ' 
    endif

  end subroutine cfitsio_print_error

  subroutine ch_cap ( c )
  !
  !*******************************************************************************
  ! Taken from CHRPACK
  ! (http://orion.math.iastate.edu/burkardt/f_src/chrpak/chrpak.html)
  !
  !! CH_CAP capitalizes a single character.
  !
  !
  !  Modified:
  !
  !    19 July 1998
  !
  !  Author:
  !
  !    John Burkardt
  !
  !  Parameters:
  !
  !    Input/output, character C, the character to capitalize.
  !
    implicit none
  !
    character c
    integer itemp
  !
    itemp = ichar ( c )
   
    if ( 97 <= itemp .and. itemp <= 122 ) then
      c = char ( itemp - 32 )
    end if
   
    return
  end subroutine ch_cap

function FF_str_eq_no_case ( s1, s2 )
!
!*******************************************************************************
! Taken from CHRPACK
! (http://orion.math.iastate.edu/burkardt/f_src/chrpak/chrpak.html)
!
!   S_EQI is a case sensitive comparison of two strings for equality, after
!  removing trailing and leading blanks
!
!
!  Examples:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is .TRUE.
!
!  Modified:
!
!    14 April 1999
!    5 March  2013
!
!  Author:
!
!    John Burkardt
!    Jacopo Chevallard
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  implicit none
!
  character c1
  character c2
  integer i
  integer len1
  integer len2
  integer lenc
  logical FF_str_eq_no_case 
  character ( len = * ) s1
  character ( len = * ) s2
!
  len1 = len_trim( adjustl(s1) )
  len2 = len_trim( adjustl(s2) )
  lenc = min ( len1, len2 )
 
  FF_str_eq_no_case = .false.

  if ( len1 /= len2 ) return

  do i = 1, lenc

    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap ( c1 )
    call ch_cap ( c2 )

    if ( c1 /= c2 ) then
      return
    end if

  end do
 
  FF_str_eq_no_case = .true.
 
  return
end function FF_str_eq_no_case


end module fits_parameters
