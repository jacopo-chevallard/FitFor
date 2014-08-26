program test_fits

  use manipulate_fits
  use fits_column

  implicit none

  class ( class_fits ), pointer :: fits
  class( table_column ), pointer                :: column
  class( class_table ), pointer                :: table

  integer                               :: i, n, unit, n_rows, status
  real, dimension(:), allocatable       :: data_sp, temp
  real, dimension(:,:), allocatable       :: data_sp_2d
  integer, dimension(:), allocatable       :: data_j
  integer(kind=2), dimension(:), allocatable       :: data_i
  integer(kind=2), dimension(:,:), allocatable       :: data_i_2d
  character(len=FF_MAX_COL_LENGTH), dimension(:), allocatable       :: data_c
  real                                          :: t1, t2           

  integer, parameter                    :: n_fields = 5
  character (len=64), dimension(n_fields)         :: cols_name
  character (len=16), dimension(n_fields)         :: cols_unit
  character (len=16), dimension(n_fields)         :: cols_form
  logical, dimension(:), allocatable          :: temp_logic
  class( table_column ), pointer :: newColumn


!!-!!  n = 100000
!!-!!  allocate( data_sp(n), temp(n), data_sp_2d(n,100), temp_logic(n) )
!!-!!  data_sp = 1. ; temp = [ (i, i=1,n) ]
!!-!!  data_sp(2) = 2.
!!-!!  data_sp(5) = 5.
!!-!!
!!-!!  temp(5) = 1.
!!-!!
!!-!!  call cpu_time( t1 )
!!-!!  do i=1, 100
!!-!!    temp_logic = almost_equal(1., temp)
!!-!!  enddo
!!-!!  call cpu_time( t2 )
!!-!!  print *, 'time: ', t2-t1
!!-!!  print *, temp_logic(1:5)
!!-!!
!!-!!  call cpu_time( t1 )
!!-!!  do i=1, 100
!!-!!    temp_logic = almost_equal(1., temp)
!!-!!  enddo
!!-!!  call cpu_time( t2 )
!!-!!  print *, 'time: ', t2-t1
!!-!!  print *, temp_logic(1:5)
!!-!!
!!-!!  pause

!!-!!  allocate( fits )
!!-!!  call fits%read( '/Users/jacopo/Surveys/SDSS/MPA_JHU/gal_info_dr7_v5_2.fit' ) 
!!-!!
!!-!!  table => fits%table%go_first( ) 
!!-!!  call table%get_column( 'ra', data_sp )
!!-!!  print *, 'data_sp: ', data_sp(1:10) 

  !call table%get_column( 'FIBERID', data_i )
  !print *, 'data_i: ', data_i(1:10) 

  !call table%get_column( 'PHOTOID', data_i_2d )
  !do i=1, 10
  !  print *, 'data_i_2d: ', data_i_2d(:,i) 
  !enddo

  !call table%get_column( 'PLUG_MAG', data_sp_2d )
  !do i=1, 10
  !  print *, 'data_sp_2d: ', data_sp_2d(:,i) 
  !enddo

  !call table%get_column( 'SPECTROTYPE', data_i )
  !print *, 'data_c: ', data_c(1:10) 


!!-!!  pause
!!-!!
!!-!!  call table%free( )
!!-!!
!!-!!  pause
!!-!!
!!-!!  call fits%table%print( )
!!-!!  deallocate( fits )

  call initialize_fits_file( fits, 'test.fits', overwrite = .true. )

  call initialize_table( table, 'Table_1' )
  fits%table => table

  cols_name(:) = [ character(len=6) :: 'Col1', 'Col2', 'Col3', 'Col4', 'Col5' ]
  cols_unit(:) = [ character(len=6)  :: 'un1', 'un2', 'un3', 'un4', 'un5' ]
  cols_form(:) = '1E'

  n_rows = 1000

  allocate( data_sp_2d(n_rows,n_fields) )
  do i=1, n_fields
    data_sp_2d(:,i) = i
  enddo

  !call table%initialize_columns( cols_name, cols_unit, cols_form, data = data_sp_2d )
  call table%initialize_columns( cols_name, cols_unit, cols_form )

  call table%insert( cols_name, data_sp_2d )

  !call table%print( )
  do i=1, n_fields
    data_sp_2d(:,i) = 0.
  enddo

  call table%insert( cols_name, data_sp_2d )

    data_sp_2d(:,:) = .5
  call table%insert( cols_name, data_sp_2d )

  call table%get_column( 'Col2', data_sp )
  print *, 'data_sp: ', data_sp(1:5)

  call table%get_column( 'Col4', data_sp )
  print *, 'data_sp: ', data_sp(1:5)

  print *, 'n_col: ', table%column%n_columns()
  call table%print()

  call fits%write_to_disk( logical_unit = unit, write_tables = .true. )


 ! call table%write_to_disk( unit )

  call ftgrsz( unit, n_rows, status)
  print *, 'n_row: ', n_rows

  call fits%close( )
  call fits%free( )

  !call table%insert( cols_name, data_sp_2d )

 ! call create_bin_table( this, columnName, columnUnit, columnFormat, tableName, size_of_row_increase )

end program test_fits
