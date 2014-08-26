module fits_table

  use fits_column 
  use fits_parameters

  implicit none

!+
  type class_table

    integer                             :: n_columns = FF_DEFAULT_INTEGER
    integer                             :: n_rows = FF_DEFAULT_INTEGER
    character(len = FF_MAX_EXT_LENGTH)  :: name = ''
    integer                             :: size_of_row_increase = FF_DEFAULT_INTEGER
    integer                             :: counter = FF_DEFAULT_INTEGER

    class( table_column ), pointer      :: column

    ! Linked list pointers

    class( class_table ), pointer       :: next => null()
    class( class_table ), pointer       :: prev => null()

    class( class_table ), pointer       :: first => null()
    class( class_table ), pointer       :: last => null()

    contains
      procedure, nopass :: initialize         => initialize_table
      procedure         :: read               => read_table
      procedure         :: add                => add_table
      procedure         :: find               => find_table
      procedure         :: append_columns     => append_columns_table
      procedure         :: write_to_disk      => write_table_to_disk
      procedure         :: initialize_columns => initialize_table_columns
      procedure         :: free               => free_table

      procedure         :: insert_values_table
      procedure         :: insert_values_table_single_col
      generic           :: insert             => insert_values_table, insert_values_table_single_col

      procedure         :: go_next            => next_table
      procedure         :: go_first           => first_table
      procedure         :: go_last            => last_table
      procedure         :: find_column        => find_column_table

      procedure         :: get_column_data_table_sp
      procedure         :: get_column_data_table_i
      procedure         :: get_column_data_table_j
      procedure         :: get_column_data_table_str

      procedure         :: get_column_data_table_sp_2d
      procedure         :: get_column_data_table_i_2d
      procedure         :: get_column_data_table_j_2d
      procedure         :: get_column_data_table_str_2d

      generic           :: get_column         => get_column_data_table_sp, get_column_data_table_i, get_column_data_table_j, get_column_data_table_str, get_column_data_table_sp_2d, get_column_data_table_i_2d, get_column_data_table_j_2d, get_column_data_table_str_2d

      procedure         :: print              => print_table

  end type class_table
!+

  contains

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function find_column_table( this, colName ) result( column )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    class( table_column ),  pointer               :: column

    class( class_table ),   pointer               :: currentTable

    currentTable => this%go_first( )

    do while( associated(currentTable) )
      column => currentTable%column%find( colName )
      if ( associated(column) ) exit
      currentTable => currentTable%go_next( )
    enddo

    if ( .not. associated(column) ) write( FF_stderr, * ) ' [ ERROR: cannot find a column with name: ', trim(adjustl(colName)), ' ] '

  end function find_column_table


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_sp( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    real,               dimension(:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = transfer( column%get_data( ), data)

  end subroutine get_column_data_table_sp

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_j( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    integer,               dimension(:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = transfer( column%get_data(), data)

  end subroutine get_column_data_table_j

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_i( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    integer(kind=2),               dimension(:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = transfer( column%get_data(), data)


  end subroutine get_column_data_table_i
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_str( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    character(len=FF_MAX_COL_LENGTH),               dimension(:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = transfer( column%get_data(), data)

  end subroutine get_column_data_table_str

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_sp_2d( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    real,               dimension(:,:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = reshape(transfer( column%get_data(), data), column%size)

  end subroutine get_column_data_table_sp_2d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_j_2d( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    integer,               dimension(:,:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = reshape(transfer( column%get_data(), data), column%size)

  end subroutine get_column_data_table_j_2d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_i_2d( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    integer(kind=2),               dimension(:,:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = reshape(transfer( column%get_data(), data), column%size)

  end subroutine get_column_data_table_i_2d
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_column_data_table_str_2d( this, colName, data )

    class( class_table ),             intent(in)  :: this
    character(len=*),                 intent(in)  :: colName
    character(len=FF_MAX_COL_LENGTH),               dimension(:,:), allocatable, intent(out) :: data

    class( table_column ),    pointer    :: column

    column => this%find_column( colName )
    data = reshape(transfer( column%get_data(), data), column%size)

  end subroutine get_column_data_table_str_2d

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine  print_table( this )

    class( class_table ),           intent(in)  :: this

    class( class_table ),   pointer             :: currentTable
    class( table_column ),  pointer             :: currentColumn

    currentTable => this%go_first( )

    do while( associated(currentTable) )
      write( FF_stdout, '(/,a,a,a,I7,a,I7,a,/)'), 'The table "', trim(adjustl(currentTable%name)), '" contains ', currentTable%n_columns, ' columns and ', currentTable%n_rows, ' rows.'
      write( FF_stdout, '(a)'), 'Column names, unit and format: '
      currentColumn => currentTable%column%go_first( )
      do while( associated(currentColumn) )
        write( FF_stdout, '(/,a,a)'),  'Name:    ', currentColumn%get_name( )
        write( FF_stdout, '(a,a)'),    'Format:  ', currentColumn%get_format( ) 
        write( FF_stdout, '(a,a)'),    'Unit:    ', currentColumn%get_unit( )
        currentColumn => currentColumn%go_next( )
      enddo
      currentTable => currentTable%go_next( )
    enddo

end subroutine print_table

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function last_table( this ) result( last )

    class( class_table ), target, intent(in)  :: this
    class( class_table ), pointer             :: last, table

    last => null()

    table => this
    do while (associated(table) )
      last => table
      table => table%go_next( )
    enddo

  end function last_table

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function first_table( this ) result( first )

    class( class_table ),         intent(in)     :: this
    class( class_table ), pointer                :: first

    first => null()
    if ( associated(this%first) ) first => this%first

  end function first_table

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function next_table( this ) result( next )

    class( class_table ),         intent(in)     :: this
    class( class_table ), pointer                :: next

    next => null()
    if ( associated(this%next) ) next => this%next

  end function next_table
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialize_table( this, tableName )

    class( class_table ), pointer,  intent(out)           :: this
    character(len=*),               intent(in), optional  :: tableName

    allocate( this )
    this%first => this

    if ( present(tableName) ) this%name = tableName

  end subroutine initialize_table

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Free the entire table
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine free_table( this )

    class( class_table ), target, intent(inout) :: this
    class( class_table ), pointer               :: current
    class( class_table ), pointer               :: next

    if ( FF_verbose_global > 1 ) write (FF_stdout, FF_FF_stdout_fmt), ' Freeing FITS table...'

    current => this

    do while (associated(current) )
      next => current%next
      if (associated( current%column) ) then
        call current%column%free( )
      endif
      deallocate(current)
      nullify(current)
      current => next
    enddo

  end subroutine free_table
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine  read_table( this, unit, columnName )

    class( class_table )                                  :: this
    integer,                        intent(in)            :: unit
    character(len=*), dimension(:), intent(in), optional  :: columnName

    integer                                                           :: i, j, repeat, status, tnull
    character(len=8)                                                  :: tdisp
    real( kind=FF_df )                                                :: tscal, tzero
    logical                                                           :: anyf
    character(len=FF_MAX_COL_LENGTH),    dimension(:), allocatable    :: colName
    character(len=FF_MAX_UNIT_LENGTH),   dimension(:), allocatable    :: colUnit
    character(len=FF_MAX_FORMAT_LENGTH), dimension(:), allocatable    :: colFormat

    integer(kind=2),                    dimension(:,:,:), allocatable :: data_i
    integer,                            dimension(:,:,:), allocatable :: data_j
    integer(kind=8),                    dimension(:,:,:), allocatable :: data_k
    real,                               dimension(:,:,:), allocatable :: data_e
    real(kind=FF_df),                   dimension(:,:,:), allocatable :: data_d
    character(len=FF_MAX_COL_LENGTH),   dimension(:,:,:), allocatable :: data_s

    integer                                                       :: n_i, n_j, n_k, n_e, n_d, n_s
    integer                                                       :: max_dim_i, max_dim_j, max_dim_k, max_dim_e, max_dim_d, max_dim_s
    integer                                                       :: indx_i, indx_j, indx_k, indx_e, indx_d, indx_s

    character(len=FF_MAX_COMMENT_LENGTH)                          :: comment
    integer                                                       :: frow, felem, naxis, maxdim, dim2, n_col_table, n_opt_rows, n_read_step, n_elems, n_rows_table
    integer,          dimension(:,:), allocatable                 :: naxes
    integer,          dimension(2)                                :: colSize
    logical,          dimension(:),   allocatable                 :: is_to_read

    class( table_column ), pointer :: column

    status = 0
    ! Get extension name ( if existing... )
    call ftgkys( unit, 'EXTNAME', this%name, comment, status )

    if ( FF_verbose_global > 1 .and. status == 0 ) write (FF_stdout, '(/,a,a,a,/)'), ' Reading the FITS table "' , trim(adjustl(this%name)), '"'
    status = 0

    ! Get number of rows
    call ftgnrw( unit, this%n_rows, status ) ; call cfitsio_print_error( status )
    if ( FF_verbose_global > 1 ) write (FF_stdout, *), ' Number of rows: ', this%n_rows
    n_rows_table = this%n_rows

    ! Get total number of columns in the table
    call ftgncl( unit, n_col_table, status ) ; call cfitsio_print_error( status )

    ! Read the names, unit, format and naxes of all columns
    allocate( colName(n_col_table), colUnit(n_col_table), colFormat(n_col_table), naxes(2,n_col_table) )

    maxdim = 2
    n_i = 0 ; n_j = 0 ; n_k = 0 ; n_e = 0 ; n_d = 0; n_s = 0
    max_dim_i = 1; max_dim_j = 1; max_dim_k = 1; max_dim_e = 1; max_dim_d = 1; max_dim_s = 1

    ! Check if you just want to read a subset of columns
    allocate( is_to_read(n_col_table) )

    do i=1, n_col_table
      call ftgbcl( unit, i, colName(i), colUnit(i), colFormat(i), repeat, tscal, tzero, tnull, tdisp, status ) ; call cfitsio_print_error( status )
      call ftgtdm( unit, i, maxdim, naxis, naxes(:,i), status ) ; call cfitsio_print_error( status )

      ! Check if the column name just read is within the column list provided by the user (if present)
      is_to_read(i) = .true.
      if ( present( columnName) ) then
        is_to_read(i) = .false.
        do j=1, size(columnName)
          if ( FF_str_eq_no_case(trim(adjustl(columnName(j))), trim(adjustl(colName(i)))) ) then
            is_to_read(i) = .true.
            exit
          endif
        enddo
      endif

      if ( is_to_read(i) ) then
        select case ( colFormat(i) )
          case ( 'I' )
            n_i = n_i + 1
            max_dim_i = max( max_dim_i, naxes(1,i) )
          case ( 'J' )
            n_j = n_j + 1
            max_dim_j = max( max_dim_j, naxes(1,i) )
          case ( 'K' )
            n_k = n_k + 1
            max_dim_k = max( max_dim_k, naxes(1,i) )
          case ( 'E' )
            n_e = n_e + 1
            max_dim_e = max( max_dim_e, naxes(1,i) )
          case ( 'D' )
            n_d = n_d + 1
            max_dim_d = max( max_dim_d, naxes(1,i) )
          case ( 'S' )
            n_s = n_s + 1
            max_dim_s = max( max_dim_s, naxes(1,i) )
        end select
      endif

    enddo

    ! Get number of columns to read
    this%n_columns = count( is_to_read )
    if ( FF_verbose_global >= 2 ) write (FF_stdout, *), ' Number of columns to read: ', this%n_columns

    frow = 1 ; felem = 1

    ! Get optimum number of rows to read at each step 
    call ftgrsz( unit, n_opt_rows, status ) ; call cfitsio_print_error( status )
    n_opt_rows = int( n_opt_rows / 2.5 )
    n_read_step = int( 1. * n_rows_table/n_opt_rows )
    if ( n_read_step * n_opt_rows /=  n_rows_table ) n_read_step = n_read_step + 1

    ! Allocate temporary arrays to contain the fits columns
    if ( n_i > 0 ) allocate( data_i(n_rows_table, n_i, max_dim_i) )
    if ( n_j > 0 ) allocate( data_j(n_rows_table, n_j, max_dim_j) )
    if ( n_k > 0 ) allocate( data_k(n_rows_table, n_k, max_dim_k) )
    if ( n_e > 0 ) allocate( data_e(n_rows_table, n_e, max_dim_e) )
    if ( n_d > 0 ) allocate( data_d(n_rows_table, n_d, max_dim_d) )
    if ( n_s > 0 ) allocate( data_s(n_rows_table, n_s, max_dim_s) )

    ! Read the FITS file columns
    if ( FF_verbose_global >= 2 ) write (FF_stdout, *), ' Reading data from the FITS file '

    do j=1, n_read_step

      n_elems = n_opt_rows
      if ( j == n_read_step ) n_elems = n_rows_table - frow

      indx_i = 1; indx_j = 1; indx_k = 1; indx_e = 1; indx_d = 1; indx_s = 1

      do i=1, n_col_table

        if ( is_to_read(i) ) then

          dim2 = naxes(1,i)

          select case ( colFormat(i) )
            case ( 'I' )
              call ftgcvi( unit, i, frow, felem, n_elems, FF_DEFAULT_SHORT_INTEGER, data_i(frow:frow+n_elems,indx_i,1:dim2), anyf, status ) ; call cfitsio_print_error( status )
              indx_i = indx_i + 1
            case ( 'J' )
              call ftgcvj( unit, i, frow, felem, n_elems, FF_DEFAULT_INTEGER, data_j(frow:frow+n_elems,indx_j,1:dim2), anyf, status ) ; call cfitsio_print_error( status )
              indx_j = indx_j + 1
            case ( 'K' )
              call ftgcvk( unit, i, frow, felem, n_elems, FF_DEFAULT_LONG_INTEGER, data_k(frow:frow+n_elems,indx_k,1:dim2), anyf, status ) ; call cfitsio_print_error( status )
              indx_k = indx_k + 1
            case ( 'E' )
              call ftgcve( unit, i, frow, felem, n_elems, FF_DEFAULT_REAL, data_e(frow:frow+n_elems,indx_e,1:dim2), anyf, status ) ; call cfitsio_print_error( status )
              indx_e = indx_e + 1
            case ( 'D' )
              call ftgcvd( unit, i, frow, felem, n_elems, FF_DEFAULT_DOUBLE, data_d(frow:frow+n_elems,indx_d,1:dim2), anyf, status ) ; call cfitsio_print_error( status )
              indx_d = indx_d + 1
            case ( 'S' )
              call ftgcvs( unit, i, frow, felem, n_elems, FF_DEFAULT_CHAR, data_s(frow:frow+n_elems,indx_s,1:dim2), anyf, status ) ; call cfitsio_print_error( status )
              indx_s = indx_s + 1
          end select

        endif

      enddo

      frow = frow + n_elems

    enddo

    ! Put data in your FITS tabel and column structures
    if ( FF_verbose_global >= 2 ) write (FF_stdout, *), ' Putting data into structure'

    indx_i = 1; indx_j = 1; indx_k = 1; indx_e = 1; indx_d = 1; indx_s = 1

    do i=1, n_col_table

      if ( is_to_read(i) ) then

        dim2 = naxes(1,i) ; colSize = [ dim2, this%n_rows ]

        select case ( colFormat(i) )
          case ( 'I' )
            if ( .not. associated(this%column) ) then 
              call initialize_column( this%column, colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_i(:,indx_i,1:dim2), mold_int ) )
            else
              column => this%column%go_last( )
              call column%add( colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_i(:,indx_i,1:dim2), mold_int ) )
            endif
            indx_i = indx_i + 1
          case ( 'J' )
            if ( .not. associated(this%column) ) then 
              call initialize_column( this%column, colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_j(:,indx_j,1:dim2), mold_int ) )
            else
              column => this%column%go_last( )
              call column%add( colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_j(:,indx_j,1:dim2), mold_int ) )
            endif
            indx_j = indx_j + 1
          case ( 'K' )
            if ( .not. associated(this%column) ) then 
              call initialize_column( this%column, colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_k(:,indx_k,1:dim2), mold_int ) )
            else
              column => this%column%go_last( )
              call column%add( colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_k(:,indx_k,1:dim2), mold_int ) )
            endif
            indx_k = indx_k + 1
          case ( 'E' )
            if ( .not. associated(this%column) ) then 
              call initialize_column( this%column, colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_e(:,indx_e,1:dim2), mold_int ) )
            else
              column => this%column%go_last( )
              call column%add( colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_e(:,indx_e,1:dim2), mold_int ) )
            endif
            indx_e = indx_e + 1
          case ( 'D' )
            if ( .not. associated(this%column) ) then 
              call initialize_column( this%column, colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_d(:,indx_d,1:dim2), mold_int ) )
            else
              column => this%column%go_last( )
              call column%add( colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_d(:,indx_d,1:dim2), mold_int ) )
            endif
            indx_d = indx_d + 1
          case ( 'S' )
            if ( .not. associated(this%column) ) then 
              call initialize_column( this%column, colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_s(:,indx_s,1:dim2), mold_int ) )
            else
              column => this%column%go_last( )
              call column%add( colName(i), colUnit(i), colFormat(i), colSize, data = transfer( data_s(:,indx_s,1:dim2), mold_int ) )
            endif
            indx_s = indx_s + 1
        end select

      endif

    enddo

  end subroutine read_table


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine  initialize_table_columns( this, columnName, columnUnit, columnFormat, size_of_row_increase, data )

    class( class_table )                              :: this

    character(len=*), dimension(:), intent(in)            :: columnName
    character(len=*), dimension(:), intent(in), optional  :: columnUnit
    character(len=*), dimension(:), intent(in), optional  :: columnFormat
    integer,                        intent(in), optional  :: size_of_row_increase
    real, dimension(:,:), intent(in), optional      :: data

    character(len=FF_MAX_COL_LENGTH)        :: colName
    character(len=FF_MAX_UNIT_LENGTH)        :: colUnit
    character(len=FF_MAX_FORMAT_LENGTH)        :: colFormat
    integer, dimension(2)                 :: colSize
    integer                                               :: i
    class( table_column ), pointer :: column


    this%n_columns = size( columnName )

    ! How many rows do you allocate at first??
    if ( present(size_of_row_increase) ) then
      this%size_of_row_increase = size_of_row_increase
    else
      this%size_of_row_increase = 1000
    endif

    if ( present(data) ) then
      this%n_rows = size( data, 1 )
    else
      this%n_rows = 0
    endif
    
    colSize = [ 1, this%n_rows ]
    colFormat = '' ; colUnit = '' 


    do i=1, this%n_columns

      colName = columnName(i) 
      if ( present(columnFormat) ) colFormat = columnFormat(i)
      if ( present(columnUnit) ) colUnit = columnUnit(i)

      ! Add new column
      if ( present(data) ) then
        if ( .not. associated(this%column) ) then 
          call initialize_column( this%column, columnName(1), columnUnit(1), columnFormat(1), colSize, data = transfer( data(:,i), mold_int ) )
        else
          column => this%column%go_last( )
          call column%add( colName, colUnit, colFormat, colSize, data = transfer( data(:,i), mold_int ) )
        endif
      else
        if ( .not. associated(this%column) ) then 
          call initialize_column( this%column, columnName(1), columnUnit(1), columnFormat(1), colSize )
        else
          column => this%column%go_last( )
          call column%add( colName, colUnit, colFormat, colSize )
        endif
      endif

    enddo

    this%counter = 0

  end subroutine initialize_table_columns

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine insert_values_table( this, columnsName, data )

    class( class_table )                     :: this
    character(len=*), dimension(:), intent(in) :: columnsName
    real, dimension(:,:), intent(in)             :: data

    integer                                      :: i, n_data

    class( table_column ), pointer                         :: column

    n_data = size(data,1)

    do i=1, size(columnsName) 

      column => this%column%find( columnsName(i) )
      if ( associated(column) ) then
        !if ( this%counter == this%n_rows ) call column%extend( max(n_data, this%size_of_row_increase) )
        call column%put_data( transfer(data(:,i), mold_int), append = .true. )
      endif

    enddo

    this%n_rows = size(column%data)
    this%counter = this%counter + n_data

  end subroutine insert_values_table
!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine insert_values_table_single_col( this, columnName, data )

    class( class_table )                        :: this
    character(len=*),               intent(in)  :: columnName
    integer,          dimension(:), intent(in)  :: data

    integer                                     :: n_data
    class( table_column ), pointer              :: column

    n_data = size(data,1)

    column => this%column%find( columnName )
    if ( associated(column) ) then
      !if ( this%counter == this%n_rows ) call column%extend( max(n_data, this%size_of_row_increase) )
      call column%put_data( data(:), append = .true. )
    endif

    this%n_rows = size(column%data)
    this%counter = this%counter + n_data

  end subroutine insert_values_table_single_col

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine append_columns_table( this, columnName, columnUnit, columnFormat, data, tableName )

    class( class_table ),         target                    :: this
    character(len=*), dimension(:),   intent(in)            :: columnName
    character(len=*), dimension(:),   intent(in), optional  :: columnUnit
    character(len=*), dimension(:),   intent(in), optional  :: columnFormat
    real,             dimension(:,:), intent(in)            :: data
    character(len=*),                 intent(in), optional  :: tableName

    character(len=FF_MAX_COL_LENGTH)                       :: colName
    character(len=FF_MAX_UNIT_LENGTH)                      :: colUnit
    character(len=FF_MAX_FORMAT_LENGTH)                    :: colFormat
    integer, dimension(2)                                  :: colSize
    integer                                                :: i
    class( class_table ),   pointer                        :: table
    class( table_column ),  pointer                        :: column

    if ( present(tableName) ) then
      table => this%find( tableName )
    else
      table => this
    endif

    colSize = [1, size(data,1) ]
    colUnit = '' ; colFormat = ''
      
    column => table%column%go_last( )

    do i = 1, size(columnName)
      colName  = columnName(i)
      if ( present(columnUnit) ) colUnit = columnUnit(i)
      if ( present(columnformat) ) colformat = columnformat(i)
      call column%add( colName, colUnit, colFormat, colSize, data = transfer( data(:,i), mold_int ) )
      column => column%go_last( )
    enddo

  end subroutine append_columns_table


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  recursive function find_table( this, name ) result( table )

    class( class_table ), target, intent(in)             :: this

    character(len=*),             intent(in)             :: name
    class( class_table ), pointer                        :: table

    table => null()

    if ( FF_str_eq_no_case( this%name, name)  ) then
      table => this 
    else if ( associated(this%next) ) then
      table => find_table( this%next, name )
    else
      write( FF_stderr, * ) ' [ ERROR: cannot find a table with name: ', trim(adjustl(name)), ' ] '
    endif

  end function find_table


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine add_table( this, tableName )

    class( class_table ), target                            :: this
    character(len=*),     intent(in), optional  :: tableName

    class( class_table ), pointer :: newTable

    ! Allocate new table
    allocate( newTable )

    newTable%next => this%next
    this%next => newTable
    !newTable%prev => this
    newTable%first => this%first

    if ( present(tableName) ) newTable%name = tableName

  end subroutine add_table


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !subroutine add_table( cols_name, cols_unit, cols_form, data, fileName, logical_unit, is_keep_open, extName, extPosition_name )
  subroutine write_table_to_disk( this, unit )

    class( class_table ), intent(in)  :: this
    integer,              intent(in)  :: unit

    class( class_table ),   pointer   :: table
    class( table_column ),  pointer   :: column
    integer                           :: i, status, varidat, n_col, frow, felem

    character(len=FF_MAX_COL_LENGTH),    dimension(:), allocatable   :: columnName
    character(len=FF_MAX_UNIT_LENGTH),   dimension(:), allocatable   :: columnUnit
    character(len=FF_MAX_FORMAT_LENGTH), dimension(:), allocatable   :: columnFormat

    status = 0 ; varidat = 0


    ! Point to the first table
    table => this%go_first( )

    do while ( associated(table) )

      ! Append/create a new empty HDU onto the end of the file and move to it.
      call ftcrhd( unit, status ) ; call cfitsio_print_error( status )

      ! Point to the first column of current table
      column => table%column%go_first()
      n_col = column%n_columns()

      if ( allocated(columnName) ) deallocate( columnName )
      if ( allocated(columnUnit) ) deallocate( columnUnit )
      if ( allocated(columnFormat) ) deallocate( columnFormat )

      allocate( columnName(n_col), columnUnit(n_col), columnFormat(n_col) )

      do i=1, n_col
        columnName(i) = column%get_name()
        columnUnit(i) = column%get_unit()
        columnFormat(i) = column%get_format()
        column => column%go_next()
      enddo

      ! FTPHBN writes all the required header keywords which define the structure of the binary table. NROWS and TFIELDS gives the number of rows and columns in the table, 
      ! and the TTYPE, TFORM, and TUNIT array, give the column name, format, and units, respectively of each column.
      call ftphbn( unit, table%n_rows, n_col, columnName, columnFormat, columnUnit, table%name, varidat, status ) ; call cfitsio_print_error( status )
      
      ! Write data to the binary table. Check if a string column is present, in which case use FTPCLS, otherwise exploit the implicit type conversion to write numerical data
      ! The FITSIO routines are column oriented, so it is usually easier to read or write data in a table in a column by column order rather than row by row.  
      ! Note that the identical subroutine calls are used to write to either ASCII or binary FITS tables.
      frow = 1 ; felem = 1

      column => table%column%go_first()

      do i=1, n_col
        if ( index(columnFormat(i), 'A') > 0 ) then
          call ftpcls( unit, i, frow, felem, table%n_rows, transfer(column%get_data(),mold_string), status ) ; call cfitsio_print_error( status )

      ! short integer (I*2, 8-bit integer)
        elseif ( index(columnFormat(i), 'I') > 0 ) then
          call ftpcli( unit, i, frow, felem, table%n_rows, transfer(column%get_data(),mold_short_int), status ) ; call cfitsio_print_error( status )

      ! integer (I*4, 16-bit integer)
        elseif ( index(columnFormat(i), 'J') > 0 ) then
          call ftpclj( unit, i, frow, felem, table%n_rows, transfer(column%get_data(),mold_int), status ) ; call cfitsio_print_error( status )

      ! integer (I*8, 32-bit integer)
        elseif ( index(columnFormat(i), 'K') > 0 ) then
          call ftpclk( unit, i, frow, felem, table%n_rows, transfer(column%get_data(),mold_long_int), status ) ; call cfitsio_print_error( status )

      ! e - real exponential floating point (R*4)
        elseif ( index(columnFormat(i), 'E') > 0 ) then
          call ftpcle( unit, i, frow, felem, table%n_rows, transfer(column%get_data(),mold_float), status ) ; call cfitsio_print_error( status )

      ! d - double precision real floating-point (R*8)
        elseif ( index(columnFormat(i), 'D') > 0 ) then
          call ftpcld( unit, i, frow, felem, table%n_rows, transfer(column%get_data(),mold_double), status ) ; call cfitsio_print_error( status )

        else
          write( FF_stderr, * ) ' [ ERROR: Unknown DATA TYPE: ', trim(adjustl(columnFormat(i))), ' ] '
        endif
        column => column%go_next()
      enddo

      table => table%go_next( )
    enddo

  end subroutine write_table_to_disk


end module fits_table
