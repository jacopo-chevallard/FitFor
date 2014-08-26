module fits_column

  use fits_parameters

  implicit none

  private
  public :: table_column, initialize_column

!+
  type :: table_column
!+

    !integer, kind = kind = kind(0.)
    !integer, n    = 3
    character(len = FF_MAX_COL_LENGTH)      :: name = ''
    character(len = FF_MAX_UNIT_LENGTH)     :: unit = ''
    character(len = FF_MAX_FORMAT_LENGTH)   :: format = ''
    integer, dimension(2)                   :: size = FF_DEFAULT_INTEGER
    integer                                 :: n_elements = 0

    integer, dimension(:), pointer          :: data => null()

    ! Linked list pointers

    class( table_column ), pointer          :: next => null()
    class( table_column ), pointer          :: prev => null()

    class( table_column ), pointer          :: first => null()
    class( table_column ), pointer          :: last => null()

    contains

      procedure :: add          => add_column
      procedure :: free         => free_columns
      procedure :: find         => find_column 
      procedure :: n_columns    => get_number_of_columns
      procedure :: get_data     => get_data_from_column 
      procedure :: put_data
      procedure :: go_next      => next_column
      procedure :: go_first     => first_column
      procedure :: go_last      => last_column
      procedure :: extend       => increase_column_rows
      procedure :: get_name     => get_column_name
      procedure :: get_format   => get_column_format
      procedure :: get_unit     => get_column_unit

  end type table_column

!+
  type table_column_ptr
!+

    type( table_column ), pointer       :: column => null()

  end type table_column_ptr
  
  contains  

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Initialize a head node SELF and optionally store the provided DATA.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initialize_column( this, columnName, columnUnit, columnFormat, columnSize, data, n_rows )

    class( table_column ), pointer  :: this

    character(len=*), intent(in)            :: columnName
    character(len=*), intent(in)            :: columnUnit
    character(len=*), intent(in)            :: columnFormat
    integer, dimension(:), intent(in)       :: columnSize
    integer, dimension(:), intent(in), optional :: data
    integer,               intent(in), optional :: n_rows

    allocate( this )

    this%first => this
    this%last => this

    this%name = columnName
    if ( len_trim(columnUnit) > 0 ) this%unit = columnUnit
    if ( len_trim(columnFormat) > 0 ) this%Format = columnFormat
    this%size = columnSize

    if ( present(n_rows) ) then
      allocate( this%data(n_rows) )
    endif

    if ( present(data) ) then
      if ( .not. associated(this%data) ) allocate( this%data(size(data)) )
      this%data(1:size(data)) = data
      this%n_elements = size(data)
    endif

end subroutine initialize_column


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Free the entire list and all data, beginning at SELF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine free_columns( this )

    class( table_column ), target  :: this
    class( table_column ), pointer :: current
    class( table_column ), pointer :: next

    if ( FF_verbose_global > 1 ) write (FF_stdout, FF_FF_stdout_fmt), ' Freeing columns...'

    current => this

    do while (associated(current) )
      next => current%next
      if (associated( current%data) ) then
        deallocate( current%data )
        nullify( current%data )
      endif
      deallocate(current)
      nullify(current)
      current => next
    enddo

  end subroutine free_columns

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine increase_column_rows( this, n_rows_to_add )

    class( table_column )                   :: this
    integer               , intent(in)      :: n_rows_to_add
    integer, dimension(:), allocatable         :: temp

    integer                                 :: n

    n = size( this%data )

    allocate( temp(n) )
    temp(1:n) = this%data 

    deallocate( this%data ) ; nullify(this%data) 
    allocate( this%data(n+n_rows_to_add) )
    this%data(1:n) = temp

  end subroutine increase_column_rows


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine add_column( this, columnName, columnUnit, columnFormat, columnSize, data, n_rows )

    class( table_column )                   :: this

    character(len=*), intent(in)            :: columnName
    character(len=*), intent(in)            :: columnUnit
    character(len=*), intent(in)            :: columnFormat
    integer, dimension(:), intent(in)       :: columnSize
    integer, dimension(:), intent(in), optional :: data
    integer,               intent(in), optional :: n_rows

    class( table_column ), pointer :: newColumn

    ! Allocate new column
    allocate( newColumn )

    newColumn%next => this%next
    this%next => newColumn
    newColumn%first => this%first
    !newColumn%last => newColumn
    !this%last => newColumn%last

    newColumn%name = trim(adjustl(columnName))
    if ( len_trim(columnUnit) > 0 ) newColumn%unit = columnUnit
    if ( len_trim(columnFormat) > 0 ) newColumn%Format = columnFormat
    newColumn%size = columnSize

    if ( present(n_rows) ) then
      allocate( newColumn%data(n_rows) )
    endif

    if ( present(data) ) then
      if ( .not. associated(newColumn%data) ) allocate( newColumn%data(size(data)) )
      newColumn%data(1:size(data)) = data
      newColumn%n_elements = size( data )
    endif

  end subroutine add_column 



!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function get_number_of_columns( this ) result( n_columns )

    class( table_column ), intent(in), target   :: this
    integer                                     :: n_columns
    class( table_column ), pointer              :: column

    column => this%go_first( )
    n_columns = 0

    do while ( associated(column) )
     n_columns = n_columns + 1
     column => column%go_next( )
    enddo

end function get_number_of_columns


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function get_column_name( this ) result( name )

    class( table_column ),        intent(in)  :: this
    character(len=FF_MAX_COL_LENGTH)             :: name

    name = trim(adjustl(this%name))

end function get_column_name

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function get_column_format( this ) result( format )

    class( table_column ),            intent(in)  :: this
    character(len=FF_MAX_FORMAT_LENGTH)              :: format

    format = trim(adjustl(this%format))

end function get_column_format

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function get_column_unit( this ) result( unit )

    class( table_column ),          intent(in)  :: this
    character(len=FF_MAX_UNIT_LENGTH)              :: unit

    unit = trim(adjustl(this%unit))

end function get_column_unit

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function get_data_from_column( this, name ) result( data )

    class( table_column ),  intent(in),  target   :: this
    character(len=*),       intent(in),  optional :: name
    integer, dimension(:), pointer                :: data

    class( table_column ), pointer                :: column

    if ( present(name) ) then
      column => this%find( name )
    else
      column => this
    endif

    data => column%data

end function get_data_from_column


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine put_data( this, data, name, append )

    class( table_column ),              intent(inout), target   :: this
    integer,              dimension(:), intent(in)              :: data
    character(len=*),                   intent(in), optional    :: name
    logical,                            intent(in), optional    :: append

    logical                                                     :: is_append
    integer                                                     :: n_data
    class( table_column ), pointer                              :: column

    if ( present(append) ) then
      is_append = append
    else
      is_append = .false.
    endif

    if ( present(name) ) then
      column => this%find( name )
    else
      column => this
    endif
    
    if ( is_append ) then
      n_data = size( data )

      if ( .not. associated(this%data) ) then
        allocate( this%data(n_data) )
        this%n_elements = 0
      else 
        if ( size(this%data) < (this%n_elements+n_data) ) call this%extend( n_data )
      endif

      this%data(this%n_elements+1:this%n_elements+n_data) = data
      this%n_elements = this%n_elements + n_data
    else
      if ( associated(this%data) ) then
        deallocate(this%data)
        nullify(this%data)
      endif
      this%data = data
      this%n_elements = size( data )
    endif


end subroutine put_data

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function last_column( this ) result( lastCol )

    class( table_column ), target, intent(in)  :: this
    class( table_column ), pointer             :: lastCol, column

    lastCol => null()

    column => this
    do while (associated(column) )
      lastCol => column
      column => column%go_next( )
    enddo

  end function last_column


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function first_column( this ) result( firstCol )

    class( table_column ), target,  intent(in)  :: this
    class( table_column ), pointer              :: firstCol

    firstCol => null()

    if ( associated(this%first) ) firstCol => this%first

  end function first_column

!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function next_column( this ) result( next )

    class( table_column ), target, intent(in)   :: this
    class( table_column ), pointer              :: next

    next => null()

    if ( associated(this%next) ) next => this%next

  end function next_column


!+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  recursive function find_column( this, name ) result( column )

    class( table_column ), target,  intent(in)  :: this
    character(len=*),               intent(in)  :: name
    class( table_column ), pointer              :: column

    column => null()

    if ( FF_str_eq_no_case( this%name, name)  ) then
      column => this 
    else if ( associated(this%next) ) then
      column => find_column( this%next, name )
    else
      if ( FF_verbose_global >= 1 ) write( FF_stderr, * ) ' [ WARNING: cannot find a column with name: ', trim(adjustl(name)), ' ] '
    endif

  end function find_column


end module fits_column
