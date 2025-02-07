
module m_err2

  implicit none

  private
  public :: t_err2



  character(*), parameter :: my_name = "my_prog_name"
  character(*), parameter :: str_warn = achar(27)//"[1;31m Warning "//achar(27)//"[0m"
  character(*), parameter :: str_err = achar(27)//"[1;31m Error "//achar(27)//"[0m"
  character(*), parameter :: str_message = ":>> Message   : "
  character(*), parameter :: str_source  = ":>> Source    : "
  character(*), parameter :: str_caller  = ":>> Caller    : "
  character(*), parameter :: str_lastcaller  = ":>> Last caller :"

  type :: t_err2
     !! Independent error-storing and reporting type,
     !! using the `__FILE__` and `__LINE__` pre-processor macros.
     !!
     !! To use `t_err2`, first initialise it:
     !!```f90
     !! type( t_err2 ), pointer :: bugs=>null()
     !!
     !! ! initialise
     !! bugs => t_err2()
     !!```
     !!
     !! When an error is captured somewhere, set a message and location as:
     !!```f90
     !! ! here some error occurs, which gives a nonzero error code
     !! ierr = some_error()
     !! if( ierr /= 0 ) then
     !!    ! set the error message and location to t_err2:
     !!    call bugs% err_set( ierr, __FILE__, __LINE__, msg="error msg", msg_arr=[words] )
     !!    return
     !! end if
     !!```
     !!
     !! When you wish to report the stored error message:
     !!```f90
     !! if( ierr /= 0 ) then
     !!    ! error code has been propagated down to some routine (i.e. main), now output it
     !!    call bugs% err_write( __FILE__, __LINE__ )
     !! end if
     !!```
     !!
     !! If you wish to add intermediate caller routines to the message:
     !!```f90
     !! ! an intermediate routine captured the ierr code, and passed it to caller
     !! ierr = something_that_passed_error()
     !! if( ierr /= 0 ) then
     !!    ! set current location into list of callers
     !!    call bugs% err_caller( __FILE__, __LINE__ )
     !!    return
     !! end if
     !!```
     integer, private :: last_ierr                   !! integer value of last error
     character(:), allocatable, private :: errsrc    !! first location where error occured
     character(:), allocatable, private :: errmsg    !! error message
     character(:), allocatable, private :: callers   !! list of all callers following the error
   contains

     !> set an error message at the location where error occured
     procedure :: err_set => t_err2_err_set

     !> add a caller to the list of callers after an error
     procedure :: err_caller => t_err2_err_caller

     !> output the error message of currntly recevied error
     procedure :: err_write => t_err2_err_write

     !> check if an error is present or not
     procedure :: err_present => t_err2_err_present

     !> reset the variables of this class
     procedure :: err_reset => t_err2_err_reset

     final :: error_destructor
  end type t_err2


  interface t_err2
     module procedure :: error_constructor
  end interface t_err2


contains

  function error_constructor()result( this )
    type( t_err2 ), pointer :: this
    allocate( t_err2 :: this )
    this% last_ierr = 0
  end function error_constructor


  subroutine error_destructor( self )
    type( t_err2 ), intent( inout ) :: self

    ! output any remaining error from here
    if( self% last_ierr /= 0 ) call self% err_write(__FILE__,__LINE__)

    ! deallocate
    self% last_ierr = 0
    if(allocated(self%errsrc))deallocate(self%errsrc)
    if(allocated(self%errmsg))deallocate(self%errmsg)
    if(allocated(self%callers))deallocate(self%callers)

  end subroutine error_destructor


  subroutine t_err2_err_set( self, ierr, file, linenr, msg, msg_arr )
    !! set the information about an error.
    !! This should be called when an error is first encountered.
    implicit none
    class( t_err2 ), intent(inout) :: self
    integer, intent(in) :: ierr
    character(*), intent(in) :: file
    integer, intent(in) :: linenr
    character(*), intent(in), optional :: msg !! single-string error message
    character(*), intent(in), optional :: msg_arr(:) !! array of words to be added at end of error message

    character(len=516) :: loc
    integer :: i

    ! output any previous error
    if( self%last_ierr /= 0 ) call self%err_write(__FILE__,__LINE__)

    loc = ""
    write(loc, "(a,1x,a,1x,i0)") file,"line:",linenr
    ! delete previous error message
    if( allocated( self% errsrc))deallocate(self%errsrc)
    self%errsrc = trim(loc)

    ! if there is an error message, copy it
    if( present(msg))then
       if( allocated(self%errmsg))deallocate(self%errmsg)
       self%errmsg=""
       self%errmsg = msg
    end if
    if(present(msg_arr)) then
       if(.not.present(msg)) self%errmsg=""
       do i = 1, size(msg_arr(:))
          self%errmsg = trim(self%errmsg)//" "//trim(msg_arr(i))
       end do
    end if

    ! set list of callers to none
    if( allocated(self%callers))deallocate(self%callers)
    self%callers=""

    ! copy ierr value
    self%last_ierr = ierr
  end subroutine t_err2_err_set

  subroutine t_err2_err_caller( self, file, linenr )
    !! add a caller to list of callers
    implicit none
    class( t_err2 ), intent(inout) :: self
    character(*), intent(in) :: file
    integer, intent(in) :: linenr

    character(len=516) :: loc
    loc = ""
    write(loc, "(a,1x,a,1x,i0)") file,"line:",linenr

    if( len_trim(self%callers) < 1)then
       ! if first caller, write
       self%callers = str_caller//trim(loc)
    else
       ! \n + append
       self%callers = self%callers//new_line("a")//str_caller//trim(loc)
    end if

  end subroutine t_err2_err_caller

  subroutine t_err2_err_write( self, file, linenr, fileunit, kill )
    !! output the error info to screen.
    !! If fileunit is provided, the unit should be open for writing.
    !! If fileunit is not provided, output to screen.
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    implicit none
    class( t_err2 ), intent(inout) :: self
    character(*), intent(in) :: file
    integer, intent(in) :: linenr
    integer, intent(in), optional :: fileunit !! output fileunit
    logical, intent(in), optional :: kill !! if `.true.`, kill the program via simple `stop`

    integer :: u0
    character(len=512) :: loc, msg
    integer :: nlen

    u0 = stdout
    if(present(fileunit)) then
       u0 = fileunit
       ! test if file is actually open...
    end if

    write(u0,"(a)") repeat("=", 60)
    write(u0,"(a,1x,i0)") ":>>"//str_warn//"from "//my_name//", ierr value:", self% last_ierr
    write(u0,"(a)") ":>> Output from err_write():"

    ! saved error message
    if( allocated(self%errmsg)) then
       nlen = min(len(msg), len(self%errmsg))
       msg=""
       msg(1:nlen) = self%errmsg(1:nlen)
    else
       msg = "Message unknown"
    end if
    write(u0,"(a,a)") str_message, trim(msg)

    ! saved error location
    if( allocated(self%errsrc))then
       nlen = min(len(loc),len(self%errsrc))
       loc=""
       loc(1:nlen) = self%errsrc(1:nlen)
    else
       loc = "Source location unknown"
    end if
    write(u0, "(a,a)") str_source, trim(loc)

    ! write list of callers
    if( len_trim(self%callers) > 1) write(u0, "(a)") trim(self%callers)

    ! write last caller (called this routine)
    write(u0,"(3(a,1x),i0)") str_lastcaller,trim(file), "line:", linenr

    if( present(kill)) then
       if( kill ) then
          write(u0,*) "Stopping due to keyword `kill`"
! #ifdef USEMPI
!           ! properly kill an mpi program with abort world_comm
! #endif
          write(u0,"(a)") repeat("=", 60)
          stop
       end if
    end if

    write(u0,"(a)") repeat("=", 60)
    ! reset the error state
    call self% err_reset()
  end subroutine t_err2_err_write

  subroutine t_err2_err_present( self, ierr )
    !! Check if an error is present or not.
    !! On output, `ierr` has value of error present in the module, and 0 otherwise
    class(t_err2), intent(inout) :: self
    integer, intent(out) :: ierr
    ierr = self% last_ierr
  end subroutine t_err2_err_present

  subroutine t_err2_err_reset( self )
    class(t_err2), intent(inout) :: self
    self% last_ierr = 0
    ! self% errsrc=""
    ! self% errmsg=""
    ! self% callers=""
    if(allocated(self%errsrc))deallocate(self%errsrc)
    if(allocated(self%errmsg))deallocate(self%errmsg)
    if(allocated(self%callers))deallocate(self%callers)
  end subroutine t_err2_err_reset

end module m_err2





subroutine err2_init( cptr )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc
  use m_err2, only: t_err2
  type( c_ptr ) :: cptr

  type( t_err2 ), pointer :: fptr
  fptr => t_err2()
  cptr = c_loc( fptr )
  nullify(fptr)
end subroutine err2_init

subroutine err2_set( cptr, ierr, file, linenr, msg )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_associated
  use m_err2, only: t_err2
  type( c_ptr ), intent(in) :: cptr
  integer, intent(in) :: ierr
  character(*), intent(in) :: file
  integer, intent(in) :: linenr
  character(*), intent(in) :: msg

  type( t_err2 ), pointer :: fptr

  if( .not. c_associated(cptr)) then
     write(*,*) repeat('-',40)
     write(*,*) " >>:: err2 is not initialised, cannot set error."
     write(*,*) repeat('-',40)
     return
  end if
  call c_f_pointer( cptr, fptr )
  call fptr% err_set( ierr, file, linenr, msg=msg )
  nullify(fptr)
end subroutine err2_set

subroutine err2_caller( cptr, file, linenr )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_associated
  use m_err2, only: t_err2
  type( c_ptr ), intent(in) :: cptr
  character(*), intent(in) :: file
  integer, intent(in) :: linenr

  type( t_err2 ), pointer :: fptr

  if( .not. c_associated(cptr)) then
     write(*,*) repeat('-',40)
     write(*,*) " >>:: err2 is not initialised."
     write(*,*) repeat('-',40)
     return
  end if
  call c_f_pointer( cptr, fptr )
  call fptr% err_caller( file, linenr )
  nullify(fptr)
end subroutine err2_caller

subroutine err2_write( cptr, file, linenr )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_associated
  use m_err2, only: t_err2
  type( c_ptr ), intent(in) :: cptr
  character(*), intent(in) :: file
  integer, intent(in) :: linenr

  type( t_err2 ), pointer :: fptr

  if( .not. c_associated(cptr)) then
     write(*,*) repeat('-',40)
     write(*,*) " >>:: err2 is not initialised."
     write(*,*) repeat('-',40)
     return
  end if
  call c_f_pointer( cptr, fptr )
  call fptr% err_write( file, linenr )
  nullify(fptr)
end subroutine err2_write

subroutine err2_present( cptr, ierr )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_associated
  use m_err2, only: t_err2
  type( c_ptr ), intent(in) :: cptr
  integer, intent(out) :: ierr

  type( t_err2 ), pointer :: fptr

  if( .not. c_associated(cptr)) then
     write(*,*) repeat('-',40)
     write(*,*) " >>:: err2 is not initialised."
     write(*,*) repeat('-',40)
     return
  end if
  call c_f_pointer( cptr, fptr )
  call fptr% err_present( ierr )
  nullify(fptr)
end subroutine err2_present

subroutine err2_reset( cptr )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_associated
  use m_err2, only: t_err2
  type( c_ptr ), intent(in) :: cptr

  type( t_err2 ), pointer :: fptr

  if( .not. c_associated(cptr)) then
     write(*,*) repeat('-',40)
     write(*,*) " >>:: err2 is not initialised."
     write(*,*) repeat('-',40)
     return
  end if
  call c_f_pointer( cptr, fptr )
  call fptr% err_reset()
  nullify(fptr)
end subroutine err2_reset

subroutine err2_close( cptr )
  use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_associated
  use m_err2, only: t_err2
  type( c_ptr ), intent(in) :: cptr

  type( t_err2 ), pointer :: fptr

  if( .not. c_associated(cptr)) then
     write(*,*) repeat('-',40)
     write(*,*) " >>:: err2 is not initialised."
     write(*,*) repeat('-',40)
     return
  end if
  call c_f_pointer( cptr, fptr )
  deallocate(fptr)
end subroutine err2_close

