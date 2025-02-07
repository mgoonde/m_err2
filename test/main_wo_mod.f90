program main_wo_mod

  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
  implicit none
  type( c_ptr ) :: bugs=c_null_ptr
  integer :: ierr

  ! initialise err2
  call err2_init( bugs )

  ! call some routine
  call caller( bugs, ierr )

  ! write final error message
  if( ierr /= 0 ) call err2_write(bugs, __FILE__,__LINE__)

  ! close err2
  call err2_close(bugs)

end program


subroutine caller( bugs, ierr )
  use, intrinsic :: iso_c_binding, only: c_ptr
  type( c_ptr ), intent(in) :: bugs
  integer, intent(out) :: ierr

  ! call routine which produces error value
  call routine_with_error( bugs, ierr )
  ! add this routine to list of callers
  if( ierr /= 0 ) call err2_caller(bugs, __FILE__,__LINE__)
end subroutine caller

subroutine routine_with_error( bugs, ierr )
  use, intrinsic :: iso_c_binding, only: c_ptr
  type( c_ptr ), intent(in) :: bugs
  integer, intent(out) :: ierr

  ! some error happens here
  ierr = -2
  call err2_set( bugs, ierr, __FILE__,__LINE__, &
       "error happened due to something here" )
  return

end subroutine routine_with_error

