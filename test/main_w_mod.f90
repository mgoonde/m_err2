program main_w_mod

  use m_err2
  implicit none
  type( t_err2 ), pointer :: bugs => null()
  integer :: ierr

  bugs => t_err2()

  ! call some routine
  call caller( bugs, ierr )

  ! write final error message
  if( ierr /= 0 ) call bugs% err_write(__FILE__,__LINE__)

  deallocate( bugs )
end program


subroutine caller( bugs, ierr )
  use m_err2, only: t_err2
  type( t_err2 ), intent(inout) :: bugs
  integer, intent(out) :: ierr

  ! call routine which produces error value
  call routine_with_error( bugs, ierr )
  ! add this routine to list of callers
  if( ierr /= 0 ) call bugs% err_caller(__FILE__,__LINE__)
end subroutine caller

subroutine routine_with_error( bugs, ierr )
  use m_err2, only: t_err2
  type( t_err2 ), intent(inout) :: bugs
  integer, intent(out) :: ierr

  ! some error happens here
  ierr = -2
  call bugs%err_set(ierr, __FILE__,__LINE__, &
       msg="error happened there due to something." // new_line('a') //&
       "Some words in array: ", msg_arr=["aa", "bb", "cc"] )
  return

end subroutine routine_with_error

