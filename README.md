# Description

Independent error-storing and reporting type,
using the ``__FILE__`` and ``__LINE__`` macros of the C-preprocessor.

# Compile

## module-only
```bash
gfortran -cpp -ffree-line-length-none -c m_err2.f90
```

## shared library
```bash
gfortran -cpp -fPIC -o liberr2.so -ffree-line-length-none -c m_err2.f90
```

# Usage

## With, or without module:
see ``test/`` directory for two examples with, and without using the ``m_err2`` module.

## Brief:
To use `t_err2`, first initialise it:

```f90
 use m_err2
 type( t_err2 ), pointer :: bugs=>null()

 ! initialise
 bugs => t_err2()
```

When an error is captured somewhere, set a message and location as:

```f90
 ! here some error occurs, which gives a nonzero error code
 ierr = some_error()
 if( ierr /= 0 ) then
    ! set the error message and location to t_err2, optionally with some more words:
    call bugs% err_set( ierr, __FILE__, __LINE__, msg="error msg", msg_arr=[words] )
    return
 end if
```

When you wish to report the stored error message:

```f90
 if( ierr /= 0 ) then
    ! error code has been propagated down to some routine (i.e. main), now output it
    call bugs% err_write( __FILE__, __LINE__ )
 end if
```

If you wish to add intermediate caller routines to the message:

```f90
 ! an intermediate routine captured the ierr code, and passed it to caller
 ierr = something_that_passed_error()
 if( ierr /= 0 ) then
    ! set current location into list of callers
    call bugs% err_caller( __FILE__, __LINE__ )
    return
 end if
```
