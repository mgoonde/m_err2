# compile with module files
gfortran -cpp --free-line-length-none -I../ -o main_w_mod.x main_w_mod.f90 -L../ -lerr2

# compile only with library
gfortran -cpp --free-line-length-none -o main_wo_mod.x main_wo_mod.f90 -L../ -lerr2
