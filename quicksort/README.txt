Matt Mecham's Quicksort Algorithm
CS354 - Spring 2022

*********************************************************************************
                                 QUICKSORT
*********************************************************************************
This program uses modules to accomplish a quicksort algorithm. You can think of
modules as Fotran's answer of OOP. Subroutines and functions are defined and
implemented in a module to be used. In my case, my main program calls
USE QuickAlg
This statement gives access to the subroutines contained in the module.
The subroutines can take any integer array and sort the values within

To compile, first download the right compiler from 
https://gcc.gnu.org/wiki/GFortranBinaries#Linux

On the Onyx server, you can run on the command line:
$gfortran -o quicksort quick.f90 quick_module.f90
