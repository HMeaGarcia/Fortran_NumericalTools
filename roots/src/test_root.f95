program roots_test
    use iso_fortran_env, only:  dp => real64, i4 => int32
    use test_functions
    use roots 
    implicit none
    
    real(dp) ::  p0 ,p1
    
    
    
    p0 = 1
    p1 = 3.1
    p0 = -1
    p1 = 0
    !call secant_method_1(p0,p1,f)
    call secant_method_1(p0,p1,f1)

contains





end program roots_test