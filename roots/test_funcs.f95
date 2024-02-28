module test_functions
    
    use iso_fortran_env, only:  dp => real64, i4 => int32
    implicit none
    


contains
    function f(x) result(y)
        !use iso_fortran_env, only:  dp => real64 
        !implicit none

        real(dp) :: y
        real(dp), intent(in) :: x
        y = cos(x) + 1

    end function f    

    function f1(x) result(y)
        !use iso_fortran_env, only:  dp => real64 
        !implicit none

        real(dp) :: y
        real(dp), intent(in) :: x
        y = -x**3 -cos(x)

    end function f1


    
end module test_functions