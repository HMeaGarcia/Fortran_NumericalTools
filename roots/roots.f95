program roots
    use iso_fortran_env, only:  dp => real64, i4 => int32
    
    implicit none
    
    integer(i4), parameter :: n = 10
    integer(i4) :: i
    real(dp) :: delta, p0 ,p1, q0, q1, p
    real(dp), parameter :: TOL =.00001

    open(unit = 69, file = 'roots.dat')

    p0 = 0.5
    p1 = 0.1
    
    q0 = f(p0)
    q1 = f(p1)  
    write(69, * ) p0
    write(69, * ) p1

    do  i = 1, n
        delta = q1*(p1 - p0)/(q1 - q0)
        p = p1 - delta
        write(69, * ) p
        if (abs(p - p1) < TOL) then
             !write(69, * ) p
            stop
        end if
       
        p0 = p1
        q0 = q1
        p1 = p
        q1 = f(p)
        


    end do 

contains
    function f(x) result(y)
        use iso_fortran_env, only:  dp => real64 
        implicit none

        real(dp) :: y
        real(dp), intent(in) :: x
        y = cos(x)

    end function f       


end program roots