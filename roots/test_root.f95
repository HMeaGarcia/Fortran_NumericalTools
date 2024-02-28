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


subroutine secant_method(p0,  p1)
    !implicit none
    real(dp) :: delta, p0 ,p1, q0, q1, p
    integer(i4) :: i
    integer(i4), parameter :: n = 50
    real(dp), parameter :: TOL =.00001
    character(*), parameter :: print_answer = 'True'
    open(unit = 69, file = 'roots.dat')

    q0 = f(p0)
    q1 = f(p1)

    if   (print_answer == 'True') then
        write(69, * ) p0
        write(69, * ) p1
    end if

    do  i = 1, n
        delta = q1*(p1 - p0)/(q1 - q0)
        p = p1 - delta
        if   (print_answer == 'True') then
            write(69, * ) p
        end if
       
        if (abs(p - p1) < TOL) then
            if   (print_answer == 'False') then
                write(69, * ) p
            end if
            stop
        end if
       
        p0 = p1
        q0 = q1
        p1 = p
        q1 = f(p)
        


    end do 
    
end subroutine secant_method


end program roots_test