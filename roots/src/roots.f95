module roots
     !compile before the main program
    use iso_fortran_env, only:  dp => real64, i4 => int32
    implicit none
   
    abstract interface
    function func(x) result(rest)

        use iso_fortran_env, only:  dp => real64
        implicit none 
        real(dp), intent(in):: x
        real(dp) :: rest
            
        end function func

    end interface



contains


subroutine secant_method_1(p0,  p1, f, sol)
    implicit none
    real(dp), intent(inout) :: p0 ,p1
    real(dp), intent(out) :: sol
    real(dp) :: delta, q0, q1, p
    integer(i4) :: i
    integer(i4), parameter :: n = 50
    real(dp), parameter :: TOL =.0000001
    character(*), parameter :: print_answer = 'True'
    procedure(func) :: f

    open(unit = 69, file = './data/roots.dat')

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
    sol = p
    
end subroutine secant_method_1

end module roots
