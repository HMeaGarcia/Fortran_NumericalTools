program shooting_eq
    use ode
    use iso_fortran_env, only:  dp => real64, i4 => int32
    implicit none
    
    
    
    integer(i4), parameter :: n = 1000 ! integer single precision
    !integer(i4), parameter :: id_file = 100! integer single precision
    
    
    !real(dp), parameter :: t_i = 0.0_dp !double precision number
    real(dp), parameter :: t_f = 10.0_dp !double precision number
    !real(dp), parameter, dimension(2) :: x_i = [0.0_dp, 48.0_dp] !double precision number
    
    !character(*), parameter :: file_name = 'Shooting.dat' ! (*) indefined string length
    !real(dp), parameter :: d_t = (t_f - t_i)/(n -1)
    
    call solver(n,  t_f)


    !real(dp) :: x(2, n)
    !real(dp) :: xf(2)
    !real(dp), dimension(n) :: t



    !integer(i4) :: i 

    
       

    !Call subroutine of the initial conditions
    !call set_time_grid(t, t_i, d_t)
    !x(:,1) = x_i

    !do i = 1 , n - 1

     !   x(:, i + 1) =  Heuns_step( x(:, i),t(i), d_t, f )  !f(x(i), t_i)*d_t + x(i)


   ! end do 

    !open(unit = id_file , file = file_name)

    !do i = 1,n
     !   write(id_file,*) t(i), x(:, i)
    !end do
   ! a, b = x(:,n-1)
    !Get final step of solutions
    !xf = x(:,n)

    !print*, xf
    



contains


subroutine solver(n,  t_f)
    !implicit none
    integer(i4), intent(in) :: n
    real(dp), intent(in):: t_f !double precision number

    !integer(i4), parameter :: n = 1000 ! integer single precision
    integer(i4), parameter :: id_file = 100! integer single precision
    
    
    real(dp), parameter :: t_i = 0.0_dp !double precision number
    
    real(dp), parameter, dimension(2) :: x_i = [0.0_dp, 48.0_dp] !double precision number
    
    character(*), parameter :: file_name = 'Shooting.dat' ! (*) indefined string length
    real(dp) :: d_t
    
    
    real(dp) :: x(2, n)
    real(dp) :: xf(2)
    real(dp), dimension(n) :: t



    integer(i4) :: i 

    d_t = (t_f - t_i)/(n -1)
       

    !Call subroutine of the initial conditions
    call set_time_grid(t, t_i, d_t)
    x(:,1) = x_i

    do i = 1 , n - 1

        x(:, i + 1) =  Heuns_step( x(:, i),t(i), d_t, f )  !f(x(i), t_i)*d_t + x(i)


    end do 

    open(unit = id_file , file = file_name)

    !do i = 1,n
     !   write(id_file,*) t(i), x(:, i)
    !end do
   ! a, b = x(:,n-1)
    !Get final step of solutions
    xf = x(:,n)

    print*, xf
    

    
end subroutine solver

subroutine set_time_grid(t, ti ,dt)
    real(dp), intent(out) :: t(:) ! because is gonna change
    real(dp), intent(in) :: ti, dt ! because they are constant 
    integer(i4) :: i 

    t(1) = ti
    ! initialize time variable 

    do  i = 1, size(t)-1
        t(i+1) = ti +  dt*i  

    end do



    

    end subroutine set_time_grid

    function f(x,t) result(y)
    
        use iso_fortran_env, only:  dp => real64 ! , i4 => int32  for use in another file
        implicit none  !
        
        real(dp), intent(in) :: x(:) 
        real(dp), intent(in) :: t
        real(dp) :: y(size(x))
        real(dp), parameter :: g = 9.8 

        ! define function

         ![y0,v0]
        ! dy/dt = v
        !dv/dt-g
        !y = [-x(1), x(1) - 2* x(2)]
        y = [x(2), -g]


    end function    f   


    
end program  shooting_eq

