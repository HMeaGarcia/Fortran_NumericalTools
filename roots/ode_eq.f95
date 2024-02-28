program shooting_eq
    use ode
    use iso_fortran_env, only:  dp => real64, i4 => int32
    use roots 
    implicit none
    
    
    !integer(i4), parameter :: n = 1000 
    !character(*), parameter :: method = 'Rk4'
    !logical :: full_sol = .false.

    !real(dp), parameter, dimension(2) :: x_i = [0.0_dp, 48.0_dp] !double precision number
    !real(dp), parameter, dimension(2) :: t_span = [0.0_dp, 28.0_dp] !double precision number
    !real(dp), dimension(2) :: x_f 

   
   

    !call solve_ivp(n, t_span ,x_i, method, full_sol, x_f)
    real(dp), parameter, dimension(2) :: x_i = [0.0_dp, 48.0_dp] !double precision number
    real(dp), dimension(2) :: x_f 
    
    call y_final(x_i, x_f)

    print*, x_f
  

        



contains


subroutine y_final(xi,  xf)
    real(dp),  intent(in) :: xi(2)
    real(dp), intent(out) :: xf(2)

    integer(i4), parameter :: n = 1000 
    character(*), parameter :: method = 'Rk4'
    logical :: full_sol = .false.

    !real(dp), parameter, dimension(2) :: x_i = [0.0_dp, 48.0_dp] !double precision number
    real(dp), parameter, dimension(2) :: t_span = [0.0_dp, 10.0_dp] !double precision number
   
    

    call solve_ivp(n, t_span ,xi, method, full_sol, xf)

    


    
end subroutine y_final


subroutine solve_ivp(n, t_span, x0, method, full_otp, xf)
    !implicit none
    logical, intent(in) :: full_otp
    real(dp), intent(out) :: xf(:)

    integer(i4), intent(in) :: n
    !real(dp), intent(in):: ti, tf 

    
    integer(i4), parameter :: id_file = 100
    character(*), intent(in) :: method 
    real(dp), intent(in) :: x0(:),t_span(:)
    
     character(*), parameter :: file_name = 'ivp_sol.dat' ! (*) indefined string length
    real(dp) :: d_t, tf,ti
    
    
    real(dp) :: x(2, n)
    !real(dp) :: xf(2)
    real(dp), dimension(n) :: t



    integer(i4) :: i 
    tf =  t_span(2)
    ti  = t_span(1)

    d_t = (tf - ti)/(n -1)
       
   

    !Call subroutine of the initial conditions
    call set_time_grid(t, ti, d_t)
    x(:,1) = x0

    if ( method == 'Rk4' ) then
        do i = 1 , n - 1
         x(:, i + 1) = RK4_step( x(:, i),t(i), d_t, f) 
        end do 

    elseif (method == 'Euler'  ) then
        do i = 1 , n - 1

         x(:, i + 1) =   euler_step( x(:, i),t(i), d_t, f) 
         end do 
    elseif ( method == 'Heuns' ) then
        do i = 1 , n - 1   
         x(:, i + 1) =   Heuns_step( x(:, i),t(i), d_t, f) 
         end do 
    end if 

    if (full_otp) then
        print*, 'The full solution will be saved'
        open(unit = id_file , file = file_name)

        do i = 1,n
            write(id_file,*) t(i), x(:, i)
        end do


    else
        print*, 'Only the final steps will be saved'
        xf = x(:,n)
        print*, xf
        

    end if
    

    
end subroutine solve_ivp

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

        
        ! dy/dt = v
        !dv/dt-g
        y = [x(2), -g]


    end function    f   


    
end program  shooting_eq
