module ode
    !compile before the main program
    use iso_fortran_env, only:  dp => real64
    implicit none
   
    !private  ! Makes all below private
    ! This defines an arbitrary funcion to use in the other functions below

    abstract interface
    function func(x, t) result(rest)

        use iso_fortran_env, only:  dp => real64
        implicit none 
        real(dp), intent(in):: x(:), t
        real(dp) :: rest(size(x))
            
        end function func

    end interface

contains

function euler_step(x ,t, dt, f) result(y)
    ! This functions implements the Euler Step 
    ! in a vectorized manner
    ! 

    implicit none
    real(dp), intent(in) :: x(:) !X is a vector of solutions 
    real(dp), intent(in) :: t
    real(dp), intent(in) :: dt
   procedure(func) :: f

    real(dp) :: y(size(x))

    ! Declaring a function as an argument . "Callbacks"
    

    y = f(x, t)*dt + x

    end function euler_step

    function Heuns_step(u_k ,t, dt, f) result(y)
    ! This functions implements the Heuns Method
    ! in a vectorized mannes
    ! u_k :: vector of finite dimension
    implicit none
    real(dp), intent(in) :: u_k(:), t, dt
    
    procedure(func) :: f
    real(dp) :: y(size(u_k)), u_star(size(u_k))

    u_star = u_k + dt*f(u_k, t)
    
    y = u_k + 0.5*dt *f(u_k, t) + 0.5*dt*f(u_star, t + dt)
    
    end function Heuns_step

function RK4_step(u_k ,t, dt, f) result(y)
        ! This functions implements the Heuns Method
        ! 
        implicit none
        real(dp), intent(in) :: u_k(:), t, dt
        
        procedure(func) :: f
        real(dp) :: y(size(u_k)), k_1(size(u_k)), k_2(size(u_k)), k_3(size(u_k)), k_4(size(u_k))
        
    
        k_1 = f(u_k, t)
        k_2 = f(u_k + k_1*dt*0.5, t + dt*0.5 )
        k_3 = f(u_k + k_2*dt*0.5, t + dt*0.5 )
        k_4 = f(u_k + dt*k_3    , t + dt     )

    
        y = u_k + dt/6* (k_1 + 2*k_2 +  2*k_3 + k_4 )
        
    end function RK4_step




end module ode