! ===== integrador_rk4.f90 =====
module integrador_rk4
  use definicao_EDO
  implicit none
contains
  subroutine passo_rk4(x,y,h,lam)
    real(8),intent(inout):: x,y(2)
    real(8),intent(in)   :: h,lam
    real(8):: k1(2),k2(2),k3(2),k4(2),yt(2)
    call f_ode(x,y,lam,k1)
    yt = y + 0.5d0*h*k1 ; call f_ode(x+0.5d0*h, yt, lam, k2)
    yt = y + 0.5d0*h*k2 ; call f_ode(x+0.5d0*h, yt, lam, k3)
    yt = y + h*k3       ; call f_ode(x+h,       yt, lam, k4)
    y = y + h*(k1+2*k2+2*k3+k4)/6.d0
    x = x + h
  end subroutine passo_rk4
end module integrador_rk4