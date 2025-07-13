! ===== definicao_EDO.f90 =====
module definicao_EDO
  use problema
  implicit none
contains
  subroutine f_ode(x,y,lam,dy)
    real(8),intent(in) :: x,lam
    real(8),intent(in) :: y(2)
    real(8),intent(out):: dy(2)
    dy(1) = y(2)/p(x)
    dy(2) = q(x)*y(1) - lam*r(x)*y(1)
  end subroutine f_ode
end module definicao_EDO