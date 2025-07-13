module atirador
  use constantes
  use problema
  use integrador_rk4
  implicit none
contains

  !==================================================================
  ! Sub-rotina SHOOT para PVI singular:
  !   y1(a)=u(a)=1
  !   y2(a)=p(a)*u'(a)= p(a)*(q(a)-λ r(a))/p'(a)
  !   devolve F=γu(b)+δu'(b) e número de nós
  !==================================================================
  subroutine shoot(lam, F, nodes)
    real(8), intent(in)  :: lam
    real(8), intent(out) :: F
    integer, intent(out):: nodes

    real(8) :: y(2), x, h, yprev, v
    integer :: i

    !--- Condições iniciais no ponto singular x=a ---
    x     = a
    y(1)  = 1.0d0
    y(2)  = p(a) * ( q(a) - lam*r(a) ) / dp(a)

    h     = (b - a) / real(passos,8)
    nodes = 0
    yprev = y(1)

    !--- Integração RK4 de x=a até x=b ---
    do i = 1, passos
      call passo_rk4(x, y, h, lam)
      if (y(1) * yprev < 0.d0) nodes = nodes + 1
      yprev = y(1)
    end do

    !--- Avalia F(λ)=γu(b)+δu'(b) ---
    v = y(2) / p(b)           ! u'(b)
    F = gamma * y(1) + delta * v

  end subroutine shoot

end module atirador
