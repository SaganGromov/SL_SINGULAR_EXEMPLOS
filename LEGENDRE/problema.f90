module problema
  implicit none
contains

  !------------------------------------------------------------------
  ! Coeficientes do S–L:
  !   p(x), p'(x), q(x), r(x)
  !------------------------------------------------------------------

  pure function p(x) result(val)
    real(8), intent(in) :: x
    real(8)             :: val
    ! Exemplo: p(x) = 1
    val = 1-x*x
  end function p

  pure function dp(x) result(val)
    real(8), intent(in) :: x
    real(8)             :: val
    ! Derivada de p(x):
    ! Se p(x)=1 ⇒ p'(x)=0
    val = -2*x
  end function dp

  pure function q(x) result(val)
    real(8), intent(in) :: x
    real(8)             :: val
    ! Exemplo atual:
    val = 0
  end function q

  pure function r(x) result(val)
    real(8), intent(in) :: x
    real(8)             :: val
    ! Exemplo atual:
    val = 1
  end function r

end module problema
