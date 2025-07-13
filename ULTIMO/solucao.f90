module solucao
  use constantes
  use problema
  use integrador_rk4
  implicit none
contains

  !==================================================================
  ! Salva a solução u(x) para cada autovalor em arquivo
  ! Mesmas condições iniciais singulares do atirador
  !==================================================================
  subroutine salvar_sol(lambda, idx)
    real(8), intent(in) :: lambda
    integer, intent(in):: idx

    real(8) :: y(2), x, h
    integer :: i
    character(len=32) :: fname

    write(fname,'(A,I0,A)') 'solucao_autovalor_', idx, '.dat'
    open(unit=20, file=fname, status='replace')

    ! Condições iniciais em x=a
    x    = a
    h    = (b - a) / real(passos,8)
    y(1) = 1.0d0
    y(2) = p(a) * ( q(a) - lambda*r(a) ) / dp(a)

    ! Grava e integra
    do i = 0, passos
      write(20,'(F10.6,1X,F12.6)') x, y(1)
      if (i < passos) call passo_rk4(x, y, h, lambda)
    end do

    close(20)
  end subroutine salvar_sol

end module solucao
