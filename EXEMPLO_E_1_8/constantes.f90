module constantes
  implicit none

  !---------------------------------------------------------
  ! Domínio
  !---------------------------------------------------------
  real(8), parameter :: a = 0.0d0 + 1.d-6
  real(8), parameter :: b = 1.0d0

  !---------------------------------------------------------
  ! Condições de contorno lineares  α y(a)+β y'(a)=0   γ y(b)+δ y'(b)=0
  !---------------------------------------------------------
  real(8), parameter :: gamma = 1.0d0, delta = 0.0d0   ! y(b)=0

  !---------------------------------------------------------
  ! Malha numérica e varredura em λ
  !---------------------------------------------------------
  integer, parameter :: passos        = 1.0d4
  real(8), parameter :: tol           = 1.0d-9
  integer, parameter :: n_eigen       = 5
  
  !---------------------------------------------------------
  ! Controle de saída detalhada
  ! 0 = Silencioso
  ! 1 = Informações básicas
  ! 2 = Informações detalhadas
  ! 3 = Debug completo (mostra cada etapa)
  !---------------------------------------------------------
  integer, parameter :: verbose_level = 0
  logical, parameter :: verbose = (verbose_level > 0)
  
  !---------------------------------------------------------
  ! Opções de debug
  !---------------------------------------------------------
  logical, parameter :: mostrar_bisseccao = (verbose_level >= 3)
  logical, parameter :: mostrar_shoot = (verbose_level >= 3)
  logical, parameter :: mostrar_validacao = (verbose_level >= 2)
  logical, parameter :: salvar_trajetoria = .false.  ! Salva F(λ) vs λ em arquivo
  
end module constantes