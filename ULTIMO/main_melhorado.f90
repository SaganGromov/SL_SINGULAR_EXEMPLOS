program principal_melhorado
  use constantes
  use espectro_melhorado
  use solucao
  use problema
  use integrador_rk4
  use atirador
  implicit none
  
  real(8) :: eig(n_eigen), eig_varredura(n_eigen)
  type(info_autovalor) :: info_detalhada(n_eigen)
  integer :: i
  real(8) :: tempo_inicio, tempo_fim
  
  
  print '(A,F5.2,A,F5.2,A)', 'Condições em x=1: ', gamma, 'y + ', delta, 'y'' = 0'
  print '(A,I2)', ' Nível de verbose: ', verbose_level
  
  ! Método 1: Com validação de autovalores (sem limites)
  print *, 'MÉTODO 1: Busca com validação (sem limites)'
  print *, '==========================================='
  call cpu_time(tempo_inicio)
  call encontrar_espectro_validado(eig, info_detalhada)
  call cpu_time(tempo_fim)
  print '(A,F8.2,A)', 'Tempo total: ', tempo_fim - tempo_inicio, ' segundos'
  
  ! Mostra resumo detalhado
  print *
  print *, 'RESUMO DOS AUTOVALORES ENCONTRADOS:'
  print *, '-----------------------------------'
  print *, ' idx       λ         Iter.Busca  Iter.Bissec   Nós    F(λ)'
  print *, '-----  ------------  ----------  -----------  ----  ----------'
  do i = 1, n_eigen
    if (eig(i) > 0.0d0) then
      write(*,'(I5,F14.8,I12,I13,I6,E12.4)') i, &
            info_detalhada(i)%lambda, &
            info_detalhada(i)%iter_busca, &
            info_detalhada(i)%iter_bisseccao, &
            info_detalhada(i)%nodes, &
            info_detalhada(i)%F_lambda
      
      ! Salva a solução
      call salvar_sol(eig(i), i)
    end if
  end do
  
  ! Análise estatística
  print *
  print *, 'ANÁLISE ESTATÍSTICA:'
  print *, '--------------------'
  call analisar_estatisticas(info_detalhada)
  
  ! Método 2 apenas se verbose_level < 3 (para não poluir muito a saída)
  if (verbose_level < 3) then
    print *
    print *, 'MÉTODO 2: Varredura direta de F(λ)'
    print *, '=================================='
    call cpu_time(tempo_inicio)
    call encontrar_por_varredura_F(eig_varredura, n_eigen)
    call cpu_time(tempo_fim)
    print '(A,F8.2,A)', 'Tempo total: ', tempo_fim - tempo_inicio, ' segundos'
    
    ! Comparação
    print *
    print *, 'COMPARAÇÃO DOS MÉTODOS:'
    print *, '-----------------------'
    print *, ' idx   Método 1      Método 2      Diferença'
    do i = 1, n_eigen
      if (eig(i) > 0.0d0 .or. eig_varredura(i) > 0.0d0) then
        write(*,'(I3,1X,F12.6,1X,F12.6,1X,E12.4)') i, eig(i), eig_varredura(i), &
              abs(eig(i) - eig_varredura(i))
      end if
    end do
  end if
  
 
  
  ! Verificação da qualidade
  print *
  print *, 'VERIFICAÇÃO DA QUALIDADE:'
  print *, '-------------------------'
  print *, ' idx       λ           F(λ)       Muda sinal?'
  do i = 1, n_eigen
    if (eig(i) > 0.0d0) then
      call verificar_qualidade(i, eig(i))
    end if
  end do
  

  
contains
  
  subroutine analisar_estatisticas(info)
    type(info_autovalor), intent(in) :: info(:)
    real(8) :: media_iter_busca, media_iter_bissec
    integer :: i, n_validos
    
    n_validos = 0
    media_iter_busca = 0.0d0
    media_iter_bissec = 0.0d0
    
    do i = 1, n_eigen
      if (info(i)%lambda > 0.0d0) then
        n_validos = n_validos + 1
        media_iter_busca = media_iter_busca + info(i)%iter_busca
        media_iter_bissec = media_iter_bissec + info(i)%iter_bisseccao
      end if
    end do
    
    if (n_validos > 0) then
      media_iter_busca = media_iter_busca / n_validos
      media_iter_bissec = media_iter_bissec / n_validos
      
      print '(A,F8.1)', ' Média de iterações de busca por autovalor: ', media_iter_busca
      print '(A,F8.1)', ' Média de iterações de bissecção: ', media_iter_bissec
      print '(A,I8)', ' Total de iterações de busca: ', info(n_validos)%iter_busca
    end if
  end subroutine analisar_estatisticas
  

    

  
  subroutine verificar_qualidade(idx, lambda)
    integer, intent(in) :: idx
    real(8), intent(in) :: lambda
    real(8) :: F, F_minus, F_plus
    integer :: nodes
    real(8) :: eps = 1.0d-6
    logical :: muda_sinal
    
    call shoot(lambda, F, nodes)
    call shoot(lambda - eps, F_minus, nodes)
    call shoot(lambda + eps, F_plus, nodes)
    
    muda_sinal = (F_minus * F_plus < 0.0d0)
    
    write(*,'(I3,1X,F14.8,1X,E12.4,6X,L1)') idx, lambda, F, muda_sinal
    
    ! Avisos
    if (abs(F) > 1.0d-4) then
      print *, '    ⚠️  AVISO: F(λ) não está próximo de zero!'
    end if
    if (.not. muda_sinal) then
      print *, '    ⚠️  AVISO: F não muda de sinal ao redor deste ponto!'
    end if
  end subroutine verificar_qualidade
  
  
end program principal_melhorado