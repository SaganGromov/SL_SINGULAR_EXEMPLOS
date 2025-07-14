module espectro_melhorado
  use constantes
  use atirador
  use modulo_bisseccao
  implicit none
  
  ! Tipo para armazenar informações detalhadas sobre cada autovalor
  type :: info_autovalor
    real(8) :: lambda
    real(8) :: F_lambda
    integer :: nodes
    integer :: iter_bisseccao
    integer :: iter_busca
    real(8) :: lambda_inicial
    real(8) :: lambda_final
  end type info_autovalor
  
contains
  
  ! Verifica se um candidato é realmente um autovalor
  function validar_autovalor(lambda) result(is_valid)
    real(8), intent(in) :: lambda
    logical :: is_valid
    real(8) :: F, F_plus, F_minus
    real(8) :: eps = 1.0d-7
    integer :: n_dummy
    
    if (mostrar_validacao) then
      print '(A,F15.8)', '    Validando candidato λ = ', lambda
    end if
    
    ! Verifica se F(λ) ≈ 0
    call shoot(lambda, F, n_dummy)
    
    if (mostrar_validacao) then
      print '(A,E12.4)', '      F(λ) = ', F
    end if
    
    if (abs(F) > tolBissec) then
      is_valid = .false.
      if (mostrar_validacao) then
        print *, '      REJEITADO: |F(λ)| > tolerância'
      end if
      return
    end if
    
    ! Verifica se há mudança de sinal em F ao redor de λ
    call shoot(lambda - eps, F_minus, n_dummy)
    call shoot(lambda + eps, F_plus, n_dummy)
    
    if (mostrar_validacao) then
      print '(A,E12.4,A,E12.4)', '      F(λ-ε) = ', F_minus, ', F(λ+ε) = ', F_plus
      print '(A,L1)', '      Mudança de sinal? ', (F_minus * F_plus < 0.0d0)
    end if
    
    ! Um autovalor verdadeiro deve ter F mudando de sinal
    is_valid = (F_minus * F_plus < 0.0d0)
    
    if (mostrar_validacao) then
      if (is_valid) then
        print *, '      ACEITO: Autovalor válido!'
      else
        print *, '      REJEITADO: F não muda de sinal'
      end if
    end if
    
  end function validar_autovalor
  
  ! Busca com informações detalhadas
  subroutine encontrar_espectro_validado(eig, info)
    real(8), intent(out) :: eig(n_eigen)
    type(info_autovalor), intent(out) :: info(n_eigen)
    
    real(8) :: lambda, lambda_prev, step, F, F_prev
    real(8) :: candidato, lambda_ini, lambda_fim
    integer :: n_dummy, n_encontrados, n_candidatos_totais, iteracoes
    integer :: iter_bissec, nodes_prev, nodes_curr
    logical :: mudou_sinal
    integer :: unit_traj
    character(len=20) :: metodo_usado
    
    ! Inicializa
    eig = 0.0d0
    n_encontrados = 0
    n_candidatos_totais = 0
    iteracoes = 0
    
    ! Abre arquivo para salvar trajetória se solicitado
    if (salvar_trajetoria) then
      unit_traj = 50
      open(unit=unit_traj, file='trajetoria_F_lambda.dat', status='replace')
      write(unit_traj, '(A)') '# lambda    F(lambda)    nodes'
    end if
    
    lambda = 0.0d0
    step = 0.01d0
    call shoot(lambda, F_prev, nodes_prev)
    
    
    
    ! Continua até encontrar todos os autovalores desejados
    do while (n_encontrados < n_eigen)
      lambda_prev = lambda
      lambda = lambda + step
      iteracoes = iteracoes + 1
      
      call shoot(lambda, F, nodes_curr)
      
      ! Salva trajetória se solicitado
      if (salvar_trajetoria) then
        write(unit_traj, '(3E16.8,I5)') lambda, F, nodes_curr
      end if
      
      ! Detecta mudança de sinal ou mudança no número de nós
      mudou_sinal = (F_prev * F < 0.0d0)
      
      if (mudou_sinal .or. (nodes_curr > nodes_prev)) then
        n_candidatos_totais = n_candidatos_totais + 1
        
        if (verbose_level >= 2) then
          print *
          print '(A,I3,A)', '>>> Candidato #', n_candidatos_totais, ' detectado:'
          print '(A,F12.6,A,F12.6)', '    Intervalo: [', lambda_prev, ', ', lambda, ']'
          if (mudou_sinal) print *, '    Razão: Mudança de sinal em F'
          if (nodes_curr > nodes_prev) then
            print '(A,I3,A,I3)', '    Razão: Mudança de nós: ', nodes_prev, ' -> ', nodes_curr
          end if
        end if
        
        lambda_ini = lambda_prev
        lambda_fim = lambda
        
        ! Refina o candidato usando bissecção robusta
        if (mostrar_bisseccao) then
          print *, '    Iniciando bissecção...'
        end if
        
        call bissec_robusta(lambda_prev, lambda, candidato, iter_bissec, metodo_usado)
        
        if (verbose_level >= 2) then
          print '(A,F15.8,A,I4,A,A,A)', '    Refinado para λ = ', candidato, &
                ' (', iter_bissec, ' iterações, método: ', trim(metodo_usado), ')'
        end if
        
        ! Se a bissecção falhou (iter_bissec = 0), pula este candidato
        if (iter_bissec == 0) then
          if (verbose_level >= 1) then
            print *, '    AVISO: Bissecção falhou, pulando candidato'
          end if
          cycle
        end if
        
        if (verbose_level >= 2) then
          print '(A,F15.8,A,I4,A)', '    Refinado para λ = ', candidato, &
                ' (', iter_bissec, ' iterações de bissecção)'
        end if
        
        ! Valida rigorosamente
        if (validar_autovalor(candidato)) then
          n_encontrados = n_encontrados + 1
          eig(n_encontrados) = candidato
          
          ! Armazena informações detalhadas
          info(n_encontrados)%lambda = candidato
          call shoot(candidato, info(n_encontrados)%F_lambda, info(n_encontrados)%nodes)
          info(n_encontrados)%iter_bisseccao = iter_bissec
          info(n_encontrados)%iter_busca = iteracoes
          info(n_encontrados)%lambda_inicial = lambda_ini
          info(n_encontrados)%lambda_final = lambda_fim
          
          if (verbose_level >= 1) then
            print '(A,I3,A,I3,A,F15.8)', &
              '*** AUTOVALOR ', n_encontrados, ' de ', n_eigen, ': λ = ', candidato
            if (verbose_level >= 2) then
              print '(A,I8)', '    Encontrado na iteração de busca: ', iteracoes
              print '(A,I3)', '    Número de nós: ', info(n_encontrados)%nodes
            end if
          end if
          
          ! Ajusta o passo
          if (n_encontrados >= 2) then
            step = (eig(n_encontrados) - eig(n_encontrados-1)) * 0.01d0
            step = max(step, 0.001d0)
            step = min(step, 5.0d0)
            if (verbose_level >= 3) then
              print '(A,F8.4)', '    Novo passo de busca: ', step
            end if
          end if
          
          ! Avança para evitar encontrar o mesmo autovalor
          lambda = candidato + 0.1d0
          call shoot(lambda, F, n_dummy)
        else
          if (verbose_level >= 1) then
            print '(A,F15.8,A)', '!!! Candidato λ = ', candidato, ' REJEITADO (espúrio)'
          end if
        end if
      end if
      
      ! Feedback de progresso
      if (mod(iteracoes, 5000) == 0 .and. verbose_level >= 1) then
        print '(A,F12.4,A,I8,A,I3,A)', 'Progresso: λ = ', lambda, &
              ' (iteração ', iteracoes, '), encontrados ', n_encontrados, ' autovalores'
      end if
      
      ! Ajusta passo se não encontra nada há muito tempo
      if (mod(iteracoes, 1000) == 0 .and. n_encontrados > 0) then
        if (lambda - eig(n_encontrados) > 50.0d0) then
          step = step * 1.5d0
        end if
      end if
      
      F_prev = F
      nodes_prev = nodes_curr
    end do
    
    if (salvar_trajetoria) then
      close(unit_traj)
      print *, 'Trajetória salva em trajetoria_F_lambda.dat'
    end if
    
    if (verbose_level >= 1) then
      print *
      print '(A,I3,A,I8,A)', 'Busca concluída! Encontrados todos os ', &
            n_encontrados, ' autovalores em ', iteracoes, ' iterações de busca.'
    end if
    
  end subroutine encontrar_espectro_validado
  
  ! Versão simplificada para compatibilidade
  subroutine encontrar_espectro_validado_simples(eig, itv)
    real(8), intent(out) :: eig(n_eigen)
    integer, intent(out) :: itv(n_eigen)
    type(info_autovalor) :: info(n_eigen)
    integer :: i
    
    call encontrar_espectro_validado(eig, info)
    
    ! Extrai apenas as iterações de bissecção para compatibilidade
    do i = 1, n_eigen
      itv(i) = info(i)%iter_bisseccao
    end do
  end subroutine encontrar_espectro_validado_simples
  

  subroutine encontrar_por_varredura_F(eig, n_max)
    real(8), intent(out) :: eig(:)
    integer, intent(in) :: n_max
    real(8) :: lambda, F_prev, F_curr, lambda_prev
    real(8) :: dlambda = 0.01d0
    integer :: n_dummy, found, iteracoes
    
    ! Inicializa o array
    eig = 0.0d0
    found = 0
    iteracoes = 0
    
    lambda = 0.0d0
    call shoot(lambda, F_prev, n_dummy)
    lambda_prev = lambda
    
    ! Continua até encontrar todos os autovalores desejados
    do while (found < n_max)
      lambda = lambda + dlambda
      iteracoes = iteracoes + 1
      call shoot(lambda, F_curr, n_dummy)
      
      ! Detecta mudança de sinal em F
      if (F_prev * F_curr < 0.0d0) then
        ! Refina usando bissecção
        call bissec_sig(lambda_prev, lambda, eig(found + 1), n_dummy)
        
        ! Valida o resultado
        if (validar_autovalor(eig(found + 1))) then
          found = found + 1
          if (verbose) then
            print '(A,I3,A,F12.6)', 'Autovalor ', found, ': λ = ', eig(found)
          end if
          
          ! Ajusta o passo se necessário
          if (found >= 2) then
            dlambda = (eig(found) - eig(found-1)) * 0.01d0
            dlambda = max(dlambda, 0.001d0)
            dlambda = min(dlambda, 1.0d0)
          end if
        end if
        
        ! Pula um pouco à frente para evitar encontrar o mesmo autovalor
        lambda = lambda + 0.1d0
        call shoot(lambda, F_curr, n_dummy)
      end if
      
      ! Feedback periódico
      if (mod(iteracoes, 10000) == 0 .and. verbose) then
        print '(A,F12.4,A)', 'Varredura em λ = ', lambda, '...'
      end if
      
      F_prev = F_curr
      lambda_prev = lambda
    end do
  end subroutine encontrar_por_varredura_F
  
end module espectro_melhorado