module modulo_bisseccao
  use constantes
  use atirador
  implicit none
contains
  !--- Δnodes = +1 ------------------------------------------
  subroutine bissec_nos(lo,hi,root,iters)
    real(8),intent(in) :: lo,hi
    real(8),intent(out):: root
    integer,intent(out):: iters
    real(8):: a,b,m,dF
    integer:: na,nb,nm
    
    call shoot(lo,dF,na)
    call shoot(hi,dF,nb)
    
    if (nb/=na+1) then
      if (verbose_level >= 2) then
        print '(A,I3,A,I3)', '    AVISO: bissec_nos: Δn = ', nb-na, ' (esperado 1)'
      end if
      ! Tenta continuar mesmo assim
      if (nb <= na) then
        root = 0.5d0*(lo+hi)
        iters = 0
        return
      end if
    end if
    
    a=lo; b=hi; iters=0
    do while (b-a>tol)
      iters=iters+1
      m=0.5d0*(a+b)
      call shoot(m,dF,nm)
      if (nm==na) then
        a=m
      else
        b=m
      end if
    end do
    root=0.5d0*(a+b)
  end subroutine bissec_nos
  
  !--- troca de sinal em F ----------------------------------
  subroutine bissec_sig(lo,hi,root,iters)
    real(8),intent(in) :: lo,hi
    real(8),intent(out):: root
    integer,intent(out):: iters
    real(8):: a,b,m,Fa,Fm
    integer:: n
    
    call shoot(lo,Fa,n)
    call shoot(hi,Fm,n)
    
    if (Fa*Fm>=0.d0) then
      if (verbose_level >= 2) then
        print '(A,E12.4,A,E12.4)', '    AVISO: bissec_sig: sem mudança de sinal. F(lo)=', Fa, ', F(hi)=', Fm
      end if
      ! Retorna o ponto médio como melhor estimativa
      root = 0.5d0*(lo+hi)
      iters = 0
      return
    end if
    
    a=lo; b=hi; iters=0
    do while (b-a>tol .and. iters < 100)  ! Limite de iterações
      iters=iters+1
      m=0.5d0*(a+b)
      call shoot(m,Fm,n)
      if (Fa*Fm<=0.d0) then
        b=m
      else
        a=m; Fa=Fm
      end if
    end do
    root=0.5d0*(a+b)
  end subroutine bissec_sig
  
  !--- Bissecção robusta que tenta ambos os métodos --------
  subroutine bissec_robusta(lo,hi,root,iters,metodo_usado)
    real(8),intent(in) :: lo,hi
    real(8),intent(out):: root
    integer,intent(out):: iters
    character(len=20),intent(out):: metodo_usado
    
    real(8):: Flo,Fhi
    integer:: nlo,nhi
    
    ! Avalia nos extremos
    call shoot(lo,Flo,nlo)
    call shoot(hi,Fhi,nhi)
    
    ! Decide qual método usar
    if (Flo*Fhi < 0.0d0) then
      ! Mudança de sinal em F - usa bissecção por sinal
      call bissec_sig(lo,hi,root,iters)
      metodo_usado = 'sinal'
    else if (nhi > nlo) then
      ! Mudança no número de nós
      call bissec_nos(lo,hi,root,iters)
      metodo_usado = 'nos'
    else
      ! Nenhum método aplicável - retorna ponto médio
      root = 0.5d0*(lo+hi)
      iters = 0
      metodo_usado = 'fallback'
      if (verbose_level >= 2) then
        print *, '    AVISO: Nenhum método de bissecção aplicável'
      end if
    end if
    
  end subroutine bissec_robusta
  
end module modulo_bisseccao