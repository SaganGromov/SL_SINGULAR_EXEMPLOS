Para compilar e executar os programas, execute o comando a seguir em suas respectivas pastas:
```bash
gfortran -o espectro_melhorado constantes.f90 problema.f90 definicao_EDO.f90 integrador_rk4.f90 atirador.f90 modulo_bisseccao.f90 espectro_melhorado.f90 solucao.f90 main_melhorado.f90 && ./espectro_melhorado
```
Após compilar, execute (ainda no diretório do respectivo exemplo)
```bash
gnuplot *.gp
```
para plotar as autofunções correspondentes.