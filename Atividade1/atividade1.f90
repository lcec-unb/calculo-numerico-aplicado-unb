program bisseccao

! Desejamos escrever um programa que resolva a raiz da seguinte equação:

! f(x) = (667.38/x)*(1.0 - exp(-0.146843*x)) - 40.0

! O método a ser utilizado deve ser o método da bissecção

! Primeiro, vamos declarar nossas variáveis

real xi, xs     ! limite inferior e superior do intervalo de busca
real xm, fm     ! valor parcial da raiz e da função f(xm)
real fi, fs     ! valor da função f(x) em xi e xs respectivamente
real tol        ! tolerância para avaliação de convergência
real erro       ! erro relativo para avaliação da convergência
real xmd	! valor provisório da raiz depois de um passo
integer iter	! número da iteração

! Definindo o intervalo inicial de busca
xi=12.0
xs=16.0

! Definindo a tolerância para fins de convergência
tol = 1.0e-05
erro =1.0
iter =0


do while(erro.gt.tol)

! Avaliando os valores da função nos intervalos xi e xs
fi=funcao(xi)
fs=funcao(xs)

! Averiguando se a função muda de sinal no intervalo
if ((fi*fs).lt.0.0) then
xm= (xi+xs)/2.0 ! valor parcial da raiz

! Checando agora se "xi < raiz < xm" ou "xm < raiz < xs"
fm=funcao(xm)

if((fi*fm).lt.0.0) then
xs=xm
else
xi=xm
end if
xmd = (xi+xs)/2.0
erro= abs(xm-xmd)
iter =iter + 1
write(*,*) iter, xi, xs, xm, erro

end if
end do

! Defindo a função f(x)

contains
function funcao(x) result(f)
    real, intent(in) :: x
    real f
    f = (667.38/x)*(1.0 - exp(-0.146843*x)) - 40.0
end function funcao

end program bisseccao
