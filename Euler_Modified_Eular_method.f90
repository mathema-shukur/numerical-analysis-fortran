    ! program for Euler's and modified Euler's method
    program euler_modified_euler
    implicit none
    INTEGER::i,n
    ! n=number of sub-interval in (a,b)
    REAL::a,b,x,y,g,er,y0,h,y1,er1
    ! a=initial point ; b=final point ; er=error in euler's method
    ! er1=error in modified euler's method ; yo=initial value of y(x)
    ! h=step-size ; y=value of y(x) for euler's method
    ! y1=value of y(x) for modified euler's method
    read (11,*)a,b,n,y0
    h=(b-a)/n
    x=a
    y=y0
    y1=y0
    write (*,13)
    write (*,13)
13  format (3x,'x(i)',7x,'eu_y(i)',3x,'exact value',4x,'eu_error',5x,'meu_y(i)',4x,'meu_error')
    do i=1,n+1
        er=ABS(g(x)-y) !error calculation for euler's method
        er1=ABS(g(x)-y1) !error calculation for modified euler's method
        write (*,14)x,y,g(x),er,y1,er1
        write (*,14)x,y,g(x),er,y1,er1
14      format (2x,f5.2,5(5x,f8.5))
        call euler(h,x,y)      ! subroutine for euler's method
        call m_euler(h,x,y1,y) ! subroutine for modified euler's method
        x=a+i*h  !generalised value of x
    end do
    end program
    ! subroutine for euler's method
    subroutine euler(h,x,y)
    implicit none
    REAL::x,y,h,f  ! dy/dx=f(x,y)
    y=y+h*f(x,y)
    end subroutine
    ! subroutine for modified euler's method
    subroutine m_euler(h,x,y1,y)
    implicit none
    REAL::x,y1,h,f,y
    y1=y1+h*(.5*(f(x,y1)+f(x+h,y))) !y=y(x+h) calculated in euler's method
    end subroutine
    ! function for dy/dx
    REAL function f(x,y)
    f=y-x**2+1
    return
    end
    !define function for exact solution
    REAL function g(x)
    g=(x+1)**2-.5*EXP(x)  !g(x)=solution of dy/dx=f(x,y)
    return
    end
