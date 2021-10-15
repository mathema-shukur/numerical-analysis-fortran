    program Newton_Raphson_Method
    implicit none
    INTEGER::i,n
    REAL::x0,x1,f,g,p
    read (*,*)x0,n
    p=.00001
    do i=1,n
        write (*,*)i,x0
        x1=x0-f(x0)/g(x0)
        if (ABS(x1-x0)<p) GOTO 14
            x0=x1
            if (i==n) then
                write (*,*)'no root found'
                stop
        end if
    end do
14  write (*,15)x1
15  format (2x,'desired root=',f12.5)
    end program
    function f(x)
    f=SIN(x)-EXP(-x**2)
    return
    end
    function g(x)
    g=COS(x)+2*x*EXP(-x**2)
    return
    end
