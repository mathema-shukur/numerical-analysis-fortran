    program bisection
    implicit none
    INTEGER::n,i
    REAL::a,b,c,f,p,f1,f2,f3
    WRITE(*,*)"Enter initial point a="
    read*,a
    WRITE(*,*)"Enter End point b="
    read*,b
    WRITE(*,*)"Enter iteration no. n="
    read*,n
    WRITE(*,*)"Enter tollerence =p"
    read*,p
    f1=f(a)
    f2=f(b)
    if (f1*f2>0) then
        write (*,13)a,b
        stop
    end if
13  format (2x,'there is no root between',1x,f4.2,1x,'and',1x,f4.2)
    do i=1,n
        c=(a+b)/2.0
        write (*,*)i,a,b,c
        f3=f(c)
        if (f1*f3<0.0) then
            b=c
            f2=f3
        else
            a=c
            f1=f3
        end if
        if (ABS(b-a)<p) GOTO 15
            if (i==n) then
                write (*,*)'there is no root between ',n,' iteration'
                stop
            end if
    end do
15  write (*,*)'desired root=',c
    end program
    function f(y)
    f=SIN(y)-EXP(-y**2)
    return
    end
