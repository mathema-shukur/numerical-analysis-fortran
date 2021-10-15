    program Fixed_point_iteration_method
    implicit none
    INTEGER::i,n
    REAL::x0,x1,f,p,g
    read (*,*)x0,n
    p=.00001
    if (ABS(g(x0))>1) then
        write (*,*)'The method is divergent.'
        stop
    end if
    do i=1,n
        write (*,*)i,x0
        x1=f(x0)
        if (ABS(x1-x0)<p) GOTO 13
            x0=x1
        if (i==n) then
            write (12,*)'The maximum no. of iteration is exceeded.'
            stop
        end if
    end do
13  write (*,14)x1
14  format (2x,'desired root=',f12.5)
    end program
    function f(x)
    f=SQRT(-LOG(SIN(x)))
    return
    end
    function g(y)
    g=-COS(y)/(2*SIN(y)*SQRT(-LOG(SIN(y))))
    return
    end
