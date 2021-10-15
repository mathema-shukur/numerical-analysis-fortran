    program simpson_3_8
    INTEGER::n,x
    REAL::a,b,h,f,g,s,exv,v,e
    read (*,*)a,b,n
    h=(b-a)/float(n)
    s=0.0
    do x=1,n-1
        if (MOD(x,3).eq.0) then
            s=s+2*f(a+x*h)
        else
            s=s+3*f(a+x*h)
        end if
    end do
    v=(3*h/8.0)*(f(a)+f(b)+s)
    write (*,13)v
13  format (2x,'calculated value=',1x,f12.5)
    exv=g(b)-g(a)
    write (*,14)exv
14  format (2x,'exact value=',1x,f12.5)
    e=exv-v
    write (*,*)'error=',e
    end program
    function f(x)
    f=x**2*EXP(-x**3)
    return
    end
    function g(y)
    g=(-1/3.0)*EXP(-y**3)
    end
