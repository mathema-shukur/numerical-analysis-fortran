    program Weddles_rule
    INTEGER::n,x
    REAL::a,b,h,f,g,s1,s2,s3,s4,exv,v,e
    read (*,*)a,b,n
    h=(b-a)/float(n)
    s1=0.0
    s2=0.0
    s3=0.0
    s4=0.0
    do x=1,n-1,2
        if (MOD(x,3).eq.0) then
            s1=s1+f(a+x*h)
        else
            s2=s2+f(a+x*h)
        end if
    end do
    do x=2,n-2,2
        if (MOD(x,6).eq.0) then
            s3=s3+f(a+x*h)
        else
            s4=s4+f(a+x*h)
        end if
    end do
    v=(3*h/10.0)*(f(a)+f(b)+6*s1+5*s2+2*s3+s4)
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
