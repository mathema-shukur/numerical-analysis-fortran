    program trapezoidal
    INTEGER::n,x
    REAL::a,b,h,s,v,f,g,e,exv
    read (*,*)a,b,n
    h=(b-a)/float(n)
    s=0.0
    do x=1,n-1
        s=s+f(a+x*h)
    end do
    v=(h/2.0)*(f(a)+f(b)+2*s)
    write (*,13)v
13  format (2x,'calculated value of integration=',f12.5)
    exv=g(b)-g(a)
    write (*,14)exv
14  format (2x,'exact value =',1x,f12.5)
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
