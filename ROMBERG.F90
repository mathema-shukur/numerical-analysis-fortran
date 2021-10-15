    ! program for Romberg integration
    program ROMBERG
    implicit none
    INTEGER::n,i,j,k
    REAL::a,b,tol,h(20),r(20,20),f,sum1,G,ER
    ! f=integrand
    read (5,*)a,b,n,tol
    ! a=lower limit; b=upper limit ; tol=tolerance
    !n=maximum number of points in the interval
    do i=1,n
        h(i)=(b-a)/2**(i-1)  !h(i)=length of sub-interval
    end do
    r(1,1)=(h(1)/2)*(f(a)+f(b)) ! value of integration for h(1) sub-interval
    write (6,13)r(1,1)
13  format (2x,f12.8)
    do i=2,n
        sum1=0.0
        do j=1,2**(i-2)
            sum1=sum1+f(a+(2*j-1)*h(i))
    !sum1= sum of the functional value at points in the interval (a,b) except for h(i-1) sub-interval
        end do
        r(i,1)=0.5*(r(i-1,1)+h(i-1)*sum1)
    ! r(i,1)=value of integration for h(i) sub-interval
        do k=2,i
            r(i,k)=r(i,k-1)+(r(i,k-1)-r(i-1,k-1))/(4**(k-1)-1)
    ! r(i,k)= upgrated value of integration
        end do
        write (6,14)(r(i,k),k=1,i)
14      format (20(2x,f12.8))
        if (ABS(r(i,i)-r(i-1,i-1))<tol) then
            write (6,*)'value of the integration=',r(i,i)
            ER=ABS((G(B)-G(A))-R(I,I))
            WRITE (6,15)ER
15          FORMAT (2X,'ERROR=',F12.8)
            stop
        end if
    end do
    end program
    !define function for integrand
    REAL function f(x)
    f=1/(1+x**2)
    return
    end
    REAL FUNCTION G(X)
    G=ATAN(X)
    RETURN
    END
