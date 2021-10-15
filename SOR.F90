    !  program for SOR iterative method
    program SOR
    implicit none
    REAL::a(5,5),b(5),x0(5),x(5),w,s1,tol,norm1,norm2
    INTEGER::n,ite,i,j,h,k,s,t,r
    read (5,*)n,w,tol,ite
    read (5,*)((a(i,j),j=1,n),i=1,n),(b(i),i=1,n),(x0(i),i=1,n)
    do k=1,ite
        do i=1,n
            s1=0
            do j=1,n
                if (i<j) s1=s1+a(i,j)*x0(j)
                if (i>j) s1=s1+a(i,j)*x(j)
            end do
            x(i)=(1-w)*x0(i)+(w*(b(i)-s1))/a(i,i)
        end do
        write (6,15)k,(x(h),h=1,n)
15      format (2x,i2,1x,5(2x,f8.4))
        norm1=ABS(x0(1)-x(1))
        norm2=ABS(x(1))
        do r=2,n
            if (norm1<ABS(x0(r)-x(r))) norm1=ABS(x0(r)-x(r))
            if (norm2<ABS(x(r))) norm2=ABS(x(r))
        end do
        if ((norm1/norm2)<tol) then
            write (6,*)'solution of the system::'
            do t=1,n
                write (6,20)t,x(t)
            end do
20          format (2x,'x',i1,'=',f8.4)
            stop
            else
                do s=1,n
                    x0(s)=x(s)
                end do
        end if
    end do
    end program
