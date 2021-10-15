    !program for gaussian elemination with pivoting
    program gau_elewp
    implicit none
    REAL::a(5,5),la,t,m1,x(5),s1
    INTEGER::n,i,j,k,p,s,l
    read (*,*)n,((a(i,j),j=1,n+1),i=1,n)
    do k=1,n-1
        p=k
        la=ABS(a(k,k))
        do i=k+1,n
            if (ABS(a(i,k))>la) then
                la=ABS(a(i,k))
                p=i
            end if
        end do
        if (p.ne.k) then
            do j=k,n+1
                t=a(p,j)
                a(p,j)=a(k,j)
                a(k,j)=t
            end do
        end if
        do s=k+1,n
            m1=a(s,k)/a(k,k)
            do l=1,n+1
                a(s,l)=a(s,l)-m1*a(k,l)
            end do
        end do
    end do
    x(n)=a(n,n+1)/a(n,n)
    do i=n-1,1,-1
        s1=0
        do j=i+1,n
            s1=s1+a(i,j)*x(j)
        end do
        x(i)=(a(i,n+1)-s1)/a(i,i)
    end do
    do i=1,n
        write (6,15)(a(i,j),j=1,n+1)
    end do
15  format(4x,5(f10.5))
    do i=1,n
        write (*,13)i,x(i)
    end do
13  format (2x,'x',i1,'=',f7.4)
    end program
