    ! program for gaussian elemination without pivoting
    program gaussian_elemiation
    implicit none
    REAL::a(5,5),p,x(5),s1
    ! x(i)= unknown variables
    INTEGER::n,i,j,l,k
    read (*,*)n,((a(l,j),j=1,n+1),l=1,n)
    ! a=coefficient matrix   n= dimension
    do i=1,n-1
        do j=i+1,n
            p=a(j,i)/a(i,i)
            do k=1,n+1
                a(j,k)=a(j,k)-p*a(i,k)
            end do
        end do
    end do
    do l=1,n
        write (6,15)(a(l,j),j=1,n+1)
    end do
15  format(4x,5(f10.5))
    x(n)=a(n,n+1)/a(n,n)
    do i=n-1,1,-1
        s1=0
        do j=i+1,n
            s1=s1+a(i,j)*x(j)
        end do
        x(i)=(a(i,n+1)-s1)/a(i,i)
    end do
    do i=1,n
        write (*,13)i,x(i)
    end do
13  format (2x,'x',i1,'=',f8.5)
    end program
