    !FORTRAN PROGRAM FOR RUNGE-KUTTA METHOD FOR SECOND AND FOURTH ORDER
    PROGRAM RK2_RK4
    IMPLICIT NONE
    REAL::A,B,X,Y1,H,ER1,G,Y0,Y2,ER2
    INTEGER::N,I
    READ (*,*)A,B,Y0,N
    H=(B-A)/N
    X=A
    Y1=Y0
    WRITE (*,15)
15  FORMAT (3X,'*RESULT FOR 2ND ORDER RUNGE-KUTTA METHOD*',//)
    WRITE (*,13)
13  FORMAT (3X,'X(I)',8X,'Y(I)',9X,'EXACT_VALUE',5X,'ERROR')
    DO I=1,N+1
        ER1=ABS(G(X)-Y1)
        WRITE (*,14)X,Y1,G(X),ER1
14      FORMAT (2X,F5.2,3(3X,F12.6))
        CALL SHUJIT(X,Y1,H)
        X=A+I*H
    END DO
    X=A
    Y2=Y0
    WRITE (*,16)
16  FORMAT (//,3X,'*RESULT FOR 4TH ORDER RUNGE-KUTTA METHOD*',//)
    WRITE (*,13)
    DO I=1,N+1
        ER2=ABS(G(X)-Y2)
        WRITE (*,14)X,Y2,G(X),ER2
        CALL RUPAM(X,Y2,H)
    X=A+I*H
    END DO
    END PROGRAM
    !SUROUTINE FOR 2ND ORDER RUNGE-KUTTA METHOD
    SUBROUTINE SHUJIT(X,Y1,H)
    IMPLICIT NONE
    REAL::X,Y1,H,K1,K2,F
    K1=H*F(X,Y1)
    K2=H*F(X+H,Y1+K1)
    Y1=Y1+.5*(K1+K2)
    END SUBROUTINE
    !SUROUTINE FOR 4TH ORDER RUNGE-KUTTA METHOD
    SUBROUTINE RUPAM(X,Y2,H)
    IMPLICIT NONE
    REAL::X,Y2,H,K1,K2,K3,K4,F
    K1=H*F(X,Y2)
    K2=H*F(X+H/2,Y2+K1/2)
    K3=H*F(X+H/2,Y2+K2/2)
    K4=H*F(X+H,Y2+K3)
    Y2=Y2+(1/6.0)*(K1+2*K2+2*K3+K4)
    END SUBROUTINE
    ! DEFINE F(X,Y) WHERE DY/DX=F(X,Y)
    REAL FUNCTION F(X,Y)
    F=5*X**2+2*x-5*y
    RETURN
    END
    ! DEFINE FUNCTION FOR EXACT SOLUTION
    REAL FUNCTION G(X)
    G=(X**2)+(1/3.0)*EXP(-5*X)
    RETURN
    END
