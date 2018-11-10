        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION A(100,100),B(100,100),C(100,100)
        OPEN (UNIT=5,FILE='DATA_INPUT.DAT',STATUS='OLD')
        OPEN (UNIT=6,FILE='DATA_OUTPUT.DAT',STATUS='REPLACE')
        READ(5,2000) N
2000    FORMAT(I4)
        DO 300 I=1,N
        DO 300 J=1,N
300     READ(5,3000) A(I,J)
3000    FORMAT(F4.0)
        DO 100 I=1,N
        DO 100 J=1,N
100     B(I,J)=A(I,J)
        CALL DECOMPOSE(N,B)
        CALL INVERT(N,B)
        DO 400 I=1,N
        DO 400 J=1,N
        SUM=0
        DO 500 K=1,N
500     SUM=SUM+A(I,K)*B(K,J)
        C(I,J)=SUM
400     CONTINUE
        WRITE(6,1002)
1002    FORMAT(26X,'ORIGINAL MATRIX')
        WRITE(6,1000) ((A(I,J),J=1,N),I=1,N)
        WRITE(6,1001)
1001    FORMAT(/)
        WRITE(6,1003)
1003    FORMAT(26X, 'MATRIX INVERSE')
        WRITE(6,1000) ((B(I,J),J=1,N),I=1,N)
        WRITE(6,1001)
        WRITE(6,1004)
1004    FORMAT(26X,'INVERSE CHECK')
        WRITE(6,1000) ((C(I,J),J=1,N),I=1,N)
1000    FORMAT(10X,3D16.9)
        STOP
        END
        
        
        SUBROUTINE DECOMPOSE (N,A)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION A(100,100)
        TEMP=0.
        DO 200 I=1,N
        DO 200 J=I,N
        SUM=A(I,J)
        DO 300 K=1,I-1
300     SUM=SUM-A(K,I)*A(K,J)
        IF (J.NE.I) A(I,J)=SUM*TEMP
        IF (J.NE.I) GO TO 200
        IF (SUM.LE.0.) WRITE(6,400)
400     FORMAT(' INVERSION FAILS')
        IF (SUM.LE.0.) STOP
        TEMP=1./SQRT(SUM)
        A(I,J)=TEMP
200     CONTINUE
        RETURN
        END
        
       SUBROUTINE INVERT(N,U)
       IMPLICIT REAL*8 (A-H,O-Z)
       DIMENSION U(100,100)
       DO 100 I=1,N
       DO 100 J=I+1,N
       SUM=0.
       DO 200 K=I,J-1
200    SUM=SUM-U(K,I)*U(K,J)
100    U(J,I)=SUM*U(J,J)
       DO 300 I=1,N
       DO 300 J=I,N
       SUM=0.
       DO 400 K=J,N
400    SUM=SUM+U(K,I)*U(K,J)
       U(J,I)=SUM
300    U(I,J)=U(J,I)
       RETURN
       END
