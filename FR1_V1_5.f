C       Structures 2 Fortran Project
C       Kelly Getsinger
C       v1.5 (full release) (11-16-2015)

C       APPENDICIES
C       ~FORMATS
C         100s: LOOP COUNTERS
C         500s: GO TOs (starts off at 499: L1
C         1000s:WRITE
C         2000s:READ
C         8000s:FORMATING
C         9000s:HEADERS (WRITE)
C         9100s:NUMBER OUTPUT FORMATS (WRITE)
C       ~

C       =============================================
C       POSSIBLE ERRORS
C       1. (1.c.8)-the true for the ts=2 statement JK(I)AX(I)    (pg.20)
C       2. (2.a.9)-the use of the variable SCM1B instead of SCM1Y(pg.25)
C       3. (2.b.1)-THE L5C1 IS IN THE DECOMPOSE CALL?            (pg.36)
C                  DOES THE GO TO CALL FROM A SUBROUTINE CALL THE MAIN?
C       4. (1.a.3 && 4.b.1-4)-CHANGED SN.GT.1 -> SN.GE.1         (pg.43)
C       5. (5.b.13)- USED AML INSTEAD OF AM TO MATCH PATTERN     (pg.50)
C       =============================================
C       Start program
C       =============================================
        PROGRAM FR1
C       Setup
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        INTEGER SN,TS,NLS,NDJ,M,N,NJ,NR,NRJ,I,J,K,JJ(100),JK(100)
        INTEGER AA,RL(100),CRL(100),J1,J2,J3,J4,J5,J6,K1,K2,K3,K4,K5,K6
        INTEGER LN,NLJ,NLM,LML(100),JE,KE,CNTA,CNTB,ERROR,TEMP
        DOUBLE PRECISION E,G,X(100),Y(100),Z(100),L(100),AX(100)
        DOUBLE PRECISION IX(100),IY(100),IZ(100),XCL,YCL,ZCL,CX,CY,CZ
        DOUBLE PRECISION R(100,100),EP,YP,ZP,XPS,YPS,ZPS,YPG,ZPG,Q,SQ
        DOUBLE PRECISION SINA,COSA,SCM,SM(100,100),SMR(100,100)
        DOUBLE PRECISION SMD(100,100),S(100,100),A(100),AML(100,100)
        DOUBLE PRECISION AE(100),AC(100),D(100),DJ(100),AR(100),TOL
C       OPEN READ AND WRITE FILES
        OPEN (UNIT=5,FILE='FR1_INPUT.DAT',STATUS='OLD')
        OPEN (UNIT=6,FILE='FR1_OUTPUT.DAT',STATUS='REPLACE')
C       =============================================
C       FORMATS
C       =============================================
2000    FORMAT(I4)
2001    FORMAT(3I4)
2002    FORMAT(4I4)
2003    FORMAT(7I4)
2005    FORMAT(3I4,F10.0)
2006    FORMAT(4I4,F8.0)
2007    FORMAT(4I4,2F8.0)
2008    FORMAT(I4,2F6.0)
2009    FORMAT(I4,3F6.0)
2010    FORMAT(I4,4F8.0)
2011    FORMAT(3I4,4F6.0,I4)
2012    FORMAT(2I4)
2013    FORMAT(I4,6F8.0)
2014    FORMAT(I4,12F8.0)
2015    FORMAT(3I4,F6.0)
2016    FORMAT(3I4,2F6.0)

8000    FORMAT(/)
8999    FORMAT(/'-----------------------------------------------------',
     &         '--------------------------------')
9000    FORMAT('KELLY GETSINGER')
9001    FORMAT('STRUCTURES 2')
9002    FORMAT('DR. FOSTER')
9003    FORMAT('FALL 2015')
9004    FORMAT(16X,'ANALYSIS OF FRAMED STRUCTURES -- FR1')
9005    FORMAT(/5X,'STRUCTURE NO.',(I2),'  CONTINOUS BEAM'/)
9006    FORMAT(/5X,'STRUCTURE NO.',(I2),'  PLANE TRUSS'/)
9007    FORMAT(/5X,'STRUCTURE NO.',(I2),'  PLANE FRAME'/)
9008    FORMAT(/5X,'STRUCTURE NO.',(I2),'  GRID'/)
9009    FORMAT(/5X,'STRUCTURE NO.',(I2),'  SPACE TRUSS'/)
9010    FORMAT(/5X,'STRUCTURE NO.',(I2),'  SPACE FRAME'/)
9011    FORMAT(5X,'STRUCTURE DATA')
9012    FORMAT(5X,'M',5X,'N',4X,'NJ',4X,'NR',3X,'NRJ',7X,'E',7X,'G')
9013    FORMAT(/5X,'COORDINATES OF JOINTS')
9014    FORMAT(5X,'JOINT',4X,'X',7X,'Y',7X,'Z')
9015    FORMAT(/5X,'MEMBER INFORMATION')
9016    FORMAT(5X,'MEMBER',2X,'JJ',3X,'JK',6X,'AX',8X,'IX',8X,'IY',
     &          8X,'IZ',5X,'AA',7X,'L')
9017    FORMAT(5X,'JOINT RESTRAINTS')
9018    FORMAT(5X,'JOINT',2X,'RL1',2X,'RL2',2X,'RL3',2X,'RL4',2X,
     &          'RL5',2X,'RL6')
9019    FORMAT(/5X,'LOADING NO.',(I2))
9020    FORMAT(5X,'NLJ',3X,'NLM')
9021    FORMAT(/5X,'ACTIONS APPLIED AT JOINTS')
9022    FORMAT(5X,'JOINT',9X,'A1',10X,'A2',10X,'A3',10X,'A4',10X,
     &          'A5',10X,'A6')
9023    FORMAT(/5X,'ACTIONS AT ENDS OF RESTRAINED MEMBERS DUE TO LOADS')
9024    FORMAT(5X,'MEMBER',7X,'AML1',8X,'AML2',8X,'AML3',8X,'AML4',
     &          8X,'AML5',8X,'AML6')
9025    FORMAT(5X,'MEMBER',7X,'AML1',8X,'AML2',8X,'AML3',8X,'AML4',
     &          8X,'AML5',8X,'AML6',8X,'AML7',8X,'AML8',8X,'AML9',
     &          8X,'AML10',8X,'AML11',8X,'AML12')
9026    FORMAT(/5X,'JOINT DISPLACEMENTS')
9027    FORMAT(5X,'JOINT',9X,'D1',10X,'D2',10X,'D3',10X,'D4',10X,'D5',
     &          10X,'D6')
9028    FORMAT(/5X,'MEMBER END-ACTIONS')
9029    FORMAT(5X,'MEMBER',7X,'AM1',9X,'AM2',9X,'AM3',9X,'AM4',
     &          9X,'AM5',9X,'AM6')
9030    FORMAT(5X,'MEMBER',7X,'AM1',9X,'AM2',9X,'AM3',9X,'AM4',
     &          9X,'AM5',9X,'AM6',9X,'AM7',9X,'AM8',9X,'AM9',
     &          8X,'AM10',8X,'AM11',8X,'AM12')
9031    FORMAT(//5X,'SUPPORT REACTIONS')
9032    FORMAT(5X,'JOINT',8X,'AR1',9X,'AR2',9X,'AR3',9X,'AR4',
     &          9X,'AR5',9X,'AR6')
     
     
9050    FORMAT('PROCEDURE FAILS')

9100    FORMAT((I6),(I6),(I6),(I6),(I5),3X,(F8.1),(F8.1))
9101    FORMAT((I6),(I6),(I6),(I6),(I5),3X,(F8.1))
9102    FORMAT(3X,I4,2F6.0)
9103    FORMAT(3X,I4,3F8.0)
9104    FORMAT(3X,I4,4F6.0)
9105    FORMAT(3X,I4,5F6.0)
9106    FORMAT(3X,I4,6F6.0,I4,F6.0)
9107    FORMAT(3X,I4,6F6.0)
9108    FORMAT(3X,I5,3X,12F12.3)
9109    FORMAT(/3X,I5,2X,' ',$)
9110    FORMAT(F12.5,$)
9111    FORMAT(I4,2F6.0)
9112    FORMAT(I4,3F6.0)
9113    FORMAT(I4,6F6.0)
9114    FORMAT(5X,3I5,30X,F10.2,5X,F10.2)
9115    FORMAT(4X,I5,1X,2I5)
9116    FORMAT(3X,I5,1X,I5)
9117    FORMAT(3X,I5,3X,2F12.3)
9118    FORMAT(3X,I5,3X,4F12.3)
9119    FORMAT(3X,I5,3X,2F12.5)
9120    FORMAT(3X,I5,1X,2F8.2)
9121    FORMAT(5X,3I5,F10.2,35X,F10.2)
9122    FORMAT(5X,3I5,F10.2,20X,F10.2,5X,F10.2)
9123    FORMAT(5X,3I5,10X,2F10.2,15X,F10.2)
9124    FORMAT(3X,I5,1X,3I5)
9125    FORMAT(3X,I5,3X,3F12.3)
9126    FORMAT(3X,I5,3X,6F12.3)
9127    FORMAT(3X,I5,3X,3F12.5)
9128    FORMAT(3X,I5,3X,6F12.5)
9129    FORMAT(3X,I5,1X,3F8.2)
9130    FORMAT(5X,3I5,F10.2,35X,F10.2)
9131    FORMAT(5X,3I5,4F10.2,I5,F10.2)
9132    FORMAT(3X,I5,1X,6I5)
C       ============================================
C       PREDEFINED VALUES
C       ============================================
        TOL = 0.001
C       ============================================
C       NUMBER OF STRUCTURES TO PERFORM PROGRAM ON (NUMSTRUCT)
C       ============================================
        READ(5,2000) NUMSTRUCT
C       ============================================
C       START OF FLOW CHART - FR1
C       ============================================
C       1.a.1  (FR1 FLOWCHART)
        WRITE(6,9000)
        WRITE(6,9001)
        WRITE(6,9002)
        WRITE(6,9003)
        WRITE(6,8000)
        WRITE(6,9004)
C       1.a.2 (L1)
499     READ(5,2001) SN,TS,NLS
C       1.a.3
        IF (SN.GE.1) THEN
           DO 100 CNTA=1,100
              RL(CNTA)=0
           DO 100 CNTB=1,100
              SM(CNTA,CNTB)=0D0
              SMR(CNTA,CNTB)=0D0
              SMD(CNTA,CNTB)=0D0
100           S(CNTA,CNTB)=0D0
        ENDIF
C       1.a.4,5
        IF (TS.EQ.1) THEN
C       LINE='STRUCTURE NO. (SN) CONTINOUS BEAM'
           WRITE(6,8999)
           WRITE(6,9005) SN
           READ(5,2005) M,NR,NRJ,E
           NJ=M+1;
           NDJ=2;
        ELSE IF (TS.EQ.2) THEN
C       LINE='STRUCTURE NO. (SN) PLANE TRUSS'
           WRITE(6,8999)
           WRITE(6,9006) SN
           READ(5,2006) M,NJ,NR,NRJ,E
           NDJ=2;
        ELSE IF (TS.EQ.3) THEN
C       LINE='STRUCTURE NO. (SN) PLANE FRAME'
           WRITE(6,8999)
           WRITE(6,9007) SN
           READ(5,2006) M,NJ,NR,NRJ,E
           NDJ=3;
        ELSE IF (TS.EQ.4) THEN
C       LINE='STRUCTURE NO. (SN) GRID'
           WRITE(6,8999)
           WRITE(6,9008) SN
           READ(5,2007) M,NJ,NR,NRJ,E,G
           NDJ=3;
        ELSE IF (TS.EQ.5) THEN
C       LINE='STRUCTURE NO. (SN) SPACE TRUSS'
           WRITE(6,8999)
           WRITE(6,9009) SN
           READ(5,2006) M,NJ,NR,NRJ,E
           NDJ=3;
        ELSE IF (TS.EQ.6) THEN
C       LINE='STRUCTURE NO. (SN) SPACE FRAME'
           WRITE(6,8999)
           WRITE(6,9010) SN
           READ(5,2007) M,NJ,NR,NRJ,E,G
           NDJ=6;
        ELSE IF (TS.GT.6) THEN
           WRITE(6,*)'ERROR: INVALID TS NUMBER'
        ELSE IF (TS.LT.1) THEN
           WRITE(6,*)'ERROR: INVALID TS NUMBER'
        END IF
C       1.a.6
        WRITE(6,9011)
        WRITE(6,9012)
C       1.a.7
        N=NDJ*NJ-NR
        IF (TS.EQ.4 .OR. TS.EQ.6) THEN
           WRITE(6,9100) M,N,NJ,NR,NRJ,E,G
        ELSE
           WRITE(6,9101) M,N,NJ,NR,NRJ,E
        END IF
C       1.b.1
        IF (TS.EQ.1) GO TO 500
C       500: L1C
C       1.b.2
        WRITE(6,9013)
        WRITE(6,9014)
C       1.b.3
        IF (TS.LT.5) THEN
           DO 101 K=1,NJ
           READ(5,2008) J,X(J),Y(J)
101        WRITE(6,9120) J,X(J),Y(J)
        ELSE
           DO 102 K=1,NJ
           READ(5,2009) J,X(J),Y(J),Z(J)
102        WRITE(6,9129) J,X(J),Y(J),Z(J)
        END IF
C       1.c.1 (L1C)
500     CONTINUE
        WRITE(6,9015)
        WRITE(6,9016)
C       1.c.2
        IF (TS.EQ.1) THEN
C       501: L1C1
           GO TO 501
        ELSE IF (TS.LT.5) THEN
C       502: L1C2
           GO TO 502
        ELSE
C       503: L1C3
           GO TO 503
        END IF
C       1.c.3 (L1C1)
501     DO 103 K=1,M
        READ(5,2008) I,L(I),IZ(I)
        JJ(I)=I
        JK(I)=I+1
103     WRITE(6,9114) I,JJ(I),JK(I),IZ(I),L(I)
C       504: L1D
        GO TO 504
C       1.c.4-9 (L1C2)
502     DO 104 K=1,M
        IF (TS.EQ.2) THEN
           READ(5,2015) I,JJ(I),JK(I),AX(I)
        ELSE IF (TS.EQ.3) THEN
           READ(5,2016) I,JJ(I),JK(I),AX(I),IZ(I)
        ELSE
           READ(5,2016) I,JJ(I),JK(I),IX(I),IY(I)
        END IF
        XCL=X(JK(I))-X(JJ(I))
        YCL=Y(JK(I))-Y(JJ(I))
        L(I)=SQRT(XCL**2+YCL**2)
        CX=XCL/L(I)
        CY=YCL/L(I)
        IF (TS.EQ.2) THEN
           WRITE(6,9121) I,JJ(I),JK(I),AX(I),L(I)
           R(I,4)=CX
           R(I,1)=R(I,4)
           R(I,2)=CY
           R(I,3)=-CY
        ELSE IF (TS.EQ.3) THEN
           WRITE(6,9122) I,JJ(I),JK(I),AX(I),IZ(I),L(I)
           R(I,3)=0.0
           R(I,6)=0.0
           R(I,7)=0.0
           R(I,8)=0.0
           R(I,1)=CX
           R(I,5)=CX
           R(I,9)=1.0
           R(I,2)=CY
           R(I,4)=-CY
        ELSE
           WRITE(6,9123) I,JJ(I),JK(I),IX(I),IY(I),L(I)
           R(I,3)=0.0
           R(I,6)=0.0
           R(I,7)=0.0
           R(I,8)=0.0
           R(I,1)=CX
           R(I,5)=CX
           R(I,9)=1.0
           R(I,2)=CY
           R(I,4)=-CY
        END IF
104     CONTINUE
C       504: L1D
        GO TO 504
C       1.c.10-18 (L1C3)
503     DO 105 K=1,M
        IF (TS.EQ.5) THEN
           READ(5,2015) I,JJ(I),JK(I),AX(I)
        ELSE
           READ(5,2011) I,JJ(I),JK(I),AX(I),IX(I),IY(I),IZ(I),AA
        END IF
        XCL = X(JK(I))-X(JJ(I))
        YCL = Y(JK(I))-Y(JJ(I))
        ZCL = Z(JK(I))-Z(JJ(I))
        L(I)= SQRT(XCL**2+YCL**2+ZCL**2)
        CX = XCL/L(I)
        CY = YCL/L(I)
        CZ = ZCL/L(I)
        IF (TS.EQ.5) THEN
           WRITE(6,9130) I,JJ(I),JK(I),AX(I),L(I)
        ELSE
           WRITE(6,9131) I,JJ(I),JK(I),AX(I),IX(I),IY(I),IZ(I),AA,L(I)
           IF (AA.EQ.1) THEN
              READ(5,2009) I,XP,YP,ZP
              XPS=XP-X(JJ(I))
              YPS=YP-Y(JJ(I))
              ZPS=ZP-Z(JJ(I))
           END IF
        END IF
        Q=SQRT(CX**2+CZ**2)
        IF (Q.LT.TOL) THEN
           R(I,1)=0.0
           R(I,3)=0.0
           R(I,5)=0.0
           R(I,6)=0.0
           R(I,7)=0.0
           R(I,8)=0.0
           R(I,2)=CY
           R(I,4)=-CY
           R(I,9)=1.0
           IF (TS.EQ.6 .AND. AA.EQ.1) THEN
              SQ=SQRT(XPS**2+ZPS**2)
              COSA=-XPS*CY/SQ
              SINA=ZPS/SQ
              R(I,4)=-CY*COSA
              R(I,6)=SINA
              R(I,7)=CY*SINA
              R(I,9)=COSA
           END IF
        ELSE
           R(I,1)=CX
           R(I,2)=CY
           R(I,3)=CZ
           R(I,4)=-CX*CY/Q
           R(I,5)=Q
           R(I,6)=-CY*CZ/Q
           R(I,7)=-CZ/Q
           R(I,8)=0.0
           R(I,9)=CX/Q
           IF (TS.EQ.6 .AND. AA.EQ.1) THEN
              YPG=R(I,4)*XPS+R(I,5)*YPS+R(I,6)*ZPS
              ZPG=R(I,7)*XPS+R(I,8)*YPS+R(I,9)*ZPS
              SQ=SQRT(YPG**2+ZPG**2)
              COSA=YPG/SQ
              SINA=ZPG/SQ
              R(I,4)=(-CX*CY*COSA-CZ*SINA)/Q
              R(I,5)=Q*COSA
              R(I,8)=-Q*SINA
              R(I,6)=(-CY*CZ*COSA+CX*SINA)/Q
              R(I,7)=(CX*CY*SINA-CZ*COSA)/Q
              R(I,9)=(CY*CZ*SINA+CX*COSA)/Q
           END IF
        END IF
105     CONTINUE
C       1.d.1 (L1D)
504     CONTINUE
        WRITE(6,8000)
        WRITE(6,9017)
        WRITE(6,9018)
C       1.d.2-4
        IF (TS.LT.3) THEN
           DO 106 J=1,NRJ
           READ(5,2001) K,RL(2*K-1),RL(2*K)
106        WRITE(6,9115) K,RL(2*K-1),RL(2*K)
C          505: L1D1
           GO TO 505
        END IF
        IF (TS.LT.6) THEN
           DO 107 J=1,NRJ
           READ(5,2002) K,RL(3*K-2),RL(3*K-1),RL(3*K)
107        WRITE(6,9124) K,RL(3*K-2),RL(3*K-1),RL(3*K)
C          505: L1D1
           GO TO 505
        END IF
        DO 108 J=1,NRJ
        READ(5,2003) K,RL(6*K-5),RL(6*K-4),RL(6*K-3),RL(6*K-2),
     &               RL(6*K-1),RL(6*K)
108     WRITE(6,9132) K,RL(6*K-5),RL(6*K-4),RL(6*K-3),RL(6*K-2),
     &               RL(6*K-1),RL(6*K)
C       1.d.5 (L1D1)
505     CRL(1)=RL(1)
C       1.d.6
        DO 109 K=2,N+NR
109     CRL(K)=CRL(K-1)+RL(K)
C       2.a.1
        I=0
C       2.a.2 (L2)
519     I=I+1
C       2.a.3
        IF (I.GT.M) GO TO 520
C       520: L2B
C       2.a.4-6
        IF (TS.LT.3) THEN
           J1=2*JJ(I)-1
           J2=2*JJ(I)
           K1=2*JK(I)-1
           K2=2*JK(I)
        ELSE IF (TS.LT.6) THEN
           J1=3*JJ(I)-2
           J2=3*JJ(I)-1
           J3=3*JJ(I)
           K1=3*JK(I)-2
           K2=3*JK(I)-1
           K3=3*JK(I)
        ELSE
           J1=6*JJ(I)-5
           J2=6*JJ(I)-4
           J3=6*JJ(I)-3
           J4=6*JJ(I)-2
           J5=6*JJ(I)-1
           J6=6*JJ(I)
           K1=6*JK(I)-5
           K2=6*JK(I)-4
           K3=6*JK(I)-3
           K4=6*JK(I)-2
           K5=6*JK(I)-1
           K6=6*JK(I)
        END IF
C       2.a.7
        IF (TS.EQ.1 .OR. TS.EQ.3 .OR. TS.EQ.6) THEN
           SCM2Z=4.0*E*IZ(I)/L(I)
           SCM3Z=1.5*SCM2Z/L(I)
           SCM4Z=2.0*SCM3Z/L(I)
        END IF
C       2.a.8
        IF (TS.EQ.2 .OR. TS.EQ.3 .OR. TS.EQ.5 .OR. TS.EQ.6) THEN
           SCM1A=E*AX(I)/L(I)
        END IF
C       2.a.9
        IF (TS.EQ.4 .OR. TS.EQ.6) THEN
           SCM1B=G*IX(I)/L(I)
           SCM2Y=4.0*E*IY(I)/L(I)
           SCM3Y=1.5*SCM2Y/L(I)
           SCM4Y=2.0*SCM3Y/L(I)
        END IF
C       2.a.10
        IF (RL(J1).EQ.0) THEN
           J1=J1-CRL(J1)
        ELSE
           J1=N+CRL(J1)
        END IF
        IF (RL(J2).EQ.0) THEN
           J2=J2-CRL(J2)
        ELSE
           J2=N+CRL(J2)
        END IF
        IF (RL(K1).EQ.0) THEN
           K1=K1-CRL(K1)
        ELSE
           K1=N+CRL(K1)
        END IF
        IF (RL(K2).EQ.0) THEN
           K2=K2-CRL(K2)
        ELSE
           K2=N+CRL(K2)
        END IF
C       2.a.11
        IF (TS.LT.3) GO TO 506
C       506: L2A
C       2.a.12
        IF (RL(J3).EQ.0) THEN
           J3=J3-CRL(J3)
        ELSE
           J3=N+CRL(J3)
        END IF
        IF (RL(K3).EQ.0) THEN
           K3=K3-CRL(K3)
        ELSE
           K3=N+CRL(K3)
        END IF
C       2.a.13
        IF (TS.LT.6) GO TO 506
C       506: L2A
C       2.a.14
        IF (RL(J4).EQ.0) THEN
           J4=J4-CRL(J4)
        ELSE
           J4=N+CRL(J4)
        END IF
        IF (RL(J5).EQ.0) THEN
           J5=J5-CRL(J5)
        ELSE
           J5=N+CRL(J5)
        END IF
        IF (RL(J6).EQ.0) THEN
           J6=J6-CRL(J6)
        ELSE
           J6=N+CRL(J6)
        END IF
        IF (RL(K4).EQ.0) THEN
           K4=K4-CRL(K4)
        ELSE
           K4=N+CRL(K4)
        END IF
        IF (RL(K5).EQ.0) THEN
           K5=K5-CRL(K5)
        ELSE
           K5=N+CRL(K5)
        END IF
        IF (RL(K6).EQ.0) THEN
           K6=K6-CRL(K6)
        ELSE
           K6=N+CRL(K6)
        END IF
C       2.a.15 (L2A)
506     IF (TS.EQ.1) GO TO 507
C       507: L2A1
        IF (TS.EQ.2) GO TO 508
C       508: L2A2
        IF (TS.EQ.3) GO TO 509
C       509: L2A3
        IF (TS.EQ.4) GO TO 510
C       510: L2A4
        IF (TS.EQ.5) GO TO 511
C       511: L2A5
        IF (TS.EQ.6) GO TO 512
C       512: L2A6
C       2.a.16 (L2A1)
507     SMD(1,1)=SCM4Z
        SMD(3,3)=SCM4Z
        SMD(1,3)=-SCM4Z
        SMD(3,1)=-SCM4Z
        SMD(1,2)=SCM3Z
        SMD(2,1)=SCM3Z
        SMD(1,4)=SCM3Z
        SMD(4,1)=SCM3Z
        SMD(2,3)=-SCM3Z
        SMD(3,2)=-SCM3Z
        SMD(3,4)=-SCM3Z
        SMD(4,3)=-SCM3Z
        SMD(2,2)=SCM2Z
        SMD(4,4)=SCM2Z
        SMD(2,4)=SCM2Z/2.0
        SMD(4,2)=SCM2Z/2.0
C       515: L2A9
        GO TO 515
C       2.a.17 (L2A2)
508     SM(1,1)=SCM1A
        SM(3,3)=SCM1A
        SM(1,3)=-SCM1A
        SM(3,1)=-SCM1A
C       513: L2A7
        GO TO 513
C       2.a.18 (L2A3)
509     SM(1,1)=SCM1A
        SM(4,4)=SCM1A
        SM(1,4)=-SCM1A
        SM(4,1)=-SCM1A
        SM(2,2)=SCM4Z
        SM(5,5)=SCM4Z
        SM(2,5)=-SCM4Z
        SM(5,2)=-SCM4Z
        SM(2,3)=SCM3Z
        SM(3,2)=SCM3Z
        SM(2,6)=SCM3Z
        SM(6,2)=SCM3Z
        SM(3,5)=-SCM3Z
        SM(5,3)=-SCM3Z
        SM(5,6)=-SCM3Z
        SM(6,5)=-SCM3Z
        SM(3,3)=SCM2Z
        SM(6,6)=SCM2Z
        SM(3,6)=SCM2Z/2.0
        SM(6,3)=SCM2Z/2.0
C       514: L2A8
        GO TO 514
C       2.a.19 (L2A4)
510     SM(1,1)=SCM1B
        SM(4,4)=SCM1B
        SM(1,4)=-SCM1B
        SM(4,1)=-SCM1B
        SM(2,2)=SCM2Y
        SM(5,5)=SCM2Y
        SM(2,5)=SCM2Y/2.0
        SM(5,2)=SCM2Y/2.0
        SM(2,6)=SCM3Y
        SM(6,2)=SCM3Y
        SM(5,6)=SCM3Y
        SM(6,5)=SCM3Y
        SM(2,3)=-SCM3Y
        SM(3,2)=-SCM3Y
        SM(5,3)=-SCM3Y
        SM(3,5)=-SCM3Y
        SM(3,3)=SCM4Y
        SM(6,6)=SCM4Y
        SM(3,6)=-SCM4Y
        SM(6,3)=-SCM4Y
C       514: L2A8
        GO TO 514
C       2.a.20 (L2A5)
511     SM(1,1)=SCM1A
        SM(4,4)=SCM1A
        SM(1,4)=-SCM1A
        SM(4,1)=-SCM1A
C       514: L2A8
        GO TO 514
C       2.a.21 (L2A6)
512     SM(1,1)=SCM1A
        SM(7,7)=SCM1A
        SM(1,7)=-SCM1A
        SM(7,1)=-SCM1A
        SM(2,2)=SCM4Z
        SM(8,8)=SCM4Z
        SM(2,8)=-SCM4Z
        SM(8,2)=-SCM4Z
        SM(2,6)=SCM3Z
        SM(6,2)=SCM3Z
        SM(2,12)=SCM3Z
        SM(12,2)=SCM3Z
        SM(8,6)=-SCM3Z
        SM(6,8)=-SCM3Z
        SM(8,12)=-SCM3Z
        SM(12,8)=-SCM3Z
        SM(3,3)=SCM4Y
        SM(9,9)=SCM4Y
        SM(3,9)=-SCM4Y
        SM(9,3)=-SCM4Y
        SM(3,5)=-SCM3Y
        SM(5,3)=-SCM3Y
        SM(3,11)=-SCM3Y
        SM(11,3)=-SCM3Y
        SM(4,4)=SCM1B
        SM(10,10)=SCM1B
        SM(4,10)=-SCM1B
        SM(10,4)=-SCM1B
        SM(5,5)=SCM2Y
        SM(11,11)=SCM2Y
        SM(5,11)=SCM2Y/2.0
        SM(11,5)=SCM2Y/2.0
        SM(5,9)=SCM3Y
        SM(9,5)=SCM3Y
        SM(11,9)=SCM3Y
        SM(9,11)=SCM3Y
        SM(6,6)=SCM2Z
        SM(12,12)=SCM2Z
        SM(6,12)=SCM2Z/2.0
        SM(12,6)=SCM2Z/2.0
C       514: L2A8
        GO TO 514
C       2.a.22 (L2A7)
513     DO 110 K=1,2
        DO 110 J=1,4
        SMR(J,2*K-1)=SM(J,2*K-1)*R(I,1)+SM(J,2*K)*R(I,3)
110     SMR(J,2*K)=SM(J,2*K-1)*R(I,2)+SM(J,2*K)*R(I,4)
C       2.a.23
        DO 111 J=1,2
        DO 111 K=1,4
        SMD(2*J-1,K)=R(I,1)*SMR(2*J-1,K)+R(I,3)*SMR(2*J,K)
111     SMD(2*J,K)=R(I,2)*SMR(2*J-1,K)+R(I,4)*SMR(2*J,K)
C       515: L2A9
        GO TO 515
C       2.a.24 (L2A8)
514     DO 112 K=1,2*NDJ/3
        DO 112 J=1,2*NDJ
        SMR(J,3*K-2)=SM(J,3*K-2)*R(I,1)+SM(J,3*K-1)*R(I,4) +
     &               SM(J,3*K)*R(I,7)
        SMR(J,3*K-1)=SM(J,3*K-2)*R(I,2)+SM(J,3*K-1)*R(I,5) +
     &               SM(J,3*K)*R(I,8)
        SMR(J,3*K)=SM(J,3*K-2)*R(I,3)+SM(J,3*K-1)*R(I,6) +
     &               SM(J,3*K)*R(I,9)
112     CONTINUE
C       2.a.25
        DO 113 J=1,2*NDJ/3
        DO 113 K=1,2*NDJ
        SMD(3*J-2,K)=R(I,1)*SMR(3*J-2,K)+R(I,4)*SMR(3*J-1,K) +
     &               R(I,7)*SMR(3*J,K)
        SMD(3*J-1,K)=R(I,2)*SMR(3*J-2,K)+R(I,5)*SMR(3*J-1,K) +
     &               R(I,8)*SMR(3*J,K)
        SMD(3*J,K)=R(I,3)*SMR(3*J-2,K)+R(I,6)*SMR(3*J-1,K) +
     &               R(I,9)*SMR(3*J,K)
113     CONTINUE
        IF (TS.LT.6) THEN
C       516: L2A10
           GO TO 516
        ELSE
C       517: L2A11
           GO TO 517
        END IF
C       2.a.26 (L2A9)
515     IF (RL(2*JJ(I)-1).EQ.0) THEN
           S(J1,J1)=S(J1,J1)+SMD(1,1)
           S(J2,J1)=S(J2,J1)+SMD(2,1)
           S(K1,J1)=SMD(3,1)
           S(K2,J1)=SMD(4,1)
        END IF
C       2.a.27
        IF (RL(2*JJ(I)).EQ.0) THEN
           S(J1,J2)=S(J1,J2)+SMD(1,2)
           S(J2,J2)=S(J2,J2)+SMD(2,2)
           S(K1,J2)=SMD(3,2)
           S(K2,J2)=SMD(4,2)
        END IF
C       2.a.28
        IF (RL(2*JK(I)-1).EQ.0) THEN
           S(J1,K1)=SMD(1,3)
           S(J2,K1)=SMD(2,3)
           S(K1,K1)=S(K1,K1)+SMD(3,3)
           S(K2,K1)=S(K2,K1)+SMD(4,3)
        END IF
C       2.a.29
        IF (RL(2*JK(I)).EQ.0) THEN
           S(J1,K2)=SMD(1,4)
           S(J2,K2)=SMD(2,4)
           S(K1,K2)=S(K1,K2)+SMD(3,4)
           S(K2,K2)=S(K2,K2)+SMD(4,4)
        END IF
C       2.a.30
C       519: L2
        GO TO 519
C       2.a.31 (L2A10)
516     IF (RL(3*JJ(I)-2).EQ.0) THEN
           S(J1,J1)=S(J1,J1)+SMD(1,1)
           S(J2,J1)=S(J2,J1)+SMD(2,1)
           S(J3,J1)=S(J3,J1)+SMD(3,1)
           S(K1,J1)=SMD(4,1)
           S(K2,J1)=SMD(5,1)
           S(K3,J1)=SMD(6,1)
        END IF
        IF (RL(3*JJ(I)-1).EQ.0) THEN
           S(J1,J2)=S(J1,J2)+SMD(1,2)
           S(J2,J2)=S(J2,J2)+SMD(2,2)
           S(J3,J2)=S(J3,J2)+SMD(3,2)
           S(K1,J2)=SMD(4,2)
           S(K2,J2)=SMD(5,2)
           S(K3,J2)=SMD(6,2)
        END IF
        IF (RL(3*JJ(I)).EQ.0) THEN
           S(J1,J3)=S(J1,J3)+SMD(1,3)
           S(J2,J3)=S(J2,J3)+SMD(2,3)
           S(J3,J3)=S(J3,J3)+SMD(3,3)
           S(K1,J3)=SMD(4,3)
           S(K2,J3)=SMD(5,3)
           S(K3,J3)=SMD(6,3)
        END IF
        IF (RL(3*JK(I)-2).EQ.0) THEN
           S(J1,K1)=SMD(1,4)
           S(J2,K1)=SMD(2,4)
           S(J3,K1)=SMD(3,4)
           S(K1,K1)=SMD(4,4)+S(K1,K1)
           S(K2,K1)=SMD(5,4)+S(K2,K1)
           S(K3,K1)=SMD(6,4)+S(K3,K1)
        END IF
        IF (RL(3*JK(I)-1).EQ.0) THEN
           S(J1,K2)=SMD(1,5)
           S(J2,K2)=SMD(2,5)
           S(J3,K2)=SMD(3,5)
           S(K1,K2)=SMD(4,5)+S(K1,K2)
           S(K2,K2)=SMD(5,5)+S(K2,K2)
           S(K3,K2)=SMD(6,5)+S(K3,K2)
        END IF
        IF (RL(3*JK(I)).EQ.0) THEN
           S(J1,K3)=SMD(1,6)
           S(J2,K3)=SMD(2,6)
           S(J3,K3)=SMD(3,6)
           S(K1,K3)=SMD(4,6)+S(K1,K3)
           S(K2,K3)=SMD(5,6)+S(K2,K3)
           S(K3,K3)=SMD(6,6)+S(K3,K3)
        END IF
C       519: L2
        GO TO 519
C       2.a.32 (L2A11)
517     IF (RL(6*JJ(I)-5).EQ.0) THEN
           S(J1,J1)=SMD(1,1)+S(J1,J1)
           S(J2,J1)=SMD(2,1)+S(J2,J1)
           S(J3,J1)=SMD(3,1)+S(J3,J1)
           S(J4,J1)=SMD(4,1)+S(J4,J1)
           S(J5,J1)=SMD(5,1)+S(J5,J1)
           S(J6,J1)=SMD(6,1)+S(J6,J1)
           S(K1,J1)=SMD(7,1)
           S(K2,J1)=SMD(8,1)
           S(K3,J1)=SMD(9,1)
           S(K4,J1)=SMD(10,1)
           S(K5,J1)=SMD(11,1)
           S(K6,J1)=SMD(12,1)
        END IF
        IF (RL(6*JJ(I)-4).EQ.0) THEN
           S(J1,J2)=SMD(1,2)+S(J1,J2)
           S(J2,J2)=SMD(2,2)+S(J2,J2)
           S(J3,J2)=SMD(3,2)+S(J3,J2)
           S(J4,J2)=SMD(4,2)+S(J4,J2)
           S(J5,J2)=SMD(5,2)+S(J5,J2)
           S(J6,J2)=SMD(6,2)+S(J6,J2)
           S(K1,J2)=SMD(7,2)
           S(K2,J2)=SMD(8,2)
           S(K3,J2)=SMD(9,2)
           S(K4,J2)=SMD(10,2)
           S(K5,J2)=SMD(11,2)
           S(K6,J2)=SMD(12,2)
        END IF
        IF (RL(6*JJ(I)-3).EQ.0) THEN
           S(J1,J3)=SMD(1,3)+S(J1,J3)
           S(J2,J3)=SMD(2,3)+S(J2,J3)
           S(J3,J3)=SMD(3,3)+S(J3,J3)
           S(J4,J3)=SMD(4,3)+S(J4,J3)
           S(J5,J3)=SMD(5,3)+S(J5,J3)
           S(J6,J3)=SMD(6,3)+S(J6,J3)
           S(K1,J3)=SMD(7,3)
           S(K2,J3)=SMD(8,3)
           S(K3,J3)=SMD(9,3)
           S(K4,J3)=SMD(10,3)
           S(K5,J3)=SMD(11,3)
           S(K6,J3)=SMD(12,3)
        END IF
        IF (RL(6*JJ(I)-2).EQ.0) THEN
           S(J1,J4)=SMD(1,4)+S(J1,J4)
           S(J2,J4)=SMD(2,4)+S(J2,J4)
           S(J3,J4)=SMD(3,4)+S(J3,J4)
           S(J4,J4)=SMD(4,4)+S(J4,J4)
           S(J5,J4)=SMD(5,4)+S(J5,J4)
           S(J6,J4)=SMD(6,4)+S(J6,J4)
           S(K1,J4)=SMD(7,4)
           S(K2,J4)=SMD(8,4)
           S(K3,J4)=SMD(9,4)
           S(K4,J4)=SMD(10,4)
           S(K5,J4)=SMD(11,4)
           S(K6,J4)=SMD(12,4)
        END IF
        IF (RL(6*JJ(I)-1).EQ.0) THEN
           S(J1,J5)=SMD(1,5)+S(J1,J5)
           S(J2,J5)=SMD(2,5)+S(J2,J5)
           S(J3,J5)=SMD(3,5)+S(J3,J5)
           S(J4,J5)=SMD(4,5)+S(J4,J5)
           S(J5,J5)=SMD(5,5)+S(J5,J5)
           S(J6,J5)=SMD(6,5)+S(J6,J5)
           S(K1,J5)=SMD(7,5)
           S(K2,J5)=SMD(8,5)
           S(K3,J5)=SMD(9,5)
           S(K4,J5)=SMD(10,5)
           S(K5,J5)=SMD(11,5)
           S(K6,J5)=SMD(12,5)
        END IF
        IF (RL(6*JJ(I)).EQ.0) THEN
           S(J1,J6)=SMD(1,6)+S(J1,J6)
           S(J2,J6)=SMD(2,6)+S(J2,J6)
           S(J3,J6)=SMD(3,6)+S(J3,J6)
           S(J4,J6)=SMD(4,6)+S(J4,J6)
           S(J5,J6)=SMD(5,6)+S(J5,J6)
           S(J6,J6)=SMD(6,6)+S(J6,J6)
           S(K1,J6)=SMD(7,6)
           S(K2,J6)=SMD(8,6)
           S(K3,J6)=SMD(9,6)
           S(K4,J6)=SMD(10,6)
           S(K5,J6)=SMD(11,6)
           S(K6,J6)=SMD(12,6)
        END IF
        IF (RL(6*JK(I)-5).EQ.0) THEN
           S(J1,K1)=SMD(1,7)
           S(J2,K1)=SMD(2,7)
           S(J3,K1)=SMD(3,7)
           S(J4,K1)=SMD(4,7)
           S(J5,K1)=SMD(5,7)
           S(J6,K1)=SMD(6,7)
           S(K1,K1)=SMD(7,7)+S(K1,K1)
           S(K2,K1)=SMD(8,7)+S(K2,K1)
           S(K3,K1)=SMD(9,7)+S(K3,K1)
           S(K4,K1)=SMD(10,7)+S(K4,K1)
           S(K5,K1)=SMD(11,7)+S(K5,K1)
           S(K6,K1)=SMD(12,7)+S(K6,K1)
        END IF
        IF (RL(6*JK(I)-4).EQ.0) THEN
           S(J1,K2)=SMD(1,8)
           S(J2,K2)=SMD(2,8)
           S(J3,K2)=SMD(3,8)
           S(J4,K2)=SMD(4,8)
           S(J5,K2)=SMD(5,8)
           S(J6,K2)=SMD(6,8)
           S(K1,K2)=SMD(7,8)+S(K1,K2)
           S(K2,K2)=SMD(8,8)+S(K2,K2)
           S(K3,K2)=SMD(9,8)+S(K3,K2)
           S(K4,K2)=SMD(10,8)+S(K4,K2)
           S(K5,K2)=SMD(11,8)+S(K5,K2)
           S(K6,K2)=SMD(12,8)+S(K6,K2)
        END IF
        IF (RL(6*JK(I)-3).EQ.0) THEN
           S(J1,K3)=SMD(1,9)
           S(J2,K3)=SMD(2,9)
           S(J3,K3)=SMD(3,9)
           S(J4,K3)=SMD(4,9)
           S(J5,K3)=SMD(5,9)
           S(J6,K3)=SMD(6,9)
           S(K1,K3)=SMD(7,9)+S(K1,K3)
           S(K2,K3)=SMD(8,9)+S(K2,K3)
           S(K3,K3)=SMD(9,9)+S(K3,K3)
           S(K4,K3)=SMD(10,9)+S(K4,K3)
           S(K5,K3)=SMD(11,9)+S(K5,K3)
           S(K6,K3)=SMD(12,9)+S(K6,K3)
        END IF
        IF (RL(6*JK(I)-2).EQ.0) THEN
           S(J1,K4)=SMD(1,10)
           S(J2,K4)=SMD(2,10)
           S(J3,K4)=SMD(3,10)
           S(J4,K4)=SMD(4,10)
           S(J5,K4)=SMD(5,10)
           S(J6,K4)=SMD(6,10)
           S(K1,K4)=SMD(7,10)+S(K1,K4)
           S(K2,K4)=SMD(8,10)+S(K2,K4)
           S(K3,K4)=SMD(9,10)+S(K3,K4)
           S(K4,K4)=SMD(10,10)+S(K4,K4)
           S(K5,K4)=SMD(11,10)+S(K5,K4)
           S(K6,K4)=SMD(12,10)+S(K6,K4)
        END IF
        IF (RL(6*JK(I)-1).EQ.0) THEN
           S(J1,K5)=SMD(1,11)
           S(J2,K5)=SMD(2,11)
           S(J3,K5)=SMD(3,11)
           S(J4,K5)=SMD(4,11)
           S(J5,K5)=SMD(5,11)
           S(J6,K5)=SMD(6,11)
           S(K1,K5)=SMD(7,11)+S(K1,K5)
           S(K2,K5)=SMD(8,11)+S(K2,K5)
           S(K3,K5)=SMD(9,11)+S(K3,K5)
           S(K4,K5)=SMD(10,11)+S(K4,K5)
           S(K5,K5)=SMD(11,11)+S(K5,K5)
           S(K6,K5)=SMD(12,11)+S(K6,K5)
        END IF
        IF (RL(6*JK(I)).EQ.0) THEN
           S(J1,K6)=SMD(1,12)
           S(J2,K6)=SMD(2,12)
           S(J3,K6)=SMD(3,12)
           S(J4,K6)=SMD(4,12)
           S(J5,K6)=SMD(5,12)
           S(J6,K6)=SMD(6,12)
           S(K1,K6)=SMD(7,12)+S(K1,K6)
           S(K2,K6)=SMD(8,12)+S(K2,K6)
           S(K3,K6)=SMD(9,12)+S(K3,K6)
           S(K4,K6)=SMD(10,12)+S(K4,K6)
           S(K5,K6)=SMD(11,12)+S(K5,K6)
           S(K6,K6)=SMD(12,12)+S(K6,K6)
        END IF
C       519: L2
        GO TO 519
C       2.b.1 (L2B)
520     ERROR=0
        CALL DECOMPOSE(N,S,ERROR)
        IF (ERROR.EQ.1) GO TO 551
C       551: L5C1
        CALL INVERT(N,S)
C       3.a.1
        LN=0
C       3.a.2 (L3)
521     LN=LN+1
C       3.a.3
        IF (SN.GT.1 .OR. LN.GT.1) THEN
        DO 114 CNTA=1,100
           LML(CNTA)=0
           A(CNTA)=0D0
           AE(CNTA)=0D0
        DO 114 CNTB=1,100
114        AML(CNTA,CNTB)=0D0
        END IF
C       3.a.4
        WRITE(6,9019) LN
        WRITE(6,9020)
        READ(5,2012) NLJ,NLM
        WRITE(6,9116) NLJ,NLM
C       3.b.1
        IF (NLJ.EQ.0) GO TO 522
C       522: L3C
C       3.b.2
        WRITE(6,9021)
        WRITE(6,9022)
C       3.b.3
        IF (TS.LT.3) THEN
           DO 115 J=1,NLJ
           READ(5,2008) K,A(2*K-1),A(2*K)
115        WRITE(6,9117) K,A(2*K-1),A(2*K)
C       522: L3C
           GO TO 522
        END IF
C       3.b.4
        IF (TS.LT.6) THEN
           DO 116  J=1,NLJ
           READ(5,2009) K,A(3*K-2),A(3*K-1),A(3*K)
116        WRITE(6,9125) K,A(3*K-2),A(3*K-1),A(3*K)
C       522: L3C
           GO TO 522
        END IF
C       3.b.5
        DO 117 J=1,NLJ
        READ(5,2013) K,A(6*K-5),A(6*K-4),A(6*K-3),A(6*K-2),
     &               A(6*K-1),A(6*K)
        WRITE(6,9126) K,A(6*K-5),A(6*K-4),A(6*K-3),A(6*K-2),
     &               A(6*K-1),A(6*K)
117     CONTINUE
C       3.c.1 (L3C)
522     IF (NLM.EQ.0) GO TO 527
C       527: L4B
C       3.c.2-3
        WRITE(6,9023)
        IF (TS.NE.6) WRITE(6,9024)
        IF (TS.EQ.6) WRITE(6,9025)
C       3.c.4
        IF (TS.LT.3) THEN
           DO 118 J=1,NLM
           READ(5,2010) I,AML(I,1),AML(I,2),AML(I,3),AML(I,4)
           WRITE(6,9118) I,AML(I,1),AML(I,2),AML(I,3),AML(I,4)
           LML(I)=1
118        CONTINUE
C          CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       3.c.6
        ELSE IF (TS.EQ.6) THEN
           DO 119 J=1,NLM
           READ(5,2014) I,AML(I,1),AML(I,2),AML(I,3),AML(I,4),
     &                  AML(I,5),AML(I,6),AML(I,7),AML(I,8),
     &                  AML(I,9),AML(I,10),AML(I,11),AML(I,12)
           WRITE(6,9108) I,AML(I,1),AML(I,2),AML(I,3),AML(I,4),
     &                  AML(I,5),AML(I,6),AML(I,7),AML(I,8),
     &                  AML(I,9),AML(I,10),AML(I,11),AML(I,12)
           LML(I)=1
119        CONTINUE
C       3.c.5
        ELSE
           DO 120 J=1,NLM
           READ(5,2013) I,AML(I,1),AML(I,2),AML(I,3),AML(I,4),
     &                  AML(I,5),AML(I,6)
           WRITE(6,9126) I,AML(I,1),AML(I,2),AML(I,3),AML(I,4),
     &                  AML(I,5),AML(I,6)
           LML(I)=1
120        CONTINUE
        END IF
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC LML(I)=1
C       4.a.1
        IF (TS.EQ.1) GO TO 523
C       523: L4A1
        IF (TS.EQ.2) GO TO 524
C       524: L4A2
        IF (TS.LT.6) GO TO 525
C       525: L4A3
C       526: L4A4
        GO TO 526
C       4.a.2 (L4A1)
523     DO 121 I=1,M
        IF (LML(I).EQ.1) THEN
           AE(2*I-1)=AE(2*I-1)-AML(I,1)
           AE(2*I)=AE(2*I)-AML(I,2)
           AE(2*I+1)=AE(2*I+1)-AML(I,3)
           AE(2*I+2)=AE(2*I+2)-AML(I,4)
        END IF
121     CONTINUE
C       527: L4B
        GO TO 527
C       4.a.3 (L4A2)
524     DO 122 I=1,M
        IF (LML(I).EQ.1) THEN
           AE(2*JJ(I)-1)=AE(2*JJ(I)-1)-R(I,1)*AML(I,1)-R(I,3)*AML(I,2)
           AE(2*JJ(I))=AE(2*JJ(I))-R(I,2)*AML(I,1)-R(I,4)*AML(I,2)
           AE(2*JK(I)-1)=AE(2*JK(I)-1)-R(I,1)*AML(I,3)-R(I,3)*AML(I,4)
           AE(2*JK(I))=AE(2*JK(I))-R(I,2)*AML(I,3)-R(I,4)*AML(I,4)
        END IF
122     CONTINUE
C       527: L4B
        GO TO 527
C       4.a.4 (L4A3)
525     DO 123 I=1,M
        IF (LML(I).EQ.1) THEN
           AE(3*JJ(I)-2)=AE(3*JJ(I)-2)-R(I,1)*AML(I,1)-R(I,4)*AML(I,2)-
     &                   R(I,7)*AML(I,3)
           AE(3*JJ(I)-1)=AE(3*JJ(I)-1)-R(I,2)*AML(I,1)-R(I,5)*AML(I,2)-
     &                   R(I,8)*AML(I,3)
           AE(3*JJ(I))=AE(3*JJ(I))-R(I,3)*AML(I,1)-R(I,6)*AML(I,2)-
     &                   R(I,9)*AML(I,3)
           AE(3*JK(I)-2)=AE(3*JK(I)-2)-R(I,1)*AML(I,4)-R(I,4)*AML(I,5)-
     &                   R(I,7)*AML(I,6)
           AE(3*JK(I)-1)=AE(3*JK(I)-1)-R(I,2)*AML(I,4)-R(I,5)*AML(I,5)-
     &                   R(I,8)*AML(I,6)
           AE(3*JK(I))=AE(3*JK(I))-R(I,3)*AML(I,4)-R(I,6)*AML(I,5)-
     &                   R(I,9)*AML(I,6)
        END IF
123     CONTINUE
C       527: L4B
        GO TO 527
C       4.a.5 (L4A4)
526     DO 124 I=1,M
        IF (LML(I).EQ.1) THEN
           AE(6*JJ(I)-5)=AE(6*JJ(I)-5)-R(I,1)*AML(I,1)-R(I,4)*AML(I,2)-
     &                   R(I,7)*AML(I,3)
           AE(6*JJ(I)-4)=AE(6*JJ(I)-4)-R(I,2)*AML(I,1)-R(I,5)*AML(I,2)-
     &                   R(I,8)*AML(I,3)
           AE(6*JJ(I)-3)=AE(6*JJ(I)-3)-R(I,3)*AML(I,1)-R(I,6)*AML(I,2)-
     &                   R(I,9)*AML(I,3)
           AE(6*JJ(I)-2)=AE(6*JJ(I)-2)-R(I,1)*AML(I,4)-R(I,4)*AML(I,5)-
     &                   R(I,7)*AML(I,6)
           AE(6*JJ(I)-1)=AE(6*JJ(I)-1)-R(I,2)*AML(I,4)-R(I,5)*AML(I,5)-
     &                   R(I,8)*AML(I,6)
           AE(6*JJ(I))=AE(6*JJ(I))-R(I,3)*AML(I,4)-R(I,6)*AML(I,5)-
     &                   R(I,9)*AML(I,6)
           AE(6*JK(I)-5)=AE(6*JK(I)-5)-R(I,1)*AML(I,7)-R(I,4)*AML(I,8)-
     &                   R(I,7)*AML(I,9)
           AE(6*JK(I)-4)=AE(6*JK(I)-4)-R(I,2)*AML(I,7)-R(I,5)*AML(I,8)-
     &                   R(I,8)*AML(I,9)
           AE(6*JK(I)-3)=AE(6*JK(I)-3)-R(I,3)*AML(I,7)-R(I,6)*AML(I,8)-
     &                   R(I,9)*AML(I,9)
           AE(6*JK(I)-2)=AE(6*JK(I)-2)-R(I,1)*AML(I,10)-R(I,4)*AML(I,11)
     &                   -R(I,7)*AML(I,12)
           AE(6*JK(I)-1)=AE(6*JK(I)-1)-R(I,2)*AML(I,10)-R(I,5)*AML(I,11)
     &                   -R(I,8)*AML(I,12)
           AE(6*JK(I))=AE(6*JK(I))-R(I,3)*AML(I,10)-R(I,6)*AML(I,11)
     &                   -R(I,9)*AML(I,12)
        END IF
124     CONTINUE
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ERROR ON THIS BLOCK    (FIXED BY CHANGING GT 1 TO GE 1 AT START)
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       4.b.1-4 (L4B)
527     DO 125 J=1,N+NR
        IF (RL(J).EQ.0) THEN
           K=J-CRL(J)
        ELSE
           K=N+CRL(J)
        END IF
125     AC(K)=A(J)+AE(J)
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       5.a.1
        WRITE(6,9026)
        WRITE(6,9027)
C       5.a.2
        DO 126 J=1,N
        D(J)=0D0
        DO 126 K=1,N
126     D(J)=D(J)+S(J,K)*AC(K)
C       5.a.3
        J=N+1
C       5.a.4
        DO 127 JE=N+NR,1,-1
        IF (RL(JE).EQ.0) THEN
           J=J-1
           DJ(JE)=D(J)
        ELSE
           DJ(JE)=0D0
        END IF
127     CONTINUE
C       5.a.5
        IF (TS.LT.3) THEN
           DO 128 JE=2,2*NJ,2
128        WRITE(6,9119) JE/2,DJ(JE-1),DJ(JE)
           GO TO 528
        ELSE IF (TS.LT.6) THEN
           DO 129 JE=3,3*NJ,3
           WRITE(9,*) DJ(JE-2)
129        WRITE(6,9127) JE/3,DJ(JE-2),DJ(JE-1),DJ(JE)
        ELSE
           DO 130 JE=6,6*NJ,6
           WRITE(6,9128) JE/6,DJ(JE-5),DJ(JE-4),DJ(JE-3),
     &                   DJ(JE-2),DJ(JE-1),DJ(JE)
130     CONTINUE
        END IF
C       5.b.1 (L5B)
528     WRITE(6,9028)
        IF (TS.EQ.6) THEN
C       5.b.2
           WRITE(6,9030)
        ELSE
C       5.b.1 (cont)
           WRITE(6,9029)
        END IF
C       5.b.3
        I=0
C       5.b.4 (L5)
550     I=I+1
C       5.b.5
        IF (I.GT.M) GO TO 538
C       538: L5C
C       5.b.6
        IF (TS.LT.3) THEN
           J1=2*JJ(I)-1
           J2=2*JJ(I)
           K1=2*JK(I)-1
           K2=2*JK(I)
        ELSE IF (TS.LT.6) THEN
           J1=3*JJ(I)-2
           J2=3*JJ(I)-1
           J3=3*JJ(I)
           K1=3*JK(I)-2
           K2=3*JK(I)-1
           K3=3*JK(I)
        ELSE
           J1=6*JJ(I)-5
           J2=6*JJ(I)-4
           J3=6*JJ(I)-3
           J4=6*JJ(I)-2
           J5=6*JJ(I)-1
           J6=6*JJ(I)
           K1=6*JK(I)-5
           K2=6*JK(I)-4
           K3=6*JK(I)-3
           K4=6*JK(I)-2
           K5=6*JK(I)-1
           K6=6*JK(I)
        END IF
        IF (TS.EQ.1 .OR. TS.EQ.3 .OR. TS.EQ.6) THEN
           SCM2Z=4.0*E*IZ(I)/L(I)
           SCM3Z=1.5*SCM2Z/L(I)
           SCM4Z=2.0*SCM3Z/L(I)
        END IF
        IF (TS.EQ.2 .OR. TS.EQ.3 .OR. TS.EQ.5 .OR. TS.EQ.6) THEN
           SCM1A=E*AX(I)/L(I)
        END IF
        IF (TS.EQ.4 .OR. TS.EQ.6) THEN
           SCM1B=G*IX(I)/L(I)
           SCM2Y=4.0*E*IY(I)/L(I)
           SCM3Y=1.5*SCM2Y/L(I)
           SCM4Y=2.0*SCM3Y/L(I)
        END IF
C       5.b.7
        IF (TS.EQ.1) GO TO 529
C       529: L5B1
        IF (TS.EQ.2) GO TO 530
C       530: L5B2
        IF (TS.EQ.3) GO TO 531
C       531: L5B3
        IF (TS.EQ.4) GO TO 532
C       532: L5B4
        IF (TS.EQ.5) GO TO 533
C       533: L5B5
        IF (TS.EQ.6) GO TO 534
C       534: L5B6
C       5.b.8 (L5B1)
529     SMR(1,1)=SCM4Z
        SMR(3,3)=SCM4Z
        SMR(1,3)=-SCM4Z
        SMR(3,1)=-SCM4Z
        SMR(1,2)=SCM3Z
        SMR(2,1)=SCM3Z
        SMR(1,4)=SCM3Z
        SMR(4,1)=SCM3Z
        SMR(2,3)=-SCM3Z
        SMR(3,2)=-SCM3Z
        SMR(3,4)=-SCM3Z
        SMR(4,3)=-SCM3Z
        SMR(2,2)=SCM2Z
        SMR(4,4)=SCM2Z
        SMR(2,4)=SCM2Z/2.0
        SMR(4,2)=SCM2Z/2.0
C       536: L5B8
        GO TO 536
C       5.b.8 cont (L5B2)
530     SM(1,1)=SCM1A
        SM(3,3)=SCM1A
        SM(1,3)=-SCM1A
        SM(3,1)=-SCM1A
C       535: L5B7
        GO TO 535
C       5.b.8 cont (L5B3)
531     SM(1,1)=SCM1A
        SM(4,4)=SCM1A
        SM(1,4)=-SCM1A
        SM(4,1)=-SCM1A
        SM(2,2)=SCM4Z
        SM(5,5)=SCM4Z
        SM(2,5)=-SCM4Z
        SM(5,2)=-SCM4Z
        SM(2,3)=SCM3Z
        SM(3,2)=SCM3Z
        SM(2,6)=SCM3Z
        SM(6,2)=SCM3Z
        SM(3,5)=-SCM3Z
        SM(5,3)=-SCM3Z
        SM(5,6)=-SCM3Z
        SM(6,5)=-SCM3Z
        SM(3,3)=SCM2Z
        SM(6,6)=SCM2Z
        SM(3,6)=SCM2Z/2.0
        SM(6,3)=SCM2Z/2.0
C       537: L5B9
        GO TO 537
C       5.b.8 cont (L5B4)
532     SM(1,1)=SCM1B
        SM(4,4)=SCM1B
        SM(1,4)=-SCM1B
        SM(4,1)=-SCM1B
        SM(2,2)=SCM2Y
        SM(5,5)=SCM2Y
        SM(2,5)=SCM2Y/2.0
        SM(5,2)=SCM2Y/2.0
        SM(2,6)=SCM3Y
        SM(6,2)=SCM3Y
        SM(5,6)=SCM3Y
        SM(6,5)=SCM3Y
        SM(2,3)=-SCM3Y
        SM(3,2)=-SCM3Y
        SM(3,5)=-SCM3Y
        SM(5,3)=-SCM3Y
        SM(3,3)=SCM4Y
        SM(6,6)=SCM4Y
        SM(3,6)=-SCM4Y
        SM(6,3)=-SCM4Y
C       537: L5B9
        GO TO 537
C       5.b.8 cont (L5B5)
533     SM(1,1)=SCM1A
        SM(4,4)=SCM1A
        SM(1,4)=-SCM1A
        SM(4,1)=-SCM1A
C       537: L5B9
        GO TO 537
C       5.b.8 cont (L5B6)
534     SM(1,1)=SCM1A
        SM(7,7)=SCM1A
        SM(1,7)=-SCM1A
        SM(7,1)=-SCM1A
        SM(2,2)=SCM4Z
        SM(8,8)=SCM4Z
        SM(2,8)=-SCM4Z
        SM(8,2)=-SCM4Z
        SM(2,6)=SCM3Z
        SM(6,2)=SCM3Z
        SM(2,12)=SCM3Z
        SM(12,2)=SCM3Z
        SM(6,8)=-SCM3Z
        SM(8,6)=-SCM3Z
        SM(8,12)=-SCM3Z
        SM(12,8)=-SCM3Z
        SM(3,3)=SCM4Y
        SM(9,9)=SCM4Y
        SM(3,9)=-SCM4Y
        SM(9,3)=-SCM4Y
        SM(3,5)=-SCM3Y
        SM(5,3)=-SCM3Y
        SM(3,11)=-SCM3Y
        SM(11,3)=-SCM3Y
        SM(4,4)=SCM1B
        SM(10,10)=SCM1B
        SM(4,10)=-SCM1B
        SM(10,4)=-SCM1B
        SM(5,5)=SCM2Y
        SM(11,11)=SCM2Y
        SM(5,11)=SCM2Y/2.0
        SM(11,5)=SCM2Y/2.0
        SM(5,9)=SCM3Y
        SM(9,5)=SCM3Y
        SM(9,11)=SCM3Y
        SM(11,9)=SCM3Y
        SM(6,6)=SCM2Z
        SM(12,12)=SCM2Z
        SM(6,12)=SCM2Z/2.0
        SM(12,6)=SCM2Z/2.0
C       537: L5B9
        GO TO 537
C       5.b.9 (L5B7)
535     DO 131 K=1,2
        DO 131 J=1,4
        SMR(J,2*K-1)=SM(J,2*K-1)*R(I,1)+SM(J,2*K)*R(I,3)
131     SMR(J,2*K)=SM(J,2*K-1)*R(I,2)+SM(J,2*K)*R(I,4)
C       5.b.10 (L5B8)
536     WRITE(6,9109) I
        DO 132 J=1,4
        WRITE(6,9110) AML(I,J)+SMR(J,1)*DJ(J1)+
     &                SMR(J,2)*DJ(J2)+SMR(J,3)*DJ(K1)+SMR(J,4)*DJ(K2)
132     CONTINUE
C       550: L5
        GO TO 550
C       5.b.11 (L5B9)
537     DO 133 K=1,2*NDJ/3
        DO 133 J=1,2*NDJ
        SMR(J,3*K-2)=SM(J,3*K-2)*R(I,1)+SM(J,3*K-1)*R(I,4)+
     &               SM(J,3*K)*R(I,7)
        SMR(J,3*K-1)=SM(J,3*K-2)*R(I,2)+SM(J,3*K-1)*R(I,5)+
     &               SM(J,3*K)*R(I,8)
        SMR(J,3*K)=SM(J,3*K-2)*R(I,3)+SM(J,3*K-1)*R(I,6)+
     &               SM(J,3*K)*R(I,9)
133     CONTINUE
C       5.b.12
        IF (TS.LT.6) THEN
           WRITE(6,9109) I
           DO 134 J=1,6
           WRITE(6,9110) AML(I,J)+SMR(J,1)*DJ(J1)+
     &                   SMR(J,2)*DJ(J2)+SMR(J,3)*DJ(J3)+SMR(J,4)*DJ(K1)
     &                   +SMR(J,5)*DJ(K2)+SMR(J,6)*DJ(K3)
134        CONTINUE
C       550: L5
           GO TO 550
C       5.b.13
        ELSE
           WRITE(6,9109) I
           DO 135 J=1,12
           WRITE(6,9110) AML(I,J)+SMR(J,1)*DJ(J1)+
     &                   SMR(J,2)*DJ(J2)+SMR(J,3)*DJ(J3)+SMR(J,4)*DJ(J4)
     &                   +SMR(J,5)*DJ(J5)+SMR(J,6)*DJ(J6)+
     &                   SMR(J,7)*DJ(K1)+SMR(J,8)*DJ(K2)+SMR(J,9)*DJ(K3)
     &               +SMR(J,10)*DJ(K4)+SMR(J,11)*DJ(K5)+SMR(J,12)*DJ(K6)
135        CONTINUE
C       550: L5
           GO TO 550
        END IF
C       5.c.1 (L5C)
538     WRITE(6,9031)
        WRITE(6,9032)
C       5.c.2
        DO 136 K=N+1,N+NR
        AR(K)=-AC(K)
        DO 136 J=1,N
136     AR(K)=AR(K)+S(K,J)*D(J)
C       5.c.3
        K=N
C       5.c.4
        DO 137 KE=1,N+NR
        IF (RL(KE).EQ.1) THEN
           K=K+1
           AR(KE)=AR(K)
        ELSE
           AR(KE)=0.0
        END IF
137     CONTINUE
C       5.c.5
        IF (TS.LT.3) THEN
           DO 138 KE=2,2*NJ,2
           IF (RL(KE-1).EQ.1 .OR. RL(KE).EQ.1) THEN
              WRITE(6,9119) KE/2,AR(KE-1),AR(KE)
           END IF
138        CONTINUE
        ELSE IF (TS.LT.6) THEN
           DO 139 KE=3,3*NJ,3
           IF (RL(KE-2).EQ.1 .OR. RL(KE-1).EQ.1 .OR. RL(KE).EQ.1) THEN
              WRITE(6,9127) KE/3,AR(KE-2),AR(KE-1),AR(KE)
           END IF
139        CONTINUE
        ELSE
           DO 140 KE=6,6*NJ,6
           IF (RL(KE-5).EQ.1 .OR. RL(KE-4).EQ.1 .OR. RL(KE-3).EQ.1 .OR.
     &         RL(KE-2).EQ.1 .OR. RL(KE-1).EQ.1 .OR. RL(KE).EQ.1) THEN
               WRITE(6,9128) KE/6,AR(KE-5),AR(KE-4),AR(KE-3),
     &                       AR(KE-2),AR(KE-1),AR(KE)
           END IF
140        CONTINUE
        END IF
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ERROR IN THE LOOPING BLOCK CAUSING CRASH
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        NUMSTRUCT=NUMSTRUCT-1
C       5.c.6
        IF (LN.LT.NLS) THEN
C       521: L3
           GO TO 521
        ELSE
           IF (NUMSTRUCT.EQ.0) GO TO 552
C       552: END PROGRAM
C       499: L1
           GO TO 499
        END IF
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       5.c.7
551     IF (ERROR.EQ.1) WRITE(6,9050)
552     CONTINUE
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       END OF FLOW CHART
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        END PROGRAM FR1

C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINES
C       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        SUBROUTINE DECOMPOSE(N,A,IERR)
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
        IF (SUM.LE.0.) THEN
           ERROR=1
           RETURN
        END IF
        TEMP=1./SQRT(SUM)
        A(I,J)=TEMP
200     CONTINUE
        RETURN
        END
        
        SUBROUTINE SOLVE(N,U,B,X)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION U(100,100)
        DIMENSION X(100)
        DO 200 I=1,N
        SUM=B(I)
        DO 300 K=1,I-1
300     SUM=SUM-U(K,I)*X(K)
200     X(I)=SUM*U(I,I)
C       SECOND SET
        DO 400 I=N,1,-1
        SUM=X(I)
        DO 500 K=I+1,N
500     SUM=SUM-U(I,K)*X(K)
400     X(I)=SUM*U(I,I)
        RETURN
        END

        SUBROUTINE INVERT(N,U)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION U(100,100)
        DO 100 I=1,N
        DO 100 J=I+1,N
        SUM=0.
        DO 200 K=I,J-1
200     SUM=SUM-U(K,I)*U(K,J)
100     U(J,I)=SUM*U(J,J)
        DO 300 I=1,N
        DO 300 J=I,N
        SUM=0.
        DO 400 K=J,N
400     SUM=SUM+U(K,I)*U(K,J)
        U(J,I)=SUM
300     U(I,J)=U(J,I)
        RETURN
        END
        
        SUBROUTINE DECOMPOSEBAND(N,UBW,A)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION A(100,100)
C       SET P TO IP, AND Q TO IQ
        DO 100 I=1,N
        IP=N-I+1
        IF (UBW.LT.IP) IP=UBW
        DO 100 J=1,IP
        IQ=UBW-J
        IF (I-1.LT.IQ) IQ=I-1
        SUM=A(I,J)
        DO 200 K=1,IQ
200     SUM=SUM-A(I-K,1+K)*A(I-K,J+K)
        IF (J.NE.1) A(I,J)=SUM*TEMP
        IF (J.NE.1) GO TO 100
        IF (SUM.LE.0.) WRITE(6,400)
400     FORMAT('DECOMPOSE FAILS')
        IF (SUM.LE.0.) STOP
        TEMP=1./SQRT(SUM)
        A(I,J)=TEMP
100     CONTINUE
        RETURN
        END
        
        SUBROUTINE SOLVEBAND(N,UBW,U,B,X)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION X(100)
C       SET SECOND SET OF I,K TO II,KK
        DO 100 I=1,N
        J=I-UBW+1
        IF (I+1.LE.UBW) J=1
        SUM=B(I)
        DO 200 K=J,I-1
200     SUM=SUM-U(K,I-K+1)*X(K)
100     X(I)=SUM*U(I,1)
C       SECOND SET
        DO 300 I=N,1,-1
        J=I+UBW-1
        IF (J.GT.N) J=N
        SUM=X(I)
        DO 400 K=I+1,J
400     SUM=SUM-U(I,K-I+1)*X(K)
300     X(I)=SUM*U(I,1)
        RETURN
        END
        
        
        
