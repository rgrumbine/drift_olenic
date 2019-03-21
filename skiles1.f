C          DATA SET NSEAICE    AT LEVEL 069 AS OF 02/05/90              00000100
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***                             00000200
C                .      .    .                                       .  00000300
C MAIN PROGRAM:  NSEAICEV    NAVY SEA-ICE DRIFT PROGRAM                 00000400
C   PRGMMR: TOWNSHEND        ORG: NMC411      DATE: 90-02-05            00000500
C                                                                       00000600
C ABSTRACT: COMPUTE THE FREE-DRIFT OF SEA ICE FLOES AT 207 POINTS IN    00000700
C   THE ARCTIC, COMPOSE AND DISSEMINATE ICE DRIFT FORECASTS FOR 12 TO   00000800
C   144 HOUR FORECAST PERIODS IN BULLETIN FORM.  RECEIVED AT FAIRBANKS  00000900
C   AND ANCHORAGE IN ALASKA AND AT KEFLAVIK IN ICELAND.                 00001000
C                                                                       00001100
C PROGRAM HISTORY LOG:                                                  00001200
C   YY-MM-DD  TOWNSHEND   ORIGINAL AUTHOR                               00001300
C   85-11-20  FARLEY      ADDED CALLS TO W3LOG                          00001400
C   88-06-03  FARLEY      REPLACED XDAM I/O WITH VSAM I/O               00001500
C   89-12-13  TOWNSHEND   CHANGED FXN TO F96 AND ADDED                  00001600
C                         F108, F120, F132, AND F144                    00001700
C   90-02-02  TOWNSHEND   RESET THE FLAG TO SEND OUTPUT TO              00001800
C                         THE FT08 FILE                                 00001900
C                                                                       00002000
C USAGE:                                                                00002100
C   INPUT FILES:                                                        00002200
C     F00      -   00-HOUR SEA-LEVEL PRESSURE FORECAST                  00002300
C     F12      -   12-HOUR SEA-LEVEL PRESSURE FORECAST                  00002400
C     F24      -   24-HOUR SEA-LEVEL PRESSURE FORECAST                  00002500
C     F36      -   36-HOUR SEA-LEVEL PRESSURE FORECAST                  00002600
C     F48      -   48-HOUR SEA-LEVEL PRESSURE FORECAST                  00002700
C     F60      -   60-HOUR SEA-LEVEL PRESSURE FORECAST                  00002800
C     F72      -   72-HOUR SEA-LEVEL PRESSURE FORECAST                  00002900
C     F84      -   84-HOUR SEA-LEVEL PRESSURE FORECAST                  00003000
C     F96      -   96-HOUR SEA-LEVEL PRESSURE FORECAST                  00003100
C     F108     -  108-HOUR SEA-LEVEL PRESSURE FORECAST                  00003200
C     F120     -  120-HOUR SEA-LEVEL PRESSURE FORECAST                  00003300
C     F132     -  132-HOUR SEA-LEVEL PRESSURE FORECAST                  00003400
C     F144     -  144-HOUR SEA-LEVEL PRESSURE FORECAST                  00003500
C                                                                       00003600
C   OUTPUT FILES:                                                       00003700
C     FT08F001 -   PRINT OUT RESULTS                                    00003800
C     FT88F001 -   BULLETIN TRANSMISSION FILE                           00003900
C     FT06F001 -   PRINTOUT                                             00004000
C                                                                       00004100
C   SUBPROGRAMS CALLED:                                                 00004200
C     UNIQUE :  DCPL1,  DCPL2,  FC06,   FCST,   PRINT,                  00004300
C               WDIR                                                    00004400
C                                                                       00004500
C     LIBRARY:                                                          00004600
C       W3LIB  -  W3AI01, W3AI15, W3AI18, W3AI19, W3AG15,               00004700
C                 W3FK40, W3FK41, W3FK43, W3FQ02, W3FS06, W3LOG         00004800
C                                                                       00004900
C   EXIT STATES:                                                        00005000
C     COND =  0    NORMAL                                               00005100
C     COND = 30    W3FK03 ERROR RETURN (W3LOG MESSAGE)                  00005200
C     COND = 40    W3AG15 ABENDED, SEE FT06 FOR KRET FROM W3AG15        00005300
C                                                                       00005400
C REMARKS:  THE INPUT IS 'PE' SEA-LEVEL PRESSURE FIELDS FROM 0000Z      00005500
C   RUN-STREAM.  THE OUTPUT IS ONE BULLETIN PER DAY PER STATION.        00005600
C   THE OUTPUT FILE DATA SET REFERENCE NUMBER IS 88.  THE BULLETINS     00005700
C   ARE BLOCKED IN THE 1280-CHARACTER RECORDS REQUIRED FOR THE          00005800
C   360/195-360/40 INTERFACE.  THE FIRST 58 CHARACTERS CONSIST OF THE   00005900
C   40-CHARACTER HEADER DESCRIBED IN OFFICE NOTE 100 AND A BULLETIN     00006000
C   HEADING OF 18 CHARACTERS (TTAAII KWBC YYGG00).  AN ETX DELINEATES   00006100
C   THE EBCDIC COMPUTER-TO-COMPUTER MESSAGE.  THE GRID USED IN THIS     00006200
C   PROGRAM IS A 47 X 51 RECTANGULAR SUBSET OF THE 65 X 65 'PE' GRID.   00006300
C   207 POINTS SELECTED FROM THIS GRID OVER THE POLAR OCEANS ARE USED   00006400
C   FOR THE ACTUAL CALCULATIONS.                                        00006500
C                                                                       00006600
C ATTRIBUTES:                                                           00006700
C   LANGUAGE: FORTRAN 77                                                00006800
C   MACHINE:  NAS 9000                                                  00006900
C                                                                       00007000
C$$$                                                                    00007100
C                                                                       00007200
      REAL*8  FILE(13),KFILE                                            00007300
C                                                                       00007400
      REAL*4  FSLP(65,65)                                               00007500
      REAL*4  XDIFF(47,51),YDIFF(47,51),SLP(47,51)                      00007600
      REAL*4  DD(13,208),FF(13,208)                                     00007700
      REAL*4  U(13,207),V(13,207)                                       00007800
      REAL*4  WD(208),WF(208)                                           00007900
      REAL*4  WU(207),WV(207),XML(207)                                  00008000
C                                                                       00008100
      INTEGER*4  MSLP(2125)                                             00008200
      INTEGER*4  IDTBL(1539)                                            00008300
      INTEGER*4  IJSEL(414)                                             00008400
      INTEGER*4  LOCT(256)                                              00008500
      INTEGER*4  NDD(207),NFF(207)                                      00008600
      INTEGER*4  IJA(138),IJB(138),IJC(138)                             00008700
      INTEGER*4  NOUT(30)                                               00008800
      INTEGER*4  KTAU(13)                                               00008900
      INTEGER*4  MON(12),LAB(12)                                        00009000
      INTEGER*4  KHDR(7),KDDFF(7),KDAY(7)                               00009100
      INTEGER*4  IDENT(6)                                               00009200
      INTEGER*4  KEOF(3)                                                00009300
C                                                                       00009400
      EQUIVALENCE (IJA(1),IJSEL(1)), (IJB(1),IJSEL(139)),               00009500
     1                               (IJC(1),IJSEL(277))                00009600
C                                                                       00009700
      DATA WD/                                                          00009800
     A270., 20.,  0.,350.,300.,340.,280.,200.,320.,330.,320.,290.,270., 00009900
     B340., 20.,190.,  0.,240.,340.,  0.,  0.,  0., 30.,340.,  0.,330., 00010000
     C 30.,140., 90.,  0.,  0.,  0.,360.,360.,170.,  0., 20., 50.,  0., 00010100
     D  0.,  0.,  0.,330.,120., 40., 50.,  0.,  0.,  0.,  0.,340.,100., 00010200
     E  0., 40.,140.,  0., 70., 80.,  0.,  0.,  0.,  0., 30.,290.,  0., 00010300
     F100., 45., 33., 40., 50., 80.,  0.,  0.,360.,340.,290.,250.,160., 00010400
     G  0.,  0.,  0.,200.,120.,  0.,320.,340.,360., 45.,340., 20.,  0., 00010500
     H300.,260., 80.,170., 30.,170.,140.,110.,200.,240.,240.,330., 10., 00010600
     I 45.,180.,110.,210.,250.,  0.,270.,200., 90., 20.,310.,250.,110., 00010700
     J170.,200., 80.,150.,270.,  0., 20., 30.,260.,360., 10.,250.,270., 00010800
     K220., 45.,270.,210.,110., 25.,100.,  0., 20., 80.,360.,  0.,310., 00010900
     L  0., 80.,200., 10., 45.,210.,270.,220.,320.,340.,180.,220.,200., 00011000
     M240.,310.,  0.,  0., 30.,  0.,  0.,240.,270.,  0.,240.,160.,270., 00011100
     N270.,  0.,  0.,  0., 20., 80.,  0.,  0.,  0.,  0.,  0., 10.,180., 00011200
     O170., 80., 70.,  0.,  0.,360.,250.,  0.,340., 20.,360.,270.,345., 00011300
     P  0.,  0.,360.,270.,360., 40., 50.,  0.,360.,350.,300.,180.,  0./ 00011400
      DATA WF/                                                          00011500
     A 12., 14.,  0., 14., 14., 11.,  7., 13., 13., 20.,  7., 10.,  1., 00011600
     B  7.,  1.,  7.,  0.,  7., 22.,  0.,  0.,  0.,  4.,  7.,  0., 11., 00011700
     C 22., 22., 29.,  0.,  0.,  0.,  3., 22., 14.,  0., 22.,  7.,  0., 00011800
     D  0.,  0.,  0., 22.,  8., 22.,  7.,  0.,  0.,  0.,  0., 22., 11., 00011900
     E  0., 22., 10.,  0.,  4.,  3.,  0.,  0.,  0.,  0., 22.,  4.,  0., 00012000
     F  3.,  1., 18.,  3.,  3.,  3.,  0.,  0., 22.,  1.,  7.,  3.,  4., 00012100
     G  0.,  0.,  0.,  4.,  4.,  0.,  1.,  1.,  1.,  4., 11., 21.,  0., 00012200
     H  1.,  3.,  1.,  1.,  3., 11.,  4.,  4.,  3.,  1.,  1.,  1.,  1., 00012300
     I  3., 11.,  4., 13.,  7.,  0.,  1.,  1.,  1.,  1.,  6.,  3.,  3., 00012400
     J  1.,  1.,  1.,  1.,  1.,  0.,  7.,  7., 29.,  7.,  7.,  7.,  1., 00012500
     K  1.,  4.,  7.,  1.,  4.,  3.,  3.,  0.,  4.,  4.,  4.,  0., 22., 00012600
     L  0., 14.,  7.,  1.,  7.,  7.,  4.,  7.,  4.,  6.,  4.,  7., 11., 00012700
     M 11., 14.,  0.,  0.,  7.,  0.,  0.,  1.,  6.,  0., 11.,  4.,  1., 00012800
     N  4.,  0.,  0.,  0.,  7.,  1.,  0.,  0.,  0.,  0.,  0.,  4.,  4., 00012900
     O  3.,  1.,  1.,  0.,  0.,  3.,  1.,  0.,  3.,  3.,  3.,  1.,  1., 00013000
     P  0.,  0.,  3.,  1.,  1.,  3.,  3.,  0.,  1.,  3.,  3.,  3.,  0./ 00013100
      DATA IJA/                                                         00013200
     A  28,15,30,15,24,16,29,16,30,16,31,16,23,17,24,17,27,17,28,17,    00013300
     B  29,17,30,17,31,17,22,18,23,18,24,18,25,18,26,18,27,18,28,18,    00013400
     C  29,18,30,18,23,19,24,19,25,19,26,19,27,19,28,19,29,19,30,19,    00013500
     D  22,20,23,20,24,20,26,20,27,20,28,20,29,20,30,20,21,21,22,21,    00013600
     E  23,21,24,21,25,21,26,21,29,21,30,21,20,22,21,22,22,22,23,22,    00013700
     F  24,22,25,22,26,22,29,22,30,22,19,23,20,23,21,23,22,23,23,23,    00013800
     G  24,23,28,23,29,23,30,23,31,23,19,24,20,24,21,24,22,24/          00013900
      DATA IJB/                                                         00014000
     A  23,24,24,24,25,24,27,24,28,24,29,24,30,24,14,25,15,25,16,25,    00014100
     B  17,25,18,25,19,25,20,25,21,25,22,25,23,25,24,25,25,25,26,25,    00014200
     C  27,25,28,25,29,25,14,26,15,26,16,26,17,26,18,26,19,26,20,26,    00014300
     D  21,26,22,26,23,26,24,26,25,26,26,26,27,26,28,26,29,26,33,26,    00014400
     E  34,26,14,27,15,27,16,27,17,27,18,27,19,27,20,27,21,27,22,27,    00014500
     F  23,27,24,27,25,27,26,27,27,27,28,27,29,27,32,27,33,27,34,27,    00014600
     G  14,28,15,28,16,28,19,28,20,28,21,28,22,28,23,28,24,28/          00014700
      DATA IJC/                                                         00014800
     A  25,28,26,28,27,28,28,28,29,28,31,28,32,28,33,28,15,29,16,29,    00014900
     B  20,29,21,29,22,29,23,29,24,29,25,29,26,29,27,29,28,29,29,29,    00015000
     C  30,29,32,29,15,30,16,30,17,30,21,30,22,30,23,30,25,30,26,30,    00015100
     D  27,30,28,30,29,30,36,30,37,30,15,31,17,31,26,31,27,31,28,31,    00015200
     E  36,31,37,31,14,32,15,32,16,32,17,32,18,32,35,32,36,32,14,33,    00015300
     F  15,33,16,33,17,33,18,33,14,34,15,34,16,34,17,34,18,34,14,35,    00015400
     G  15,35,16,35,16,36,16,37,16,38,17,38,17,39,18,40,19,40/          00015500
C                                                                       00015600
      DATA KHDR/4HSURF,4HACE ,4HGEOS,4HTROP,4HHIC ,4HWIND,4HS   /       00015700
      DATA K19/2H19/, K00/4H0000/, KZ/1HZ/, KBLANK/1H /                 00015800
      DATA KDDFF/28,30,30,30,30,30,29/                                  00015900
      DATA FILE/8HF00     ,8HF12     ,8HF24     ,8HF36     ,8HF48       00016000
     1         ,8HF60     ,8HF72     ,8HF84     ,8HF96     ,8HF108      00016100
     2         ,8HF120    ,8HF132    ,8HF144    /                       00016200
      DATA KEOF/4HEND ,4HOF F,4HILE /                                   00016300
      DATA MXRS/255/, NSIZE/2125/                                       00016400
      DATA KTAU/00,12,24,36,48,60,72,84,96,108,120,132,144/             00016500
      DATA KDENT/Z'00808000'/                                             00016600
      DATA IDENT/Z'00808000',3*0,Z'1B',0/                                   00016700
      DATA IPOLE/24/, JPOLE/26/                                         00016800
      DATA MINUS/1H-/                                                   00016900
      DATA MTAU/13/                                                     00017000
      DATA MON/3HJAN, 3HFEB, 3HMAR, 3HAPR, 3HMAY, 3HJUN,                00017100
     1         3HJUL, 3HAUG, 3HSEP, 3HOCT, 3HNOV, 3HDEC/                00017200
C                                                                       00017300
C...................................................................... 00017400
C                                                                       00017500
      CALL W3LOG('$S90036.55','NSEAICEV')                               00017600
       CALL W3TAGB('NSEAICEV',0090,0036,0055,'NMC411 ')                 00017700
      LL=120                                                            00017800
      IFIRST=0                                                          00017900
      WD(208)=0.                                                        00018000
      WF(208)=0.                                                        00018100
      WT1=1.                                                            00018200
      WT2=1.                                                            00018300
      LHR=0                                                             00018400
      LDY=0                                                             00018500
      MO = 0                                                            00018600
      PRTFLG = 1.                                                       00018700
C                                                                       00018800
C     TO GET OUTPUT OF FORECASTS TO PRINTER, SET PRTFLG = 1.            00018900
C                                                                       00019000
      CALL DCPL1(WD,WF,WU,WV)                                           00019100
      DO 100 NTAU=1,MTAU                                                00019200
      IF (NTAU.GT.13) GO TO 30                                          00019300
      KFILE=FILE(NTAU)                                                  00019400
      CALL W3FK40(KFILE,LOCT ,MXRS)                                     00019500
      CALL W3FK41(KFILE,IDTBL,MXRS)                                     00019600
      IF (IFIRST.NE.0) GO TO 30                                         00019700
      KRONOS = IDTBL(3)                                                 00019800
      IYR = KRONOS / 16777216                                           00019900
      IMO = KRONOS / 65536 - 256 * IYR                                  00020000
      IDY = KRONOS / 256 - 65536 * IYR - 256 * IMO                      00020100
      IHR = KRONOS - 16777216 * IYR - 65536 * IMO - 256 * IDY           00020200
      CALL W3AI15(IHR,ITIME,1,2,MINUS)                                  00020300
      CALL W3AI15(IYR,IYEAR,1,2,MINUS)                                  00020400
      CALL W3AI15(IMO,IMNTH,1,2,MINUS)                                  00020500
      CALL W3AI15(IDY, IDAY,1,2,MINUS)                                  00020600
      IMON=MON(IMO)                                                     00020700
      MO=IMO                                                            00020800
      LYR=IYR+1900                                                      00020900
      LHR=IHR                                                           00021000
      LDY=IDY                                                           00021100
      M=1                                                               00021200
      CALL W3FS06(IYR,IMO,IDY,KDAYWK,M)                                 00021300
      N=0                                                               00021400
      CALL W3AI18(KHDR  ,1,28,NOUT,LL,3,N)                              00021500
      CALL W3AI18(KDAYWK,1, 3,NOUT,LL,0,N)                              00021600
      CALL W3AI18(K00   ,1, 4,NOUT,LL,3,N)                              00021700
      CALL W3AI18(KZ    ,1, 1,NOUT,LL,0,N)                              00021800
      CALL W3AI18(IDAY  ,1, 2,NOUT,LL,1,N)                              00021900
      CALL W3AI18(IMON  ,1, 3,NOUT,LL,1,N)                              00022000
      CALL W3AI18(K19   ,1, 2,NOUT,LL,1,N)                              00022100
      CALL W3AI18(IYEAR ,1, 2,NOUT,LL,0,N)                              00022200
      CALL W3AI18(KBLANK,1, 4,NOUT,LL,7,N)                              00022300
C                                                                       00022400
C     WRITE (6,74) (NOUT(L),L=1,30)                                     00022500
C SIMULATE END OF FILE ON THE TAPE.                                     00022600
C                                                                       00022700
      N=0                                                               00022800
      CALL W3AI18(KEOF   ,1,12,NOUT,LL,0,N)                             00022900
C                                                                       00023000
C     WRITE (6,74) (NOUT(L),L=1,30)                                     00023100
C FORM THE MAP FACTOR IN 'XMF' FOR THE SELECTED POINTS.                 00023200
C                                                                       00023300
      K=0                                                               00023400
      DO 20  M=1,414,2                                                  00023500
      K=K+1                                                             00023600
      I=IJSEL(M)                                                        00023700
      J=IJSEL(M+1)                                                      00023800
      FID=IPOLE-I                                                       00023900
      FJD=JPOLE-J                                                       00024000
      RSQ=FID**2 + FJD**2                                               00024100
      SINL=(973.71 - RSQ)/(973.71 + RSQ)                                00024200
      XML(K)=1.35985/SINL                                               00024300
   20 CONTINUE                                                          00024400
C                                                                       00024500
C READ THE MSL PRESSURE FLD.                                            00024600
C                                                                       00024700
   30 CONTINUE                                                          00024800
      IDENT(1)=KDENT + KTAU(NTAU)                                       00024900
      CALL W3FK43(KFILE,IDTBL,IDENT,MSLP,MXRS,NSIZE,IERR)               00025000
      IF (IERR.EQ.0)  GO TO 38                                          00025100
C                                                                       00025200
      WRITE (6,34) IERR,KFILE                                           00025300
   34 FORMAT ('  IERR = ',I2,'  ERROR IN PROCESSING ',A8,' FILE.')      00025400
      CALL W3LOG('$E',30,' W3FK43 ERROR RETURN:')                       00025500
       CALL W3TAGE('NSEAICEV')                                          00025600
      STOP 30                                                           00025700
C                                                                       00025800
   38 CONTINUE                                                          00025900
      CALL W3AI01(MSLP,FSLP,LAB)                                        00026000
C                                                                       00026100
C     EXTRACT 47 X 51 SUBSET FROM 65 X 65 ARRAY.                        00026200
C                                                                       00026300
      DO 50 JJ=1,51                                                     00026400
      DO 40 II=1,47                                                     00026500
      SLP(II,JJ)=FSLP(II+9,JJ+7)                                        00026600
   40 CONTINUE                                                          00026700
   50 CONTINUE                                                          00026800
      CALL FC06(SLP,FSLP,XDIFF,YDIFF,47,51)                             00026900
      K=0                                                               00027000
      DO 60  M=1,414,2                                                  00027100
      K=K+1                                                             00027200
      I = IJSEL(M)                                                      00027300
      J = IJSEL(M+1)                                                    00027400
      FID=IPOLE-I                                                       00027500
      FJD=JPOLE-J                                                       00027600
      UG=-XML(K) * YDIFF(I,J)                                           00027700
      VG= XML(K) * XDIFF(I,J)                                           00027800
      CALL W3FC02(FID,FJD,UG,VG,D,F)                                    00027900
      DD(NTAU,K)=D                                                      00028000
      FF(NTAU,K)=F                                                      00028100
      NDD(K)=D/10. + 0.5                                                00028200
      NFF(K)=F  +0.5                                                    00028300
      IF (NFF(K).GT.99)  NFF(K) = 99                                    00028400
   60 CONTINUE                                                          00028500
      CALL W3AI15(KTAU(NTAU),ITAU,1,2,MINUS)                            00028600
      N=0                                                               00028700
      CALL W3AI18(ITAU  ,1, 2,NOUT,LL,0,N)                              00028800
      CALL W3AI18(ITIME ,1, 2,NOUT,LL,0,N)                              00028900
      CALL W3AI18(IDAY  ,1, 2,NOUT,LL,0,N)                              00029000
      CALL W3AI18(IMNTH ,1, 2,NOUT,LL,0,N)                              00029100
      KA=1                                                              00029200
      KB=0                                                              00029300
      DO 79  II=1,7                                                     00029400
      KB=KB+KDDFF(II)                                                   00029500
      DO 72  K=KA,KB                                                    00029600
      CALL W3AI15(NDD(K),NDD(K),1,2,MINUS)                              00029700
      CALL W3AI15(NFF(K),NFF(K),1,2,MINUS)                              00029800
      CALL W3AI18(NDD(K),1,2,NOUT,LL,0,N)                               00029900
      CALL W3AI18(NFF(K),1,2,NOUT,LL,0,N)                               00030000
   72 CONTINUE                                                          00030100
      IF (II .EQ. 7)  CALL W3AI18(K00,1,4,NOUT,LL,0,N)                  00030200
      N=0                                                               00030300
      KA=KB+1                                                           00030400
C                                                                       00030500
C     WRITE (6,74) (NOUT(L),L=1,30)                                     00030600
C                                                                       00030700
   74 FORMAT(1X,30A4)                                                   00030800
   79 CONTINUE                                                          00030900
C                                                                       00031000
C     SIMULATE END-OF-FILE ON TAPE.                                     00031100
C                                                                       00031200
      N=0                                                               00031300
      CALL W3AI18(KEOF   ,1,12,NOUT,LL,0,N)                             00031400
C                                                                       00031500
C     WRITE (6,74) (NOUT(L),L=1,30)                                     00031600
C                                                                       00031700
      IFIRST=IFIRST+1                                                   00031800
      CALL W3FK49(KFILE)                                                00031900
  100 CONTINUE                                                          00032000
      CALL DCPL2(DD,FF,U,V)                                             00032100
      CALL FCST(U,V,WU,WV,DD,FF,MO,LDY,LYR,WT1,WT2,PRTFLG)              00032200
C                                                                       00032300
      REWIND 88                                                         00032400
      CALL W3AG15('FT88F001','TRAN    ',KRET)                           00032500
      IF (KRET .EQ. 0) GO TO 200                                        00032600
        WRITE (6,110) KRET                                              00032700
  110   FORMAT ('  W3AG15 ENDED WITH RETURN CODE = ',I4)                00032800
        CALL W3LOG('$E',40,'W3AG15 ABENDED:')                           00032900
       CALL W3TAGE('NSEAICEV')                                          00033000
        STOP 40                                                         00033100
C                                                                       00033200
  200 CALL W3LOG('$E')                                                  00033300
       CALL W3TAGE('NSEAICEV')                                          00033400
      STOP                                                              00033500
      END                                                               00033600
      SUBROUTINE FC06(B,W,BX,BY,LI,LJ)                                  00033700
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    00033800
C                .      .    .                                          00033900
C SUBPROGRAM:    FC06        DB/DX AND DB/DY FROM 'B'                   00034000
C   PRGMMR: TOWNSHEND        ORG: W/NMC411   DATE: 89-09-27             00034100
C                                                                       00034200
C ABSTRACT: FORM DB/DX IN 'BX' AND 'DB/DY IN 'BY' FROM ROUGHENED        00034300
C   FIELD 'B'.                                                          00034400
C                                                                       00034500
C PROGRAM HISTORY LOG:                                                  00034600
C   89-09-27  TOWNSHEND                                                 00034700
C                                                                       00034800
C USAGE:    CALL FC06(B,W,BX,BY,LI,LJ)                                  00034900
C   INPUT ARGUMENT LIST:                                                00035000
C     B    - 47 X 51 SUBSET OF 'W' FIELD                                00035100
C     W    - 65 X 65 WORK FIELD                                         00035200
C     LI   - NUMBER OF COLUMNS IN SUBSET                                00035300
C     LJ   - NUMBER OF ROWS IN SUBSET                                   00035400
C                                                                       00035500
C   OUTPUT ARGUMENT LIST:                                               00035600
C     BX   - SHUMAN DERIVATIVE IN 'X'                                   00035700
C     BY   - SHUMAN DERIVATIVE IN 'Y'                                   00035800
C                                                                       00035900
C REMARKS: FORM DB/DX IN 'BX' AND DB/DY IN 'BY' FROM ROUGHENED 'B'      00036000
C   FIELD.  GENERAL FOR RECTANGLE DIMENSIONED 'LI' BY 'LJ'              00036100
C   B = INPUT      W = WORK    X = X-DERIVATIVE     Y = Y-DERIVATIVE    00036200
C                                                                       00036300
C   LANGUAGE: FORTRAN 77                                                00036400
C   MACHINE:  NAS                                                       00036500
C                                                                       00036600
C$$$                                                                    00036700
C                                                                       00036800
      DIMENSION B(LI,LJ), W(LI,LJ), BX(LI,LJ), BY(LI,LJ)                00036900
C                                                                       00037000
      LI1=LI-1                                                          00037100
      LJ1=LJ-1                                                          00037200
      D0 = 16./9.                                                       00037300
      D1 =        2./9.                                                 00037400
      D2 =              1./36.                                          00037500
      C0 = 2./3.                                                        00037600
      C1 =        1./6.                                                 00037700
      DO 32 J = 1,LJ                                                    00037800
      DO 31 I = 1,LI                                                    00037900
      W(I,J)= B(I,J)                                                    00038000
      BX(I,J) =0.                                                       00038100
      BY(I,J)= 0.                                                       00038200
   31 CONTINUE                                                          00038300
   32 CONTINUE                                                          00038400
C                                                                       00038500
C  ROUGHEN INTERIOR OF B AND STORE IN W.                                00038600
C                                                                       00038700
      DO 36 J=2,LJ1                                                     00038800
      DO 35 I=2,LI1                                                     00038900
      W(I,J)=D0*(B(I,J))-D1*(B(I+1,J)+B(I-1,J)+B(I,J+1)+B(I,J-1))       00039000
     1        +D2*(B(I+1,J+1)+B(I-1,J+1)+B(I-1,J-1)+B(I+1,J-1))         00039100
   35 CONTINUE                                                          00039200
   36 CONTINUE                                                          00039300
C                                                                       00039400
C  FORM SHUMAN TYPE DERIVATIVES.                                        00039500
C                                                                       00039600
      DO 38 J=2,LJ1                                                     00039700
      DO 37 I=2,LI1                                                     00039800
      BX(I,J)=C1*(W(I+1,J+1)-W(I-1,J-1)+W(I+1,J-1)-W(I-1,J+1))          00039900
     1       +C0*(W(I+1,J)-W(I-1,J))                                    00040000
      BY(I,J)=C1*(W(I+1,J+1)-W(I-1,J-1)-W(I+1,J-1)+W(I-1,J+1))          00040100
     1       +C0*(W(I,J+1)-W(I,J-1))                                    00040200
   37 CONTINUE                                                          00040300
   38 CONTINUE                                                          00040400
      RETURN                                                            00040500
      END                                                               00040600
      SUBROUTINE DCPL1(D,F,U,V)                                         00040700
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    00040800
C                .      .    .                                          00040900
C SUBPROGRAM:    DCPL1       U- AND V-COMPONENT OF ICE DRIFT            00041000
C   PRGMMR: TOWNSHEND        ORG: W/NMC411   DATE: 89-09-27             00041100
C                                                                       00041200
C ABSTRACT: CALCULATE ZONAL (U) AND MERIDIONAL (V) COMPONENTS           00041300
C   OF ICE DRIFT.                                                       00041400
C                                                                       00041500
C PROGRAM HISTORY LOG:                                                  00041600
C   89-09-27  TOWNSHEND   ORIGINAL AUTHOR                               00041700
C                                                                       00041800
C USAGE:    CALL DCPL1(D,F,U,V)                                         00041900
C   INPUT ARGUMENT LIST:                                                00042000
C     D    - DRIFT DIRECTION                                            00042100
C     F    - DRIFT SPEED                                                00042200
C                                                                       00042300
C   OUTPUT ARGUMENT LIST:                                               00042400
C     U    - ZONAL COMPONENT                                            00042500
C     V    - MERIDIONAL COMPONENT                                       00042600
C                                                                       00042700
C   LANGUAGE: FORTRAN 77                                                00042800
C   MACHINE:  NAS 9000                                                  00042900
C                                                                       00043000
C$$$                                                                    00043100
C                                                                       00043200
      REAL*4  D(208),F(208)                                             00043300
      REAL*4  U(207),V(207)                                             00043400
C                                                                       00043500
      DATA  RADIAN/57.29578/                                            00043600
C                                                                       00043700
      DO 10 I=1,207                                                     00043800
      CELL=270.-D(I)                                                    00043900
      IF (CELL.LT.0.) CELL=CELL+360.                                    00044000
      A=CELL/RADIAN                                                     00044100
      U(I)=F(I)*COS(A)                                                  00044200
      V(I)=F(I)*SIN(A)                                                  00044300
   10 CONTINUE                                                          00044400
      RETURN                                                            00044500
      END                                                               00044600
      SUBROUTINE DCPL2(D,F,U,V)                                         00044700
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    00044800
C                .      .    .                                          00044900
C SUBPROGRAM:    DCPL2       'U' AND 'V' COMPONENTS OF ICE DRIFT        00045000
C   PRGMMR: TOWNSHEND        ORG: W/NMC411   DATE: 89-09-27             00045100
C                                                                       00045200
C                                                                       00045300
C ABSTRACT: CALCULATE 'U' AND 'V' COMPONENTS OF ICE DRIFT.              00045400
C                                                                       00045500
C PROGRAM HISTORY LOG:                                                  00045600
C   89-09-27  TOWNSHEND   ORIGINAL AUTHOR                               00045700
C                                                                       00045800
C USAGE:    CALL DCPL2(D,F,U,V)                                         00045900
C   INPUT ARGUMENT LIST:                                                00046000
C     D    - DRIFT DIRECTION                                            00046100
C     F    - DRIFT SPEED                                                00046200
C                                                                       00046300
C   OUTPUT ARGUMENT LIST:                                               00046400
C     U    - ZONAL (E-W) COMPONENT OF ICE DRIFT                         00046500
C     V    - MERIDIONAL (N-S) COMPONENT OF ICE DRIFT                    00046600
C                                                                       00046700
C   LANGUAGE: FORTRAN                                                   00046800
C   MACHINE:  NAS 9000                                                  00046900
C                                                                       00047000
C$$$                                                                    00047100
C                                                                       00047200
      REAL*4  D(13,208),F(13,208)                                       00047300
      REAL*4  U(13,207),V(13,207)                                       00047400
C                                                                       00047500
      DATA  RADIAN/57.29578/                                            00047600
C                                                                       00047700
      DO 20 I=1,13                                                      00047800
      DO 18 J=1,207                                                     00047900
      CELL=270.-D(I,J)                                                  00048000
      IF (CELL.LT.0.) CELL=CELL+360.                                    00048100
      A=CELL/RADIAN                                                     00048200
      U(I,J)=F(I,J)*COS(A)                                              00048300
      V(I,J)=F(I,J)*SIN(A)                                              00048400
   18 CONTINUE                                                          00048500
   20 CONTINUE                                                          00048600
      RETURN                                                            00048700
      END                                                               00048800
      SUBROUTINE WDIR(U,V,D)                                            00048900
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    00049000
C                .      .    .                                          00049100
C SUBPROGRAM:    WDIR        MEAN ICE-DRIFT VECTOR DIRECTION            00049200
C   PRGMMR: TOWNSHEND        ORG: W/NMC411   DATE: 89-09-27             00049300
C                                                                       00049400
C ABSTRACT: CALCULATE DIRECTION OF MEAN ICE-DRIFT VECTOR.               00049500
C                                                                       00049600
C PROGRAM HISTORY LOG:                                                  00049700
C   89-09-27  TOWNSHEND   ORIGINAL AUTHOR                               00049800
C                                                                       00049900
C USACE:    CALL WDIR(U,V,D)                                            00050000
C   INPUT ARGUMENT LIST:                                                00050100
C     U    - E-W COMPONENT OF MEAN ICE-DRIFT VECTOR                     00050200
C     V    - N-S COMPONENT OF MEAN ICE-DRIFT VECTOR                     00050300
C                                                                       00050400
C   OUTPUT ARGUMENT LIST:                                               00050500
C     D    - DIRECTION (DEGREES) OF MEAN ICE-DRIFT VECTOR               00050600
C                                                                       00050700
C   LANGUAGE: FORTRAN 77                                                00050800
C   MACHINE:  NAS 9000                                                  00050900
C                                                                       00051000
C$$$                                                                    00051100
C                                                                       00051200
      REAL*4  D(13,208),U(13,207),V(13,207)                             00051300
C                                                                       00051400
      DATA  RADIAN/57.29578/                                            00051500
C                                                                       00051600
      DO 75 I=1,12                                                      00051700
      DO 70 J=1,207                                                     00051800
      IF (U(I,J).EQ.0.) GO TO 60                                        00051900
      IF (U(I,J).LT.0.) GO TO 40                                        00052000
      IF (V(I,J).LT.0.) GO TO 30                                        00052100
      R=V(I,J)/U(I,J)                                                   00052200
      D(I,J)=270.-ATAN(R)*RADIAN                                        00052300
      GO TO 70                                                          00052400
C                                                                       00052500
C     NW QUAD                                                           00052600
C                                                                       00052700
   30 CONTINUE                                                          00052800
      R=V(I,J)/U(I,J)                                                   00052900
      D(I,J)=270.-ATAN(R)*RADIAN                                        00053000
      GO TO 70                                                          00053100
C                                                                       00053200
C     NE QUAD                                                           00053300
C                                                                       00053400
   40 CONTINUE                                                          00053500
      IF (V(I,J).GT.0.) GO TO 50                                        00053600
      R=V(I,J)/U(I,J)                                                   00053700
      D(I,J)=90.-ATAN(R)*RADIAN                                         00053800
      GO TO 70                                                          00053900
C                                                                       00054000
C     SE QUAD                                                           00054100
C                                                                       00054200
   50 CONTINUE                                                          00054300
      R=V(I,J)/U(I,J)                                                   00054400
      D(I,J)=90.-ATAN(R)*RADIAN                                         00054500
      GO TO 70                                                          00054600
   60 CONTINUE                                                          00054700
      IF (V(I,J).GT.0.) D(I,J)=180.                                     00054800
      IF (V(I,J).LT.0.) D(I,J)=0.                                       00054900
   70 CONTINUE                                                          00055000
   75 CONTINUE                                                          00055100
      RETURN                                                            00055200
      END                                                               00055300
      SUBROUTINE FCST(U,V,W1,W2,D,F,MO,LDY,LYR,WT1,WT2,PRTFLG)          00055400
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    00055500
C                .      .    .                                          00055600
C SUBPROGRAM:    FCST        MEAN U-V COMPS. 12 THRU 144 HRS.           00055700
C   PRGMMR: TOWNSHEND        ORG: W/NMC411   DATE: 89-09-27             00055800
C                                                                       00055900
C ABSTRACT: CALCULATE MEAN 'U' AND 'V' COMPONENTS AT 12, 24, THRU       00056000
C   144 HOURS.                                                          00056100
C                                                                       00056200
C PROGRAM HISTORY LOG:                                                  00056300
C   89-09-27  TOWNSHEND   ORIGINAL AUTHOR                               00056400
C                                                                       00056500
C USAGE:    CALL FCST(U,V,W1,W2,D,F,MO,LDY,LYR,WT1,WT2,PRTFLG)          00056600
C   INPUT ARGUMENT LIST:                                                00056700
C     D      - DIRECTION OF FORECAST RAW ICE-DRIFT                      00056800
C     F      - SPEED OF FORECAST RAW ICE-DRIFT                          00056900
C     MO     - MONTH OF YEAR                                            00057000
C     LDY    - DAY OF MONTH                                             00057100
C     LYR    - LAST TWO DIGITS OF YEAR                                  00057200
C     WT1    - SET TO 1.                                                00057300
C     WT2    - SET TO 1.                                                00057400
C     PRTFLG - PRINT FLAG                                               00057500
C                                                                       00057600
C   OUTPUT ARGUMENT LIST:                                               00057700
C     U      - U-COMPONENT OF MEAN ICE-DRIFT VECTOR                     00057800
C     V      - V-COMPONENT OF MEAN ICE-DRIFT VECTOR                     00057900
C     W1     - U-COMPONENT OF CURRENT DRIFT                             00058000
C     W2     - V-COMPONENT OF CURRENT DRIFT                             00058100
C                                                                       00058200
C REMARKS: USE UBAR AND VBAR TO CALCULATE MEAN ICE-DRIFT VECTORS.       00058300
C                                                                       00058400
C   LANGUAGE: FORTRAN 77                                                00058500
C   MACHINE:  NAS 9000                                                  00058600
C                                                                       00058700
C$$$                                                                    00058800
C                                                                       00058900
      REAL*4  D(13,208),F(13,208)                                       00059000
      REAL*4  UBAR(13,207),VBAR(13,207)                                 00059100
      REAL*4  U(13,207),V(13,207)                                       00059200
      REAL*4  W1(207),W2(207)                                           00059300
C                                                                       00059400
      DO 10 J=1,207                                                     00059500
      UBAR(1,J)=(U(1,J)+U(2,J))/2.                                      00059600
      VBAR(1,J)=(V(1,J)+V(2,J))/2.                                      00059700
   10 CONTINUE                                                          00059800
      DO 20 I=2,12                                                      00059900
      DO 15 J=1,207                                                     00060000
      UBAR(I,J)=UBAR(I-1,J)*(I*1.)/((I+1)*1.)+U(I+1,J)/((I+1)*1.)       00060100
      VBAR(I,J)=VBAR(I-1,J)*(I*1.)/((I+1)*1.)+V(I+1,J)/((I+1)*1.)       00060200
   15 CONTINUE                                                          00060300
   20 CONTINUE                                                          00060400
      CALL WDIR(UBAR,VBAR,D)                                            00060500
      DO 50 I=1,12                                                      00060600
      DO 40 J=1,207                                                     00060700
      UNIT=UBAR(I,J)**2+VBAR(I,J)**2                                    00060800
      IF (UNIT.EQ.0.) GO TO 33                                          00060900
      F(I,J)=SQRT(UNIT)                                                 00061000
      GO TO 35                                                          00061100
   33 CONTINUE                                                          00061200
      F(I,J)=0.                                                         00061300
   35 CONTINUE                                                          00061400
      X=-0.168*F(I,J)                                                   00061500
      D(I,J)=D(I,J)+31.3*EXP(X)                                         00061600
      IF (D(I,J).GT.360.) D(I,J)=D(I,J)-360.                            00061700
      F(I,J)=0.025+0.00772*F(I,J)                                       00061800
   40 CONTINUE                                                          00061900
   50 CONTINUE                                                          00062000
C                                                                       00062100
C     GET U,V ICE DRIFT COMPONENTS IN NM/HR AND EXTRAPOLATE.            00062200
C                                                                       00062300
      CALL DCPL2(D,F,U,V)                                               00062400
      DO 46 I=1,12                                                      00062500
      DO 44 J=1,207                                                     00062600
      U(I,J)=U(I,J)*(I*12.)                                             00062700
      V(I,J)=V(I,J)*(I*12.)                                             00062800
      CELL=U(I,J)**2+V(I,J)**2                                          00062900
      IF (CELL.EQ.0.) GO TO 42                                          00063000
      F(I,J)=SQRT(CELL)                                                 00063100
      GO TO 44                                                          00063200
   42 CONTINUE                                                          00063300
      F(I,J)=0.                                                         00063400
   44 CONTINUE                                                          00063500
   46 CONTINUE                                                          00063600
      FLAG=0.                                                           00063700
      CALL PRINT(D,F,MO,LDY,LYR,FLAG,WT1,WT2,PRTFLG)                    00063800
C                                                                       00063900
C     ADD U,V ICE DRIFT, CURRENT DRIFT COMPS.                           00064000
C                                                                       00064100
      DO 65 I=1,12                                                      00064200
      DO 60 J=1,207                                                     00064300
      U(I,J)=U(I,J)*WT1 + W1(J)*WT2/(13. - I*1.)                        00064400
      V(I,J)=V(I,J)*WT1 + W2(J)*WT2/(13. - I*1.)                        00064500
   60 CONTINUE                                                          00064600
   65 CONTINUE                                                          00064700
      CALL WDIR(U,V,D)                                                  00064800
      DO 75 I=1,12                                                      00064900
      DO 74 J=1,207                                                     00065000
      UNIT = U(I,J) * U(I,J) + V(I,J) * V(I,J)                          00065100
      IF (UNIT.EQ.0.) GO TO 72                                          00065200
      F(I,J)=SQRT(UNIT)                                                 00065300
      GO TO 74                                                          00065400
   72 CONTINUE                                                          00065500
      F(I,J)=0.                                                         00065600
   74 CONTINUE                                                          00065700
   75 CONTINUE                                                          00065800
      FLAG=1.                                                           00065900
      CALL PRINT(D,F,MO,LDY,LYR,FLAG,WT1,WT2,PRTFLG)                    00066000
      RETURN                                                            00066100
      END                                                               00066200
      SUBROUTINE PRINT(DD,FF,MO,LDY,LYR,FLAG,WT1,WT2,PRTFLG)            00066300
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    00066400
C                .      .    .                                          00066500
C SUBPROGRAM:    PRINT       MESSAGES IN BULLETIN FORM                  00066600
C   PRGMMR: TOWNSHEND        ORG: W/NMC411   DATE: 89-09-27             00066700
C                                                                       00066800
C ABSTRACT: BUILDS AND BLOCKS MESSAGES IN FORM SUITABLE FOR             00066900
C   TELETYPE TRANSMISSION.                                              00067000
C                                                                       00067100
C PROGRAM HISTORY LOG:                                                  00067200
C   89-09-27  TOWNSHEND   ORIGINAL AUTHOR                               00067300
C                                                                       00067400
C USAGE:    CALL PRINT(DD,FF,MO,LDY,LYR,FLAG,WT1,WT2,PRTFLG)            00067500
C   INPUT ARGUMENT LIST:                                                00067600
C     DD     - DIRECTION OF MEAN ICE-DRIFT VECTOR                       00067700
C     FF     - SPEED OF MEAN ICE-DRIFT VECTOR                           00067800
C     MO     - MONTH OF YEAR                                            00067900
C     LDY    - DAY OF MONTH                                             00068000
C     LYR    - LAST TWO DIGITS OF YEAR                                  00068100
C     FLAG   - IF (FLAG=1) PRINT MESSAGES; OTHERWISE SKIP               00068200
C     WT1    - SET TO 1.                                                00068300
C     WT2    - SET TO 1.                                                00068400
C     PRTFLG - PRINT FLAG                                               00068500
C                                                                       00068600
C REMARKS: THIS OUTPUT ROUTINE BUILDS AND BLOCKS MESSAGES.  ROUTINE     00068700
C   TESTS FOR LEAP YEARS, ENDS OF MONTH, AND ENDS OF YEAR, SUPPLYING    00068800
C   THE CORRECT DATES UNTIL 1 JANUARY 2000.  A VARIABLE LINE LENGTH     00068900
C   IS USED TO BUILD LINES, MAKING IT EASIER TO ADD AND DELETE LINES    00069000
C   OF BULLETIN TEXT WITHOUT CHANGING THE LENGTH OF REMAINING LINES.    00069100
C                                                                       00069200
C   LANGUAGE: FORTRAN 77                                                00069300
C   MACHINE:  NAS 9000                                                  00069400
C                                                                       00069500
C$$$                                                                    00069600
C                                                                       00069700
      REAL*4  DD(13,208),FF(13,208)                                     00069800
C                                                                       00069900
      INTEGER*4  MES(780)                                               00070000
      INTEGER*4  NBLK(320)                                              00070100
      INTEGER*4  NDD(207),NFF(207)                                      00070200
      INTEGER*4  IRAY(36),ISEL(36)                                      00070300
      INTEGER*4  KEF(18),NK(18)                                         00070400
      INTEGER*4  LINE(16)                                               00070500
      INTEGER*4  KHDRA(13),KHDRK(13)                                    00070600
      INTEGER*4  ID(12),NM(12),KDUM(12)                                 00070700
      INTEGER*4  KFHDR3(10),KFHDR7(10)                                  00070800
      INTEGER*4  KHDR2(8),KHDR7(8),ITIME(8)                             00070900
      INTEGER*4  KHDR6(3)                                               00071000
C                                                                       00071100
      DATA NK / 29, 37, 45, 54, 56, 63, 64, 65, 73,                     00071200
     1          74, 75, 76, 90, 91, 92,106,107,108/                     00071300
      DATA NM / 4HJAN ,4HFEB ,4HMAR ,4HAPR ,4HMAY ,4HJUN ,              00071400
     1          4HJUL ,4HAUG ,4HSEP ,4HOCT ,4HNOV ,4HDEC /              00071500
      DATA ID / 31,28,31,30,31,30,31,31,30,31,30,31 /                   00071600
      DATA KDUM/4H1.  ,4H24  ,4H2.  ,4H48  ,4H3.  ,4H72  ,              00071700
     1          4H4.  ,4H96  ,4H5. 1,4H20  ,4H6. 1,4H44  /              00071800
      DATA IEOL/4H<<@ /,IETX/4H%   /                                    00071900
      DATA IETB/4H>   /,IFILL/4H~~~ /                                   00072000
      DATA KHDR2  / 4HFM N,4HAVPO,4HLARO,4HCEAN,                        00072100
     1              4HCEN ,4HSUIT,4HLAND,4H MD /                        00072200
      DATA KFHDR3 / 4HTO R,4HUDIS,4HDI/N,4HAVOC,4HEANC,                 00072300
     1              4HOMFA,4HC KE,4HFLAV,4HIK I,4HC   /                 00072400
      DATA KFHDR7/4H1.  ,4HICE ,4HVECT,4HOR A,4HNALY,                   00072500
     1            4HSIS ,4HPERI,4HOD 0,4H0-12,4HZ   /                   00072600
      DATA KHDR6/4HICE ,4HVECT,4HORS /                                  00072700
      DATA KHDR7/4HHR I,4HCE V,4HECTO,4HR FO,                           00072800
     1           4HRECA,4HST V,4HT  0,4H0Z  /                           00072900
      DATA KHDRA/4H'104,4H922 ,8*4H    ,4HFQGA,4H40 K,4HWBC /           00073000
      DATA KHDRK/4H'104,4H923 ,8*4H    ,4HFQAC,4H1 KW,4HBC  /           00073100
      DATA IRAY  / 4H57. ,4H62. ,4H63. ,4H66. ,4H67. ,4H68. ,           00073200
     A             4H77. ,4H78. ,4H79. ,4H80. ,4H81. ,4H82. ,           00073300
     B             4H83. ,4H84. ,4H93. ,4H94. ,4H95. ,4H96. ,           00073400
     C             4H97. ,4H98. ,4H99. ,4H100.,4H111.,4H112.,           00073500
     D             4H113.,4H114.,4H115.,4H116.,4H117.,4H118.,           00073600
     E             4H130.,4H131.,4H132.,4H133.,4H134.,4H135. /          00073700
      DATA KEF/4H29. ,4H37. ,4H45. ,4H54. ,4H55. ,4H63. ,               00073800
     1         4H64. ,4H65. ,4H73. ,4H74. ,4H75. ,4H76. ,               00073900
     2         4H90. ,4H91. ,4H92. ,4H106.,4H107.,4H108./               00074000
      DATA ISEL  /  57, 62, 63, 66, 67, 68, 77, 78, 79, 80, 81, 82,     00074100
     A              83, 84, 93, 94, 95, 96, 97, 98, 99,100,111,112,     00074200
     B             113,114,115,116,117,118,130,131,132,133,134,135 /    00074300
      DATA KSLANT/1H//,KZ/1HZ/                                          00074400
      DATA KBLANK/1H /,KR/1HR/                                          00074500
      DATA MINUS/1H-/                                                   00074600
C                                                                       00074700
   53 FORMAT (1H1,16(1X,20A4,/))                                        00074800
   71 FORMAT (1H1,5X,I3,37H-HOUR FORECAST ICE DRIFT, WINDS ONLY,,       00074900
     1  12H DAY ZERO = ,I2,1X,A4,1X,I4/)                                00075000
   72 FORMAT (1X,5HWT1= ,F4.2,2X,5HWT2= ,F4.2,/)                        00075100
   73 FORMAT (1X,23HPOINT MET DIR DISTC(NM),5X,                         00075200
     1           23HPOINT MET DIR DISTC(NM),5X,                         00075300
     2           23HPOINT MET DIR DISTC(NM),5X,                         00075400
     3           23HPOINT MET DIR DISTC(NM),/ )                         00075500
   75 FORMAT (2X,I3,3X,F5.1,4X,F5.1,8X,                                 00075600
     1           I3,3X,F5.1,4X,F5.1,8X,                                 00075700
     2           I3,3X,F5.1,4X,F5.1,8X,                                 00075800
     3           I3,3X,F5.1,4X,F5.1 )                                   00075900
   83 FORMAT (1H1,5X,I3,43H-HOUR FORECAST ICE DRIFT, WINDS + CURRENTS,, 00076000
     1  12H DAY ZERO = ,I2,1X,A4,1X,I4/ )                               00076100
  105 FORMAT (2(160A4))                                                 00076200
C                                                                       00076300
C...................................................................... 00076400
C                                                                       00076500
      CALL W3FQ02(ITIME,0)                                              00076600
      CALL W3AI15(ITIME(5),LGMT,1,4,MINUS)                              00076700
      LDUMYR=LYR                                                        00076800
      MF=MO                                                             00076900
      LPR=1976                                                          00077000
C                                                                       00077100
C     LEAP YEAR CORRECTION                                              00077200
C                                                                       00077300
      XLYR=LYR                                                          00077400
      X=XLYR/4.                                                         00077500
      IPART=X/1                                                         00077600
      RMDR=X-IPART                                                      00077700
      IF (RMDR.NE.0) GO TO 8                                            00077800
      ID(2)=29                                                          00077900
    8 CONTINUE                                                          00078000
C                                                                       00078100
C     BRANCH TO DO MSSGS USING WINDS + CURRENT INDUCED DRIFT            00078200
C                                                                       00078300
      IF (FLAG.EQ.1.) GO TO 30                                          00078400
      IF (PRTFLG.EQ.0.) GO TO 25                                        00078500
      DO 15 I=2,12,2                                                    00078600
      INC=I*12                                                          00078700
      WRITE (8,71) INC,LDY,NM(MO),LYR                                   00078800
      WRITE (8,72) WT1,WT2                                              00078900
      WRITE (8,73)                                                      00079000
      DO 5 J=1,52                                                       00079100
      K=J+ 52                                                           00079200
      L=J+104                                                           00079300
      M=J+156                                                           00079400
      DD(I,208)=0.                                                      00079500
      FF(I,208)=0.                                                      00079600
      WRITE (8,75) J,DD(I,J),FF(I,J),K,DD(I,K),FF(I,K),                 00079700
     1             L,DD(I,L),FF(I,L),M,DD(I,M),FF(I,M)                  00079800
    5 CONTINUE                                                          00079900
   15 CONTINUE                                                          00080000
   25 CONTINUE                                                          00080100
      RETURN                                                            00080200
C                                                                       00080300
C     NOW DO BULLETINS                                                  00080400
C     BUILD AND BLOCK HEADER LINES                                      00080500
C                                                                       00080600
   30 CONTINUE                                                          00080700
      KDY=LDY                                                           00080800
      K=0                                                               00080900
      N=0                                                               00081000
      NEXT=0                                                            00081100
      NIN=1280                                                          00081200
      ININ=NIN                                                          00081300
      NCHAR=-1                                                          00081400
      DO 61 KT=1,780                                                    00081500
      MES(KT)=0                                                         00081600
   61 CONTINUE                                                          00081700
      DO 62 KT=1,320                                                    00081800
      NBLK(KT)=0                                                        00081900
   62 CONTINUE                                                          00082000
      MESS=3120                                                         00082100
      IBYTE=0                                                           00082200
C                                                                       00082300
      DO 250 I=2,12,2                                                   00082400
      LYR=LDUMYR                                                        00082500
      N=0                                                               00082600
      IF (I.NE.2) GO TO 11                                              00082700
C                                                                       00082800
C     CONVERT TO 'EBCDIC'.                                              00082900
C     BUILD FIRST HEADER LINE.                                          00083000
C                                                                       00083100
      N=0                                                               00083200
      L=64                                                              00083300
      NBYTE=IBYTE                                                       00083400
      IBYTE=IBYTE+L                                                     00083500
      CALL W3AI15(ITIME(5),LGMT,1,4,MINUS)                              00083600
      CALL W3AI15(KDY,KDAY,1,2,MINUS)                                   00083700
      CALL W3AI18(KHDRA,13,4,LINE,L,0,N)                                00083800
      CALL W3AI18( KDAY,1,2,LINE,L,0,N)                                 00083900
      CALL W3AI18( LGMT,1,4,LINE,L,0,N)                                 00084000
      CALL W3AI18( IEOL,1,3,LINE,L,0,N)                                 00084100
      CALL W3AI18(IFILL,1,3,LINE,L,0,N)                                 00084200
      CALL W3AI19(LINE,N,MES,MESS,NEXT)                                 00084300
      NCHAR=NCHAR+N                                                     00084400
      N=0                                                               00084500
C                                                                       00084600
C     BUILD SECOND HEADER LINE.                                         00084700
C                                                                       00084800
      CALL W3AI15(LDY,LDAY,1,2,MINUS)                                   00084900
      CALL W3AI15(LYR,LYEAR,1,4,MINUS)                                  00085000
      L=15                                                              00085100
      NBYTE=IBYTE                                                       00085200
      IBYTE=IBYTE+L                                                     00085300
      CALL W3AI18(KHDR6,3,4,LINE,L,K,N)                                 00085400
      CALL W3AI18( IEOL,1,3,LINE,L,K,N)                                 00085500
      CALL W3AI19(LINE,N,MES,MESS,NEXT)                                 00085600
      NCHAR=NCHAR+N                                                     00085700
      N=0                                                               00085800
   11 CONTINUE                                                          00085900
      INC=I*12                                                          00086000
      NV=INC/24+LDY                                                     00086100
      IF (NV.LE.ID(MO)) GO TO 10                                        00086200
      NV=NV-ID(MO)                                                      00086300
      MF=MO+1                                                           00086400
      IF (MF.LE.12) GO TO 10                                            00086500
C                                                                       00086600
C     END OF YEAR DATE CHANGE                                           00086700
C                                                                       00086800
      MF=1                                                              00086900
      LYR=LYR + 1                                                       00087000
      CALL W3AI15(LYR,LYEAR,1,4,MINUS)                                  00087100
   10 CONTINUE                                                          00087200
      L=55                                                              00087300
      NBYTE=IBYTE                                                       00087400
      IBYTE=IBYTE+L                                                     00087500
      CALL W3AI15(NV,NVDAY,1,2,MINUS)                                   00087600
      CALL W3AI18(KDUM(I-1),1,4,LINE,L,K,N)                             00087700
      CALL W3AI18(  KDUM(I),1,4,LINE,L,K,N)                             00087800
      CALL W3AI18(    KHDR7,8,4,LINE,L,K,N)                             00087900
      CALL W3AI18(    NVDAY,1,2,LINE,L,K,N)                             00088000
      CALL W3AI18(   NM(MF),1,4,LINE,L,1,N)                             00088100
      CALL W3AI18(    LYEAR,1,4,LINE,L,1,N)                             00088200
      CALL W3AI18(     IEOL,1,3,LINE,L,K,N)                             00088300
C                                                                       00088400
C     BUILD THIRD HEADER LINE.                                          00088500
C                                                                       00088600
      CALL W3AI19(LINE,N,MES,MESS,NEXT)                                 00088700
      NCHAR=NCHAR+N                                                     00088800
      N=0                                                               00088900
      L=57                                                              00089000
C                                                                       00089100
C                                                                       00089200
      DO 240 J=1,36                                                     00089300
      JJ=ISEL(J)                                                        00089400
      NDD(JJ)=DD(I,JJ)                                                  00089500
      NFF(JJ)=FF(I,JJ)                                                  00089600
      IF (NFF(JJ).GE.100) NFF(JJ)=99                                    00089700
  240 CONTINUE                                                          00089800
      NN=1                                                              00089900
C                                                                       00090000
C     KSEL DETERMINES THE NUMBER OF DD/FF GROUPS PER TELETYPE LINE.     00090100
C                                                                       00090200
      KSEL=5                                                            00090300
      N=0                                                               00090400
C                                                                       00090500
C     BUILD TRAILER LINES.                                              00090600
C                                                                       00090700
  230 CONTINUE                                                          00090800
      NBYTE=IBYTE                                                       00090900
      IBYTE=IBYTE+L                                                     00091000
      IF (.NOT.((IBYTE.GT.1280).AND.(NBYTE.LT.1280))) GO TO 590         00091100
      N=0                                                               00091200
      CALL W3AI18(IETB,1,1,LINE,L,0,N)                                  00091300
      CALL W3AI19(LINE,N,MES,MESS,NEXT)                                 00091400
      NCHAR=NCHAR+N                                                     00091500
      IREM=ININ-NEXT                                                    00091600
      NCHAR=NCHAR+IREM                                                  00091700
      NEXT=ININ                                                         00091800
      ININ=ININ+NIN                                                     00091900
      N=0                                                               00092000
      IBYTE=0                                                           00092100
      NBYTE=IBYTE                                                       00092200
      IBYTE=IBYTE+L                                                     00092300
  590 CONTINUE                                                          00092400
      DO 600 KK=NN,KSEL                                                 00092500
      IF (NN.EQ.36) L=13                                                00092600
      IF (NN.NE.36) L=57                                                00092700
      JJ=ISEL(KK)                                                       00092800
      CALL W3AI15(NDD(JJ),NDD(JJ),1,3,MINUS)                            00092900
      CALL W3AI15(NFF(JJ),NFF(JJ),1,2,MINUS)                            00093000
      CALL W3AI18(IRAY(KK),1,4,LINE,L,0,N)                              00093100
      CALL W3AI18(  NDD(JJ),1,3,LINE,L,0,N)                             00093200
      CALL W3AI18(   KSLANT,1,1,LINE,L,0,N)                             00093300
      CALL W3AI18(  NFF(JJ),1,2,LINE,L,0,N)                             00093400
      IF ((MOD(KK,5).EQ.0).OR.(KK.EQ.36)) GO TO 601                     00093500
      CALL W3AI18(   KBLANK,1,1,LINE,L,0,N)                             00093600
  600 CONTINUE                                                          00093700
C                                                                       00093800
C     AFTER 2ND LINE TEST FOR END OF BULLETIN.                          00093900
C                                                                       00094000
  601 CONTINUE                                                          00094100
      IF (KSEL.GT.35) GO TO 605                                         00094200
      CALL W3AI18(IEOL,1,3,LINE,L,K,N)                                  00094300
      CALL W3AI19(LINE,N,MES,MESS,NEXT)                                 00094400
C                                                                       00094500
      NCHAR=NCHAR+N                                                     00094600
      N=0                                                               00094700
      NN = NN + 5                                                       00094800
      KSEL = KSEL + 5                                                   00094900
  602 CONTINUE                                                          00095000
C                                                                       00095100
      IF (NN.EQ.36) KSEL = 36                                           00095200
      GO TO 230                                                         00095300
C                                                                       00095400
  605 CONTINUE                                                          00095500
      IF (KSEL.EQ.36.AND.I.EQ.12) GO TO 247                             00095600
      CALL W3AI18(IEOL,1,3,LINE,L,K,N)                                  00095700
      CALL W3AI19(LINE,N,MES,MESS,NEXT)                                 00095800
      NCHAR=NCHAR+N                                                     00095900
      N=0                                                               00096000
      IF (I.NE.12) GO TO 250                                            00096100
  247 CONTINUE                                                          00096200
C                                                                       00096300
C     TEST FOR LAST LINE OF ALASKA BULLETIN (FQGA40 KWBC DDHHMM), WHERE 00096400
C              DD = DAY                                                 00096500
C              HH = HOUR                                                00096600
C              MM = MINUTE                                              00096700
C                                                                       00096800
C     CALL W3AI18(IEOL,1,3,LINE,L,K,N)                                  00096900
      CALL W3AI18(IETX,1,1,LINE,L,K,N)                                  00097000
      CALL W3AI19(LINE,N,MES,MESS,NEXT)                                 00097100
      NCHAR=NCHAR+N                                                     00097200
      N=0                                                               00097300
C                                                                       00097400
C     BLOCK ENTIRE BULLETIN.                                            00097500
C                                                                       00097600
      INK = NCHAR / NIN                                                 00097700
      NREST = NCHAR - INK * NIN + INK                                   00097800
      NEXT=0                                                            00097900
      IN=1                                                              00098000
      IF (NREST.EQ.NCHAR) GO TO 94                                      00098100
C                                                                       00098200
   91 CONTINUE                                                          00098300
      CALL W3AI19(MES(IN),NIN,NBLK,NIN,NEXT)                            00098400
      WRITE (88,105) (NBLK(KT),KT=1,320)                                00098500
      IF (PRTFLG.NE.0.) WRITE (8,53) (NBLK(KT),KT=1,320)                00098600
      NEXT=0                                                            00098700
      IN = IN + 320                                                     00098800
      IF (IN - (320 * INK)) 91,95,95                                    00098900
   94 CONTINUE                                                          00099000
      NREST = NREST + 1                                                 00099100
   95 CONTINUE                                                          00099200
      CALL W3AI19(MES(IN),NREST,NBLK,NIN,NEXT)                          00099300
      NREM = 4 - MOD(NREST,4)                                           00099400
      NRST = (NREST+NREM) / 4                                           00099500
      WRITE (88,105)  (NBLK(KT),KT=1,NRST)                              00099600
      IF  (PRTFLG.NE.0.)  WRITE (6,53) (NBLK(KT),KT=1,NRST)             00099700
C                                                                       00099800
C          LINE 16.                                                     00099900
C                                                                       00100000
  250 CONTINUE                                                          00100100
      N=0                                                               00100200
      NEXT=0                                                            00100300
C                                                                       00100400
C     BUILD AND BLOCK HEADING                                           00100500
C                                                                       00100600
      DO 255 I=1,12,11                                                  00100700
      LYR=LDUMYR                                                        00100800
      L=52                                                              00100900
      N=0                                                               00101000
      IF (I.NE.1) GO TO 12                                              00101100
C                                                                       00101200
C           LINE 1.                                                     00101300
C                                                                       00101400
      NEXT = 0                                                          00101500
      N=0                                                               00101600
      L=64                                                              00101700
      CALL W3AI15(ITIME(5),LGMT,1,4,MINUS)                              00101800
      CALL W3AI15(KDY,KDAY,1,2,MINUS)                                   00101900
      CALL W3AI18(KHDRK,13,4,LINE,L,0,N)                                00102000
      CALL W3AI18( KDAY,1,2,LINE,L,0,N)                                 00102100
      CALL W3AI18( LGMT,1,4,LINE,L,0,N)                                 00102200
      CALL W3AI18( IEOL,1,3,LINE,L,0,N)                                 00102300
      CALL W3AI18(IFILL,1,3,LINE,L,0,N)                                 00102400
      N=0                                                               00102500
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00102600
      L=25                                                              00102700
      CALL W3AI15(LYR,LYEAR,1,4,MINUS)                                  00102800
      CALL W3AI15(ITIME(5),LGMT,1,4,MINUS)                              00102900
      CALL W3AI18(    KR,1,1,LINE,L,K,N)                                00103000
      CALL W3AI18(  LDAY,1,2,LINE,L,1,N)                                00103100
      CALL W3AI18(  LGMT,1,4,LINE,L,K,N)                                00103200
      CALL W3AI18(    KZ,1,1,LINE,L,K,N)                                00103300
      CALL W3AI18(NM(MO),1,4,LINE,L,4,N)                                00103400
      CALL W3AI18( LYEAR,1,4,LINE,L,1,N)                                00103500
      CALL W3AI18(  IEOL,1,3,LINE,L,K,N)                                00103600
      N=0                                                               00103700
C                                                                       00103800
C          LINE 2.                                                      00103900
C                                                                       00104000
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00104100
      L=35                                                              00104200
      CALL W3AI18( KHDR2,8,4,LINE,L,K,N)                                00104300
      CALL W3AI18(  IEOL,1,3,LINE,L,K,N)                                00104400
      N=0                                                               00104500
C                                                                       00104600
C          LINE 3.                                                      00104700
C                                                                       00104800
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00104900
      L=43                                                              00105000
      CALL W3AI18(KFHDR3,10,4,LINE,L,K,N)                               00105100
      CALL W3AI18(   IEOL,1,3,LINE,L,K,N)                               00105200
      N=0                                                               00105300
C                                                                       00105400
C          LINE 4.                                                      00105500
C                                                                       00105600
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00105700
      L=15                                                              00105800
      CALL W3AI18(  KHDR6,3,4,LINE,L,K,N)                               00105900
      CALL W3AI18(   IEOL,1,3,LINE,L,K,N)                               00106000
      N=0                                                               00106100
C                                                                       00106200
C          LINE 5.                                                      00106300
C                                                                       00106400
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00106500
C                                                                       00106600
C     COMPUTE FORECAST PERIOD, VALID DAY                                00106700
C                                                                       00106800
   12 CONTINUE                                                          00106900
      NV=0                                                              00107000
      INC=I*12                                                          00107100
      NV=INC/24+LDY                                                     00107200
      IF (NV.LE.ID(MO)) GO TO 14                                        00107300
      NV=NV-ID(MO)                                                      00107400
      MF=MO+1                                                           00107500
      IF (MF.LE.12) GO TO 14                                            00107600
C                                                                       00107700
C     END OF YEAR DATE CHANGE........                                   00107800
C                                                                       00107900
      MF=1                                                              00108000
      LYR=LYR+1                                                         00108100
      CALL W3AI15(LYR,LYEAR,1,4,MINUS)                                  00108200
   14 CONTINUE                                                          00108300
      L=55                                                              00108400
      IF (INC.GT.12) GO TO 320                                          00108500
      CALL W3AI18(KFHDR7,10,4,LINE,L,K,N)                               00108600
      CALL W3AI18(   LDAY,1,2,LINE,L,K,N)                               00108700
      CALL W3AI18( NM(MO),1,4,LINE,L,1,N)                               00108800
      CALL W3AI18(  LYEAR,1,4,LINE,L,1,N)                               00108900
      CALL W3AI18(   IEOL,1,3,LINE,L,K,N)                               00109000
      N=0                                                               00109100
C                                                                       00109200
C          LINE 6.                                                      00109300
C                                                                       00109400
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00109500
C                                                                       00109600
      GO TO 325                                                         00109700
  320 CONTINUE                                                          00109800
      L=55                                                              00109900
      CALL W3AI15(NV,NVDAY,1,2,MINUS)                                   00110000
      CALL W3AI18(KDUM(I-1),1,4,LINE,L,K,N)                             00110100
      CALL W3AI18(  KDUM(I),1,4,LINE,L,K,N)                             00110200
      CALL W3AI18(    KHDR7,8,4,LINE,L,K,N)                             00110300
      CALL W3AI18(    NVDAY,1,2,LINE,L,K,N)                             00110400
      CALL W3AI18(   NM(MF),1,4,LINE,L,1,N)                             00110500
      CALL W3AI18(    LYEAR,1,4,LINE,L,1,N)                             00110600
      CALL W3AI18(     IEOL,1,3,LINE,L,K,N)                             00110700
      N=0                                                               00110800
C                                                                       00110900
C          LINE 11.                                                     00111000
C                                                                       00111100
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00111200
C                                                                       00111300
C     SELECT KEFLAVIK MESSAGE GRID POINTS.                              00111400
C                                                                       00111500
  325 CONTINUE                                                          00111600
      DO 330 J=1,18                                                     00111700
      JJ=NK(J)                                                          00111800
      NDD(JJ)=DD(I,JJ)                                                  00111900
      NFF(JJ)=FF(I,JJ)                                                  00112000
      IF (NFF(JJ).GE.100) NFF(JJ)=99                                    00112100
  330 CONTINUE                                                          00112200
      NN=1                                                              00112300
      KSEL=5                                                            00112400
      N=0                                                               00112500
C                                                                       00112600
C          LINES 7 THRU 10 AND 12 THRU 15                               00112700
C                                                                       00112800
      L=57                                                              00112900
  340 CONTINUE                                                          00113000
      DO 400 KK=NN,KSEL                                                 00113100
      JJ=NK(KK)                                                         00113200
      CALL W3AI15(NDD(JJ),NDD(JJ),1,3,MINUS)                            00113300
      CALL W3AI15(NFF(JJ),NFF(JJ),1,2,MINUS)                            00113400
      CALL W3AI18(KEF(KK),1,4,LINE,L,K,N)                               00113500
      CALL W3AI18(NDD(JJ),1,3,LINE,L,K,N)                               00113600
      CALL W3AI18( KSLANT,1,1,LINE,L,K,N)                               00113700
      CALL W3AI18(NFF(JJ),1,2,LINE,L,0,N)                               00113800
      IF (KK.EQ. 5.OR.KK.EQ.10) GO TO 401                               00113900
      IF (KK.EQ.15.OR.KK.EQ.18) GO TO 401                               00114000
      CALL W3AI18( KBLANK,1,1,LINE,L,K,N)                               00114100
  400 CONTINUE                                                          00114200
  401 CONTINUE                                                          00114300
      IF (KK.EQ.18.AND.I.EQ.12) GO TO 420                               00114400
      CALL W3AI18(IEOL,1,3,LINE,L,K,N)                                  00114500
      N=0                                                               00114600
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00114700
      IF (KK.EQ.18) GO TO 420                                           00114800
      IF (KK.EQ.15) L=35                                                00114900
      N=0                                                               00115000
      NN=KK+1                                                           00115100
      KSEL=KSEL+5                                                       00115200
      GO TO 340                                                         00115300
C                                                                       00115400
  420 CONTINUE                                                          00115500
      IF (I.NE.12) GO TO 255                                            00115600
      L=33                                                              00115700
      CALL W3AI18(IETX,1,1,LINE,L,K,N)                                  00115800
      N=0                                                               00115900
      CALL W3AI19(LINE,L,NBLK,1280,NEXT)                                00116000
      WRITE (88,105) NBLK                                               00116100
      IF (PRTFLG.NE.0.) WRITE (8,53) NBLK                               00116200
      NEXT=0                                                            00116300
  255 CONTINUE                                                          00116400
      IF (PRTFLG.EQ.0.) GO TO 40                                        00116500
      DO 35 I=2,12,2                                                    00116600
      INC=I*12                                                          00116700
      WRITE (8,83) INC,LDY,NM(MO),LYR                                   00116800
      WRITE (8,72) WT1,WT2                                              00116900
      WRITE (8,73)                                                      00117000
      DO 33 J=1,52                                                      00117100
      K=J+ 52                                                           00117200
      L=J+104                                                           00117300
      M=J+156                                                           00117400
      DD(I,208)=0.                                                      00117500
      FF(I,208)=0.                                                      00117600
      WRITE (8,75) J,DD(I,J),FF(I,J),K,DD(I,K),FF(I,K),                 00117700
     1             L,DD(I,L),FF(I,L),M,DD(I,M),FF(I,M)                  00117800
   33 CONTINUE                                                          00117900
   35 CONTINUE                                                          00118000
   40 CONTINUE                                                          00118100
C                                                                       00118200
      RETURN                                                            00118300
      END                                                               00118400
C****     DATE COPIED MAY 30, 1985 - 0922 AUTHOR STACKPOLE              00000100
      SUBROUTINE W3FC02(FFID,FFJD,FGU,FGV,DIR,SPD)                      00000200
C                                                                       00000300
C$$$  SUBROUTINE DOCUMENTATION BLOCK ***                                00000400
C                                                                       00000500
C SUBR: W3FC02   - GRID U,V WIND COMPS. TO DIR. AND SPEED               00000600
C   AUTHOR: STACKPOLE          ORG: W324          DATE: 30 DEC 81       00000700
C                                                                       00000800
C ABSTRACT: GIVEN THE GRID-ORIENTED WIND COMPONENTS ON A NORTHERN       00000900
C   HEMISPHERE POLAR STEREOGRAPHIC GRID POINT, COMPUTE THE DIRECTION    00001000
C   AND SPEED OF THE WIND AT THAT POINT.  INPUT WINDS AT THE NORTH      00001100
C   POLE POINT ARE ASSUMED TO HAVE THEIR COMPONENTS FOLLOW THE WMO      00001200
C   STANDARDS FOR REPORTING WINDS AT THE NORTH POLE.                    00001300
C   (SEE OFFICE NOTE 241 FOR WMO DEFINITION). OUTPUT DIRECTION          00001400
C   WILL FOLLOW WMO CONVENTION.                                         00001500
C                                                                       00001600
C USAGE: CALL W3FC02 (FFID, FFJD, FGU, FGV, DIR, SPD)                   00001700
C                                                                       00001800
C   INPUT ARGUMENTS:                                                    00001900
C     FFID  -  REAL*4  =  I(NORTH POLE) - I(POINT)                      00002000
C     FFJD  -  REAL*4  =  J(NORTH POLE) - J(POINT)                      00002100
C     FGU   -  REAL*4  -  GRID-ORIENTED U-COMPONENT                     00002200
C     FGV   -  REAL*4  -  GRID-ORIENTED V-COMPONENT                     00002300
C                                                                       00002400
C   INPUT FILES:  NONE                                                  00002500
C                                                                       00002600
C   OUTPUT ARGUMENTS:                                                   00002700
C     DIR   -  REAL*4  -  WIND DIRECTION, DEGREES                       00002800
C     SPD   -  REAL*4  -  WIND SPEED                                    00002900
C                                                                       00003000
C   OUTPUT FILES:  NONE                                                 00003100
C                                                                       00003200
C   RETURN CONDITIONS:  NONE                                            00003300
C                                                                       00003400
C   SUBPROGRAMS CALLED:  NONE                                           00003500
C                                                                       00003600
C ATTRIBUTES:                                                           00003700
C   LANGUAGE:   FORTRAN H + ENHANCED                                    00003800
C   SOURCE STATEMENTS:   26                PGM SIZE:  834 BYTES         00003900
C                                                                       00004000
C   FOR DETAILS OBTAIN WRITEUP FROM NMC/AD/SAB                          00004100
C                                                                       00004200
C$$$                                                                    00004300
C                                                                       00004400
      SPD = SQRT(FGU * FGU + FGV * FGV)                                 00004500
      IF (SPD.NE.0.) GO TO 1000                                         00004600
         FGU = 0.                                                       00004700
         FGV = 0.                                                       00004800
         GO TO 3000                                                     00004900
 1000 CONTINUE                                                          00005000
      DFP = SQRT(FFID * FFID + FFJD * FFJD)                             00005100
      IF (DFP.NE.0.) GO TO 2000                                         00005200
         XLAM = ACOS(FGU / SPD)                                         00005300
         XLAM = XLAM * 57.29578                                         00005400
         IF (FGV.LT.0.) DIR = 170. + XLAM                               00005500
         IF ((FGV.GT.0.).AND.(XLAM.LT.170.)) DIR = 170. - XLAM          00005600
         IF ((FGV.GT.0.).AND.(XLAM.GE.170.)) DIR = 530. - XLAM          00005700
         IF ((ABS(FGV).LE.0.001).AND.(FGU.GT.0.)) DIR = 170.            00005800
         IF ((ABS(FGV).LE.0.001).AND.(FGU.LT.0.)) DIR = 350.            00005900
         GO TO 3000                                                     00006000
 2000 CONTINUE                                                          00006100
         CAL = FFJD / DFP                                               00006200
         SAL = FFID / DFP                                               00006300
         U = FGU * CAL - FGV * SAL                                      00006400
         V = FGU * SAL + FGV * CAL                                      00006500
         DIR = 57.29578 * ATAN2(U,V) + 180.                             00006600
 3000 CONTINUE                                                          00006700
      RETURN                                                            00006800
      END                                                               00006900
