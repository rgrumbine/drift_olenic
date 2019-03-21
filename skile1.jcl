//WW0930L6 JOB (WD42008M1H1040X,NAVY-FWF),'FWF',REGION=1200K,TIME=4,    00010000
//  MSGCLASS=X                                                          00020000
//*MAIN CLASS=NMCFEN2,ORG=LOCAL                                         00030000
//*                                                                     00040000
//*  GRIDBULV  -                                                        00050000
//*  FUNTWN2V  -                                                        00060000
//*  FPAWNDTV  -                                                        00070000
//*  NAVYTRNV  -                                                        00080000
//*  CANAMETV  -                                                        00090000
//*  NSEAICEV  -                                                        00100000
//*  IEBGENER  -  REPLACED DISPOS FOR NSEAICE PRINTOUT                  00110000
//*                                                                     00120000
//PURGTX    EXEC  PGM=IEFBR14                                           00130000
//GRIDBUL   DD DSN=NMC.TRAN.GRIDBUL.OPRNL,                              00140000
//            UNIT=3380,VOL=SER=NMCSW2,DISP=(SHR,DELETE)                00150000
//FUNT930L  DD DSN=NMC.TRAN.FUNT930L.OPRNL,                             00160000
//            UNIT=3380,VOL=SER=NMCSW2,DISP=(SHR,DELETE)                00160100
//FPAWNDTV  DD DSN=NMC.FPAWNDTV.OPRNL,                                  00160200
//            UNIT=3380,VOL=SER=NMCSW2,DISP=(SHR,DELETE)                00160300
//NAVYFXN   DD DSN=NMC.XTRN.NAVYFXN.OPRNL,                              00160400
//            UNIT=3380,VOL=SER=NMCSW2,DISP=(SHR,DELETE)                00160500
//CANAMETX  DD DSN=NMC.TRAN.CANAMETX.OPRNL,                             00160600
//            UNIT=3380,VOL=SER=NMCSW2,DISP=(SHR,DELETE)                00160700
//NSEAICE   DD DSN=NMC.TRAN.NSEAICE.OPRNL,                              00160800
//            UNIT=3380,VOL=SER=NMCSW2,DISP=(SHR,DELETE)                00160900
//*                                                                     00161000
//FT06F001  DD DSN=&&TEMP06,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1995),     00162000
//            UNIT=SYSWK,SPACE=(CYL,(4,2)),DISP=(NEW,PASS)              00163000
//*                                                                     00165000
//GRIDBULV  EXEC  PGM=GRIDBULV                                          00166000
//STEPLIB   DD DSN=NMC.PROD.PAN1.LOAD,DISP=SHR                          00167000
//JOBLOG    DD DSN=NMC.PROD.JOBLOG,DISP=SHR                             00168000
//FT05F001  DD *                                                        00190000
FT88F001TRAN                                                            00200000
/*                                                                      00210000
//FT06F001  DD DSN=&&TEMP06,DISP=(MOD,PASS)                             00220000
//FT30F001  DD DSN=NMC.PROD.JCDATA(GRIDBMRF),LABEL=(,,,IN),DISP=SHR     00230000
//FT88F001  DD DSN=NMC.TRAN.GRIDBUL.OPRNL,DISP=(NEW,KEEP),              00240000
//            DCB=(RECFM=FB,LRECL=1280,BLKSIZE=12800),                  00250000
//            UNIT=3380,VOL=SER=NMCSW2,SPACE=(CYL,(3,1))                00251000
//F72       DD DSN=NMC.PROD.VF72.T00Z.MRF,DISP=SHR                      00169000
//F96       DD DSN=NMC.PROD.VF96.T00Z.MRF,DISP=SHR                      00170000
//F120      DD DSN=NMC.PROD.VF120.T00Z.MRF,DISP=SHR                     00180000
//BACKUP    DD DUMMY,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)                 00252000
//*                                                                     00260000
//FUNTWN2V  EXEC  PGM=FUNTWN2V                                          00270000
//STEPLIB   DD DSN=NMC.PROD.PAN2.LOAD,DISP=SHR                          00280000
//JOBLOG    DD DSN=NMC.PROD.JOBLOG,DISP=SHR                             00290000
//FT05F001  DD DSN=NMC.PROD.JCDATA(TWN2HR96),DISP=SHR,LABEL=(,,,IN)     00300000
//FT06F001  DD DSN=&&TEMP06,DISP=(MOD,PASS)                             00310000
//FT88F001  DD DSN=NMC.TRAN.FUNT930L.OPRNL,DISP=(NEW,KEEP),             00350000
//            DCB=(RECFM=FB,LRECL=1280,BLKSIZE=12800),                  00360000
//            UNIT=3380,VOL=SER=NMCSW2,SPACE=(CYL,(3,1))                00370000
//F48       DD DSN=NMC.PROD.VF48.T00Z.MRF,DISP=SHR                      00320000
//F72       DD DSN=NMC.PROD.VF72.T00Z.MRF,DISP=SHR                      00330000
//F96       DD DSN=NMC.PROD.VF96.T00Z.MRF,DISP=SHR                      00380000
//F120      DD DSN=NMC.PROD.VF120.T00Z.MRF,DISP=SHR                     00390000
//*                                                                     00370200
//FPAWNDTV  EXEC  PGM=FPAWNDTV,COND=EVEN,PARM=('TIME(00)',              00370300
//   'UPST(FT11F001,WW4005,NMCTP9,FSDRPA  ,NMC.FPAWNDTV.OPRNL')         00370400
//STEPLIB   DD DSN=NMC.PROD.PAN2.LOAD,DISP=SHR                          00370500
//JOBLOG    DD DSN=NMC.PROD.JOBLOG,DISP=SHR                             00370600
//FT06F001  DD DSN=&&TEMP06,DISP=(MOD,PASS)                             00370700
//FT11F001  DD DSN=NMC.FPAWNDTV.OPRNL,DISP=(NEW,KEEP),                  00370800
//            DCB=(RECFM=FB,LRECL=1280,BLKSIZE=12800),                  00370900
//            UNIT=3380,VOL=SER=NMCSW2,SPACE=(TRK,20)                   00371000
//F00       DD DSN=NMC.PROD.VF00.T00Z.MRF,DISP=SHR                      00371100
//F12       DD DSN=NMC.PROD.VF12.T00Z.MRF,DISP=SHR                      00371200
//F24       DD DSN=NMC.PROD.VF24.T00Z.MRF,DISP=SHR                      00371300
//F36       DD DSN=NMC.PROD.VF36.T00Z.MRF,DISP=SHR                      00371400
//F48       DD DSN=NMC.PROD.VF48.T00Z.MRF,DISP=SHR                      00371500
//F60       DD DSN=NMC.PROD.VF60.T00Z.MRF,DISP=SHR                      00371600
//F72       DD DSN=NMC.PROD.VF72.T00Z.MRF,DISP=SHR                      00371700
//F84       DD DSN=NMC.PROD.VF84.T00Z.MRF,DISP=SHR                      00371800
//OUTPUT    DD SYSOUT=(A,INTRDR),DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)     00371900
//BACKUP    DD DUMMY,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)                 00372000
//*                                                                     00372100
//NAVYTRNV  EXEC  PGM=NAVYTRNV,COND=EVEN                                00372200
//STEPLIB   DD DSN=NMC.PROD.PAN2.LOAD,DISP=SHR                          00372300
//JOBLOG    DD DSN=NMC.PROD.JOBLOG,DISP=SHR                             00372400
//FT05F001  DD *                                                        00372700
FT88F001XTRN                                                            00372800
/*                                                                      00372900
//FT06F001  DD SYSOUT=A,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1995)          00373000
//FT10F001  DD DSN=NMC.PROD.JCDATA(NAVYFXNA),DISP=SHR,LABEL=(,,,IN)     00373100
//FT88F001  DD DSN=NMC.XTRN.NAVYFXN.OPRNL,DISP=(NEW,KEEP),              00373200
//            SPACE=(CYL,6),DCB=(RECFM=FB,LRECL=1280,BLKSIZE=12800),    00373300
//            UNIT=3380,VOL=SER=NMCSW2                                  00373400
//F84       DD DSN=NMC.PROD.VF84.T00Z.MRF,DISP=SHR                      00372500
//F96       DD DSN=NMC.PROD.VF96.T00Z.MRF,DISP=SHR                      00372500
//F108      DD DSN=NMC.PROD.VF108.T00Z.MRF,DISP=SHR                     00372600
//F120      DD DSN=NMC.PROD.VF120.T00Z.MRF,DISP=SHR                     00372600
//F132      DD DSN=NMC.PROD.VF132.T00Z.MRF,DISP=SHR                     00372600
//F144      DD DSN=NMC.PROD.VF144.T00Z.MRF,DISP=SHR                     00372600
//BACKUP    DD DUMMY,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)                 00373500
//*                                                                     00373600
//CANAMETV  EXEC  PGM=CANAMETV                                          00373700
//STEPLIB   DD DSN=NMC.PROD.PAN2.LOAD,DISP=SHR                          00373800
//JOBLOG    DD DSN=NMC.PROD.JOBLOG,DISP=SHR                             00373900
//FT06F001  DD DSN=&&TEMP06,DISP=(MOD,PASS)                             00374000
//FT88F001  DD DSN=NMC.TRAN.CANAMETX.OPRNL,DISP=(NEW,KEEP),             00375000
//            DCB=(RECFM=FB,LRECL=1280,BLKSIZE=12800),                  00376000
//            UNIT=3380,VOL=SER=NMCSW2,SPACE=(CYL,(3,3))                00377000
//F24       DD DSN=NMC.PROD.VF24.T00Z.MRF,DISP=SHR                      00378000
//F48       DD DSN=NMC.PROD.VF48.T00Z.MRF,DISP=SHR                      00379000
//F72       DD DSN=NMC.PROD.VF72.T00Z.MRF,DISP=SHR                      00380000
//F96       DD DSN=NMC.PROD.VF96.T00Z.MRF,DISP=SHR                      00380000
//F120      DD DSN=NMC.PROD.VF120.T00Z.MRF,DISP=SHR                     00390000
//BACKUP    DD DUMMY,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)                 00400000
//*                                                                     00410000
//NSEAICEV  EXEC  PGM=NSEAICEV                                          00420000
//STEPLIB   DD DSN=NMC.PROD.PAN2.LOAD,DISP=SHR                          00430000
//JOBLOG    DD DSN=NMC.PROD.JOBLOG,DISP=SHR                             00440000
//ANL       DD DSN=NMC.PROD.VANL.T00Z.AVN,DISP=SHR                      00460000
//GES       DD DSN=NMC.PROD.VGES.T00Z,DISP=SHR                          00450000
//F00       DD DSN=NMC.PROD.VF00.T00Z.MRF,DISP=SHR                      00470000
//F12       DD DSN=NMC.PROD.VF12.T00Z.MRF,DISP=SHR                      00480000
//F24       DD DSN=NMC.PROD.VF24.T00Z.MRF,DISP=SHR                      00490000
//F36       DD DSN=NMC.PROD.VF36.T00Z.MRF,DISP=SHR                      00500000
//F48       DD DSN=NMC.PROD.VF48.T00Z.MRF,DISP=SHR                      00510000
//F60       DD DSN=NMC.PROD.VF60.T00Z.MRF,DISP=SHR                      00520000
//F72       DD DSN=NMC.PROD.VF72.T00Z.MRF,DISP=SHR                      00530000
//F84       DD DSN=NMC.PROD.VF84.T00Z.MRF,DISP=SHR                      00540000
//F96       DD DSN=NMC.PROD.VF96.T00Z.MRF,DISP=SHR                      00380000
//F108      DD DSN=NMC.PROD.VF108.T00Z.MRF,DISP=SHR                     00390000
//F120      DD DSN=NMC.PROD.VF120.T00Z.MRF,DISP=SHR                     00390000
//F132      DD DSN=NMC.PROD.VF132.T00Z.MRF,DISP=SHR                     00390000
//F144      DD DSN=NMC.PROD.VF144.T00Z.MRF,DISP=SHR                     00390000
//FT06F001  DD DSN=&&TEMP06,DISP=(MOD,PASS)                             00560000
//FT08F001  DD DSN=&&DISPOSE,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1995),    00570000
//            SPACE=(TRK,10),UNIT=SYSWK,DISP=(NEW,PASS)                 00570100
//FT88F001  DD DSN=NMC.TRAN.NSEAICE.OPRNL,DISP=(NEW,KEEP),              00570200
//            SPACE=(CYL,3),DCB=(RECFM=FB,LRECL=1280,BLKSIZE=12800),    00570300
//            UNIT=3380,VOL=SER=NMCSW2                                  00570400
//BACKUP    DD DUMMY,DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)                 00570500
//*                                                                     00570600
//DISPOSE1  EXEC  PGM=IEBGENER                                          00570700
//IN     DD DUMMY                                                       00570800
//SYSPRINT  DD DUMMY                                                    00570900
//SYSUT1    DD DSN=&&DISPOSE,DISP=(OLD,PASS)                            00571000
//SYSUT2    DD SYSOUT=A,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1995)          00572000
//*                                                                     00573000
//NOPRINT   EXEC  PGM=IEFBR14,COND=(1,LT)                               00574000
//*                                                                     00575000
//COPYPRT6  EXEC  PGM=IEBGENER,COND=((0,EQ,NOPRINT),EVEN)               00576000
//SYSIN     DD DUMMY                                                    00577000
//SYSPRINT  DD DUMMY                                                    00578000
//SYSUT1    DD DSN=&&TEMP06,DISP=(OLD,PASS)                             00579000
//SYSUT2    DD SYSOUT=A                                                 00580000
//*                                                                     00590000
//*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*
//*       ABOVE DECK CREATED  TUESDAY   JAN  9, 1990 - 0836 14.96 LST
//*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*.*
//
