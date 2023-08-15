//ZBNKCORE JOB 'CORE DUMP',CLASS=A,MSGCLASS=A
//*
//BANKDO   EXEC PGM=ZBNKCORE
//REPT10    DD SYSOUT=*
//*REPT10   DD DSN=EBC.BANK10.REPORT,
//*            DISP=(NEW,CATLG,DELETE),                                
//*            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800),                    
//*            UNIT=SYSDA,SPACE=(TRK,(2,1),RLSE)                
//BANK10   DD DSN=EBC.BANK10,DISP=OLD
//SYSOUT   DD SYSOUT=A


