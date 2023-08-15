      *****************************************************************
      *                                                               *
      * Copyright (C) 2010-2021 Micro Focus.  All Rights Reserved     *
      * This software may be used, modified, and distributed          *
      * (provided this notice is included without modification)       *
      * solely for internal demonstration purposes with other         *
      * Micro Focus software, and is otherwise subject to the EULA at *
      * https://www.microfocus.com/en-us/legal/software-licensing.    *
      *                                                               *
      * THIS SOFTWARE IS PROVIDED "AS IS" AND ALL IMPLIED             *
      * WARRANTIES, INCLUDING THE IMPLIED WARRANTIES OF               *
      * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE,         *
      * SHALL NOT APPLY.                                              *
      * TO THE EXTENT PERMITTED BY LAW, IN NO EVENT WILL              *
      * MICRO FOCUS HAVE ANY LIABILITY WHATSOEVER IN CONNECTION       *
      * WITH THIS SOFTWARE.                                           *
      *                                                               *
      *****************************************************************

      *****************************************************************
      * CBANKEXT.CPY                                                  *
      *---------------------------------------------------------------*
      *****************************************************************
           10  EXT-DATA                            PIC X(1024).

           10  EXT-IP-DATA REDEFINES EXT-DATA.
             15  EXT-IP-AID                        PIC X(5).
             15  EXT-IP-AREA                       PIC X(256).
             15  EXT-IP00-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP00-SEL                    PIC X(1).
             15  EXT-IP10-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP10-USERID                 PIC X(5).
               20  EXT-IP10-PSWD                   PIC X(8).
             15  EXT-IP20-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP20-SEL1ID                 PIC X(1).
               20  EXT-IP20-SEL1IP                 PIC X(1).
               20  EXT-IP20-SEL2ID                 PIC X(1).
               20  EXT-IP20-SEL2IP                 PIC X(1).
               20  EXT-IP20-SEL3ID                 PIC X(1).
               20  EXT-IP20-SEL3IP                 PIC X(1).
               20  EXT-IP20-SEL4ID                 PIC X(1).
               20  EXT-IP20-SEL4IP                 PIC X(1).
               20  EXT-IP20-SEL5ID                 PIC X(1).
               20  EXT-IP20-SEL5IP                 PIC X(1).
               20  EXT-IP20-SEL6ID                 PIC X(1).
               20  EXT-IP20-SEL6IP                 PIC X(1).
               20  EXT-IP20-SEL7ID                 PIC X(1).
               20  EXT-IP20-SEL7IP                 PIC X(1).
             15  EXT-IP30-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP30-DET1                   PIC X(1).
               20  EXT-IP30-DET2                   PIC X(1).
               20  EXT-IP30-DET3                   PIC X(1).
               20  EXT-IP30-DET4                   PIC X(1).
               20  EXT-IP30-DET5                   PIC X(1).
               20  EXT-IP30-DET6                   PIC X(1).
             15  EXT-IP35-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP35-DUMMY                  PIC X(1).
             15  EXT-IP40-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP40-DUMMY                  PIC X(1).
             15  EXT-IP50-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP50-XFER                   PIC X(8).
               20  EXT-IP50-FRM1                   PIC X(1).
               20  EXT-IP50-TO1                    PIC X(1).
               20  EXT-IP50-FRM2                   PIC X(1).
               20  EXT-IP50-TO2                    PIC X(1).
               20  EXT-IP50-FRM3                   PIC X(1).
               20  EXT-IP50-TO3                    PIC X(1).
               20  EXT-IP50-FRM4                   PIC X(1).
               20  EXT-IP50-TO4                    PIC X(1).
               20  EXT-IP50-FRM5                   PIC X(1).
               20  EXT-IP50-TO5                    PIC X(1).
               20  EXT-IP50-FRM6                   PIC X(1).
               20  EXT-IP50-TO6                    PIC X(1).
             15  EXT-IP60-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP60-NADDR1                 PIC X(25).
               20  EXT-IP60-NADDR2                 PIC X(25).
               20  EXT-IP60-NSTATE                 PIC X(2).
               20  EXT-IP60-NCNTRY                 PIC X(6).
               20  EXT-IP60-NPSTCDE                PIC X(6).
               20  EXT-IP60-NTELNO                 PIC X(12).
               20  EXT-IP60-NEMAIL                 PIC X(30).
               20  EXT-IP60-NSMAIL                 PIC X(1).
               20  EXT-IP60-NSEMAIL                PIC X(1).
             15  EXT-IP70-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP70-AMOUNT                 PIC X(7).
               20  EXT-IP70-RATE                   PIC X(5).
               20  EXT-IP70-TERM                   PIC X(5).
             15  EXT-IP80-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IP80-OPT1                   PIC X(1).
               20  EXT-IP80-OPT2                   PIC X(1).
             15  EXT-IPZZ-DATA REDEFINES EXT-IP-AREA.
               20  EXT-IPZZ-SEL1ID                 PIC X(1).
               20  EXT-IPZZ-SEL1IP                 PIC X(1).
               20  EXT-IPZZ-SEL2ID                 PIC X(1).
               20  EXT-IPZZ-SEL2IP                 PIC X(1).
               20  EXT-IPZZ-SEL3ID                 PIC X(1).
               20  EXT-IPZZ-SEL3IP                 PIC X(1).
               20  EXT-IPZZ-SEL4ID                 PIC X(1).
               20  EXT-IPZZ-SEL4IP                 PIC X(1).
               20  EXT-IPZZ-SEL5ID                 PIC X(1).
               20  EXT-IPZZ-SEL5IP                 PIC X(1).
               20  EXT-IPZZ-SEL6ID                 PIC X(1).
               20  EXT-IPZZ-SEL6IP                 PIC X(1).
               20  EXT-IPZZ-SEL7ID                 PIC X(1).
               20  EXT-IPZZ-SEL7IP                 PIC X(1).
               20  EXT-IPZZ-SEL8ID                 PIC X(1).
               20  EXT-IPZZ-SEL8IP                 PIC X(1).

           10  EXT-OP-DATA REDEFINES EXT-DATA.
             15  EXT-OP-TRAN                       PIC X(4).
             15  EXT-OP-SCREEN                     PIC X(8).
             15  EXT-OP-DATE                       PIC X(11).
             15  EXT-OP-TIME                       PIC X(8).
             15  EXT-OP-HEAD1                      PIC X(50).
             15  EXT-OP-HEAD2                      PIC X(50).
             15  EXT-OP-VERSION                    PIC X(7).
             15  EXT-OP-ERR-MSG                    PIC X(75).
             15  EXT-OP-USERID                     PIC X(5).
             15  EXT-OP-NAME                       PIC X(25).
             15  EXT-OP-AREA                       PIC X(768).
             15  EXT-OP00-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP00-TEXT                   PIC X(8).
             15  EXT-OP10-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP10-PSWD                   PIC X(8).
             15  EXT-OP10-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP10-NAME                   PIC X(25).
             15  EXT-OP20-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP20-SEL1ID                 PIC X(1).
               20  EXT-OP20-SEL1IP                 PIC X(1).
               20  EXT-OP20-SEL1TX                 PIC X(40).
               20  EXT-OP20-SEL2ID                 PIC X(1).
               20  EXT-OP20-SEL2IP                 PIC X(1).
               20  EXT-OP20-SEL2TX                 PIC X(40).
               20  EXT-OP20-SEL3ID                 PIC X(1).
               20  EXT-OP20-SEL3IP                 PIC X(1).
               20  EXT-OP20-SEL3TX                 PIC X(40).
               20  EXT-OP20-SEL4ID                 PIC X(1).
               20  EXT-OP20-SEL4IP                 PIC X(1).
               20  EXT-OP20-SEL4TX                 PIC X(40).
               20  EXT-OP20-SEL5ID                 PIC X(1).
               20  EXT-OP20-SEL5IP                 PIC X(1).
               20  EXT-OP20-SEL5TX                 PIC X(40).
               20  EXT-OP20-SEL6ID                 PIC X(1).
               20  EXT-OP20-SEL6IP                 PIC X(1).
               20  EXT-OP20-SEL6TX                 PIC X(40).
               20  EXT-OP20-SEL7ID                 PIC X(1).
               20  EXT-OP20-SEL7IP                 PIC X(1).
               20  EXT-OP20-SEL7TX                 PIC X(40).
             15  EXT-OP30-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP30-DET1                   PIC X(9).
               20  EXT-OP30-ACC1                   PIC X(9).
               20  EXT-OP30-DSC1                   PIC X(15).
               20  EXT-OP30-BAL1                   PIC X(13).
               20  EXT-OP30-SRV1                   PIC X(6).
               20  EXT-OP30-DTE1                   PIC X(11).
               20  EXT-OP30-TXN1                   PIC X(1).
               20  EXT-OP30-TXS1                   PIC X(1).
               20  EXT-OP30-DET2                   PIC X(9).
               20  EXT-OP30-ACC2                   PIC X(9).
               20  EXT-OP30-DSC2                   PIC X(15).
               20  EXT-OP30-BAL2                   PIC X(13).
               20  EXT-OP30-SRV2                   PIC X(6).
               20  EXT-OP30-DTE2                   PIC X(11).
               20  EXT-OP30-TXN2                   PIC X(1).
               20  EXT-OP30-TXS2                   PIC X(1).
               20  EXT-OP30-DET3                   PIC X(9).
               20  EXT-OP30-ACC3                   PIC X(9).
               20  EXT-OP30-DSC3                   PIC X(15).
               20  EXT-OP30-BAL3                   PIC X(13).
               20  EXT-OP30-SRV3                   PIC X(6).
               20  EXT-OP30-DTE3                   PIC X(11).
               20  EXT-OP30-TXN3                   PIC X(1).
               20  EXT-OP30-TXS3                   PIC X(1).
               20  EXT-OP30-DET4                   PIC X(9).
               20  EXT-OP30-ACC4                   PIC X(9).
               20  EXT-OP30-DSC4                   PIC X(15).
               20  EXT-OP30-BAL4                   PIC X(13).
               20  EXT-OP30-SRV4                   PIC X(6).
               20  EXT-OP30-DTE4                   PIC X(11).
               20  EXT-OP30-TXN4                   PIC X(1).
               20  EXT-OP30-TXS4                   PIC X(1).
               20  EXT-OP30-DET5                   PIC X(9).
               20  EXT-OP30-ACC5                   PIC X(9).
               20  EXT-OP30-DSC5                   PIC X(15).
               20  EXT-OP30-BAL5                   PIC X(13).
               20  EXT-OP30-SRV5                   PIC X(6).
               20  EXT-OP30-DTE5                   PIC X(11).
               20  EXT-OP30-TXN5                   PIC X(1).
               20  EXT-OP30-TXS5                   PIC X(1).
               20  EXT-OP30-DET6                   PIC X(9).
               20  EXT-OP30-ACC6                   PIC X(9).
               20  EXT-OP30-DSC6                   PIC X(15).
               20  EXT-OP30-BAL6                   PIC X(13).
               20  EXT-OP30-SRV6                   PIC X(6).
               20  EXT-OP30-DTE6                   PIC X(11).
               20  EXT-OP30-TXN6                   PIC X(1).
               20  EXT-OP30-TXS6                   PIC X(1).
             15  EXT-OP35-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP35-ACCNO                  PIC X(9).
               20  EXT-OP35-ACCTYPE                PIC X(15).
               20  EXT-OP35-BALANCE                PIC X(13).
               20  EXT-OP35-STMT-DATE              PIC X(11).
               20  EXT-OP35-ATM-DETAILS.
                 25  EXT-OP35-ATM-VIS              PIC X(1).
                 25  EXT-OP35-ATM-LIM              PIC X(3).
                 25  EXT-OP35-ATM-LDTE             PIC X(11).
                 25  EXT-OP35-ATM-LAMT             PIC X(3).
               20  EXT-OP35-RP-DETAILS             OCCURS 3 TIMES.
                 25  EXT-OP35-RP-DAY               PIC X(2).
                 25  EXT-OP35-RP-AMT               PIC X(8).
                 25  EXT-OP35-RP-PID               PIC X(5).
                 25  EXT-OP35-RP-ACC               PIC X(9).
                 25  EXT-OP35-RP-DTE               PIC X(11).
             15  EXT-OP40-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP40-ACCNO                  PIC X(9).
               20  EXT-OP40-ACCTYPE                PIC X(25).
               20  EXT-OP40-PAGING-STATUS          PIC X(1).
                 88  EXT-OP40-PAGING-OFF           VALUE LOW-VALUES.
                 88  EXT-OP40-PAGING-FIRST         VALUE '1'.
                 88  EXT-OP40-PAGING-MIDDLE        VALUE '2'.
                 88  EXT-OP40-PAGING-LAST          VALUE '3'.
               20  EXT-OP40-DETAILS                OCCURS 8 TIMES.
                 25  EXT-OP40-DATE                 PIC X(11).
                 25  EXT-OP40-TIME                 PIC X(8).
                 25  EXT-OP40-AMNT                 PIC X(13).
                 25  EXT-OP40-DESC                 PIC X(25).
             15  EXT-OP50-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP50-XFER                   PIC X(9).
               20  EXT-OP50-FRM1                   PIC X(1).
               20  EXT-OP50-TO1                    PIC X(1).
               20  EXT-OP50-ACC1                   PIC X(9).
               20  EXT-OP50-DSC1                   PIC X(15).
               20  EXT-OP50-BAL1                   PIC X(13).
               20  EXT-OP50-FRM2                   PIC X(1).
               20  EXT-OP50-TO2                    PIC X(1).
               20  EXT-OP50-ACC2                   PIC X(9).
               20  EXT-OP50-DSC2                   PIC X(15).
               20  EXT-OP50-BAL2                   PIC X(13).
               20  EXT-OP50-FRM3                   PIC X(1).
               20  EXT-OP50-TO3                    PIC X(1).
               20  EXT-OP50-ACC3                   PIC X(9).
               20  EXT-OP50-DSC3                   PIC X(15).
               20  EXT-OP50-BAL3                   PIC X(13).
               20  EXT-OP50-FRM4                   PIC X(1).
               20  EXT-OP50-TO4                    PIC X(1).
               20  EXT-OP50-ACC4                   PIC X(9).
               20  EXT-OP50-DSC4                   PIC X(15).
               20  EXT-OP50-BAL4                   PIC X(13).
               20  EXT-OP50-FRM5                   PIC X(1).
               20  EXT-OP50-TO5                    PIC X(1).
               20  EXT-OP50-ACC5                   PIC X(9).
               20  EXT-OP50-DSC5                   PIC X(15).
               20  EXT-OP50-BAL5                   PIC X(13).
               20  EXT-OP50-FRM6                   PIC X(1).
               20  EXT-OP50-TO6                    PIC X(1).
               20  EXT-OP50-ACC6                   PIC X(9).
               20  EXT-OP50-DSC6                   PIC X(15).
               20  EXT-OP50-BAL6                   PIC X(13).
             15  EXT-OP60-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP60-OADDR1                 PIC X(25).
               20  EXT-OP60-OADDR2                 PIC X(25).
               20  EXT-OP60-OSTATE                 PIC X(2).
               20  EXT-OP60-OCNTRY                 PIC X(6).
               20  EXT-OP60-OPSTCDE                PIC X(6).
               20  EXT-OP60-OTELNO                 PIC X(12).
               20  EXT-OP60-NADDR1                 PIC X(25).
               20  EXT-OP60-NADDR2                 PIC X(25).
               20  EXT-OP60-NSTATE                 PIC X(2).
               20  EXT-OP60-NCNTRY                 PIC X(6).
               20  EXT-OP60-NPSTCDE                PIC X(6).
               20  EXT-OP60-NTELNO                 PIC X(12).
               20  EXT-OP60-NEMAIL                 PIC X(30).
               20  EXT-OP60-NSMAIL                 PIC X(1).
               20  EXT-OP60-NSEMAIL                PIC X(1).
             15  EXT-OP70-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP70-AMOUNT                 PIC X(7).
               20  EXT-OP70-RATE                   PIC X(7).
               20  EXT-OP70-TERM                   PIC X(5).
               20  EXT-OP70-PAYMENT                PIC X(9).
             15  EXT-OP80-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OP80-ADDR1                 PIC X(25).
               20  EXT-OP80-ADDR2                 PIC X(25).
               20  EXT-OP80-STATE                 PIC X(2).
               20  EXT-OP80-CNTRY                 PIC X(6).
               20  EXT-OP80-PSTCDE                PIC X(6).
               20  EXT-OP80-EMAIL                 PIC X(30).
               20  EXT-OP80-OPT1                  PIC X(1).
               20  EXT-OP80-OPT2                  PIC X(1).
             15  EXT-OPZZ-DATA REDEFINES EXT-OP-AREA.
               20  EXT-OPZZ-SEL1ID                 PIC X(1).
               20  EXT-OPZZ-SEL1IP                 PIC X(1).
               20  EXT-OPZZ-SEL1TX                 PIC X(40).
               20  EXT-OPZZ-SEL2ID                 PIC X(1).
               20  EXT-OPZZ-SEL2IP                 PIC X(1).
               20  EXT-OPZZ-SEL2TX                 PIC X(40).
               20  EXT-OPZZ-SEL3ID                 PIC X(1).
               20  EXT-OPZZ-SEL3IP                 PIC X(1).
               20  EXT-OPZZ-SEL3TX                 PIC X(40).
               20  EXT-OPZZ-SEL4ID                 PIC X(1).
               20  EXT-OPZZ-SEL4IP                 PIC X(1).
               20  EXT-OPZZ-SEL4TX                 PIC X(40).
               20  EXT-OPZZ-SEL5ID                 PIC X(1).
               20  EXT-OPZZ-SEL5IP                 PIC X(1).
               20  EXT-OPZZ-SEL5TX                 PIC X(40).
               20  EXT-OPZZ-SEL6ID                 PIC X(1).
               20  EXT-OPZZ-SEL6IP                 PIC X(1).
               20  EXT-OPZZ-SEL6TX                 PIC X(40).
               20  EXT-OPZZ-SEL7ID                 PIC X(1).
               20  EXT-OPZZ-SEL7IP                 PIC X(1).
               20  EXT-OPZZ-SEL7TX                 PIC X(40).
               20  EXT-OPZZ-SEL8ID                 PIC X(1).
               20  EXT-OPZZ-SEL8IP                 PIC X(1).
               20  EXT-OPZZ-SEL8TX                 PIC X(40).

      * $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm
