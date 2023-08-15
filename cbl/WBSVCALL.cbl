      *SET CICSECM(LINKAGE=YES)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WBSVCALL IS INITIAL.
      ******************************************************************
      ******************************************************************
      *+ This program accepts the web service call to pass the         *
      *+ received data area on to the appropriate program.             *
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      ******************************************************************
      *  WORKING STORAGE SECTION
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *  WORK AREAS
      ******************************************************************
       01  WA-WORKAREAS.
           05  WA-CALL-LENGTH          PIC S9(04)  COMP. 
           05  WA-CNT-1                PIC  9(05)  VALUE 1.
           05  WA-CNT-2                PIC  9(05)  VALUE 1.
       
       01  WC-CONSTANTS.
           05  WC-HELOWRLD-PROGRAM     PIC  X(08)  VALUE 'HELOWRLD'.
           05  WC-BAD-CALL-MSG         PIC  X(23)  VALUE
               'Unknown program called '.
           03  HTTP-HOST-CODE-PAGE     PIC X(8)    VALUE '037'.         
           03  HTTP-CHARSET-HDR        PIC X(14)   VALUE                
               'Accept-Charset'.                                        
           03  HTTP-HEADER-CONTENT     PIC X(12)   VALUE                
               'Content-Type'.                       

       01  WS-DATE-TIME-AREA.                                           
           05  WS-ABSTIME              PIC S9(15)  COMP-3 VALUE 0.     
           05  WS-START-EIBDATE        PIC 9(06).                       
           05  WS-START-EIBTIME        PIC 9(06).                       

       01  WS-SWITCHES.
           05  WS-ABEND                PIC X       VALUE LOW-VALUE.
               88  WS-FIRST-ABEND                  VALUE LOW-VALUE.
           05  WS-LINK                 PIC X       VALUE LOW-VALUE.
               88  WS-LINK-TRUE                    VALUE HIGH-VALUE.

       01  WS-WORK.                                                     
           03  WS-SYSTEM-DATE          PIC X(10)   VALUE SPACES.        
           03  WS-SYSTEM-TIME          PIC X(8)    VALUE SPACES.        
                                                                        
           03  WS-RESP-CODE            PIC S9(8)   BINARY VALUE +0.     
           03  WS-RESP-CODE2           PIC S9(8)   BINARY VALUE +0.     
           03  WS-RESP-DISP            PIC  9(08)  VALUE 0.
           03  WS-RESP-DISP2           PIC  9(08)  VALUE 0.
           03  WS-ERROR-CODE           PIC X(4)    VALUE ZEROES.        
           03  WS-ERROR-MESSAGE        PIC X(80)   VALUE SPACES.        
                                                                        
           03  WS-CICS-APPLID          PIC X(8)    VALUE SPACES.
           03  WS-CLIENT-NAME          PIC X(80)   VALUE SPACES.        
           03  WS-CLIENT-ADDR          PIC X(15)   VALUE SPACES.        
           03  WS-SERVER-NAME          PIC X(80)   VALUE SPACES.        
           03  WS-SERVER-ADDR          PIC X(15)   VALUE SPACES.        
           03  WS-TCPIP-SERVICE        PIC X(8)    VALUE SPACES.        
           03  WS-PORT-NUMBER          PIC X(5)    VALUE SPACES.        
                                                                        
           03  WS-CLIENT-NAME-LEN      PIC S9(8)   BINARY VALUE +0.     
           03  WS-CLIENT-ADDR-LEN      PIC S9(8)   BINARY VALUE +0.     
           03  WS-SERVER-NAME-LEN      PIC S9(8)   BINARY VALUE +0.     
           03  WS-SERVER-ADDR-LEN      PIC S9(8)   BINARY VALUE +0.     
           03  WS-SSL-TYPE             PIC S9(8)   BINARY VALUE +0.     
           03  WS-AUTH-TYPE            PIC S9(8)   BINARY VALUE +0.     
                                                                        
           03  WS-HTTP-METH            PIC X(80)   VALUE SPACES.        
           03  WS-HTTP-VERS            PIC X(80)   VALUE SPACES.        
           03  WS-HTTP-PATH            PIC X(50)   VALUE SPACES.        
           03  WS-HTTP-QSTR            PIC X(500)  VALUE SPACES.        
                                                                        
           03  WS-HTTP-METH-LEN        PIC S9(8)   BINARY VALUE +0.     
           03  WS-HTTP-VERS-LEN        PIC S9(8)   BINARY VALUE +0.     
           03  WS-HTTP-PATH-LEN        PIC S9(8)   BINARY VALUE +0.     
           03  WS-HTTP-QSTR-LEN        PIC S9(8)   BINARY VALUE +0.     
                                                                        
           03  WS-HTTP-HDR-CONTENT     PIC X(25)   VALUE SPACES.        
           03  WS-HTTP-HDR-NAME        PIC X(80)   VALUE SPACES.        
           03  WS-HTTP-HDR-VALUE       PIC X(80)   VALUE SPACES.        
           03  WS-HTTP-HDR-NAME-LEN    PIC S9(8)   BINARY VALUE +0.     
           03  WS-HTTP-HDR-VALUE-LEN   PIC S9(8)   BINARY VALUE +0.     
                                                                        
           03  WS-CLIENT-CHARSET       PIC X(40)   VALUE SPACES.        
           03  WS-DOCUMENT-TOKEN       PIC X(16)   VALUE SPACES.        
                                                                        
           03  WS-RCVD-MSG-LEN         PIC S9(8)   BINARY VALUE +0.     
           03  WS-RCVD-MSG-MAXLEN      PIC S9(8)   BINARY VALUE +0. 
           03  WS-RCVD-MSG             PIC X(5000) VALUE SPACES.        
           03  WS-RCVD-MSG-CLN         PIC X(5000) VALUE SPACES.    

           03  WS-REPLY-MSG-LEN        PIC S9(8)   BINARY VALUE +0. 
           03  WS-REPLY-MSG            PIC X(5000) VALUE SPACES. 
  
       01  WBSVCALL-PROGRAM.                                            
           03  WBSVCALL-PGM-HDR        PIC X(4)    VALUE SPACES.
           03  WBSVCALL-PGM-NM         PIC X(8)    VALUE SPACES.

       01  WS-DFHCOMMAREA              PIC X(5000) VALUE SPACES.

       LINKAGE SECTION.

      **************************************************************
      *  PROCEDURE DIVISION.
      **************************************************************

       PROCEDURE DIVISION.

       S010-000-MAIN-LOGIC SECTION.

           PERFORM S100-000-WEB-RECEIVE.

           PERFORM S200-000-LINK-OUT    

           PERFORM S300-000-WEB-SEND.


       S010-RETURN-TO-CICS.
           
           EXEC CICS RETURN                                           
           END-EXEC.     

       S010-999-EXIT.
           EXIT.


       S100-000-WEB-RECEIVE SECTION.
      *
      *****************************************************************
      *?  S100-000-WEB-RECEIVE SECTION                                *
      *+                                                              *
      *+  Initialize and then WEB EXTRACT, RECEIVE, AND CREATE DOC    *
      *****************************************************************
      *
           MOVE EIBTIME                  TO WS-START-EIBTIME.           
           MOVE EIBDATE                  TO WS-START-EIBDATE.           
                                                                      
           EXEC CICS ASKTIME                                          
                ABSTIME (WS-ABSTIME)                                  
           END-EXEC.                                                  
                                                                      
           EXEC CICS HANDLE ABEND
                LABEL   (S999-000-ERROR-SEND)                           
           END-EXEC.                                                  

           INITIALIZE WS-WORK.                                    
           MOVE 'iso-8859-1'             TO WS-CLIENT-CHARSET.          

           MOVE  ZEROES                  TO WS-ERROR-CODE.

           EXEC CICS ASSIGN                                             
                     APPLID   (WS-CICS-APPLID)                          
           END-EXEC.    

           EXEC CICS FORMATTIME                                         
                     ABSTIME  (WS-ABSTIME)                              
                     YYYYMMDD (WS-SYSTEM-DATE) DATESEP('/')             
                     TIME     (WS-SYSTEM-TIME) TIMESEP(':')             
           END-EXEC.                                          

           MOVE LENGTH OF WS-CLIENT-NAME TO WS-CLIENT-NAME-LEN.         
           MOVE LENGTH OF WS-CLIENT-ADDR TO WS-CLIENT-ADDR-LEN.         
           MOVE LENGTH OF WS-SERVER-NAME TO WS-SERVER-NAME-LEN.         
           MOVE LENGTH OF WS-SERVER-ADDR TO WS-SERVER-ADDR-LEN.         
                                                                        
           EXEC CICS EXTRACT TCPIP                                      
                     AUTHENTICATE (WS-AUTH-TYPE)                        
                     CLIENTADDR   (WS-CLIENT-ADDR)                      
                     CADDRLENGTH  (WS-CLIENT-ADDR-LEN)                  
                     CLIENTNAME   (WS-CLIENT-NAME)                      
                     CNAMELENGTH  (WS-CLIENT-NAME-LEN)                  
                     SERVERNAME   (WS-SERVER-NAME)                      
                     SNAMELENGTH  (WS-SERVER-NAME-LEN)                  
                     SERVERADDR   (WS-SERVER-ADDR)                      
                     SADDRLENGTH  (WS-SERVER-ADDR-LEN)                  
                     SSLTYPE      (WS-SSL-TYPE)                         
                     TCPIPSERVICE (WS-TCPIP-SERVICE)                    
                     PORTNUMBER   (WS-PORT-NUMBER)                      
                     RESP         (WS-RESP-CODE)                        
                     RESP2        (WS-RESP-CODE2)                       
           END-EXEC.                                          
                                                                        
           IF  WS-RESP-CODE NOT = DFHRESP(NORMAL)                       
               MOVE '1001'               TO WS-ERROR-CODE              
               MOVE 'EXTRACT TCPIP COMMAND ERROR'                       
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR                                   
           END-IF.                                                      
                                                                        
           EVALUATE TRUE                                                
             WHEN WS-CLIENT-NAME-LEN > LENGTH OF WS-CLIENT-NAME         
              MOVE LENGTH OF WS-CLIENT-NAME TO WS-CLIENT-NAME-LEN     
             WHEN WS-CLIENT-NAME-LEN = 0                                
              MOVE 1                        TO WS-CLIENT-NAME-LEN       
           END-EVALUATE.                                                
                                                                        
           EVALUATE TRUE                                                
             WHEN WS-SERVER-NAME-LEN > LENGTH OF WS-SERVER-NAME         
              MOVE LENGTH OF WS-SERVER-NAME TO WS-SERVER-NAME-LEN     
             WHEN WS-SERVER-NAME-LEN = 0                                
              MOVE 1                        TO WS-SERVER-NAME-LEN       
           END-EVALUATE.                                                
                                                                        
      **** Obtain the http path - the portion of the URL that comes     
      **** between the hostname/port number and the question mark       

      **** The query string is everything that comes after the ? in     
      **** the URL. This is a common way to pass parameters to server   
      **** applications.                                                   
           MOVE LENGTH OF WS-HTTP-METH   TO WS-HTTP-METH-LEN.           
           MOVE LENGTH OF WS-HTTP-VERS   TO WS-HTTP-VERS-LEN.           
           MOVE LENGTH OF WS-HTTP-PATH   TO WS-HTTP-PATH-LEN.           
           MOVE LENGTH OF WS-HTTP-QSTR   TO WS-HTTP-QSTR-LEN.           
                                                                        
           EXEC CICS WEB EXTRACT                                        
                     HTTPMETHOD   (WS-HTTP-METH)                        
                     METHODLENGTH (WS-HTTP-METH-LEN)                    
                     HTTPVERSION  (WS-HTTP-VERS)                        
                     VERSIONLEN   (WS-HTTP-VERS-LEN)                    
                     PATH         (WS-HTTP-PATH)                        
                     PATHLENGTH   (WS-HTTP-PATH-LEN)                    
                     QUERYSTRING  (WS-HTTP-QSTR)                        
                     QUERYSTRLEN  (WS-HTTP-QSTR-LEN)                    
                     RESP         (WS-RESP-CODE)                        
                     RESP2        (WS-RESP-CODE2)                       
           END-EXEC.                                          
                                                                        
           IF  WS-RESP-CODE NOT = DFHRESP(NORMAL)                       
               MOVE '1002'               TO WS-ERROR-CODE               
               MOVE 'WEB EXTRACT COMMAND ERROR'                         
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR                                   
           END-IF.                                                      

      * WS-HTTP-QSTR WILL CONTAIN THE PROGRAM NAME TO CALL
      * WS-RCVD-MSG WILL CONTAIN THE DATA TO PASS TO THE CICS PROGRAM
      
           IF  WS-HTTP-QSTR = SPACES                                    
               MOVE '1003'               TO WS-ERROR-CODE               
               MOVE 'QUERY STRING EMPTY'                                
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR    
           ELSE
               MOVE WS-HTTP-QSTR(1:12)   TO WBSVCALL-PROGRAM
           END-IF.
                                                                        
           MOVE +5000                    TO WS-RCVD-MSG-MAXLEN.
                                                                        
           EXEC CICS WEB RECEIVE                                        
                         INTO         (WS-RCVD-MSG)                     
                         LENGTH       (WS-RCVD-MSG-LEN)              
                         MAXLENGTH    (WS-RCVD-MSG-MAXLEN)              
                         CHARACTERSET (WS-CLIENT-CHARSET)               
                         RESP         (WS-RESP-CODE)                    
                         RESP2        (WS-RESP-CODE2)                   
           END-EXEC.                                      
                                                                        
      *    IF  WS-RESP-CODE = DFHRESP(NOTFND)                           
      *        GO TO S100-PARAGRAPH-CONTINUED                          
      *    END-IF.                                                      
                                                                        
           IF  WS-RESP-CODE NOT = DFHRESP(NORMAL)                       
               MOVE '1004'               TO WS-ERROR-CODE               
               MOVE 'WEB RECEIVE COMMAND ERROR'                         
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR                                   
           END-IF.                                                      
                                                                        
           IF  WS-RCVD-MSG = SPACES                                     
               MOVE '1005'               TO WS-ERROR-CODE               
               MOVE 'RECEIVE MESSAGE EMPTY'                      
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR    
           END-IF.
                                                                        
       S100-PARAGRAPH-CONTINUED.                                       
                                                                        
           INSPECT WS-RCVD-MSG REPLACING ALL X'00' BY SPACES.           

      * This will convert %20 to a one character space
           PERFORM UNTIL WA-CNT-1 > WS-RCVD-MSG-LEN OR 5000          
             IF WS-RCVD-MSG(WA-CNT-1:3) = '%20'                         
                MOVE ' '                 TO WS-RCVD-MSG-CLN(WA-CNT-2:1)
                ADD 2                    TO WA-CNT-1                    
             ELSE                                                   
                MOVE WS-RCVD-MSG(WA-CNT-1:1) 
                                         TO WS-RCVD-MSG-CLN(WA-CNT-2:1)
             END-IF                                                 

             ADD 1                       TO WA-CNT-1 
                                            WA-CNT-2
           END-PERFORM.

      **** Create the document into which we will insert the http       
      **** protocol header and html/text/xml that constitutes the       
      **** reply to be sent to the requestor.                           
                                                                        
           EXEC CICS DOCUMENT CREATE                                    
                     DOCTOKEN (WS-DOCUMENT-TOKEN)                       
                     RESP     (WS-RESP-CODE)                            
                     RESP2    (WS-RESP-CODE2)                           
           END-EXEC.                                          
                                                                        
           IF  WS-RESP-CODE NOT = DFHRESP(NORMAL)                       
               MOVE '1006'               TO WS-ERROR-CODE               
               MOVE 'CREATE DOCUMENT COMMAND ERROR'                     
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR                                   
           END-IF.                                                      

       S100-999-EXIT.
           EXIT.


       S200-000-LINK-OUT SECTION.
      *
      *****************************************************************
      *?  S200-000-LINK-OUT SECTION                                   *
      *+                                                              *
      *+  Link to appropriate program with needed linkage.            *
      *****************************************************************
      *
           MOVE HIGH-VALUE               TO WS-LINK.

           EVALUATE TRUE
            
             WHEN WBSVCALL-PGM-NM = WC-HELOWRLD-PROGRAM
               MOVE WS-RCVD-MSG-CLN      TO WS-DFHCOMMAREA             
               MOVE 50                   TO WA-CALL-LENGTH          
             WHEN OTHER
               MOVE WC-BAD-CALL-MSG      TO WS-ERROR-MESSAGE
               PERFORM S999-000-ERROR-SEND   
               
           END-EVALUATE.

           EXEC CICS LINK
                PROGRAM  (WBSVCALL-PGM-NM)
                COMMAREA (WS-DFHCOMMAREA)
                LENGTH   (WA-CALL-LENGTH)
           END-EXEC.

           IF EIBRESP = 0
               MOVE WS-DFHCOMMAREA(1:WA-CALL-LENGTH) 
                                         TO WS-REPLY-MSG
           ELSE
               PERFORM S999-000-ERROR-SEND
           END-IF.

           IF WS-RCVD-MSG-CLN = WS-REPLY-MSG
               MOVE 'ISSUE WITH LINK - INPUT/OUTPUT MSG THE SAME'
                                         TO WS-ERROR-MESSAGE
               PERFORM S999-000-ERROR-SEND
           END-IF.

           MOVE LOW-VALUE TO WS-LINK.
                                                
       S200-999-EXIT.
           EXIT.


       S300-000-WEB-SEND SECTION.
      *
      *****************************************************************
      *?  S300-000-WEB-SEND SECTION                                   *
      *+                                                              *
      *+  Send return response with WEB DOC INSERT, WRITE, and SEND.  * 
      *****************************************************************
      *
           IF WS-ERROR-MESSAGE NOT = SPACES
               MOVE WS-ERROR-MESSAGE     TO WS-REPLY-MSG
           END-IF.
                                                                        
           MOVE LENGTH OF WS-REPLY-MSG   TO WS-REPLY-MSG-LEN.     
                                                                        
           EXEC CICS DOCUMENT INSERT                                    
                     DOCTOKEN     (WS-DOCUMENT-TOKEN)                   
                     TEXT         (WS-REPLY-MSG)                    
                     LENGTH       (WS-REPLY-MSG-LEN)                
                     HOSTCODEPAGE (HTTP-HOST-CODE-PAGE)                 
                     RESP         (WS-RESP-CODE)                        
                     RESP2        (WS-RESP-CODE2)                       
           END-EXEC.                                          
                                                                        
           IF  WS-RESP-CODE NOT = DFHRESP(NORMAL)                       
               MOVE '3001'               TO WS-ERROR-CODE               
               MOVE 'DOCUMENT INSERT COMMAND ERROR'                     
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR                                   
           END-IF.                                                      

           MOVE HTTP-HEADER-CONTENT      TO WS-HTTP-HDR-NAME.           
           MOVE LENGTH OF HTTP-HEADER-CONTENT                           
                                         TO WS-HTTP-HDR-NAME-LEN.       
           MOVE 'text/plain'             TO WS-HTTP-HDR-CONTENT.        
           MOVE WS-HTTP-HDR-CONTENT      TO WS-HTTP-HDR-VALUE.          
           MOVE LENGTH OF WS-HTTP-HDR-CONTENT                           
                                         TO WS-HTTP-HDR-VALUE-LEN.      

           EXEC CICS WEB WRITE                                          
                     HTTPHEADER  (WS-HTTP-HDR-NAME)                     
                     NAMELENGTH  (WS-HTTP-HDR-NAME-LEN)                 
                     VALUE       (WS-HTTP-HDR-VALUE)                    
                     VALUELENGTH (WS-HTTP-HDR-VALUE-LEN)                
                     RESP        (WS-RESP-CODE)                         
                     RESP2       (WS-RESP-CODE2)                        
           END-EXEC.                                          
                                                                        
           IF  WS-RESP-CODE NOT = DFHRESP(NORMAL)                       
               MOVE '3002'               TO WS-ERROR-CODE               
               MOVE 'WEB WRITE COMMAND ERROR'                           
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR                                   
           END-IF.  
                                                                        
      **** Send the reply constructed in the document back to           
      **** the requester.                                               
                                                                        
           EXEC CICS WEB SEND                                           
                     DOCTOKEN     (WS-DOCUMENT-TOKEN)                   
                     CLNTCODEPAGE (WS-CLIENT-CHARSET)                   
                     RESP         (WS-RESP-CODE)                        
                     RESP2        (WS-RESP-CODE2)                       
           END-EXEC.                                          
                                                                        
           IF  WS-RESP-CODE NOT = DFHRESP(NORMAL)                       
               MOVE '3003'               TO WS-ERROR-CODE               
               MOVE 'WEB SEND COMMAND ERROR'                            
                                         TO WS-ERROR-MESSAGE            
               PERFORM S900-000-ERROR                                   
           END-IF.                                                      
                                                                        
       S300-999-EXIT.
           EXIT.


       S900-000-ERROR SECTION.
      *
      *****************************************************************
      *?  S900-000-ERROR SECTION                                      *
      *+                                                              *
      *+  Display error record to SSTM and then exit to CICS          *
      *****************************************************************
      *
           DISPLAY 'WBSVCALL S900-ERROR - ' WS-ERROR-CODE.
           DISPLAY WS-ERROR-MESSAGE.

           GO TO S010-RETURN-TO-CICS.
      *                                                               
       S900-EXIT.                                                     
            EXIT.                                                     


       S999-000-ERROR-SEND SECTION.
      *
      *****************************************************************
      *?  S999-000-ABEND SECTION                                      * 
      *+                                                              *
      *+  Display abend record to SSTM, send reply, and exit to CICS  * 
      *****************************************************************
      *
           DISPLAY 'WBSVCALL S999-ERROR-SEND'.

      * If abend was with LINK then try to return message to caller
           IF WS-FIRST-ABEND AND WS-LINK-TRUE
               MOVE HIGH-VALUE           TO WS-ABEND
               MOVE EIBRESP              TO WS-RESP-DISP
               MOVE EIBRESP2             TO WS-RESP-DISP2
               EVALUATE TRUE
                 WHEN EIBFN = X'0E02' 
                   STRING 'ERROR LINKING TO PROGRAM ' WBSVCALL-PGM-NM   
                     ' EIBRESP ' WS-RESP-DISP 
                     ' EIBRESP2 ' WS-RESP-DISP2
                     DELIMITED BY SIZE
                                       INTO WS-ERROR-MESSAGE
                 WHEN OTHER
                   CONTINUE
               END-EVALUATE
               PERFORM S300-000-WEB-SEND
           END-IF.

           IF WS-ERROR-MESSAGE NOT = SPACES
               DISPLAY WS-ERROR-MESSAGE 
           END-IF.
           
           GO TO S010-RETURN-TO-CICS.
      *                                                               
       S999-EXIT.                                                     
            EXIT.                                                     
