      *****************************************************************
      * PROGRAM:    COBWEB                                          *
      * AUTHOR:     MILÁN ATANÁZ MAJOR                                *
      * DATE:       01-MAR-2025                                       *
      * PURPOSE:    IMPLEMENTS A BASIC HTTP SERVER IN COBOL           *
      *             SERVING STATIC FILES                              *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBAPPLICATION.
      *****************************************************************
      * ENVIRONMENT DIVISION SPECIFIES PHYSICAL ASPECTS OF PROGRAM    *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    CONFIG FILE CONTAINS SERVER SETTINGS                       *
           SELECT CONFIG-FILE ASSIGN TO DYNAMIC CONFIG-FILE-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
      *    REQUEST-FILE IS USED TO READ FILES TO BE SERVED            *
           SELECT REQUEST-FILE ASSIGN TO DYNAMIC FULL-PATH
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
      *****************************************************************
      * DATA DIVISION DECLARES VARIABLES AND FILE STRUCTURES          *
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  REQUEST-FILE.
       01  REQUEST-RECORD    PIC X(1).
       FD  CONFIG-FILE.
       01  CONFIG-RECORD    PIC X(100). 
       WORKING-STORAGE SECTION.
      *    SERVER CONFIGURATION VARIABLES                             *
       01  CONFIG-FILE-PATH  PIC X(255).
          88 CONFIG-FILE-DEFAULT
          VALUE "/etc/cobweb/cobweb.conf".
       01  CONFIG-VARIABLE   PIC X(20).
       01  CONFIG-VALUE      PIC X(80).
       01  ROOT-FOLDER       PIC X(255) VALUE "/var/www/html/".
       01  DEFAULT-PAGE      PIC X(11) VALUE "index.html".
       01  PORT-NUMBER       PIC 9(5) VALUE 80.
      *    FILE HANDLING VARIABLES                                    *
       01  FULL-PATH         PIC X(512) VALUE SPACES.
       01  FILE-BUFFER       PIC X(1).
       01  FILE-SIZE         PIC S9(9) COMP-5 VALUE 0.
       01  SEND-LENGTH       PIC S9(5) COMP-5 VALUE 0.
       01  WS-CONTENT-LEN    PIC 9(9).
       01  CONTENT-LENGTH    PIC X(20).
       01  HTTP-HEADER       PIC X(256).
       01  WS-RETURN-CODE    PIC S9(4) COMP-5 VALUE 0.
       01  WS-FILE-STATUS    PIC XX.
       01  FILE-OK           PIC X VALUE 'N'.
       01  I                 PIC 9(6).
      *    MIME TYPE HANDLING                                         *
       01  MIME-TYPE         PIC X(50).
       01  FILE-EXTENSION    PIC X(10).
       01  TRIMMED-EXTENSION PIC X(10).
       01  TRIMMED-LOOKUP    PIC X(10).
      *    HTTP REQUEST/RESPONSE HANDLING                             *
       01  BUFFER            PIC X(1024).
       01  REQUEST-METHOD    PIC X(10).
       01  REQUEST-PATH      PIC X(255).
       01  RUNNING           PIC X VALUE 'Y'.
      *    SOCKET HANDLING VARIABLES                                  *
       01  BACKLOG           PIC S9(2) COMP-5 VALUE 5.
       01  CLIENT-ADDR       PIC X(16).
       01  CLIENT-ADDR-LEN   PIC S9(4) COMP-5 VALUE 16.
       01  AF-INET           PIC S9(4) COMP-5 VALUE 2.
       01  SOCK-STREAM       PIC S9(4) COMP-5 VALUE 1.
       01  IP-PROTO          PIC S9(4) COMP-5 VALUE 0.
       01  SERVER-SOCKET     PIC S9(4) COMP-5 VALUE 0.
       01  CLIENT-SOCKET     PIC S9(4) COMP-5 VALUE 0.
      *    SOCKET ADDRESS STRUCTURE                                   *
       01  MY-ADDR.
           05  FAMILY        PIC S9(4) COMP-5 VALUE 2.
           05  FILLER.
               07  MY-PORT   PIC S9(4) COMP.  *> network byte order
               07  INET-ADDR PIC X(4) VALUE LOW-VALUE.
               07  FILLER    PIC X(8) VALUE LOW-VALUE.
      *    ERROR HANDLING                                             *
       01  ERROR-CODE        PIC 9(3).
       01  RESPONSE          PIC X(4096) VALUE SPACES.
      *    DEFAULT MIME TYPE                                          *
       01  DEFAULT-MIME      PIC X(24) 
          VALUE "application/octet-stream".
      *    MIME TYPE LOOKUP VARIABLES                                 *
       01  NORMALIZED-EXT     PIC X(10).
       01  NORMALIZED-LOOKUP  PIC X(10).
       01  ACTUAL-LEN         PIC 9(2).
       01  LOOKUP-LEN         PIC 9(2).
      *    MIME TYPE TABLE                                            *
       01  MIME-TYPE-LOOKUP.
           05  MIME-COUNT    PIC 9(2) VALUE 36.
           05  MIME-ENTRY OCCURS 36 TIMES.
               10  EXTENSION      PIC X(10).
               10  MIME           PIC X(50).
      *    FILE CONTENT BUFFER                                        *
       01  FILE-CONTENT        PIC X(102400) VALUE SPACES.
       01  BYTES-READ          PIC 9(4) COMP VALUE 0.
       01  TOTAL-BYTES-READ    PIC 9(9) COMP VALUE 0.
       01  CONTENT-LEN-STR     PIC Z(9)9.
      *01  BYTES-TO-READ       PIC S9(4) COMP VALUE 1024.
       78  LF                  VALUE x'0d0a'.
      *****************************************************************
      * PROCEDURE DIVISION CONTAINS PROGRAM LOGIC                     *
      *****************************************************************
       PROCEDURE DIVISION.
      *    MAIN PROGRAM LOGIC                                         *
       MAIN-LOGIC.
           PERFORM INITIALIZE-MIME-TYPE-LOOKUP
           PERFORM READ-CONFIG-FILE.
           PERFORM INITIALIZE-SERVER
           PERFORM ACCEPT-CONNECTIONS
           PERFORM CLEANUP
           STOP RUN.
      *    INCLUDE MIME TYPES FROM EXTERNAL FILE                      *
       COPY "sources/mime-types.cpy".
      *****************************************************************
      * READ SERVER CONFIGURATION FROM FILE                           *
      *****************************************************************
       READ-CONFIG-FILE.
          ACCEPT CONFIG-FILE-PATH FROM ENVIRONMENT "COBWEB_CONFIG"
            ON EXCEPTION
               SET CONFIG-FILE-DEFAULT TO TRUE
          END-ACCEPT
          OPEN INPUT CONFIG-FILE
          IF WS-FILE-STATUS NOT = "00"
              DISPLAY "Error opening config file: " WS-FILE-STATUS
              STOP RUN
          END-IF
      *    READ CONFIG FILE UNTIL END OF FILE                         *
          PERFORM UNTIL WS-FILE-STATUS = "10"
              READ CONFIG-FILE INTO CONFIG-RECORD
                  AT END
                      MOVE "10" TO WS-FILE-STATUS
                  NOT AT END
                      DISPLAY "Read config line: " CONFIG-RECORD
                      PERFORM PROCESS-CONFIG-RECORD
              END-READ
          END-PERFORM
          CLOSE CONFIG-FILE.           
      *****************************************************************
      * PROCESS CONFIGURATION RECORD                                  *
      *****************************************************************
       PROCESS-CONFIG-RECORD.
      *    PARSE CONFIG LINE INTO VARIABLE AND VALUE                  *
          UNSTRING CONFIG-RECORD DELIMITED BY "="
              INTO CONFIG-VARIABLE CONFIG-VALUE
          END-UNSTRING
          
          DISPLAY "Config Variable: " CONFIG-VARIABLE
          DISPLAY "Config Value: " CONFIG-VALUE
          
      *    SET APPROPRIATE CONFIGURATION VARIABLES                    *
          EVALUATE CONFIG-VARIABLE
              WHEN "FULL-PATH"
                  MOVE CONFIG-VALUE TO FULL-PATH
              WHEN "PORT-NUMBER"
                  MOVE FUNCTION NUMVAL(CONFIG-VALUE) TO PORT-NUMBER
              WHEN OTHER
                  CONTINUE
          END-EVALUATE.
      *****************************************************************
      * INITIALIZE WEB SERVER                                         *
      *****************************************************************
       INITIALIZE-SERVER.
      *    CONVERT PORT NUMBER TO NETWORK BYTE ORDER                  *
           MOVE PORT-NUMBER TO MY-PORT OF MY-ADDR
           DISPLAY "Starting COBOL Web Server on port " PORT-NUMBER
           DISPLAY "Serving files from: " ROOT-FOLDER
           
      *    CREATE TCP SOCKET                                          *
           CALL "socket" USING BY VALUE AF-INET
               BY VALUE SOCK-STREAM
               BY VALUE IP-PROTO
               RETURNING SERVER-SOCKET
           END-CALL
           
           IF SERVER-SOCKET < 0
               DISPLAY "Error creating socket."
               MOVE 'N' TO RUNNING
               PERFORM CLEANUP
               STOP RUN
           END-IF
           
      *    BIND SOCKET TO PORT                                        *
           CALL "bind" USING BY VALUE SERVER-SOCKET
               BY REFERENCE MY-ADDR
               BY VALUE LENGTH OF MY-ADDR
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE NOT = 0
               DISPLAY "Error binding socket. Return: " WS-RETURN-CODE
               MOVE 'N' TO RUNNING
               PERFORM CLEANUP
               STOP RUN
           ELSE
               DISPLAY "Socket successfully bound."
           END-IF
           
      *    START LISTENING FOR CONNECTIONS                            *
           CALL "listen" USING BY VALUE SERVER-SOCKET
               BY VALUE BACKLOG
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE NOT = 0
               DISPLAY "Error listening on socket."
               MOVE 'N' TO RUNNING
               PERFORM CLEANUP
               STOP RUN
           END-IF
           
           DISPLAY "Listening for connections.".
      *****************************************************************
      * ACCEPT AND PROCESS CLIENT CONNECTIONS                         *
      *****************************************************************
       ACCEPT-CONNECTIONS.
           PERFORM UNTIL RUNNING = 'N'
               DISPLAY "Waiting for client connection..."
      *        ACCEPT NEW CONNECTION                                  *
               CALL "accept" USING BY VALUE SERVER-SOCKET
                   BY REFERENCE CLIENT-ADDR
                   BY REFERENCE CLIENT-ADDR-LEN
                   RETURNING CLIENT-SOCKET
               END-CALL
               
               IF CLIENT-SOCKET < 0
                   DISPLAY "Error accepting connection."
               ELSE
                   PERFORM PROCESS-REQUEST
      *            CLOSE CLIENT SOCKET AFTER PROCESSING               *
                   CALL "close" USING BY VALUE CLIENT-SOCKET
                       RETURNING WS-RETURN-CODE
                   END-CALL
                   
                   IF WS-RETURN-CODE NOT = 0
                       DISPLAY "Error closing client socket."
                   END-IF
               END-IF
           END-PERFORM.
      *****************************************************************
      * PROCESS HTTP REQUEST                                          *
      *****************************************************************
       PROCESS-REQUEST.
      *    CLEAR BUFFER AND RECEIVE DATA                              *
           MOVE SPACES TO BUFFER
           CALL "recv" USING BY VALUE CLIENT-SOCKET
               BY REFERENCE BUFFER
               BY VALUE LENGTH OF BUFFER
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error receiving data from client."
           ELSE 
               IF WS-RETURN-CODE = 0
                   DISPLAY "Client closed the connection."
               ELSE
                   DISPLAY "Received " 
                       WS-RETURN-CODE " bytes from client."
                   PERFORM PARSE-REQUEST
                   
      *            HANDLE REQUEST BASED ON METHOD                     *
                   IF REQUEST-METHOD = "GET"
                       PERFORM SERVE-FILE
                   ELSE
                       MOVE 405 TO ERROR-CODE
                       PERFORM HTTPERROR
                   END-IF
               END-IF
           END-IF.
      *****************************************************************
      * PARSE HTTP REQUEST                                            *
      *****************************************************************
       PARSE-REQUEST.
      *    EXTRACT METHOD AND PATH FROM REQUEST                       *
           UNSTRING BUFFER DELIMITED BY SPACE
               INTO REQUEST-METHOD REQUEST-PATH
           END-UNSTRING
           
      *    STRIP QUERY STRING IF PRESENT                              *
           UNSTRING REQUEST-PATH DELIMITED BY "?"
               INTO REQUEST-PATH
           END-UNSTRING
           
           DISPLAY "Request Method: " REQUEST-METHOD
           DISPLAY "Request Path: " REQUEST-PATH
           PERFORM SANITIZE-REQUEST-PATH.
      *****************************************************************
      * SANITIZE REQUEST PATH FOR SECURITY                            *
      *****************************************************************
       SANITIZE-REQUEST-PATH.
      *    ENSURE PATH BEGINS WITH SLASH                              *
           IF REQUEST-PATH(1:1) = "/" 
               CONTINUE
           ELSE    
               STRING "/" REQUEST-PATH DELIMITED BY SPACE
                   INTO REQUEST-PATH
               END-STRING
           END-IF
           
      *    PREVENT DIRECTORY TRAVERSAL ATTACKS                        *
           INSPECT REQUEST-PATH REPLACING ALL ".." BY "xx"
           DISPLAY "Sanitized Path: " REQUEST-PATH.
      *****************************************************************
      * SERVE REQUESTED FILE                                          *
      *****************************************************************
       SERVE-FILE.
      *    USE DEFAULT PAGE IF ROOT REQUESTED                         *
           IF REQUEST-PATH = "/" OR REQUEST-PATH = SPACES
               MOVE DEFAULT-PAGE TO REQUEST-PATH
           END-IF
           
      *    BUILD FULL PATH TO FILE                                    *
           MOVE SPACES TO FULL-PATH
           STRING ROOT-FOLDER REQUEST-PATH
               DELIMITED BY SPACE
               INTO FULL-PATH
           END-STRING
           
           INSPECT FULL-PATH REPLACING ALL LOW-VALUES BY SPACES
           DISPLAY "Attempting to serve: " FULL-PATH
           
      *    DETERMINE FILE TYPE                                        *
           PERFORM GET-FILE-EXTENSION
           
      *    ATTEMPT TO OPEN REQUESTED FILE                             *
           MOVE "N" TO FILE-OK
           OPEN INPUT REQUEST-FILE
           
           IF WS-FILE-STATUS = "00"
               MOVE "Y" TO FILE-OK
               PERFORM HANDLE-FILE
               CLOSE REQUEST-FILE
           ELSE
               DISPLAY "File open failed: " WS-FILE-STATUS
               MOVE 404 TO ERROR-CODE
               PERFORM HTTPERROR
           END-IF.
      *****************************************************************
      * EXTRACT FILE EXTENSION FROM PATH                              *
      *****************************************************************
       GET-FILE-EXTENSION.
           MOVE SPACES TO FILE-EXTENSION
      *    SCAN BACKWARDS TO FIND EXTENSION                           *
           PERFORM VARYING I FROM LENGTH OF REQUEST-PATH BY -1 
               UNTIL I < 1 OR REQUEST-PATH(I:1) = "."
               CONTINUE
           END-PERFORM
           
           IF I > 0 AND REQUEST-PATH(I:1) = "."
               MOVE REQUEST-PATH(I:) TO FILE-EXTENSION
               DISPLAY "Extracted File Extension: " FILE-EXTENSION
           ELSE
               DISPLAY "No extension found in REQUEST: " REQUEST-PATH
           END-IF
           
           PERFORM DETERMINE-MIME-TYPE.

      *****************************************************************
      * DETERMINE MIME TYPE FROM FILE EXTENSION                       *
      *****************************************************************
       DETERMINE-MIME-TYPE.
      *    DISPLAY DEBUGGING INFO ABOUT FILE EXTENSION                *
       DISPLAY "Determining MIME: '" FILE-EXTENSION "' (Length: " 
           FUNCTION LENGTH(FILE-EXTENSION) ")"
      *    SET DEFAULT MIME TYPE INITIALLY                            *
       MOVE DEFAULT-MIME TO MIME-TYPE
      *    NORMALIZE EXTENSION FOR COMPARISON                         *
       MOVE FILE-EXTENSION TO NORMALIZED-EXT
       INSPECT NORMALIZED-EXT REPLACING TRAILING SPACES BY LOW-VALUE
       COMPUTE ACTUAL-LEN = FUNCTION LENGTH(NORMALIZED-EXT)
      *    SEARCH THROUGH MIME TYPE TABLE FOR MATCHING EXTENSION      *
       PERFORM VARYING I FROM 1 BY 1 UNTIL I > MIME-COUNT
           MOVE EXTENSION(I) TO NORMALIZED-LOOKUP
           INSPECT NORMALIZED-LOOKUP REPLACING 
               TRAILING SPACES BY LOW-VALUE
           COMPUTE LOOKUP-LEN = FUNCTION LENGTH(NORMALIZED-LOOKUP)
      *        CHECK IF EXTENSION MATCHES CURRENT TABLE ENTRY         *
           IF NORMALIZED-EXT(1:ACTUAL-LEN) = 
                   NORMALIZED-LOOKUP(1:LOOKUP-LEN)
               MOVE MIME(I) TO MIME-TYPE
               DISPLAY "MIME type matched and set to: " MIME-TYPE
               EXIT PERFORM
           END-IF
       END-PERFORM.
      *****************************************************************
      * HANDLE FILE FOUND AND READ FOR SENDING                        *
      *****************************************************************
       HANDLE-FILE.
      *    INITIALIZE FILE CONTENT BUFFER                             *
       MOVE SPACES TO FILE-CONTENT
       MOVE 0 TO TOTAL-BYTES-READ
      *    READ FILE INTO MEMORY                                      *
       PERFORM READ-ENTIRE-FILE
      *    CHECK IF FILE CONTENT WAS READ SUCCESSFULLY                *
       IF TOTAL-BYTES-READ > 0
               DISPLAY "File read successfully, bytes read: " 
               TOTAL-BYTES-READ
      *        SEND FILE CONTENT TO CLIENT                            *
           PERFORM SEND-FILE-CONTENT
       ELSE
           DISPLAY "File read failed, no bytes read."
      *        SEND 404 NOT FOUND ERROR                               *
           MOVE 404 TO ERROR-CODE
           PERFORM HTTPERROR
       END-IF.
      *****************************************************************
      * READ ENTIRE FILE INTO MEMORY BUFFER                           *
      *****************************************************************
       READ-ENTIRE-FILE.
      *    INITIALIZE FILE CONTENT BUFFER                             *
       MOVE SPACES TO FILE-CONTENT
       MOVE 0 TO TOTAL-BYTES-READ
      *MOVE 1 TO BYTES-TO-READ 
      *    READ FILE BYTE BY BYTE UNTIL END OF FILE                   *
       PERFORM UNTIL 1 = 0
           READ REQUEST-FILE
               INTO FILE-BUFFER
               AT END
                   EXIT PERFORM
               NOT AT END
      *                GET LENGTH OF CURRENT BYTE                     *
                   MOVE FUNCTION LENGTH(FILE-BUFFER) TO BYTES-READ
                   IF BYTES-READ > 0
      *                    APPEND BYTE TO CONTENT BUFFER              *
                      MOVE FILE-BUFFER TO 
                      FILE-CONTENT(TOTAL-BYTES-READ + 1: BYTES-READ)
                      ADD BYTES-READ TO TOTAL-BYTES-READ
                   END-IF
           END-READ
       END-PERFORM.
      *****************************************************************
      * SEND FILE CONTENT TO CLIENT WITH HTTP HEADERS                 *
      *****************************************************************
       SEND-FILE-CONTENT.
      *    INITIALIZE HTTP HEADER                                     *
       MOVE SPACES TO HTTP-HEADER
      *    CONVERT CONTENT LENGTH TO STRING                           *
       MOVE TOTAL-BYTES-READ TO CONTENT-LEN-STR
      *INSPECT CONTENT-LEN-STR REPLACING LEADING SPACES BY ZEROS
      *    BUILD HTTP RESPONSE HEADER                                 *
       STRING 
           "HTTP/1.1 200 OK"                                   LF
           "Content-Type: "   DELIMITED BY SIZE
           MIME-TYPE          DELIMITED BY SPACE
           LF
           "Content-Length: "  FUNCTION TRIM (CONTENT-LEN-STR) LF
           "Server: COBOL Web Server" LF
           LF                 DELIMITED BY SIZE
           INTO HTTP-HEADER
       END-STRING
      *    SEND HTTP HEADER TO CLIENT                                 *
       CALL "send" USING 
           BY VALUE CLIENT-SOCKET
           BY REFERENCE HTTP-HEADER
           BY VALUE FUNCTION LENGTH(FUNCTION TRIM(HTTP-HEADER))
           BY VALUE 0
           RETURNING WS-RETURN-CODE
       END-CALL
       IF WS-RETURN-CODE > 0
      *        SEND FILE CONTENT TO CLIENT                            *
           CALL "send" USING 
               BY VALUE CLIENT-SOCKET
               BY REFERENCE FILE-CONTENT
               BY VALUE TOTAL-BYTES-READ
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE > 0
               DISPLAY "Successfully sent " WS-RETURN-CODE " bytes"
           ELSE
               DISPLAY "Error sending file content: " WS-RETURN-CODE
           END-IF
       ELSE
           DISPLAY "Error sending header: " WS-RETURN-CODE
       END-IF.
      *****************************************************************
      *    INCLUDE HTTP ERROR HANDLING PROCEDURES                     *
      *****************************************************************    
       COPY "sources/httperror.cpy".
      *****************************************************************
      * CLEANUP RESOURCES BEFORE PROGRAM TERMINATION                  *
      *****************************************************************
       CLEANUP.
       DISPLAY "Performing cleanup..."
      *    CLOSE CLIENT SOCKET IF OPEN                                *
       IF CLIENT-SOCKET > 0
           CALL "close" USING BY VALUE CLIENT-SOCKET
               RETURNING WS-RETURN-CODE
           END-CALL
       END-IF
      *    CLOSE SERVER SOCKET IF OPEN                                *
       IF SERVER-SOCKET > 0
           CALL "close" USING BY VALUE SERVER-SOCKET
               RETURNING WS-RETURN-CODE
           END-CALL
       END-IF
       
       DISPLAY "Cleanup complete.".
      *****************************************************************
      * TERMINATE PROGRAM - HAVE FUN!!!                               *
      *****************************************************************    
       END PROGRAM WEBAPPLICATION.
