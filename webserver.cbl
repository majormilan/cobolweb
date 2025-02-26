       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBSERVER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REQUEST-FILE ASSIGN TO DYNAMIC FULL-PATH
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  REQUEST-FILE.
       01  REQUEST-RECORD    PIC X(4096).
       WORKING-STORAGE SECTION.
       01  SERVER-SOCKET    PIC S9(4) COMP-5 VALUE 0.
       01  CLIENT-SOCKET    PIC S9(4) COMP-5 VALUE 0.
       01  BUFFER           PIC X(1024).
       01  REQUEST-METHOD   PIC X(10).
       01  REQUEST-PATH     PIC X(255).
       01  FULL-PATH        PIC X(512) VALUE SPACES.
       01  RESPONSE         PIC X(4096).
       01  FILE-BUFFER      PIC X(4096).
       01  SEND-LENGTH      PIC S9(5) COMP-5 VALUE 0.
       01  FILE-SIZE        PIC S9(9) COMP-5 VALUE 0.
       01  CONTENT-LENGTH   PIC X(20).
       01  ROOT-FOLDER      PIC X(255) VALUE "/www".
       01  RUNNING          PIC X VALUE 'Y'.
       01  WS-FILE-STATUS   PIC XX.
       01  PORT-NUMBER      PIC 9(5) COMP-5 VALUE 8080.
       01  BACKLOG          PIC S9(2) COMP-5 VALUE 5.
       01  CLIENT-ADDR      PIC X(16).
       01  CLIENT-ADDR-LEN  PIC S9(4) COMP-5 VALUE 16.
       01  WS-RETURN-CODE   PIC S9(4) COMP-5 VALUE 0.
       01  MY-ADDR.
           05  FAMILY        PIC S9(4) COMP-5 VALUE 2.
           05  PORT          PIC S9(4) COMP-5.
           05  INET-ADDR     PIC X(4) VALUE LOW-VALUE.
           05  FILLER        PIC X(8) VALUE LOW-VALUE.
       01  AF-INET          PIC S9(4) COMP-5 VALUE 2.
       01  SOCK-STREAM      PIC S9(4) COMP-5 VALUE 1.
       01  IP-PROTO         PIC S9(4) COMP-5 VALUE 0.
       01  HTTP-HEADER      PIC X(256).
       01  PORT-DISPLAY     PIC 9(5).
       01  WS-CONTENT-LEN   PIC 9(9).
       01  I                PIC 9(4).
       01  FILE-OK          PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM SETUP-SOCKET
           PERFORM ACCEPT-CONNECTIONS UNTIL RUNNING = 'N'
           PERFORM CLEANUP
           STOP RUN.
       SETUP-SOCKET.
           MOVE PORT-NUMBER TO PORT-DISPLAY
           CALL "htons" USING BY VALUE PORT-NUMBER
               RETURNING PORT OF MY-ADDR
           END-CALL
           DISPLAY "Starting COBOL Web Server on port "
               PORT-DISPLAY
           DISPLAY "Serving files from: " ROOT-FOLDER
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
           CALL "bind" USING BY VALUE SERVER-SOCKET
               BY REFERENCE MY-ADDR
               BY VALUE LENGTH OF MY-ADDR
               RETURNING WS-RETURN-CODE
           END-CALL
           IF WS-RETURN-CODE NOT = 0
               DISPLAY "Error binding socket. Return: "
                   WS-RETURN-CODE
               IF WS-RETURN-CODE = -1
                   DISPLAY "Possible cause: Address already in use."
               END-IF
               MOVE 'N' TO RUNNING
               PERFORM CLEANUP
               STOP RUN
           ELSE
               DISPLAY "Socket successfully bound."
           END-IF
           CALL "listen" USING BY VALUE SERVER-SOCKET
               BY VALUE BACKLOG
               RETURNING WS-RETURN-CODE
           END-CALL
           IF WS-RETURN-CODE NOT = 0
               DISPLAY "Error listening on socket."
               MOVE 'N' TO RUNNING
               PERFORM CLEANUP
               STOP RUN
           END-IF.
       ACCEPT-CONNECTIONS.
           PERFORM UNTIL RUNNING = 'N'
               DISPLAY "Waiting for client connection..."
               CALL "accept" USING BY VALUE SERVER-SOCKET
                   BY REFERENCE CLIENT-ADDR
                   BY REFERENCE CLIENT-ADDR-LEN
                   RETURNING CLIENT-SOCKET
               END-CALL
               IF CLIENT-SOCKET < 0
                   DISPLAY "Error accepting connection."
               ELSE
                   PERFORM PROCESS-REQUEST
                   CALL "close" USING BY VALUE CLIENT-SOCKET
                       RETURNING WS-RETURN-CODE
                   END-CALL
                   IF WS-RETURN-CODE NOT = 0
                       DISPLAY "Error closing client socket."
                   END-IF
               END-IF
           END-PERFORM.
       PROCESS-REQUEST.
           MOVE SPACES TO BUFFER
           CALL "recv" USING BY VALUE CLIENT-SOCKET
               BY REFERENCE BUFFER
               BY VALUE LENGTH OF BUFFER
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           IF WS-RETURN-CODE <= 0
               DISPLAY "Error reading from client."
           ELSE
               PERFORM PARSE-REQUEST
               IF REQUEST-METHOD = "GET"
                   PERFORM SERVE-FILE
               ELSE
                   PERFORM ERROR-405
               END-IF
           END-IF.
       PARSE-REQUEST.
           UNSTRING BUFFER DELIMITED BY SPACE
               INTO REQUEST-METHOD REQUEST-PATH
           END-UNSTRING
           DISPLAY "Request Method: " REQUEST-METHOD
           DISPLAY "Request Path: " REQUEST-PATH
           PERFORM SANITIZE-REQUEST-PATH.
       SANITIZE-REQUEST-PATH.
           IF REQUEST-PATH(1:1) = "/" 
               CONTINUE
           ELSE    
               STRING "/" REQUEST-PATH DELIMITED BY SPACE
                  INTO REQUEST-PATH
               END-STRING
           END-IF
           
           INSPECT REQUEST-PATH REPLACING ALL ".." BY "xx"
           
           DISPLAY "Sanitized Path: " REQUEST-PATH.
       SERVE-FILE.
           IF REQUEST-PATH = "/" OR REQUEST-PATH = SPACES
               MOVE "/index.html" TO REQUEST-PATH
           END-IF
           MOVE SPACES TO FULL-PATH
           INSPECT ROOT-FOLDER REPLACING TRAILING SPACES
               BY LOW-VALUES
           INSPECT REQUEST-PATH REPLACING TRAILING SPACES
               BY LOW-VALUES
           
           STRING ROOT-FOLDER DELIMITED BY LOW-VALUES
               REQUEST-PATH DELIMITED BY LOW-VALUES
               INTO FULL-PATH
           END-STRING
           
           INSPECT FULL-PATH REPLACING ALL LOW-VALUES BY SPACES
           DISPLAY "Attempting to serve: " FULL-PATH
           
           MOVE "N" TO FILE-OK
           OPEN INPUT REQUEST-FILE
           IF WS-FILE-STATUS = "00"
               MOVE "Y" TO FILE-OK
               PERFORM HANDLE-FILE
               CLOSE REQUEST-FILE
           ELSE
               DISPLAY "File open failed: " WS-FILE-STATUS
               PERFORM ERROR-404
           END-IF.
       HANDLE-FILE.
           MOVE SPACES TO FILE-BUFFER
           MOVE 0 TO FILE-SIZE
           
           READ REQUEST-FILE INTO FILE-BUFFER
               AT END
                   DISPLAY "Read failed: End of file reached."
               NOT AT END
                   MOVE "Y" TO FILE-OK
           END-READ
           
           IF FILE-OK = "Y"
               PERFORM CALCULATE-FILE-SIZE
               PERFORM SEND-FILE-CONTENT
           ELSE
               PERFORM ERROR-404
           END-IF.
       
       CALCULATE-FILE-SIZE.
           MOVE 0 TO FILE-SIZE
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF FILE-BUFFER
               IF FILE-BUFFER(I:1) = LOW-VALUES
                   EXIT PERFORM
               END-IF
               ADD 1 TO FILE-SIZE
           END-PERFORM
           
           DISPLAY "Calculated file size: " FILE-SIZE.
           
       SEND-FILE-CONTENT.
           MOVE SPACES TO HTTP-HEADER
           MOVE FILE-SIZE TO WS-CONTENT-LEN
           MOVE SPACES TO CONTENT-LENGTH
           
           MOVE WS-CONTENT-LEN TO CONTENT-LENGTH
           INSPECT CONTENT-LENGTH REPLACING LEADING SPACES BY ZEROS
           
           STRING "HTTP/1.1 200 OK" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Type: text/html" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Server: Cobol Web Server" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Length: " DELIMITED BY SIZE
               FUNCTION TRIM(CONTENT-LENGTH LEADING) DELIMITED BY SIZE
               X"0D0A0D0A" DELIMITED BY SIZE
               INTO HTTP-HEADER
           END-STRING
           
           DISPLAY "Sending HTTP-HEADER with Content-Length: " 
               FUNCTION TRIM(CONTENT-LENGTH LEADING)
               
           CALL "send" USING BY VALUE CLIENT-SOCKET
               BY REFERENCE HTTP-HEADER
           BY VALUE FUNCTION LENGTH(FUNCTION TRIM(HTTP-HEADER TRAILING))
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending headers: " WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
           MOVE FILE-SIZE TO SEND-LENGTH
           CALL "send" USING BY VALUE CLIENT-SOCKET
               BY REFERENCE FILE-BUFFER
               BY VALUE SEND-LENGTH
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending file content: " WS-RETURN-CODE
           ELSE
               DISPLAY "File sent successfully, LENGTH: " SEND-LENGTH
           END-IF.
       
       ERROR-404.
           MOVE SPACES TO RESPONSE
           STRING "HTTP/1.1 404 Not Found" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Type: text/html" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Server: Cobol Web Server" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Length: 26" DELIMITED BY SIZE
               X"0D0A0D0A" DELIMITED BY SIZE
               "<h1>404 - Not Found</h1>" DELIMITED BY SIZE
               INTO RESPONSE
           END-STRING
           
           CALL "send" USING BY VALUE CLIENT-SOCKET
               BY REFERENCE RESPONSE
              BY VALUE FUNCTION LENGTH(FUNCTION TRIM(RESPONSE TRAILING))
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           DISPLAY "Error 404: File not found: " FULL-PATH
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending 404 response."
           END-IF.
           
       ERROR-405.
           MOVE SPACES TO RESPONSE
           STRING "HTTP/1.1 405 Method Not Allowed" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Type: text/html" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Server: Cobol Web Server" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Length: 36" DELIMITED BY SIZE
               X"0D0A0D0A" DELIMITED BY SIZE
               "<h1>405 - Method Not Allowed</h1>" DELIMITED BY SIZE
               INTO RESPONSE
           END-STRING
           
           CALL "send" USING BY VALUE CLIENT-SOCKET
               BY REFERENCE RESPONSE
              BY VALUE FUNCTION LENGTH(FUNCTION TRIM(RESPONSE TRAILING))
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending 405 response."
           END-IF.
           
       CLEANUP.
           IF SERVER-SOCKET > 0
               CALL "close" USING BY VALUE SERVER-SOCKET
                   RETURNING WS-RETURN-CODE
               END-CALL
               IF WS-RETURN-CODE NOT = 0
                   DISPLAY "Error closing server socket."
               END-IF
           END-IF
           DISPLAY "Web server stopped.".
       END PROGRAM WEBSERVER.
