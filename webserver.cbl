       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBAPPLICATION.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REQUEST-FILE ASSIGN TO DYNAMIC FULL-PATH
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       FD  REQUEST-FILE.
       01  REQUEST-RECORD    PIC X(1024).
       
       WORKING-STORAGE SECTION.
       01  FULL-PATH         PIC X(512) VALUE SPACES.
       01  FILE-BUFFER       PIC X(102400).
       01  FILE-SIZE         PIC S9(9) COMP-5 VALUE 0.
       01  SEND-LENGTH       PIC S9(5) COMP-5 VALUE 0.
       01  WS-CONTENT-LEN    PIC 9(9).
       01  CONTENT-LENGTH    PIC X(20).
       01  HTTP-HEADER       PIC X(256).
       01  WS-RETURN-CODE    PIC S9(4) COMP-5 VALUE 0.
       01  WS-FILE-STATUS    PIC XX.
       01  FILE-OK           PIC X VALUE 'N'.
       01  I                 PIC 9(6).
       01  MIME-TYPE         PIC X(50).
       01  FILE-EXTENSION    PIC X(5).
       01  BUFFER            PIC X(1024).
       01  REQUEST-METHOD    PIC X(10).
       01  REQUEST-PATH      PIC X(255).
       01  ROOT-FOLDER       PIC X(255) VALUE "/www".
       01  RUNNING           PIC X VALUE 'Y'.
       01  PORT-NUMBER       PIC 9(5) COMP-5 VALUE 8080.
       01  BACKLOG           PIC S9(2) COMP-5 VALUE 5.
       01  CLIENT-ADDR       PIC X(16).
       01  CLIENT-ADDR-LEN   PIC S9(4) COMP-5 VALUE 16.
       01  AF-INET           PIC S9(4) COMP-5 VALUE 2.
       01  SOCK-STREAM       PIC S9(4) COMP-5 VALUE 1.
       01  IP-PROTO          PIC S9(4) COMP-5 VALUE 0.
       01  SERVER-SOCKET     PIC S9(4) COMP-5 VALUE 0.
       01  TEMP-FILE-EXTENSION  PIC X(10).
       01  TEMP-EXTENSION    PIC X(10).
       01  CLIENT-SOCKET     PIC S9(4) COMP-5 VALUE 0.
       01  MY-PORT-VALUE     PIC S9(4) COMP-5.
       01  MY-ADDR.
           05  FAMILY        PIC S9(4) COMP-5 VALUE 2.
           05  MY-PORT       PIC S9(4) COMP-5.
           05  INET-ADDR     PIC X(4) VALUE LOW-VALUE.
           05  FILLER        PIC X(8) VALUE LOW-VALUE.
       01  ERROR-CODE        PIC 9(3).
       01  RESPONSE          PIC X(4096) VALUE SPACES.
       01  BUFFER-OFFSET     PIC 9(8) VALUE 1.
       01  CHUNK-SIZE        PIC 9(8) VALUE 4096.
       01  TOTAL-BYTES-SENT  PIC 9(8) VALUE 0.
       01  DEFAULT-MIME      PIC X(24) VALUE "application/octet-stream".
       01  DEFAULT-PAGE      PIC X(11) VALUE "/index.html".
       01  MIME-TYPE-LOOKUP.
           05  MIME-COUNT    PIC 9(2) VALUE 10.
           05  MIME-ENTRY OCCURS 10 TIMES.
               10  EXTENSION      PIC X(10).
               10  MIME           PIC X(50).
               
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-MIME-TYPE-LOOKUP
           PERFORM INITIALIZE-SERVER
           PERFORM ACCEPT-CONNECTIONS
           PERFORM CLEANUP
           STOP RUN.
           
       INITIALIZE-MIME-TYPE-LOOKUP.
           MOVE ".html" TO EXTENSION(1)
           MOVE "text/html" TO MIME(1)
           MOVE ".htm" TO EXTENSION(2)
           MOVE "text/html" TO MIME(2)
           MOVE ".css" TO EXTENSION(3)
           MOVE "text/css" TO MIME(3)
           MOVE ".js" TO EXTENSION(4)
           MOVE "application/javascript" TO MIME(4)
           MOVE ".jpg" TO EXTENSION(5)
           MOVE "image/jpeg" TO MIME(5)
           MOVE ".jpeg" TO EXTENSION(6)
           MOVE "image/jpeg" TO MIME(6)
           MOVE ".png" TO EXTENSION(7)
           MOVE "image/png" TO MIME(7)
           MOVE ".gif" TO EXTENSION(8)
           MOVE "image/gif" TO MIME(8)
           MOVE ".txt" TO EXTENSION(9)
           MOVE "text/plain" TO MIME(9)
           MOVE ".json" TO EXTENSION(10)
           MOVE "application/json" TO MIME(10).
           
       INITIALIZE-SERVER.
           MOVE PORT-NUMBER TO MY-PORT-VALUE
           CALL "htons" USING BY VALUE MY-PORT-VALUE
               RETURNING MY-PORT-VALUE
           END-CALL
           MOVE MY-PORT-VALUE TO MY-PORT OF MY-ADDR
           DISPLAY "Starting COBOL Web Server on port " PORT-NUMBER
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
               DISPLAY "Error binding socket. Return: " WS-RETURN-CODE
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
           END-IF
           
           DISPLAY "Listening for connections.".
           
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
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error receiving data from client."
           ELSE 
               IF WS-RETURN-CODE = 0
                   DISPLAY "Client closed the connection."
               ELSE
                DISPLAY "Received " WS-RETURN-CODE " bytes from client."
                   PERFORM PARSE-REQUEST
                   
                   IF REQUEST-METHOD = "GET"
                       PERFORM SERVE-FILE
                   ELSE
                       MOVE 405 TO ERROR-CODE
                       PERFORM HTTPERROR
                   END-IF
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
               MOVE DEFAULT-PAGE TO REQUEST-PATH
           END-IF
           
           MOVE SPACES TO FULL-PATH
           INSPECT ROOT-FOLDER REPLACING TRAILING SPACES BY LOW-VALUES
           INSPECT REQUEST-PATH REPLACING TRAILING SPACES BY LOW-VALUES
           
           STRING ROOT-FOLDER DELIMITED BY LOW-VALUES
               REQUEST-PATH DELIMITED BY LOW-VALUES
               INTO FULL-PATH
           END-STRING
           
           INSPECT FULL-PATH REPLACING ALL LOW-VALUES BY SPACES
           DISPLAY "Attempting to serve: " FULL-PATH
           
           PERFORM GET-FILE-EXTENSION
           
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
           
       GET-FILE-EXTENSION.
           MOVE SPACES TO FILE-EXTENSION
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
           
           DISPLAY "File Extension: " FILE-EXTENSION
           PERFORM DETERMINE-MIME-TYPE.
           
        DETERMINE-MIME-TYPE.
        DISPLAY "Determining MIME: '" FILE-EXTENSION "'"
        MOVE DEFAULT-MIME TO MIME-TYPE
    
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > MIME-COUNT
        MOVE FUNCTION TRIM(FILE-EXTENSION) TO TEMP-FILE-EXTENSION
        MOVE FUNCTION TRIM(EXTENSION(I)) TO TEMP-EXTENSION
        
        DISPLAY "DEBUG: comparing '"
                TEMP-FILE-EXTENSION
                "' with '"
                TEMP-EXTENSION
                "' Length1: "
                FUNCTION LENGTH(FUNCTION TRIM(TEMP-FILE-EXTENSION))
                " Length2: "
                FUNCTION LENGTH(FUNCTION TRIM(TEMP-EXTENSION))
        
        IF TEMP-FILE-EXTENSION = TEMP-EXTENSION
            MOVE MIME(I) TO MIME-TYPE
            DISPLAY "MIME type set to: " MIME-TYPE
            EXIT PERFORM
        END-IF
        END-PERFORM
    
        DISPLAY "Final MIME type: " MIME-TYPE.           
       HANDLE-FILE.
           MOVE SPACES TO FILE-BUFFER
           MOVE 0 TO FILE-SIZE
           
           DISPLAY "Starting to read the file. Initial SIZE: " FILE-SIZE
           
           PERFORM READ-ENTIRE-FILE
           
           IF FILE-OK = "Y"
               PERFORM SEND-FILE-CONTENT
           ELSE
               MOVE 404 TO ERROR-CODE
               PERFORM HTTPERROR
           END-IF.
           
       READ-ENTIRE-FILE.
           MOVE SPACES TO FILE-BUFFER
           MOVE 1 TO FILE-SIZE
           
           PERFORM UNTIL WS-FILE-STATUS NOT = "00"
               READ REQUEST-FILE INTO REQUEST-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                      IF FILE-SIZE + FUNCTION LENGTH(REQUEST-RECORD) <= 
                           FUNCTION LENGTH(FILE-BUFFER)
                           STRING REQUEST-RECORD DELIMITED BY SIZE
                               INTO FILE-BUFFER
                               WITH POINTER FILE-SIZE
                           END-STRING
                           
                           COMPUTE FILE-SIZE = FILE-SIZE + 
                               FUNCTION LENGTH(REQUEST-RECORD) - 1
                               
                        DISPLAY "Read " FUNCTION LENGTH(REQUEST-RECORD) 
                               " bytes. Updated FILE-SIZE: " FILE-SIZE
                       ELSE
                           DISPLAY "File too large for buffer"
                           EXIT PERFORM
                       END-IF
               END-READ
           END-PERFORM
           
           DISPLAY "Final FILE-SIZE after reading: " FILE-SIZE.
           
       SEND-FILE-CONTENT.
           MOVE SPACES TO HTTP-HEADER
           MOVE FILE-SIZE TO WS-CONTENT-LEN
           MOVE SPACES TO CONTENT-LENGTH
           
           MOVE WS-CONTENT-LEN TO CONTENT-LENGTH
           INSPECT CONTENT-LENGTH REPLACING LEADING SPACES BY ZEROS
           
           STRING 
               "HTTP/1.1 200 OK" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Type: " DELIMITED BY SIZE
               FUNCTION TRIM(MIME-TYPE, TRAILING) DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Length: " DELIMITED BY SIZE
               FUNCTION TRIM(CONTENT-LENGTH, LEADING) DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Server: COBOL Web Server" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               INTO HTTP-HEADER
           END-STRING
           
           CALL "send" USING 
               BY VALUE CLIENT-SOCKET
               BY REFERENCE HTTP-HEADER
               BY VALUE FUNCTION LENGTH(HTTP-HEADER)
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending header."
           ELSE
               MOVE 1 TO BUFFER-OFFSET
               MOVE 0 TO TOTAL-BYTES-SENT
               
               PERFORM UNTIL TOTAL-BYTES-SENT >= FILE-SIZE
                   COMPUTE SEND-LENGTH = FUNCTION MIN(
                       CHUNK-SIZE, 
                       FILE-SIZE - TOTAL-BYTES-SENT)
                   
                   CALL "send" USING 
                       BY VALUE CLIENT-SOCKET
                     BY REFERENCE FILE-BUFFER(BUFFER-OFFSET:SEND-LENGTH)
                       BY VALUE SEND-LENGTH
                       BY VALUE 0
                       RETURNING WS-RETURN-CODE
                   END-CALL
                   
                   IF WS-RETURN-CODE < 0
                       DISPLAY "Error sending file content."
                       EXIT PERFORM
                   ELSE
                       ADD WS-RETURN-CODE TO TOTAL-BYTES-SENT
                       ADD WS-RETURN-CODE TO BUFFER-OFFSET
                   END-IF
               END-PERFORM
               
              DISPLAY "Sent " TOTAL-BYTES-SENT " of " FILE-SIZE " bytes"
           END-IF.
           
       HTTPERROR.
           EVALUATE ERROR-CODE
               WHEN 404
                   PERFORM ERROR-404
               WHEN 405
                   PERFORM ERROR-405
               WHEN OTHER
                   PERFORM ERROR-500
           END-EVALUATE.
           
       ERROR-404.
           MOVE SPACES TO RESPONSE
           STRING "HTTP/1.1 404 Not Found" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Type: text/html" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Server: COBOL Web Server" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Length: 97" DELIMITED BY SIZE
               X"0D0A0D0A" DELIMITED BY SIZE
          "<html><head><title>404</title></head>" DELIMITED BY SIZE
          "<body>404 Not Found</body></html>" DELIMITED BY SIZE
               INTO RESPONSE
           END-STRING
           
           CALL 'send' USING 
               BY VALUE CLIENT-SOCKET
               BY REFERENCE RESPONSE
             BY VALUE FUNCTION LENGTH(FUNCTION TRIM(RESPONSE, TRAILING))
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending 404 response."
           END-IF
           
           DISPLAY "Error 404: File not found".
           
       ERROR-405.
           MOVE SPACES TO RESPONSE
           STRING "HTTP/1.1 405 Method Not Allowed" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Type: text/html" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Server: COBOL Web Server" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Length: 117" DELIMITED BY SIZE
               X"0D0A0D0A" DELIMITED BY SIZE
       "<html><head><title>Error 405</title></head>" DELIMITED BY SIZE
          "<body><h1>Error 405 </h1></body></html>" DELIMITED BY SIZE
               INTO RESPONSE
           END-STRING
           
           CALL 'send' USING 
               BY VALUE CLIENT-SOCKET
               BY REFERENCE RESPONSE
             BY VALUE FUNCTION LENGTH(FUNCTION TRIM(RESPONSE, TRAILING))
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending 405 response."
           END-IF
           
           DISPLAY "Error 405: Method Not Allowed".
           
       ERROR-500.
           MOVE SPACES TO RESPONSE
           STRING "HTTP/1.1 500 Internal Server Error" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Type: text/html" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Server: COBOL Web Server" DELIMITED BY SIZE
               X"0D0A" DELIMITED BY SIZE
               "Content-Length: 121" DELIMITED BY SIZE
               X"0D0A0D0A" DELIMITED BY SIZE
       "<html><head><title>Error 500</title></head>" DELIMITED BY SIZE
           "<body><h1>Error 500</h1></body></html>" DELIMITED BY SIZE
               INTO RESPONSE
           END-STRING
           
           CALL 'send' USING 
               BY VALUE CLIENT-SOCKET
               BY REFERENCE RESPONSE
             BY VALUE FUNCTION LENGTH(FUNCTION TRIM(RESPONSE, TRAILING))
               BY VALUE 0
               RETURNING WS-RETURN-CODE
           END-CALL
           
           IF WS-RETURN-CODE < 0
               DISPLAY "Error sending 500 response."
           END-IF
           
           DISPLAY "Error 500: Internal Server Error".
           
       CLEANUP.
           DISPLAY "Performing cleanup..."
           IF CLIENT-SOCKET > 0
               CALL "close" USING BY VALUE CLIENT-SOCKET
                   RETURNING WS-RETURN-CODE
               END-CALL
           END-IF
           
           IF SERVER-SOCKET > 0
               CALL "close" USING BY VALUE SERVER-SOCKET
                   RETURNING WS-RETURN-CODE
               END-CALL
           END-IF
           
           DISPLAY "Cleanup complete.".
           
       END PROGRAM WEBAPPLICATION.
