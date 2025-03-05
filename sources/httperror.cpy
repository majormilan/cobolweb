        HTTPERROR.
            MOVE SPACES TO RESPONSE

            EVALUATE ERROR-CODE
                WHEN 404
                    PERFORM ERROR-404
                WHEN 405
                    PERFORM ERROR-405
                WHEN OTHER
                    PERFORM ERROR-500
            END-EVALUATE.

            CALL 'send' USING
                BY VALUE CLIENT-SOCKET
                BY REFERENCE RESPONSE
                BY VALUE FUNCTION LENGTH(FUNCTION TRIM(RESPONSE))
                BY VALUE 0
                RETURNING WS-RETURN-CODE
            END-CALL

            IF WS-RETURN-CODE < 0
                DISPLAY "Error sending response for error-code "
                        ERROR-CODE "."
            END-IF.

        ERROR-404.
            DISPLAY "Error 404: File not found"
            

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
               X"00" DELIMITED BY SIZE
                INTO RESPONSE
                END-STRING.

        ERROR-405.
            DISPLAY "Error 405: Method Not Allowed"

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
               X"00" DELIMITED BY SIZE
                INTO RESPONSE
            END-STRING.

       ERROR-500.
           DISPLAY "Error 500: Internal Server Error"

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
               X"00" DELIMITED BY SIZE
               INTO RESPONSE
           END-STRING.
