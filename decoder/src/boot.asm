bits 16
org 0x7c00
start:
    ;; mov si, msg
    mov cx, 10                  ; Want to print the letter 10 times
    mov ah, 0x0e                ; teletype output
    mov al, 'e'                 ; al holds the char to be sent to output
    int 0x10
loop:
    sub cx, 1
    jz halt
    int 0x10
    jmp loop
halt:
    hlt

times 510-($-$$) db 0
dw 0xAA55

;; msg:
;;     db "hello world", 0
