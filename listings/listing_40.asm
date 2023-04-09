    ; ========================================================================
;
; (C) Copyright 2023 by Molly Rocket, Inc., All Rights Reserved.
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Please see https://computerenhance.com for further information
;
; ========================================================================

; ========================================================================
; LISTING 40
; ========================================================================

bits 16

;;  testing
;; add [bx + 10], bh
;;  testing

; Signed displacements
;; mov ax, [bx + di - 37]
;; mov [si - 300], cx
;; mov dx, [bx - 32]

; Explicit sizes
;; add [bp + di], byte 7
;; mov [bp + di], byte 7
;; mov [bp + di + 16], byte 7
;; mov [bp + di + 768], byte 7
;; mov [di], byte 32
;; mov [di + 90], byte 32
;; mov [di], byte 687              ; 0xC6 2 bytes
;; mov [di], word 687              ; 0xC7 3 bytes
;; mov [di + 788], word 1200              ; 0xC7 4 bytes
;; mov [di + 901], word 347

; Direct address
;; mov bp, [5]
;; mov bx, [3458]

; Memory-to-accumulator test
;; mov ax, [2555]
add bx, 879
add [16], byte 78
;; add ax, 23
;; mov ax, 16
;; mov ax, [16]
;; mov [16], al
;; mov ax, [16]
; Accumulator-to-memory test
;; mov [2554], ax
;; mov [15], ax
