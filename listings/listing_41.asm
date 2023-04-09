bits 16
mov al, [78]
mov [65520], al
add al, 178
add ax, 7829
sub al, 178
sub ax, 7829
cmp al, 178
cmp ax, 7829

add [bx + si], al
add [bx + si], ax
add al, [bx + si]
add ax, [bx + si]
add ax, [bx + si + 72]
add bx, [bx + si - 878]
add [bx + si], ax
add [bx + si + 0], ax
add ax, [bx + si + 5]
add [bx + si + 72], ax

add al, [78]
add ax, [78]

add ax, [di + 78]
add bx, [bx+si]
add bx, [bp]
add al, 2
add [bx], word 15               ; 131 7 15
add [bx], byte 15
add [bx], byte 789
add [bx + 7], byte 78              ; 128 71 7 78

add [bx], word 1500             ; 129 7 46 6
add [bx + 782], word 1582       ; 129 135 14 3 46 6
add bx, [si + 4]

add bl, 78
add bl, 78
add bl, -78
add bx, 788
add bx, byte 788
add bx, word 828
add [bx], byte 78


add [bx + si], al               ; 0 0
add al, [bx + si]               ; 2 0
add al, [8]                     ; 2 6 8 0

add bx, dx                      ; 1 211
add ax, [bx + si]               ; 3 0
add al, 8                       ; 4 8
add ax, 728                     ; 5 216 2
add bl, al                      ; 0 195

add bx, 7                      ; 131 195 7
add bl, 7                      ; 128 195 7



add [77], bx                    ; 1 30 77 0
add bx, [77]                    ; 3 30 77 0


add bl, 72                      ; 128 195 172
add [8], al
add ax, 872
add bx, word 8


add bx, [bp + 0]
add cx, [bx + 2]
add bh, [bp + si + 4]
add di, [bp + di + 6]
add [bx+si], bx
add [bp], bx
add [bp + 0], bx
add [bx + 2], cx
add [bp + si + 4], bh
add [bp + di + 6], di
add byte [bx], 34
add word [bp + si], byte 29
add word [bp + si], 29
add word [bp + si + 1000], 29
add word [bp + si + 1000], 928
add ax, [bp]
add al, [bx + si]
add ax, bx
add al, ah
add ax, 1000
add al, -30
add al, 9

sub bx, [bx+si]
sub bx, [bp]
sub si, 2
sub bp, 2
sub cx, 8
sub bx, [bp + 0]
sub cx, [bx + 2]
sub bh, [bp + si + 4]
sub di, [bp + di + 6]
sub [bx+si], bx
sub [bp], bx
sub [bp + 0], bx
sub [bx + 2], cx
;; -------------------------------------------------
sub [bp + si + 4], bh
;; --------------------------------------------------
sub [bp + di + 6], di
sub byte [bx], 34
sub word [bx + di], 29
sub ax, [bp]
sub al, [bx + si]
sub ax, bx
sub al, ah
sub ax, 1000
sub al, -30
sub al, 9

cmp bx, [bx+si]
cmp bx, [bp]
cmp si, 2
cmp bp, 2
cmp cx, 8
cmp bx, [bp + 0]
cmp cx, [bx + 2]
cmp bh, [bp + si + 4]
cmp di, [bp + di + 6]
cmp [bx+si], bx
cmp [bp], bx
cmp [bp + 0], bx
cmp [bx + 2], cx
cmp [bp + si + 4], bh
cmp [bp + di + 6], di
cmp byte [bx], 34
cmp word [4834], 29
cmp ax, [bp]
cmp al, [bx + si]
cmp ax, bx
cmp al, ah
cmp ax, 1000
cmp al, -30
cmp al, 9

test_label0:
jnz test_label1
jnz test_label0
test_label1:
jnz test_label0
jnz test_label1

label:
je label
jl label
jle label
jb label
jbe label
jp label
jo label
js label
jne label
jnl label
jg label
jnb label
ja label
jnp label
jno label
jns label
loop label
loopz label
loopnz label
jcxz label
