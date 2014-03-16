
;;; Symbols can be lables representing an address or
;;; a defined symbol using the def syntax.
        
        ;; Example code for the RISC-V processor

include myfile.asm
        
absolute 0xffff
        
section .text                   ; define a section name
        
extern  foo                     ; say that a symbol is defined elsewhere
        
global  foo2                    ; say that this symbol must be visible
        
mydef   equ     3               ; Gives the symbol a specific value
test    equ     23

align   2                       ; Align for 2 bytes boundary


        byte    [32]            ; Reserves 32 bytes

        res     byte[32]        ; Reserves 32 bytes
        res     int32[100]      ; Reserves 100 32 bit integers

        
;;; Data allocation     
        byte    12,42,12,32     ; Can define a sequence of bytes
        int32   0x12, 123,      ; Sequence of 32-bit words
        int16   1232h, 1212h
        int8    "Hej"           ; int8 is a synonym of byte
        

start:
        mov     x1,0
        add     x3,x1,x3        ; This is a comment.
        lw      x1,1bf1h

another:

        lw      x2,[x1+12]
        lw      x2,12(x1)       ; Alternative syntax 
        sw      x1,[x2]
        sw      x1,(x1)         ; Alternative syntax
        mov     [x1+5],x1

;;; Registers
        add     r1,r2           ; Can use r1 to r31
        add     x1,x4           ; or use x1 to x31
        
;;; Pseudo instructions
        
        mov     [x1+1], 5       ; Mov can be used instead of store
        mov     r1, [r2]        ; Mov can also be used for load
        mov     r3,r5           ; It can also be used between registers
        mov     x1,3212a677h    ; Will generate several instructions
        beqz    x1,mylable
        beq     x1,x0,mylable   ; same as above, but expanded.
        srli    x1,21           
addr21:

        j       label           ; 0x0010201
        
;;; Numerical constants
        mov     r1,400          ; decimal
        mov     r1,0x32         ; hex
        mov     r1,32h          ; hex
        mov     r1,0b101101     ; binary
        mov     r1,101101b      ; binary
        mov     r1,1101_1101b   ; binary

;;; Character strings
        byte    "Hello"         ; Each char as a byte. Supports utf8
        mov     r1,"heja"       ; Uses the endianness for the target
        
        
        

        ;; Instruction operands can be registers (x1, x2, etc.),
        ;; addresses, constants, or expressions



