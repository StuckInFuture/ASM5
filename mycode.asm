.model small
.286
.stack 200h

.data

file_path db 126 dup(0)
length dw 0

cmd_string db 126 dup(0)

handle dw 0

symbol db 0

answer dd 0

was_sign db 0 

was_newl db 0

file_open_error_str db "Can't open input file", 10, 13, "$"
file_close_error_str db "Can't close input file", 10, 13, "$"
short_cmd db "Not enough arguments.", 10, 13, "$"
too_many_args_str db "To many arguments", 10, 13, "$"
error_reading_data db "Can't read data from file.", 10, 13, "$"  
error_writing_data db "Can't write data in file.", 10, 13, "$"  
answer_is_OVERFLAW db "Can't read data from file.", 10, 13, "$"  
file_delete_error_str db "Can't delete file.", 10, 13, "$" 
file_create_error_str db "Can't create file.", 10, 13, "$" 

is_find_any_symbol db 0 


symbol_end_of_line db 0Dh, 0Ah 


.code
.386

output_string macro current_string
    pusha    
    mov ah, 09h
    lea dx, current_string
    int 21h    
    popa        
endm

parse_cmd proc
    cld 							
	xor cx, cx
	mov cl, es:[0080h]
	mov ax, @data
	mov es, ax  
	push ds
	mov ds, ax
	cmp cl, 7
	ja go_next_parse_cmd 
	
	output_string short_cmd 	
	jmp end_of_programm
	
	go_next_parse_cmd:  
	pop ds
	;inc cl
	mov di, offset cmd_string
	mov si, 81h
	;push ax
	;push es
	push di
	;mov ax, ds
	;mov es, ax
	mov di, si
	mov al, ' '
	repz scasb
	dec di
	inc cx
	inc cx
	mov si,di 
	pop di
	;pop es
	rep movsb
	dec cx
	;pop ax
	 
	mov ax, @data
	mov ds, ax
	
	mov di, 0
	mov si, 0
	xor bx, bx
	
	
	skip_spaces:
		cmp cmd_string[di], ' ' 					
		jne read_file_path
		inc di
		jmp skip_spaces 					
	
	read_file_path:
		cmp cmd_string[di], byte ptr 0dh 			
		je file_path_readen
		cmp cmd_string[di], byte ptr ' ' 			
		je file_path_readen

		mov bl, cmd_string[di]					
		mov file_path[si], bl
		inc di
		inc si
		jmp read_file_path

	file_path_readen:   					
		 
    skip_spaces2:
	    cmp cmd_string[di], ' ' 					
	    jne check_end_cmd
	    inc di
	jmp skip_spaces2    
	 
    check_end_cmd:
	    cmp cmd_string[di], byte ptr 0dh
	    jne too_many_args 
	    call check_file_path_length	
	    cmp length, 0
	    jne end_parse_cmd
	    output_string short_cmd
	    jmp end_of_programm    
	    too_many_args:
		output_string too_many_args_str
		jmp end_of_programm
		
	end_parse_cmd:
	ret
parse_cmd endp   

check_file_path_length	proc
    pusha   
    mov si, 0
    cycle1:
    cmp file_path[si], 00h
    je  end_of_check 
    inc si
    jmp cycle1   
    end_of_check:  
    mov length, si  
    popa
    ret
    endp

read_symbol proc
    mov ah, 3Fh
    mov bx, handle
    mov cx, 1
    mov dx, offset symbol
    int 21h
    jc error_reading
    jmp end_read_symbol 
    error_reading:
        output_string error_reading_data 
        jmp call_close_file 
    end_read_symbol:
    ret
endp

output_symbol proc
    pusha
    
    mov ah, 40h
    mov bx, handle
    mov cx, 2
    mov dx, offset symbol_end_of_line
    int 21h
    jc error_writing
    jmp end_write_symbol 
    error_writing:
        output_string error_writing_data 
        jmp call_close_file 
    end_write_symbol:
   
    
    
    popa
    ret
endp

open_file proc 
    pusha
    
    mov ah, 3Dh
    mov al, 00h
    mov dx, offset file_path
    int 21h
    jnc success_open_file
    output_string file_open_error_str 
    jmp end_of_programm
    success_open_file:
    mov handle, ax
        
    popa    
    ret
endp 

create_open_file proc
    pusha
    
    mov ah, 5Bh
    mov cx, 00000000b
    mov dx, offset file_path
    int 21h
    jnc end_create_open_file
    output_string file_create_error_str
    jmp end_of_programm
    end_create_open_file:
    mov handle, ax
    
    popa
    ret
endp

close_file proc
    pusha
	mov bx, handle
	mov ah, 3eh
	int 21h
	jnc end_close_file
	output_string file_close_error_str 		
	jmp end_of_programm
	end_close_file:
	popa	
    ret
endp


delete_file proc
    pusha
    mov ah, 41h
    mov dx, offset file_path
    int 21h
    jnc end_delete_file
    output_string file_delete_error_str
    jmp end_of_programm
    end_delete_file:
    popa
    ret
endp   

end_of_line proc
   call cheking_symbol
   mov is_find_any_symbol, 0  
   ret
endp 

cheking_symbol proc
    cmp is_find_any_symbol, 1
    jne not_empty 
    cmp was_newl, 1
    jne end_of_checking_symbol    
    inc answer
    jmp end_of_checking_symbol
    not_empty:
    mov was_newl, 0   
    end_of_checking_symbol:
    ret
endp

start:
    call parse_cmd 
    call open_file   
    mov di, 0   
    call read_symbol
    cmp symbol, 0Dh
    jne isLF 
    inc answer
    isLF:
    cmp symbol, 0Ah  
    jne iffirst
    inc answer 
    jmp iffirst
    
   cycle:
        call read_symbol 
   iffirst:     
        cmp ax, 0h                        ;OD - CR, 0A-LF               lf g
        je end_cycle
                                                                ; ws 1, wn 0 fs 0
        cmp symbol, 0Dh                   ;1)ssfCRLF 
        jne noCR                          ;  CRLF
        mov was_sign, 1 
        mov is_find_any_symbol, 1
        call end_of_line 
        mov was_newl, 1 
        jmp go_next_symbol
        
        noCR: 
        cmp symbol, 0Ah  
        jne not_this   ;����� CR ��� LF
        cmp was_sign, 1
        je another_symbol      ;���� LF � ���� inc ��-�� CR
        mov is_find_any_symbol, 1     ;��� CR ����� LF
        call end_of_line  
        mov was_newl, 1
        jmp go_next_symbol  
        not_this:
        mov is_find_any_symbol, 0
        call end_of_line
        
                          
        another_symbol:
        mov was_sign, 0 
        ;call end_of_line
        ;mov is_find_any_symbol, 1
        
        go_next_symbol: 
        jmp cycle
        
    
   end_cycle: 
   
   call close_file
   call delete_file
   call create_open_file
   cmp answer, 00000000h
   je call_close_file
   cycle_output: 
        ;cmp answer, 00000000h
        call output_symbol
        dec answer
        cmp answer, 00000000h
        jne cycle_output
         
    call_close_file:
        call close_file
    
    end_of_programm:
    mov ax, 4c00h
    int 21h  
    

end start
