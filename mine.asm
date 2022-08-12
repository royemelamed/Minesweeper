P186
IDEAL
MODEL small
STACK 100h
jumps

;----------------------------------------------------------------------------

;DEFINES
;-----------------------------------------
ONE_ASCII equ 31h
MINE equ 2ah
EMPTY_IN_BOARD equ 2dh
EMPTY_IN_REVEAL_BOARD equ 23h
ROW_COL_LEN equ 8
BOARD_LEN equ 64
NUMBER_OF_MINE equ 10
HEADER_LEN equ 54
;-----------------------------------------

DATASEG
;----------------------------------------------------------------------------

;THE-GAME-BOARDS
;-------------------------------------
board db 64 dup(2dh)
revealBoard db 64 dup(23h)
;-------------------------------------


;RINT-PICTURE-DATA
;-------------------------------------
fileLose db 'LOSE.bmp',0	
fileWin db 'WIN.bmp',0	
filehandle dw 0
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)
ErrorMsg db 'ERROR', 10, 13
isOpenFileLose db 0
;-------------------------------------


;GAME-MESSAGES
;-------------------------------------
inputRowMSG db 'Please enter row: ', '$'
inputColMSG db 'Please enter column: ', '$'
gameFrameMSG db 10,13
db '  	 __  __ _                                                   ',10,13
db ' 	 |  \/  (_)                                                  ',10,13
db ' 	 | \  / |_ _ __   ___  _____      _____  ___ _ __   ___ _ __ ',10,13
db ' 	 | |\/| | | \_ \ / _ \/ __\ \ /\ / / _ \/ _ \ \_ \ / _ \ \__|',10,13
db ' 	 | |  | | | | | |  __/\__ \\ V  V /  __/  __/ |_) |  __/ |   ',10,13
db ' 	 |_|  |_|_|_| |_|\___||___/ \_/\_/ \___|\___| .__/ \___|_|   ',10,13
db '  	                                           | |              ',10,13
db '  	                                           |_|              ',10,13,10,13,10,13,10,13
db '     ------------------FOR-GAME-RULES-CLICK-ANY-KEY------------------','$'


openScreenMSG db 'The game board is a square board, divided into squares.',10,13
db 'Invisible mines are scattered on the board in random locations.', 10,13
db 'The board is divided into 2 types of squares: squares with mines', 10,13
db 'and squares without mines. Squares without mines are also divided into 2 types:', 10,13
db 'squares with a number and without a number. ', 10,13
db 'Each slot where there is no mine will contain a number that says how', 10,13
db 'many mines there are around it. ', 10,13
db 'If there is no number, there is no mine around it.', 10,13
db 'The object of the game is to uncover all the non-mined squares',10,13
db 'using the elimination method and simple logic,', 10,13
db 'though sometimes there is no way to find out without a guess.', 10,13
db 'When the player clicks on any slot, the slot is exposed.',10,13
db 'To reveal a cell, enter the row and the line that write near the cell.',10,13
db 'If there is a minefield, the game is over.', 10,13,10,13,10,13
db 'ENJOY!!', '$'

newLineMSG db '',10,13, '$'
spaceMSG db '  $'
askMineMSG db 'Place mine at place that you want', 10,13, '$'

easierLevelMSG db 'Do you want an easier game level?',10,13
db '(1 - yes. any key - no): ', '$'

;-------------------------------------

;GAME-DATA
;-------------------------------------
isWin db 0
isGameOver db 0
inputRow db 0
inputCol db 0
indexAfterCon dw 0
validInput db 0
cnt db 0
;-------------------------------------

;SOUND-DATA
;-------------------------------------
frequency dw 0a98h
;-------------------------------------


CODESEG
;----------------------------------------------------------------------------


proc gameFrame
	pusha

	; clear DOS-Box window
	mov ax, 3h
    int 10h
	
	
	; set the resolution to (640 x 480) with 256 colors
	mov ax, 4F02h
	mov bx, 101h
	int 10h

	; print game frame screen
	mov dx, offset gameframeMSG
	mov ah, 9h
	int 21h
	
	; wait for key press
	mov ah, 0h
	int 16h

	; clear DOS-Box window
	mov ax, 3h
    int 10h

	; print opening screen
	mov dx, offset openScreenMSG
	mov ah, 9h
	int 21h
	
	; wait for key press
	mov ah, 0h
	int 16h
	
	; switch to graphic mode (40 x 25)	
	mov ax, 13h
	int 10h
	
	
	popa
	ret
endp gameFrame



proc printBoard

	push bp
	mov bp, sp
	pusha

	call printnumbersuptheboard

	;Inserts the values ​​from the stack into registers
	mov bx, [bp + 4] ;board
	mov cx, [bp + 6] ;board length
	printArrayLoop:
	

	mov al, [byte ptr bx]
	mov dx, ax
	mov ah, 2h
	int 21h

	mov dx, offset spaceMSG
	mov ah, 9h
	int 21h
	
	;----------------------------
	mov ax, cx
	dec ax
	mov dl, ROW_COL_LEN
	div dl
	cmp ah, 0
	je printLine
	jmp notPrintLine
	;----------------------------
	
	printLine:
	call printLineInBoard

	notPrintLine:
	inc bx
	loop printArrayLoop
	
	mov dx, offset newlineMSG
	mov ah, 9h
	int 21h
	

	popa
	pop bp
	ret 4

endp printBoard


proc printNumbersUpTheBoard
	pusha
	
	mov [cnt], 31h
	xor cx, cx
	mov cl, ROW_COL_LEN

	mov dx, offset spaceMSG
	mov ah, 9h
	int 21h

	mov dx, offset spaceMSG
	mov ah, 9h
	int 21h
	numLoop:

	mov al, [cnt]
	mov dx, ax
	mov ah, 2h
	int 21h

	mov dx, offset spaceMSG
	mov ah, 9h
	int 21h

	inc [cnt]
	loop numloop

	mov dx, offset newlineMSG
	mov ah, 9h
	int 21h


	mov dx, offset spaceMSG
	mov ah, 9h
	int 21h

	mov al, '_'
	mov dx, ax
	mov ah, 2h
	int 21h

	mov al, '_'
	mov dx, ax
	mov ah, 2h
	int 21h

	xor cx, cx
	mov cl, 22
	numLoop2:

	mov al, '_'
	mov dx, ax
	mov ah, 2h
	int 21h

	loop numloop2

	
	mov dx, offset newlineMSG
	mov ah, 9h
	int 21h
	
	mov al, ' '
	mov dx, ax
	mov ah, 2h
	int 21h


	mov al, '|'
	mov dx, ax
	mov ah, 2h
	int 21h

	mov dx, offset newlineMSG
	mov ah, 9h
	int 21h
	
	
	mov [cnt], 31h
	mov al, [cnt]
	mov dx, ax
	mov ah, 2h
	int 21h

	inc [cnt]

	mov al, '|'
	mov dx, ax
	mov ah, 2h
	int 21h


	mov dx, offset spaceMSG
	mov ah, 9h
	int 21h

	
	popa
	ret
endp printnumbersuptheboard


proc printLineInBoard
	pusha
	
	mov dx, offset newlineMSG
	mov ah, 9h
	int 21h

	cmp [cnt], 39h
	je enddddd

	mov al, ' '
	mov dx, ax
	mov ah, 2h
	int 21h

	mov al, '|'
	mov dx, ax
	mov ah, 2h
	int 21h

	mov dx, offset newlineMSG
	mov ah, 9h
	int 21h
	
	mov al, [cnt]
	mov dx, ax
	mov ah, 2h
	int 21h

	inc [cnt]


	mov al, '|'
	mov dx, ax
	mov ah, 2h
	int 21h


	mov dx, offset spaceMSG
	mov ah, 9h
	int 21h
	
	enddddd:
	popa
	ret
endp printLineInBoard

proc setMines
	pusha

	mov cl, NUMBER_OF_MINE
	setMinesRndloop:
		mov ah, 2Ch
		push cx ;preserve value of cx
		int 21h ;dl stores milliseconds
		pop cx
		xor ax, ax
		mov al, dl
		mov bl, BOARD_LEN
		div bl ;ah stores the remainder
		mov bx, offset board
		add bl, ah
		cmp [byte ptr bx], MINE ;is there a bomb already?
		je setMinesRndloop
		; avoid the 4 center cells
		cmp bl, 27
		je setMinesRndloop
		cmp bl, 28
		je setMinesRndloop
		cmp bl, 35
		je setMinesRndloop
		cmp bl, 36
		je setMinesRndloop
		mov [byte ptr bx], MINE

		loop setMinesRndloop
	popa
	ret
endp setMines


proc addNumber 
	add bx, offset board
	add bx, cx
	cmp [byte ptr bx], MINE ;is there a mine?
	je ADDNUM_continue

	cmp [byte ptr bx], EMPTY_IN_BOARD
	je isEmpty
	add [byte ptr bx], 1
	jmp ADDNUM_continue

	isEmpty:
	mov [byte ptr bx], ONE_ASCII

	ADDNUM_continue:
		ret
endp addNumber


proc checkLeft 
	mov ax, cx
	mov bl, 8
	div bl
	cmp ah, 0
	ret
endp checkLeft


proc checkRight 
	mov ax, cx
	mov bl, 8
	div bl
	cmp ah, 7
	ret
endp checkRight


proc initNumbers
	pusha

	mov cx, 0
	MAP_loop:
			mov bx, offset board
			add bx, cx
			cmp [byte ptr bx], MINE ;is there a mine?
			jne MAP_continue
			; check for free space above
			cmp cl, 8
			jl MAP_left
			; check for free space to the left
			call checkLeft
			je MAP_top
			; add number
			mov bx, -9
			call addNumber
		MAP_top:
			; add number
			mov bx, -8
			call addNumber
		MAP_topright:
			; check for free space to the right
			call checkRight
			je MAP_left
			; add number
			mov bx, -7
			call addNumber
		MAP_left:
			; check for free space to the left
			call checkLeft
			je MAP_right
			; add number
			mov bx, -1
			call addNumber
		MAP_right:
			; check for free space to the right
			call checkRight
			je MAP_bottomleft
			; add number
			mov bx, 1
			call addNumber
		MAP_bottomleft:
			; check for free space below
			cmp cl, 55
			jg MAP_continue
			; check for free space to the left
			call checkLeft
			je MAP_bottom
			; add number
			mov bx, 7
			call addNumber
		MAP_bottom:
			; add number
			mov bx, 8
			call addNumber
		MAP_bottomright:
			; check for free space to the right
			call checkRight
			je MAP_continue
			; add number
			mov bx, 9
			call addNumber
		MAP_continue:
			inc cx
			cmp cx, BOARD_LEN
			jne MAP_loop

	popa
	ret
endp initNumbers


proc inputToIndex
	pusha

	mov dx, offset newLineMSG
	mov ah, 9
	int 21h

	mov dx, offset inputRowMSG
	mov ah, 9
	int 21h

	call input
	mov al, [validInput]
	mov [inputRow], al
	dec [inputRow]

	mov dx, offset newLineMSG
	mov ah, 9
	int 21h

	mov dx, offset inputColMSG
	mov ah, 9
	int 21h

	call input
	mov al, [validInput]
	mov [inputCol], al
	dec [inputCol]
	call convertRowCol

	popa
	ret

endp inputToIndex


proc input
	pusha
	
	checkInputt:
	mov ah, 08h
	int 21h
	sub al, '0'

	cmp al, 0
	je checkInputt

	cmp al, 8
	ja checkInputt
	jmp successCheck
	
	successCheck:

	mov [validInput], al

	add [validinput], '0'

	mov dl,[validinput]
	mov ah,2
	int 21h

	sub [validinput], '0'

	popa 
	ret
endp input


proc revealCell
	pusha
	
	mov si, offset revealBoard
	mov di, offset board

	add si, [indexAfterCon]
	add di, [indexAfterCon]
	
	cmp [byte ptr di], MINE
	je mineCell

	notMineCell:
	mov al, [byte ptr di]
	mov [byte ptr si], al
	jmp endd


	mineCell:
	mov [isGameOver], 1
	jmp endd
	

	endd:
	popa
	ret
endp revealCell


proc convertRowCol
	pusha

	xor ax, ax
	mov al, ROW_COL_LEN
	mul [inputRow]
	add al, [inputCol]
	mov [indexAfterCon], ax

	popa
	ret
endp convertRowCol


proc checkWin
	push bp
	mov bp, sp
	pusha

	;Inserts the values ​​from the stack into registers
	mov si, [bp + 4] ;board
	mov di, [bp + 6] ;reavel board
	mov cx, [bp + 8]
	
	xor bx, bx
	checkWinLoop:
	mov al, [di + bx]
	cmp al, 23h ;Check if is a cell that not opened
	je checkMine
	jmp continueLoop

	checkMine:
	mov al, [si + bx] 
	cmp al, 2ah
	jne notWin

	continueLoop:
	inc bx
	loop checkWinLoop
	jmp win

	win:
	mov [iswin], 1
	jmp enddd

	notWin:
	mov [iswin], 0

	enddd:
	popa
	pop bp
	ret 6
endp checkwin


proc gameOverP
	pusha

	; clear DOS-Box window
	mov ax, 3h
    int 10h
	
	; switch to graphic mode (40 x 25)	
	mov ax, 13h
	int 10h

	mov [isOpenFileLose], 1
	call OpenFile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap


	; wait for key press
	mov ah, 0h
	int 16h

	; returns to text mode
	mov ah,0h
	mov al, 03h
	int 10h

	popa
	ret
endp gameOverP


proc winGameP
	pusha

	; clear DOS-Box window
	mov ax, 3h
    int 10h

	; switch to graphic mode (40 x 25)	
	mov ax, 13h
	int 10h

	mov [isOpenFileLose], 0
	call openFile
	call readHeader
	call readPalette
	call copyPal
	call copyBitmap

	; wait for key press
	mov ah, 0h
	int 16h

	; returns to text mode
	mov ah,0h
	mov al, 03h
	int 10h


	popa
	ret
endp winGameP


proc openFile
	mov ah, 3Dh
	xor al, al

	cmp [isOpenFileLose], 1
	je openLose
	jne openWin

	openLose:
	mov dx, offset fileLose
	jmp continueOpen

	openWin:
	mov dx, offset fileWin
	jmp continueOpen

	continueOpen:
	int 21h
	jc openerror
	mov [filehandle], ax

	ret
	openerror:
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	ret
endp openFile


proc readHeader
	; Read BMP file header, 54 bytes
	mov ah, 3fh
	mov bx, [filehandle]
	mov cx, HEADER_LEN
	mov dx, offset Header
	int 21h
	ret
endp readHeader


proc readPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah, 3fh
	mov cx, 400h
	mov dx, offset Palette
	int 21h
	ret
endp readPalette


proc copyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si, offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB.
	mov al,[si + 2] ; Get red value.
	;shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it.
	mov al,[si + 1] ; Get green value.
	shr al,2
	out dx,al ; Send it.
	mov al,[si] ; Get blue value.
	shr al,2
	out dx,al ; Send it.
	add si,4 ; Point to next color.
	; (There is a null chr. after every color.)

	loop PalLoop
	ret
endp copyPal


proc copyBitmap
	; BMP graphics are saved upside-down.
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop:
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx, offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si, offset ScrLine

	rep movsb ; Copy line to the screen

	pop cx
	loop PrintBMPLoop
	ret
endp copyBitmap


proc playCellSound
	pusha
	
	; open speaker
	in al, 61h
	or al, 00000011b
	out 61h, al
	; send control word to change frequency
	mov al, 0B6h
	out 43h, al
	; play frequency
	mov ax, [frequency]
	out 42h, al ; Sending lower byte
	mov al, ah
	out 42h, al ; Sending upper byte

	;delay the sound
	mov cx, 02H
	mov dx, 4240H
	mov ah, 86h
	int 15h

	; close the speaker
	in al, 61h
	and al, 11111100b
	out 61h, al

	popa
	ret
endp playcellsound


proc askMine
	pusha

	mov dx, offset easierlevelMSG
	mov ah, 9h
	int 21h

	mov ah, 08h
	int 21h
	sub al, '0'
	
	cmp al, 1
	jne notAskMine

	; clear DOS-Box window
	mov ax, 3h
    int 10h
	
	; switch to graphic mode (40 x 25)	
	mov ax, 13h
	int 10h

	mov dx, offset askMineMSG
	mov ah, 9h
	int 21h

	call inputToIndex
	mov bx, offset board
	add bx, [indexAfterCon]
	mov [byte ptr bx], MINE

	; clear DOS-Box window
	mov ax, 3h
    int 10h
	
	; switch to graphic mode (40 x 25)	
	mov ax, 13h
	int 10h

	notAskMine:
	; clear DOS-Box window
	mov ax, 3h
    int 10h
	
	; switch to graphic mode (40 x 25)	
	mov ax, 13h
	int 10h
	
	popa
	ret
endp askMine


proc game
	pusha

	mov dl, 0 ; Column
	mov dh, 0 ; Row
	mov bx, 0 ; Page number, 0 for graphics modes
	mov ah, 2h
	int 10h

	push BOARD_LEN
	push offset revealBoard
	call printBoard

	gameLoop:

	push BOARD_LEN
	push offset revealBoard
	push offset board
	call checkWin
	cmp [iswin], 1
	je winGame
	
	call inputToIndex
	call revealcell

	cmp [isGameOver], 1
	je gameOverr

	mov dl, 0 ; Column
	mov dh, 0 ; Row
	mov bx, 0 ; Page number, 0 for graphics modes
	mov ah, 2h
	int 10h

	push BOARD_LEN
	push offset revealBoard
	call printBoard
	call playcellsound
	jmp gameloop

	gameOverr:
	call gameOverP
	jmp endddd


	winGame:
	call winGameP
	jmp endddd


	endddd:
	popa
	ret

endp game

proc runGame
	pusha

	call gameFrame
	call askmine
	call setMines
	call initNumbers
	
	;For check, you can remove the note and see the answer board
	;###################
	;push 64
	;push offset board
	;call printBoard
	;###################
	
	call game

	popa
	ret
endp runGame


;----------------------------------------------------------------------------
start:
	mov ax, @data
	mov ds, ax

	call rungame
;----------------------------------------------------------------------------	
exit:
	mov ax, 4c00h
	int 21h
END start
;----------------------------------------------------------------------------