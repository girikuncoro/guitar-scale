tg801		equ	25H
tg802		equ	26H
tg803		equ	27H
tg1001		equ	28H
tg1002		equ	29H
tg1003		equ	30H
tg1201		equ	31H
tg1202		equ	32H
tg1203		equ	33H
tg1401		equ	34H
tg1402		equ	35H
tg1403		equ	36H
del1		equ	37H
del2		equ	38H
del3		equ	39H
pinRS 		bit 	P3.0
pinE		bit 	P3.1

org 00h
call init_lcd
;=============inisial awal==============================
start:       
       	mov r0,#0
       	mov r2,#1
       	mov r3,#1
       	mov r5,#0
       	mov dptr,#word  ;DPTR = [ address word ]
       	mov r4,#16      ;r4=16,number character to be display
       	mov r1,#80h     ;R1=80h,address DDRAM start position
       	call write_inst
       	call write
       	mov dptr,#word2
       	mov r4,#16
       	mov r1,#0c0h
       	call write_inst
       	call write
       	ajmp tekan
write:
	clr a           ; A = 0
       	movc a,@a+dptr  ; A = [A+ DPTR]
       	mov r1,A        ; R1 = A 
       	inc dptr        ; DPTR = DPTR +1 
       	call write_data
       	djnz r4,write	; r4 = r4-1, 
       	ret
init_lcd:
    	mov r1,#00000001b 	;Display clear
    	call write_inst  	
    	mov r1,#00111000b 	;Function set, Data 8 bit,2 line font 5x7
    	call write_inst  	
    	mov r1,#00001100b 	;Display on, cursor off,cursor blink off
    	call write_inst
    	mov r1,#00000110b 	;Entry mode, Set increment
    	call write_inst
    	ret    	
write_inst:
    	clr pinRS  	; RS = P3.0 = 0, write mode instruction
    	mov P1,R1 	; D7 s/d D0 = P0 = R1
    	setb pinE 	; EN = 1 = P3.1 
    	call delay	; call delay time
    	clr pinE  	; EN = 0 = P3.1
    	ret
write_data:
    	setb pinRS 	; RS = P3.0 = 1, write mode data
    	mov P1,R1 	; D7 s/d D0 = P0 = R1
    	setb pinE 	; EN = 1 = P3.1
    	call delay	; call delay time
    	clr pinE  	; EN = 0 = P3.1
    	ret
delay: 	
		mov del3,#0
delay1:	mov del2,#0fh
       	djnz del2,$
       	djnz del3,delay1
       	ret
;==================== tempo ===============================
wait:	jnb p3.6,balik
	cjne r0,#1,tunggu	;delay 0.75 s
	mov tg803,#37
wait1:	mov tg802,#100
wait2:	mov tg801,#100
	djnz tg801, $
 	djnz tg802, wait2
 	djnz tg803, wait1
 	ret
 	
tunggu:	jnb p3.6,balik
	cjne r0,#2,bentar	;delay 0.6 s
	mov tg1003,#29
tunggu1:mov tg1002,#102
tunggu2:mov tg1001,#100
	djnz tg1001, $
 	djnz tg1002, tunggu2
 	djnz tg1003, tunggu1
 	ret	
 		
bentar:	jnb p3.6,balik
	cjne r0,#3,heula	;delay 0.5 s	
	mov tg1203,#24
bentar1:mov tg1202,#102
bentar2:mov tg1201,#100
	djnz tg1201, $
 	djnz tg1202, bentar2
 	djnz tg1203, bentar1
 	ret	
 		
heula:	jnb p3.6,balik
	cjne r0,#4,wait		;delay 0.4286 s
	mov tg1403,#21
heula1:	mov tg1402,#100
heula2:	mov tg1401,#100
	djnz tg1401, $
 	djnz tg1402, heula2
 	djnz tg1403, heula1
 	ret
;=====================tekan tombol=============================
tekan:
		mov r7,#3
		jnb p3.3,rst
		jnb p3.4,naik
		jnb p3.5,temp
		jnb p3.6,salah
		ljmp tekan
temp:		
		ljmp tempo
naik:
		ljmp up		
rst:
		ljmp start
balik:
		jnb p3.6,$
		call delay
		mov p2,#255
		jmp tekan
salah:		
	jnb p3.6,$
	call delay
	cjne r0,#0,salah1
	cjne r5,#0,salah2
	jmp salah3
salah1:	cjne r5,#0,jalan1
	mov dptr,#word_tekan
       	mov r4,#16
       	mov r1,#80h
       	call write_inst
       	call write
       	jmp tekan
salah2:	mov dptr,#word_tekan2
       	mov r4,#16
       	mov r1,#0c0h
       	call write_inst
       	call write
       	jmp tekan
salah3:	mov dptr,#word_tekan3
       	mov r4,#16
       	mov r1,#80h
       	call write_inst
       	call write
       	mov dptr,#word_tekan4
       	mov r4,#16
       	mov r1,#0c0h
       	call write_inst
       	call write
	mov r0,#3
	call wait
	mov r0,#0
       	ajmp start

jalan1:
	cjne r5,#1,jalan2
	call nyala_awal
aoelian:	call dor1
	call phry2
	call dor3
	call phry4
	call phry5
	call dor6
	jb p3.6,aoelian
	ljmp balik
jalan2:
	cjne r5,#2,jalan3
	call nyala_awal
arabian:	call ion1
	call loc2
	call mix3
		mov	p0, #00010000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00011000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00011100b
		mov	p2, #11110111b
		call wait
		mov	p0, #00011101b
		mov	p2, #11110111b
		call wait
	call arabian5
	call ion6
	jb p3.6,arabian
	ljmp balik
jalan3:
	cjne r5,#3,jalan4
	call nyala_awal
arpmaj:	mov p0, #00100000b
	mov p2, #11111110b
	call wait
	mov p0, #00100010b
	mov p2, #11111110b
	call wait
	call arpeggio_minor2
	call arpeggio_major3
		mov	p0, #00010000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00010010b
		mov	p2, #11110111b
		call wait
	call arpeggio_minor5
		mov	p0, #00100000b
		mov	p2, #11011111b
		call wait
		mov	p0, #00100010b
		mov	p2, #11011111b
		call wait
	jb p3.6,arpmaj
	ljmp balik
jalan4:
	cjne r5,#4,jalan5
	call nyala_awal
arpmin:	call arpeggio_minor1
	call arpeggio_minor2
	call arpeggio_major3
	mov	p0, #00001001b
	mov	p2, #11111011b
	call wait
		mov	p0, #00100000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00100010b
		mov	p2, #11110111b
		call wait
	call arpeggio_minor5
	call arpeggio_minor6
	jb p3.6,arpmin
	ljmp balik
jalan5:
	cjne r5,#5,jalan6
	call nyala_awal
augmented:
		mov	p0, #00100000b
		mov	p2, #11111110b
		call wait
		mov	p0, #00100100b
		mov	p2, #11111110b
		call wait
		mov	p0, #00100110b
		mov	p2, #11111110b
		call wait

		mov	p0, #00001000b
		mov	p2, #11111101b
		call wait
		mov	p0, #00001100b
		mov	p2, #11111101b
		call wait
	call harmonic_minor3
		mov	p0, #00100000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00110000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00110010b
		mov	p2, #11110111b
		call wait
		mov	p0, #00110011b
		mov	p2, #11110111b
		call wait
	call neamin5
		mov	p0, #00100000b
		mov	p2, #11011111b
		call wait
		mov	p0, #00100100b
		mov	p2, #11011111b
		call wait
		mov	p0, #00100110b
		mov	p2, #11011111b
		call wait
	jb p3.6,augmented
	ljmp balik
jalan6:
	cjne r5,#6,jalan7
	call nyala_awal
be_bop:
	call ion1
	call dor2
		mov	p0, #00100000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00110000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00111000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00111010b
		mov	p2, #11111011b
		call wait
	call ion4
		mov	p0, #00100000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00101000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00101100b
		mov	p2, #11101111b
		call wait
		mov	p0, #00101110b
		mov	p2, #11101111b
		call wait
		mov	p0, #00101111b
		mov	p2, #11101111b
		call wait
	call ion6
	jb p3.6,be_bop
	ljmp balik
jalan7:
	cjne r5,#7,jalan8
	call nyala_awal
bluemaj:
	call blumaj1
	call blumaj2
	call blumaj3
	call blumaj4
	mov p0,#00100000b
	mov p2,#11101111b	
	call wait
	mov p0,#00101000b
	mov p2,#11101111b
	call wait
	mov p0,#00101001b
	mov p2,#11101111b
	call wait
	call blumaj6
	jb p3.6,bluemaj
	ljmp balik
jalan8:
	cjne r5,#8,jalan9
	call nyala_awal
bluemin:
	call blumaj1
	call blumaj2
		mov p0,#00100000b
		mov p2,#11111011b
		call wait
	call blumaj3
	call blumaj4
		mov p0,#00100000b
		mov p2,#11101111b
		call wait
		mov p0,#00100100b
		mov p2,#11101111b
		call wait
		mov p0,#00100101b
		mov p2,#11101111b
		call wait
	call blumaj6
	jb p3.6,bluemin
	ljmp balik
walk:	ljmp jalan10
jalan9:
	cjne r5,#9,walk
	call nyala_awal
chrom:		mov	p0, #00100000b	;11111111
		call chrom1
		mov	p0, #00110000b
		call chrom1		
		mov	p0, #00111000b
		call chrom1		
		mov	p0, #00111100b
		call chrom1
		mov	p0, #00111110b
		call chrom1		
		mov	p0, #00111111b
		call chrom1
		mov	p0, #00100000b
		call chrom2
		mov	p0, #00110000b
		call chrom2
		mov	p0, #00111000b
		call chrom2
		mov	p0, #00111100b
		call chrom2
		mov	p0, #00111110b
		call chrom2
		mov	p0, #00111111b
		call chrom2
		mov	p0, #00100000b
		call chrom3
		mov	p0, #00110000b
		call chrom3
		mov	p0, #00111000b
		call chrom3
		mov	p0, #00111100b
		call chrom3
		mov	p0, #00111110b
		call chrom3
		mov	p0, #00111111b
		call chrom3
		mov	p0, #00100000b
		call chrom4
		mov	p0, #00110000b
		call chrom4
		mov	p0, #00111000b
		call chrom4
		mov	p0, #00111100b
		call chrom4
		mov	p0, #00111110b
		call chrom4
		mov	p0, #00111111b
		call chrom4
		mov	p0, #00100000b
		call chrom5
		mov	p0, #00110000b
		call chrom5
		mov	p0, #00111000b
		call chrom5
		mov	p0, #00111100b
		call chrom5
		mov	p0, #00111110b
		call chrom5
		mov	p0, #00111111b
		call chrom5
		mov	p0, #00100000b
		call chrom6
		mov	p0, #00110000b
		call chrom6
		mov	p0, #00111000b
		call chrom6
		mov	p0, #00111100b
		call chrom6
		mov	p0, #00111110b
		call chrom6
		mov	p0, #00111111b
		call chrom6
	jb p3.6,loncat
	ljmp balik
loncat:	ljmp chrom
jalan10:
	cjne r5,#10,jalan11
	call nyala_awal
dimarp:
	call arpeggio_minor1	
		mov	p0, #00010000b
		mov	p2, #11111101b
		call wait
		mov	p0, #00010010b
		mov	p2, #11111101b
		call wait
	call arpeggio_minor3
		mov	p0, #00100000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00100100b
		mov	p2, #11110111b
		call wait
		mov	p0, #00001000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00001001b
		mov	p2, #11101111b
		call wait
	call arpeggio_minor6
	jb p3.6,dimarp
	ljmp balik
jalan11:
	cjne r5,#11,jalan12
	call nyala_awal
dorian:
	call dor1
	call dor2
	call dor3
	call dor4
	call dor5
	call dor6
	jb p3.6,dorian
	ljmp balik
jalan12:
	cjne r5,#12,jalan13
	call nyala_awal
harmin:
		mov	p0, #00100000b
		mov	p2, #11111110b
		call wait
		mov	p0, #00101000b
		mov	p2, #11111110b
		call wait
		mov	p0, #00101100b
		mov	p2, #11111110b
		call wait	

		mov	p0, #00100000b
		mov	p2, #11111101b
		call wait
		mov	p0, #00101000b
		mov	p2, #11111101b
		call wait
		mov	p0, #00101100b
		mov	p2, #11111101b
		call wait
	call harmonic_minor3
	call dor4
	mov	p0, #00101011b
	mov	p2, #11110111b
	call wait
		mov	p0, #00100000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00110000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00110001b
		mov	p2, #11101111b
		call wait

		mov	p0, #00100000b
		mov	p2, #11011111b
		call wait
		mov	p0, #00101000b
		mov	p2, #11011111b
		call wait
		mov	p0, #00101100b
		mov	p2, #11011111b
		call wait
	jb p3.6,harmin
	ljmp balik

jalan13:
	cjne r5,#13,jalan14
	call nyala_awal
ionian:
	call ion1
	call ion2
	call ion3
	call ion4
	call ion5
	call ion6
	jb p3.6,ionian
	ljmp balik
jalan14:
	cjne r5,#14,jalan15
	call nyala_awal
locrian:
	call loc1
	call loc2
	call loc3
		mov p0,#00100000b
		mov p2,#11110111b		
		call wait
		mov p0,#00101000b
		mov p2,#11110111b
		call wait
		mov p0,#00101100b
		mov p2,#11110111b
		call wait
		mov p0,#00101101b
		mov p2,#11110111b
		call wait
	call loc5
	call loc6
	jb p3.6,locrian
	ljmp balik
jalan15:
	cjne r5,#15,jalan16
	call nyala_awal
lydian:
	call lyd1
		mov p0,#00010000b
		mov p2,#11111101b	;010110
		call wait
		mov p0,#00011000b
		mov p2,#11111101b
		call wait
		mov p0,#00011010b
		mov p2,#11111101b
		call wait
	call ion3
		mov p0,#00010000b
		mov p2,#11110111b	;011010
		call wait
		mov p0,#00010100b
		mov p2,#11110111b
		call wait
		mov p0,#00010110b
		mov p2,#11110111b
		call wait
	call ion5
	call lyd6
	jb p3.6,lydian
	ljmp balik
jalan16:
	cjne r5,#16,jalan17
	call nyala_awal
major:	call ion1
	call ion2	
	call ion3
	call ion4
		mov	p0, #00100000b
		mov	p2, #11101111b	;101010
		call wait
		mov	p0, #00101000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00101010b
		mov	p2, #11101111b
		call wait
	call lyd6
	jb p3.6,major
	ljmp balik
jalan17:
	cjne r5,#17,jalan18
	call nyala_awal
minor:	call dor1
	call phry2
	call dor3
	call phry4
	call phry5
	call dor6
	jb p3.6,minor
	ljmp balik
jalan18:
	cjne r5,#18,jalan19
	call nyala_awal
melodic:
	call dor1
	call ion2
	call ion3
	mov p0,#00011011b
	mov p2,#11111011b		
	call wait
	call dor4
	call ion5
	call dor6
	jb p3.6,melodic
	ljmp balik
jalan19:
	cjne r5,#19,jalan20
	call nyala_awal
mixo:	call ion1
	call dor2
	call mix3	
	call ion4
	call dor5
	call ion6
	jb p3.6,mixo
	ljmp balik
jalan20:
	cjne r5,#20,jalan21
	call nyala_awal
neamaj:	call loc1
	call ion2
	call neamin3
	call dor4
	call ion5
	call loc6
	jb p3.6,neamaj
	ljmp balik
jalan21:
	cjne r5,#21,jalan22
	call nyala_awal
neamin: call loc1
	call neamin2
	call neamin3
	call dor4
	mov p0,#00101011b
	mov p2,#11110111b
	call wait
	call neamin5
	call loc6
	jb p3.6,neamin
	ljmp balik
jalan22:
	cjne r5,#22,jalan23
	call nyala_awal
penmaj: call lyd1
		mov	p0, #00001000b
		mov	p2, #11111101b
		call wait
		mov	p0, #00001010b
		mov	p2, #11111101b
		call wait
	
		mov	p0, #00001000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00001010b
		mov	p2, #11111011b
		call wait
	
		mov	p0, #00010000b
		mov	p2, #11110111b
		call wait
		mov	p0, #00010010b
		mov	p2, #11110111b
		call wait
		
		mov	p0, #00100000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00101000b
		mov	p2, #11101111b
		call wait
	call lyd6
	jb p3.6,penmaj
	ljmp balik
jalan23:
	cjne r5,#23,jalan24
	call nyala_awal
penmin: 
		mov	p0, #00100000b
		mov	p2, #11111110b
		call wait
		mov	p0, #00100001b
		mov	p2, #11111110b
		call wait
		
		mov	p0, #00100000b
		mov	p2, #11111101b
		call wait
		mov	p0, #00101000b
		mov	p2, #11111101b
		call wait
		mov	p0, #00101001b
		mov	p2, #11111101b
		call wait
		
		mov	p0, #00100000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00101000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00101001b
		mov	p2, #11111011b
		call wait
	call dor4
		mov	p0, #00100000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00100100b
		mov	p2, #11101111b
		call wait
		mov	p0, #00100101b
		mov	p2, #11101111b
		call wait
	call blumaj6
	jb p3.6,penmin
	ljmp balik
jalan24:
	cjne r5,#24,jalan25
	call nyala_awal
phrygian:
	call loc1
	call phry2
	call loc3
	call phry4
	call phry5	
	call loc6
	jb p3.6,phrygian
	ljmp balik
jalan25:
	mov r5,#0
	ljmp rst
;=======================pilih scale=================================
up:
	jnb p3.4,$
	call delay
	cjne r2,#1,up2
	mov dptr,#word3
cetak:	mov r4,#16 
       	mov r1,#80h
       	call write_inst
       	call write
       	inc r2
       	inc r5
       	ljmp tekan

up2:	cjne r2,#2,up3
		mov dptr,#word4
		ljmp cetak
up3:	cjne r2,#3,up4
		mov dptr,#word5
		ljmp cetak
up4:	cjne r2,#4,up5
		mov dptr,#word6
		ljmp cetak
up5:	cjne r2,#5,up6
		mov dptr,#word7
		ljmp cetak
up6:	cjne r2,#6,up7
		mov dptr,#word8
		ljmp cetak
up7:	cjne r2,#7,up8
		mov dptr,#word9
		ljmp cetak
up8:	cjne r2,#8,up9
		mov dptr,#word10
		ljmp cetak
up9:	cjne r2,#9,up10
		mov dptr,#word11
		ljmp cetak
up10:	cjne r2,#10,up11
		mov dptr,#word12
		ljmp cetak
up11:	cjne r2,#11,up12
		mov dptr,#word13
		ljmp cetak
up12:	cjne r2,#12,up13
		mov dptr,#word14
		ljmp cetak
up13:	cjne r2,#13,up14
		mov dptr,#word15
		ljmp cetak
up14:	cjne r2,#14,up15
		mov dptr,#word16
		ljmp cetak
up15:	cjne r2,#15,up16
		mov dptr,#word17
		ljmp cetak
up16:	cjne r2,#16,up17
		mov dptr,#word18
		ljmp cetak
up17:	cjne r2,#17,up18
		mov dptr,#word19
		ljmp cetak
up18:	cjne r2,#18,up19
		mov dptr,#word20
		ljmp cetak
up19:	cjne r2,#19,up20
		mov dptr,#word21
		ljmp cetak
up20:	cjne r2,#20,up21
		mov dptr,#word22
		ljmp cetak
up21:	cjne r2,#21,up22
		mov dptr,#word23
		ljmp cetak
up22:	cjne r2,#22,up23
		mov dptr,#word24
		ljmp cetak
up23:	cjne r2,#23,up24
		mov dptr,#word25
		ljmp cetak
up24:	cjne r2,#24,up25
		mov dptr,#word26
		ljmp cetak
up25:	mov r2,#1
		ljmp up	       	
;==================pilih tempo=============================
tempo:	jnb p3.5,$
		call delay
		cjne r3,#1,tempo2
		mov r0,#1
		mov dptr,#word27
print:	mov r4,#16 
       	mov r1,#0c0h
       	call write_inst
       	call write
       	inc r3
       	ljmp tekan   
tempo2:	cjne r3,#2,tempo3
		mov r0,#2
		mov dptr,#word28
		ljmp print
tempo3:	cjne r3,#3,tempo4
		mov r0,#3
		mov dptr,#word29
		ljmp print
tempo4:	cjne r3,#4,tempo5
		mov r0,#4
		mov dptr,#word30
		ljmp print
tempo5:	mov r3,#1
		ljmp tempo			       	
;==================kesamaan scale===========================
nyala_awal:	clr p3.7
		call wait
		setb p3.7
		call wait
		djnz r7,nyala_awal
		ret
arabian5:
		mov p0,#00010000b	;phrygian5,aeolian5,minor5,(locrian5,arabian5)+6
		mov p2,#11101111b	;101011
		call wait
		mov p0,#00010100b
		mov p2,#11101111b
		call wait
		mov p0,#00010101b
		mov p2,#11101111b
		call wait
		ret	
arpeggio_major3:
		mov	p0, #00001000b
		mov	p2, #11111011b
		call wait
		ret
arpeggio_minor1:		;arpeggio minor1, diminished arpeggio 1
		mov	p0, #00100000b
		mov	p2, #11111110b
		call wait
		mov	p0, #00100100b
		mov	p2, #11111110b
		call wait
		ret
arpeggio_minor2:		;arpeggio minor2, arpeggio major2,
		mov	p0, #00001000b
		mov	p2, #11111101b
		call wait
		ret
arpeggio_minor3:		;arpeggio minor3, diminished arpeggio 3
		mov	p0, #00001000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00001001b
		mov	p2, #11111011b
		call wait
		ret
arpeggio_minor5:		;arpeggio minor5, arpeggio major 5,
		mov	p0, #00100000b
		mov	p2, #11101111b
		call wait
		mov	p0, #00100001b
		mov	p2, #11101111b
		call wait
		ret
arpeggio_minor6: 		;arpeggio minor6, diminished arpeggio 6
		mov	p0, #00100000b
		mov	p2, #11011111b
		call wait
		mov	p0, #00100100b
		mov	p2, #11011111b
		call wait
		ret
blumaj1:
		mov p0,#00100000b
		mov p2,#11111110b	;101001 blues major1,blues minor1
		call wait
		mov p0,#00100100b
		mov p2,#11111110b		
		call wait
		mov p0,#00100101b
		mov p2,#11111110b
		call wait
		ret
blumaj2:
		mov p0,#00100000b	;blues major2,blues minor2
		mov p2,#11111101b	;010111
		call wait
		mov p0,#00110000b
		mov p2,#11111101b		
		call wait
		mov p0,#00111000b
		mov p2,#11111101b
		call wait
		mov p0,#00111010b
		mov p2,#11111101b
		call wait
		ret
blumaj3:
		mov p0,#00001000b		;blues major3,blues minor3+1
		mov p2,#11111011b	;100100
		call wait
		mov p0,#00001001b
		mov p2,#11111011b		
		call wait
		ret
blumaj4:	
		mov p0,#00100000b		;blues major4,blues minor4
		mov p2,#11110111b	;011101
		call wait
		mov p0,#00101000b
		mov p2,#11110111b	
		call wait
		mov p0,#00101100b
		mov p2,#11110111b		
		call wait
		mov p0,#00101110b
		mov p2,#11110111b	
		call wait
		ret	
blumaj6:	
		mov p0,#00100000b		;blues major6,blues minor6,pentatonic minor6
		mov p2,#11011111b	;101001	
		call wait
		mov p0,#00100100b
		mov p2,#11011111b
		call wait
		mov p0,#00100101b
		mov p2,#11011111b
		call wait
		ret
chrom1:		mov	p2, #11111110b	;chrom satu"nya
		call wait
		ret
chrom2:		mov	p2, #11111101b
		call wait
		ret
chrom3:		mov	p2, #11111011b
		call wait
		ret
chrom4:		mov	p2, #11110111b
		call wait
		ret
chrom5:		mov	p2, #11101111b
		call wait
		ret
chrom6:		mov	p2, #11011111b
		call wait
		ret
dor1:	
		mov p0,#00100000b	;dorian1,aeolian1,minor1,melodic1
		mov p2,#11111110b	;101101
		call wait
		mov p0,#00101000b
		mov p2,#11111110b		
		call wait
		mov p0,#00101100b
		mov p2,#11111110b
		call wait
		mov p0,#00101101b
		mov p2,#11111110b
		call wait
		ret
dor2:
		mov p0,#00100000b	;dorian2,mixolydian2,Be-Bop2
		mov p2,#11111101b	;110101
		call wait
		mov p0,#00101000b
		mov p2,#11111101b	
		call wait
		mov p0,#00101010b
		mov p2,#11111101b
		call wait
		mov p0,#00101011b
		mov p2,#11111101b
		call wait
		ret
dor3:
		mov p0,#00100000b	;dorian3,aeolian3,minor3
		mov p2,#11111011b	;110101
		call wait
		mov p0,#00101000b	
		mov p2,#11111011b	
		
		call wait
		mov p0,#00101010b
		mov p2,#11111011b
		call wait
		mov p0,#00101011b
		mov p2,#11111011b
		call wait
		ret		
dor4:
		mov p0,#00100000b	;dorian4,pentatonic minor4,melodic4,neapolitan major4
		mov p2,#11110111b	;010101
		call wait
		mov p0,#00101000b
		mov p2,#11110111b		
		call wait
		mov p0,#00101010b
		mov p2,#11110111b
		call wait
		ret
dor5:
		mov p0,#00100000b	;dorian5,mixolydian5,
		mov p2,#11101111b	;101101	
		call wait
		mov p0,#00101000b
		mov p2,#11101111b		
		call wait
		mov p0,#00101100b
		mov p2,#11101111b
		call wait
		mov p0,#00101101b
		mov p2,#11101111b
		call wait
		ret
dor6:
		mov p0,#00100000b	;dorian6,aeolian6,minor6,melodic6
		mov p2,#11011111b	;101101
		call wait
		mov p0,#00101000b
		mov p2,#11011111b		
		call wait
		mov p0,#00101100b
		mov p2,#11011111b
		call wait
		mov p0,#00101101b
		mov p2,#11011111b
		call wait
		ret
harmonic_minor3:            ; harmonicminor 3 augmented3
		mov	p0, #00010000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00011000b
		mov	p2, #11111011b
		call wait
		mov	p0, #00011001b
		mov	p2, #11111011b
		call wait
		ret
ion1:						;110101
		mov p0,#00100000b
		mov p2,#11111110b	;ionian1,mixolydian1,major1,be-bop1,arabian1
		call wait
		mov p0,#00101000b
		mov p2,#11111110b
		call wait
		mov p0,#00101010b
		mov p2,#11111110b
		call wait
		mov p0,#00101011b
		mov p2,#11111110b
		call wait
		ret
ion2:						;010101
		mov p0,#00100000b
		mov p2,#11111101b	;ionian2,melodic2,neapolitan major2
		call wait
		mov p0,#00101000b
		mov p2,#11111101b
		call wait
		mov p0,#00101010b
		mov p2,#11111101b
		call wait
		ret
ion3:						;010110
		mov p0,#00010000b
		mov p2,#11111011b	;ionian3,lydian3,major3,
		call wait
		mov p0,#00011000b
		mov p2,#11111011b
		call wait
		mov p0,#00011010b
		mov p2,#11111011b
		call wait
		ret
ion4:						;010110
		mov p0,#00010000b
		mov p2,#11110111b	;ionian4,mixolydian4,major4,be-bop4
		call wait
		mov p0,#00011000b
		mov p2,#11110111b
		call wait
		mov p0,#00011010b
		mov p2,#11110111b
		call wait		
		ret
ion5:						;110101
		mov p0,#00100000b
		mov p2,#11101111b	;ionian5,lydian5,melodic5,neapolitan major5
		call wait
		mov p0,#00101000b
		mov p2,#11101111b
		call wait
		mov p0,#00101010b
		mov p2,#11101111b
		call wait		
		mov p0,#00101011b
		mov p2,#11101111b
		call wait
		ret
ion6:						;110101
		mov p0,#00100000b
		mov p2,#11011111b	;ionian6,mixolydian6,Be-bop6,Arabian6
		call wait
		mov p0,#00101000b
		mov p2,#11011111b
		call wait
		mov p0,#00101010b
		mov p2,#11011111b
		call wait		
		mov p0,#00101011b
		mov p2,#11011111b
		call wait
		ret
loc1:
		mov p0,#00100000b	;locrian1,phrygian1,neapolitan major1,neapolitan minor1
		mov p2,#11111110b	;101011
		call wait
		mov p0,#00110000b
		mov p2,#11111110b
		call wait
		mov p0,#00110100b
		mov p2,#11111110b
		call wait
		mov p0,#00110101b
		mov p2,#11111110b	
		call wait
		ret
loc2:
		mov p0,#00100000b	;locrian2,arabian2,
		mov p2,#11111101b	;101011
		call wait
		mov p0,#00110000b
		mov p2,#11111101b
		call wait
		mov p0,#00110100b
		mov p2,#11111101b
		call wait
		mov p0,#00110101b
		mov p2,#11111101b
		call wait
		ret
loc3:
		mov p0,#00100000b	;locrian3,phrygian3,
		mov p2,#11111011b	;101101
		call wait
		mov p0,#00101000b
		mov p2,#11111011b
		call wait
		mov p0,#00101100b
		mov p2,#11111011b
		call wait
		mov p0,#00101101b
		mov p2,#11111011b
		call wait
		ret		
loc5:
		mov p0,#00010000b	;locrian5,arabian5,
		mov p2,#11101111b	;101010
		call wait
		mov p0,#00010100b
		mov p2,#11101111b
		call wait
		mov p0,#00010101b
		mov p2,#11101111b
		call wait
		ret
loc6:
		mov p0,#00100000b	;locrian6,phrygian6,neapolitan major6,neapolitan minor6
		mov p2,#11011111b	;101011
		call wait
		mov p0,#00110000b
		mov p2,#11011111b		
		call wait
		mov p0,#00110100b
		mov p2,#11011111b
		call wait
		mov p0,#00110101b
		mov p2,#11011111b
		call wait
		ret	
lyd1:
		mov p0,#00100000b	;lydian1,pentatonic major1
		mov p2,#11111110b	;010101
		call wait
		mov p0,#00101000b
		mov p2,#11111110b
		call wait
		mov p0,#00101010b
		mov p2,#11111110b
		call wait
		ret
lyd6:
		mov p0,#00100000b	;lydian6,major6,pentatonic major6
		mov p2,#11011111b	;010101
		call wait
		mov p0,#00101000b
		mov p2,#11011111b
		call wait
		mov p0,#00101010b
		mov p2,#11011111b
		call wait
		ret
mix3:
		mov p0,#00100000b	;mixolydian3,arabian3
		mov p2,#11111011b	;010101
		call wait
		mov p0,#00101000b
		mov p2,#11111011b		
		call wait
		mov p0,#00101010b
		mov p2,#11111011b
		call wait
		ret
neamin2:
		mov p0,#00100000b	;phrygian2,aeolian2,minor2,neapolitan minor2+1,augmented 2+1+6
		mov p2,#11111101b	;101101
		call wait
		mov p0,#00101000b
		mov p2,#11111101b		
		call wait
		mov p0,#00101100b
		mov p2,#11111101b
		call wait
		ret
neamin3:
		mov p0,#00010000b	;neapolitan major3,neapolitan minor3
		mov p2,#11111011b	;101110
		call wait
		mov p0,#00011000b
		mov p2,#11111011b
		call wait
		mov p0,#00011100b
		mov p2,#11111011b
		call wait
		mov p0,#00011101b
		mov p2,#11111011b
		call wait
		ret
neamin5:
		mov p0,#00100000b	;neapolitan minor5,augmented5
		mov p2,#11101111b	;110011
		call wait
		mov p0,#00110000b
		mov p2,#11101111b
		call wait
		mov p0,#00110010b
		mov p2,#11101111b
		call wait
		mov p0,#00110011b
		mov p2,#11101111b
		call wait
		ret
phry2:	
		call neamin2
		mov p0,#00101101b
		mov p2,#11111101b
		call wait
		ret
phry4:
		mov p0,#00100000b	;phrygian4,aeolian4,minor4,(dorian4,pentatonic minor4,melodic4,neapolitan major4)+1
		mov p2,#11110111b	;110101
		call wait
		mov p0,#00101000b
		mov p2,#11110111b		
		call wait
		mov p0,#00101010b
		mov p2,#11110111b
		call wait
		mov p0,#00101011b
		mov p2,#11110111b
		call wait
		ret
phry5:
		mov p0,#00100000b	;phrygian5,aeolian5,minor5,(locrian5,arabian5)+6
		mov p2,#11101111b	;101011
		call wait
		mov p0,#00110000b
		mov p2,#11101111b
		call wait
		mov p0,#00110100b
		mov p2,#11101111b
		call wait
		mov p0,#00110101b
		mov p2,#11101111b
		call wait
		ret	
;=====================data LCD============================
word:			db '  SELECT SCALE  '	
word2:			db '  SELECT TEMPO  '		
word3:			db '     Aeolian    '
word4:			db '     Arabian    '
word5:			db ' Arpeggio Major '
word6:			db ' Arpeggio Minor '
word7:			db '    Augmented   '
word8:			db '     Be-Bop     '
word9:			db '   Blues Major  '		
word10:			db '   Blues Minor  '
word11:			db '    Chromatic   '
word12:			db '  Dim Arpeggio  '
word13:			db '     Dorian     '
word14:			db ' Harmonic Minor '
word15:			db '     Ionian     '
word16:			db '     Locrian    '
word17:			db '     Lydian     '
word18:			db '      Major     '
word19:			db '      Minor     '
word20:			db 'Melodic/Jazz min'
word21:			db '   Mixolydian   '
word22:			db 'Neapolitan Major'
word23:			db 'Neapolitan Minor'
word24:			db 'Pentatonic Major'
word25:			db 'Pentatonic Minor'
word26:			db '   Phyrigian    '	
word27:			db '   TEMPO: 80    '
word28:			db '   TEMPO: 100   '
word29:			db '   TEMPO: 120   '
word30:			db '   TEMPO: 140   ' 
word_tekan:		db ' CHOOSE SCALE!! ' 
word_tekan2:		db ' CHOOSE TEMPO!! '	        
word_tekan3:		db ' CHOOSE FIRST!! '
word_tekan4:		db '' 
end