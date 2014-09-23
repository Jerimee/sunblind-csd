/* sine_bass_wave.inc */

; ignore the name - this instrument
; does not necessarily produce
; a bass sound

idur	= p3
ivelcy 	= p4
ifreq 	= (p5 > 15 ? p5 : cpspch(p5))


imyrandom random 0, 9
irdvd = imyrandom / 4
iroll = (30 * int(irdvd)) 
;printks2 "Roll! A fifth of the time it is 60, 2/5 out is 30, and 2/5 out is 0. And the roll is %d!!\n", iroll
ifreq = (ifreq + iroll) + 40

iampForKenv = (ivelcy/127)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; http://www.csounds.com/manual/html/adsr.html
; 			attTIME		decayTIME	sustVOL		relTIME
kenv adsr 	0.02, 		0.5, 		iampForKenv, 			0.075
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

iampForLinseg 	= ((ivelcy+10)/254)*10
iseg 			= idur/200
iX 				= iampForLinseg
iH 				= iampForLinseg/1.5
iQ 				= iampForLinseg/2.5
iTINY 			= 0.003

; kenv expseg \
;	 	iTINY, 					idur/5, \ 
;	 	iampForKenv/1.05, 		idur/5, \ 
;	 	iampForKenv/0.80,		idur/5, \
;	 	iampForKenv,			idur/5, \
;	 	iampForKenv/1.01,		idur/5, \	 	 
;	 	iTINY

klinseg expseg \
	 	iTINY, 		iseg, \ 
		iTINY,		iseg, \ 
		iTINY,		iseg, \ ; 03
		iTINY,		iseg, \ ; 04
		iTINY,		iseg, \ ; 05
	 	iH, 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ ; 08
		iQ,			iseg, \ ; 09
		iX,			iseg, \ ; 10
	 	iQ, 		iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 15
	 	iX,	 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ ; 18
		iQ,			iseg, \
		iH,			iseg, \ ; 20
	 	iQ, 		iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iX,			iseg, \
		iQ,			iseg, \ ; 25
	 	iX, 		iseg, \
		iQ,			iseg, \
		iH,			iseg, \ ; 28
		iQ,			iseg, \ ; 29
		iX,			iseg, \ ; 30
		iQ,			iseg, \
	 	iX, 		iseg, \
		iQ,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 35
		iH,			iseg, \
	 	iQ,			iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iH,			iseg, \ ; 40
		iQ,			iseg, \
	 	iH,	 		iseg, \
		iQ,			iseg, \ ; 43
		iH,			iseg, \
		iQ,			iseg, \ ; 45
		iH,			iseg, \
	 	iX, 		iseg, \
		iH,			iseg, \
		iX,			iseg, \
		iQ,			iseg, \ ; 50
	 	iX, 		iseg, \ 
		iH,			iseg, \ 
		iQ,			iseg, \ ; 53
		iX,			iseg, \ ; 54
		iQ,			iseg, \ ; 55
	 	iX, 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ ; 58
		iQ,			iseg, \ ; 59
		iX,			iseg, \ ; 60
	 	iQ, 		iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 65
	 	iX,	 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ ; 68
		iQ,			iseg, \
		iH,			iseg, \ ; 70
	 	iQ, 		iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iX,			iseg, \
		iQ,			iseg, \ ; 75
	 	iX, 		iseg, \
		iH,			iseg, \
		iH,			iseg, \ ; 
		iH,			iseg, \ ; 
		iX,			iseg, \ ; 80
		iH,			iseg, \
	 	iX, 		iseg, \
		iH,			iseg, \
		iH,			iseg, \
		iH,			iseg, \ ; 85
		iQ,			iseg, \
	 	iH, 		iseg, \
		iX,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 90
		iH,			iseg, \
	 	iQ,	 		iseg, \
		iH,			iseg, \ ; 93
		iQ,			iseg, \
		iH,			iseg, \ ; 95
		iH,			iseg, \
	 	iH,	 		iseg, \
		iX,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 100
	 	iH, 		iseg, \ 
		iX,			iseg, \ 
		iQ,			iseg, \ ; 103
		iQ,			iseg, \ ; 104
		iQ,			iseg, \ ; 105
	 	iX, 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ ; 108
		iQ,			iseg, \ ; 109
		iX,			iseg, \ ; 110
	 	iQ, 		iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 115
	 	iX,	 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ ; 118
		iQ,			iseg, \
		iH,			iseg, \ ; 120
	 	iQ, 		iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iQ,			iseg, \
		iQ,			iseg, \ ; 125
	 	iX, 		iseg, \
		iQ,			iseg, \
		iH,			iseg, \ ; 128
		iQ,			iseg, \ ; 129
		iX,			iseg, \ ; 130
		iQ,			iseg, \
	 	iX, 		iseg, \
		iQ,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 135
		iH,			iseg, \
	 	iQ,			iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iH,			iseg, \ 
		iQ,			iseg, \
	 	iH,	 		iseg, \
		iQ,			iseg, \ 
		iH,			iseg, \
		iQ,			iseg, \ 
		iH,			iseg, \
	 	iX, 		iseg, \
		iH,			iseg, \
		iX,			iseg, \
		iQ,			iseg, \ ; 150
	 	iX, 		iseg, \ 
		iH,			iseg, \ 
		iQ,			iseg, \ ; 153
		iX,			iseg, \ ; 154
		iQ,			iseg, \ ; 155
	 	iX, 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ ; 158
		iQ,			iseg, \ ; 159
		iX,			iseg, \ ; 160
	 	iQ, 		iseg, \
		iX,			iseg, \
		iQ,			iseg, \
		iH,			iseg, \
		iQ,			iseg, \ ; 165
	 	iX,	 		iseg, \
		iQ,			iseg, \
		iX,			iseg, \ 
		iQ,			iseg, \
		iH,			iseg, \ ; 170
	 	iTINY, 		iseg, \
		iX,			iseg, \
		iTINY,		iseg, \
		iQ,			iseg, \
		iTINY,		iseg, \ ; 175
	 	iX, 		iseg, \
		iTINY,		iseg, \
		iH,			iseg, \  
		iTINY,		iseg, \  
		iX,			iseg, \ 
		iTINY,		iseg, \
	 	iX, 		iseg, \
		iTINY,		iseg, \
		iH,			iseg, \
		iTINY,		iseg, \ ; 185
		iQ,			iseg, \
	 	iTINY, 		iseg, \
		iX,			iseg, \
		iTINY,		iseg, \
		iQ,			iseg, \ ; 190
		iTINY,		iseg, \
	 	iQ,	 		iseg, \
		iTINY,		iseg, \ ; 193
		iQ,			iseg, \
		iTINY,		iseg, \ ; 195
		iH,			iseg, \
	 	iTINY, 		iseg, \
		iX,			iseg, \
		iTINY,		iseg, \
		iQ,			iseg, \ ; 200
		iTINY	 

aout init 0
ksampnum init 0
kcount = 0
iperiod = sr / ifreq
i2pi = 3.14159 * 2
kincrement = 1.003 ; messing with the loop increment makes it sound more artificial and scratchy (def: 1)

loopStart:
	kphase = (ksampnum % iperiod) / iperiod
	knewval = sin(kphase * i2pi)
	vaset knewval, kcount,aout
	ksampnum = ksampnum + 1

;http://www.csounds.com/manual/html/loop_lt.html
loop_lt kcount, kincrement, ksmps, loopStart 

; fuck it
; if (ivelcy==120) then
if (1==1) then
    ;Sfoo    strcpy "SPECIAL SPECIAL SPECIAL SPECIAL"
    ;puts Sfoo, 1
	aout = (aout * kenv) 
	goto theend
else
	aout = ((aout*2) * klinseg)
endif

theend:
outs aout, aout

;;;;;;;;;;;;;;;;;;
; ins 2
; iChan StartTime Dur Pitch Vel
; 2:1 starts 10.9

;i2	10.91   	0.136508	41.2     	127
;i2	11.181859	0.136508	41.2     	123
;i2	11.454649	0.272789	41.2     	127
;i2	11.863719	0.136508	55.000000	127
;i2	12.136281	0.136735	55.000000	117
;i2	12.409070	0.136735	55.000000	114
;i2	12.545578	0.272789	55.000000	123
;i2	13.090930	0.136508	61.733671	127