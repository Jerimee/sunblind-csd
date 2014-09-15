/* x11.inc */

; A polynomial nonlinear phasor
; This instrument crossfades between two tables
; found in suntables.inc

idur   = p3
ivelval = p5/127.9 ; turn vel into a value < 1
iamp   = ivelval 
ifreq  = p4

; sine and cosign make a nice bass drum
; other tables make the sound change 
; entirely
itableA = gicosine ; giffitch3 
itableB = gisine ; gitonewheel3

aenv	linseg	0, .001, iamp, idur - .051, iamp, .05, 0	; declicking envelope
aosc	phasor	ifreq	; create a linear phasor
aout3	tablei	aosc, itableA, 1 ; read a table without the linear phasor
apd11	polynomial aosc, 0,0,0,0,0,0,0,0,0,0,0,1 ; distort the phasor with x^11
aout11	tablei	apd11, itableA, 1	 ; read a table with the nonlinear phasor
aout3B	tablei	aosc, itableB, 1 ; read a table without the linear phasor
aout11B	tablei	apd11, itableB, 1	 ; read a table with the nonlinear phasor
kamount	linseg	1.0, 0.05, 0.9, 1.0, 0.0	 ; crossfade between two outputs

aoA				= aout3*kamount + aout11*(1.0 - kamount)
aoB 			= aout3B*kamount + aout11B*(1.0 - kamount)
aout 			= (aoA*0.699)+(aoB*0.299)

irando 			= birnd(0.25)+0.5 ; generate a random number from 0.25 to 0.75
irandoInverted 	= 1 - irando

aL 				= aenv*aout*irando
aR 				= aenv*aout*irandoInverted

outs aL, aR