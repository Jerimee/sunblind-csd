<CsoundSynthesizer>
<CsOptions>
-odac
--env:SSDIR+=assets/ ; needed for instrument 12
;-+skip_seconds=9
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 128
nchnls = 2
0dbfs = 1.0


; Include the file with all the various tables
#include "includes/suntables.inc"
; Include user-defined opcodes
#include "includes/sunopcodes.inc"
; Include the mixer instruments
#include "instruments/busseffects.inc"

instr	12 ; WavPlayer
	idur		= p3  
	kSpeed  	init p4           ; playback speed
	iSkip   	init p2           ; inskip into file (in seconds)
	iLoop  		init 0           ; looping switch (0=off 1=on)
	iselect    =p5
	ixcf 	=p6
		
	kenv     linseg 0, idur*.02, 0.3, idur*.96, 1, idur*.02, 0
	
	; read audio from disk using diskin2 opcode
	if (p5 = 3) then
		a1,a2     diskin2  "sunblind-justi3.wav", kSpeed, iSkip, iLoop
	elseif (p5 = 5) then
		a1,a2     diskin2  "sunblind-justi5.wav", kSpeed, iSkip, iLoop
		afil fofilter a1, ixcf, 0.005, 0.04
		afil2 fofilter a2, ixcf-40, 0.01, 0.04
		

		a1f   = a1*.5		;reduce volume a bit
		at   tone    a1f, 500	;smooth estimated envelope
		af   follow  at, 0.005
		asin poscil3 .5, 440, 1
		; "provides amplitude for poscil
		asig balance afil, asin
		
		
	endif

		AssignSend		        p1, 0.0001, 0.0001, 0.3
		SendOut		        p1, (a1*0.4)+(asig*0.4), (a2*0.6)+(afil2*kenv)

endin ; end 12


</CsInstruments>
<CsScore>

t 0 65 ; set tempo faster

; EFFECTS MATRIX
;	isend=p4
;	ibuss0=p5
;	igain0=p6
; Chorus to Reverb (210)
i100 0 0 200 210 0.0
; Chorus to Output (220)
i100 0 0 200 220 0.0
; Reverb (210) to Output (220)
i100 0 0 210 220 0.28

; 200:1 MASTER EFFECT CONTROLS
; Chorus.
; Insno Start   Dur Delay   Divisor of Delay
i200   0       -1      20      10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Reverb.
; Insno Start   Dur Delay   Pitch mod   Cutoff
i210   0       -1      0.75    0.009       17000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Master output. 
i220   0       -1  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

; 12:1
; wav file of rendered i3
; broken into sections and
; then slightly speed up

i12  0 	 30		1.0		5	600
i12 +	20	.	.	750
i12 +	10	.	.	620
i12 +	.	.	.	<
i12 +	30	.	.	<
i12 +	30	.	.	1010
i12 +	10	.	.	<
i12 +	.	.	.	<
i12 +	.	.	.	<
i12 +	30	.	.	650
i12 +	10	.	.	<
i12 +	.	.	.	<
i12 +	.	.	.	800

</CsScore>
</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>100</x>
 <y>100</y>
 <width>320</width>
 <height>240</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="nobackground">
  <r>255</r>
  <g>255</g>
  <b>255</b>
 </bgcolor>
</bsbPanel>
<bsbPresets>
</bsbPresets>
