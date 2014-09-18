<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
-odac     ;;;realtime audio out
--env:SSDIR+=assets/ ; needed for instrument 30
;-+skip_seconds=60
;-iadc    ;;;uncomment -iadc if RT audio input is needed too
;-o sunblind-justi3.wav -W ;;; for file output any platform
;-o sunblind.ogg --ogg
</CsOptions>
<CsInstruments>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; N O T E
; the intent is that this file be the main
; control file and be primarily a collection
; of include statements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sr = 44100
ksmps = 128 ;64 ;32
nchnls = 2
0dbfs = 4 ; 1 is standard and probably better advised

; Include the file with all the various tables
#include "includes/suntables.inc"
; Include user-defined opcodes
#include "includes/sunopcodes.inc"
                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G L O B A L S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

giFirstThree 				= 1
giSecondThree				= 1
giLastFive 				= 1
giTwoThreeFiveandSeven		= 1
giFourAndNine				= 1
gievenon				= 1
gioddon  				= 1
	
	
gi01on = 1     *gioddon*giFirstThree                                              		/* inst 1 sco is a simple backup highlight */
gi02on = 1     *gievenon*giTwoThreeFiveandSeven*giFirstThree 		/* inst 2 sco is a faint tap percusive in time with i3 */
gi03on = 1     *gioddon*giTwoThreeFiveandSeven*giFirstThree 		/* inst 3 sco is a repetative rhythm in time with i2 */
gi04on = 1     *gievenon*giSecondThree*giFourAndNine			/* inst 4 sco is a vocal */
gi05on = 1     *gioddon*giTwoThreeFiveandSeven*giSecondThree		/* inst 5 sco is repetative bass line */
gi06on = 1     *gievenon*giSecondThree					/* inst 6 sco is */
gi07on = 1     *gioddon*giTwoThreeFiveandSeven*giLastFive 		/* inst 7 sco is a drum rhythm*/
gi08on = 1     *gievenon*giLastFive					/* inst 8 sco is flourishy mario paint trill */
gi09on = 1     *gioddon*giLastFive*giFourAndNine			/* inst 9 sco is */
gi10on = 1     *gievenon*giLastFive					/* inst 10 sco is rapid drums */
gi11on = 1    *gioddon*giLastFive					/* inst 11 sco is */
gi30on = 1    								/* inst 30 sco is ten 20 second chunks for WavPlayer */

giamp   = 0.2 ; base volume control
gi01amp = giamp + 0.53
gi02amp = giamp + 0.155
gi03amp = giamp - 0.2
gi04amp = giamp * 0.9
gi05amp = giamp + 0.30
gi06amp = giamp 
gi07amp = giamp + 0.2 ; can we turn it up beyond 1? yes
gi08amp = giamp - 0.1 ; can we turn it down beyond 0? NO, somehow it actually gets louder if you do that!
gi09amp = giamp * 0.175
gi10amp = giamp 
gi11amp = giamp - 0.08
gi30amp = giamp + 0.4

gicount = 0 ; I don't know how to do a counter without a global var
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I N S T R U M E N T   D E F I N I T I O N S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Include the mixer instruments
#include "instruments/busseffects.inc"

instr mymarimba
	#include "instruments/marimba.inc"
endin
instr sine_bass_wave 
	#include "instruments/sine_bass_wave.inc"
endin
instr sweepy 
	#include "instruments/sweepy.inc"
endin
instr x11 
	#include "instruments/x11.inc"
endin

instr 9 ; Bell
kc1 init 100
kc2 duserrnd 211
kvdepth = 0.005
kvrate = 6
ifn1 = 1
ifn2 = 1
ifn3 = 1
ifn4 = 1
ivfn = 1
; midinoteonpch p4, p5
idur = p3
iatt = p3 * 0.2
irel = p3 * 0.25
isus = idur - (iatt+ irel)
kpch = p4
kamp = p5/127
kfreq = kpch ; cpspch(kpch)
asig fmbell kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn
aenvL linseg 0, iatt, 0.8, isus, 0.95, irel,0
aenvR linseg 0, irel, 0.8, isus, 0.95, iatt,0
if (gi09on==1) then  
	AssignSend		p1, 0.1, 0.1, gi09amp
	SendOut		p1, asig*aenvL, asig*aenvR
endif
endin

instr 1 ; Moog Fleur
	#include "instruments/moogfleur.inc"
	if (gi01on==1) then
		AssignSend	p1, 0.2, 0.45, gi01amp
		SendOut	 p1, asig*kpanl, asig*kpanr
	endif
endin ; end ins 1

instr 2 ; sine_bass_wave
	ipitch = p4
	ivel = p5
	aSubOut subinstr "sine_bass_wave", ivel, ipitch
	if (gi02on==1) then  
								;insno,ic,ir,id 	
		AssignSendNamed	        p1, 0.3, 0.7, gi02amp
		SendOutNamed	        p1, aSubOut, aSubOut
	endif
endin ; end ins 2

instr 3 
	ipitch = p4
	ivel = p5
	aSubOutL, aSubOutR subinstr "sweepy", ivel, ipitch
	if (gi03on==1) then  
		AssignSend		        p1, 0.25, 0.05, gi03amp
		SendOut		        p1, aSubOutL, aSubOutR
	endif
endin ; end ins 3

instr	30 ; WavPlayer
	idur	=        p3  
	kSpeed  init     p4           ; playback speed
	iSkip   	init     p2           ; inskip into file (in seconds)
	iLoop  	init     0           ; looping switch (0=off 1=on)
				;double volume			
	kenv     linseg 0, idur*.2, 2, idur*.4, 1, idur*.4, 0
	; read audio from disk using diskin2 opcode
	a1,a2     diskin2  "sunblind-justi3.wav", kSpeed, iSkip, iLoop
	;outs      a1*kenv,a2*kenv          ; send audio to outputs
	if (gi30on==1) then  
		AssignSend		        p1, 0.1, 0.15, gi30amp
		SendOut		        p1, a1*kenv, a2*kenv
	endif
endin ; end 30

instr 4 ; Buzzy Horn
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
; What we are trying to do is...
; velocity should influence
; attack, and to a lesser extent
; it should also influence amplitude
;;;;
idur = p3
iastep1 = p5*0.77 ; make the value less than 100
iastep3 = (iastep1*0.01)
;printks2 "  buzzy p env step 3 is %f\n ", iastep3
iastep4 = (1-(iastep3))*2
;printks2 "  buzzy p env step 4 is %f\n", iastep4
iattpr = iastep4 ; 87 - 127... 127 is short
;printks2 "  buzzy p p5 is %f\n\n", p5
;printks2 "  buzzy p attk is %f\n  - it should be small when p5 is big\n\n", iattpr
iatt = (idur*iattpr)
;printks2 "  buzzy p att length for env is %f\n", iatt
iremainingafteratt= (idur-iatt)
irel  = (iremainingafteratt*0.2) 
irem = (iremainingafteratt - irel)
idec  = irem * 0.2  
islev = 1 - iattpr ;irem * 0.7

kenv	xadsr iatt, idec, islev, irel

kcps =  p4 	  ;frequency
asig	vco2  kenv, kcps
if (gi04on==1) then  
	AssignSend		        p1, 0.2, 0.4, gi04amp
	SendOut  p1, asig, asig
endif
endin ; end ins 4

instr 5   ; Rhodes Piano
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rhodes elec piano model originally by 
;; Perry Cook. Note that I've set the pitch
;; to offpitch where p5 is 127.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pset           0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0

;cigoto condition, label 
cigoto (p5==127), dorand  
  igoto norand

dorand:
  ikrnd random -3, 5
  ivariance = ikrnd * 2
  iamplitude = 0.4 
  ;gicount = gicount +1
  ;String sprintfk "\nnew ikrnd : %d\n", gicount
  ;puts String, 1
  goto onward
                    
norand:
  ivariance = 0
  iamplitude = 0.3 
  goto onward ;this goto may be redundant

onward:
kHz = p4 + ivariance

; envelope
iattratio = 1.05-(p5/127) ; p5 is velocity
irelratio  = 0.05
isusratio =  1 - (iattratio + irelratio)
iatt = p3 * iattratio
irel = p3 * irelratio
isus = p3 * isusratio
aenv linseg 0, iatt, 0.99, isus, 0.99, irel, 0 ; end envelope

iindex = 4.1
icrossfade = 3.1
ivibedepth = 0.2
iviberate  = 6
ifn1 = gisine
ifn2 = gicosine
ifn3 = gisine ;giharpsichord
ifn4 = gisine ;giSigmoFall
ivibefn  = gibergeman ;  giSigmoFall;gisine
asig                 fmrhode                 iamplitude, kHz, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
if (gi05on==1) then  
;note that Reverb is high
	AssignSend		        p1, 0.2, 3, gi05amp
	SendOut			        p1, asig*aenv, asig*aenv
endif
endin ; end ins 5

instr 6 ; Biquad Horn
;;;;;;;;;;;;;;;
/*  modal synthesis using biquad filters as oscillators
    Example by Scott Lindroth 2007 */

    ipi 		= $M_PI ;3.1415926
    idenom		= sr*0.5
    icps     	= p4
    ipan 		= 0.7
    iamp    	= 0.01
    iModes 	= 2 ; wood bar or pot lid
    ifilterw 	= p5

    apulse    mpulse iamp, 0

    ;icps    = cpspch( icps )

    ; filter gain
    iamp1 = 4 + (p5/127) 
    iamp2 = 4 + (p5/127)
    iamp3 = 4
    iamp4 = 4 
    iamp5 = 4 
    iamp6 = 4

    ; resonance
    irpole1 = 0.99999
    irpole2 = irpole1
    irpole3 = irpole1
    irpole4 = irpole1
    irpole5 = irpole1
    irpole6 = irpole1

    ; modal frequencies
    if (iModes == 1) goto modes1
    if (iModes == 2) goto modes2
    
    modes1:
    if1    = icps * 1            ;pot lid
    if2    = icps * 6.27
    if3    = icps * 3.2
    if4    = icps * 9.92
    if5    = icps * 14.15
    if6    = icps * 6.23
    goto nextPart

    modes2:
    if1     = icps * 1            ;uniform wood bar
    if2     = icps * 2.572
    if3     = icps * 4.644
    if4     = icps * 6.984
    if5     = icps * 9.723
    if6     = icps * 12.0
    goto nextPart

    nextPart:

    ; convert frequency to radian frequency
    itheta1 = (if1/idenom) * ipi
    itheta2 = (if2/idenom) * ipi
    itheta3 = (if3/idenom) * ipi
    itheta4 = (if4/idenom) * ipi
    itheta5 = (if5/idenom) * ipi
    itheta6 = (if6/idenom) * ipi

    ; calculate coefficients
    ib11 = -2 * irpole1 * cos(itheta1)
    ib21 = irpole1 * irpole1
    ib12 = -2 * irpole2 * cos(itheta2)
    ib22 = irpole2 * irpole2
    ib13 = -2 * irpole3 * cos(itheta3)
    ib23 = irpole3 * irpole3
    ib14 = -2 * irpole4 * cos(itheta4)
    ib24 = irpole4 * irpole4
    ib15 = -2 * irpole5 * cos(itheta5)
    ib25 = irpole5 * irpole5
    ib16 = -2 * irpole6 * cos(itheta6)
    ib26 = irpole6 * irpole6

;also try setting the -1 coeff. to 0, but scale down the amplitude!

asin1 biquad  apulse * iamp1, 1, 0, -1, 1, ib11, ib21
asin2 biquad  apulse * iamp2, 1, 0, -1, 1, ib12, ib22
asin3 biquad  apulse * iamp3, 1, 0,  0, 1, ib13, ib23
asin4 biquad  apulse * iamp4, 1, 0, -1, 1, ib14, ib24
asin5 biquad  apulse * iamp5, 1, 0, -1, 1, ib15, ib25
asin6 biquad  apulse * iamp6, 1, 0, -1, 1, ib16, ib26
afin  = (asin1 + asin2 + asin3 + asin4 + asin5 + asin6)
aL = afin*sqrt(ipan)
aR = afin*sqrt(1- ipan)
if (gi06on==1) then    
  AssignSend		        p1, 0.1, 0.7, gi06amp
  SendOut			        p1, aL, aR
endif
endin ; end ins 6

instr 7 
	ipitch = p4
	ivel = p5
	aSubOutL, aSubOutR subinstr "x11", ivel, ipitch
	if (gi07on==1) then  
	        AssignSend		        	p1, 0.5, 0.1, gi07amp
	        SendOut	 		p1, aSubOutL, aSubOutR
	endif
endin ; end ins 7

instr 8 ; Sunshape
;;;;;;;;;;;;;;;;;;;;
	imaxamp    =20
	ilineend = (p5/5)
	itiny =  0.000001
	ipfo = p4
	;kshapeamt  line        itiny,p3,ilineend
    kshapeamt  line        1,p3,ilineend

	; if we have a note (ie 7.07) convert to cycles
	; otherwise we have cycles (ie 670) so do nothing
	ifreq = (p4 > 15 ? ipfo : cpspch(ipfo))
	
	isq ftgenonce 100001,0,128,10,1,0,0.3,0,0.2,0,0.14,0,0.111   ; Square with a small amount of data
	ipulse ftgenonce 100002,0,32768,10,1,1,1,1, 0.7,0.5,0.3,0.1          ; Pulse 
	isine ftgenonce 100003, 0, 1024, 10, 1 
	if (p5>120) then
	  ifunc = isq
	else
	  ifunc = ipulse ;pulse
	endif
	
	aOsc       oscili      0.6, ifreq, isine ;ifunc
	aSqOrPulse         oscili      0.6, ifreq, ifunc
	aToOutOsc       powershape  aOsc, kshapeamt
	aToOutSqOrPulse       powershape  aSqOrPulse, kshapeamt
	; below we set how much relative influence the 
	;two oscilis will have and then push them together
	krando = birnd(0.2)+0.55 ; random number from 0.35 to 0.75
	krandoInverted = 1 - krando
	aout      = aToOutOsc*krando ;sine
	aoutSOP   = aToOutSqOrPulse*krandoInverted ;sq or pulse
	afinalout = (aout+aoutSOP)*2
	
	idurf = p3/10
	;                                 v,t,v,t,v 
	aenv   expseg   0.8, idurf, 1.0, idurf,0.4, idurf,1.0, idurf,0.3, idurf, 0.9, idurf,0.6, idurf,0.2, idurf,0.05, idurf,0.01, idurf
	;wobble the env by reducing it
	;the krandoInverted number just happens to fit
	afinalenv = aenv-krandoInverted
    adeclick   linseg      0.0, 0.01, 1.0, p3 - 0.06, 1.0, 0.05, 0.0
    adone = afinalout * adeclick * imaxamp*afinalenv
			
if (gi08on==1) then									
	AssignSend		        p1, 0.0, 0.2, gi08amp
	SendOut			        p1, adone, (adone*0.92)
endif
endin ; end ins 8

; Instrument #10 - Demonstrates the subinstr opcode.
instr 10
	ivel = p5
	ipitch = p4
	abasic subinstr "mymarimba", ivel, ipitch
	if (gi10on==1) then
	  AssignSendNamed		       	p1, 0.5, 0.7, gi10amp
	  SendOutNamed			  	p1, abasic, abasic
	endif
endin

instr 90 ; Suntikkel
; *************************************************
; example of soft synchronization of two partikkel instances
; *************************************************

/*score parameters*/
;igrainrate	= p4		; grain rate
;igrainsize	= p5		; grain size in ms
;igrainFreq	= p6		; fundamental frequency of source waveform
;iosc2Dev	= p7		; partikkel instance 2 grain rate deviation factor
;iMaxSync	= p8		; max soft sync amount (increasing to this value during length of note)

/*score parameters*/
igrainrate	= p4/2		; grain rate
igrainsize	= 0.15		; grain size in ms 1.5
igrainFreq	= p4 ; fundamental freq of source wave
iosc2Dev	= p5/(127*2)	; partikkel instance 2 grain rate deviation factor
iMaxSync	= p5/(127*4)	; max soft sync amount (increasing to this value during length of note)

/*overall envelope*/
iattack		= p5/(127*9) ; 0.001
idecay		= p5/(127*4)
isustain	= p5/(127*2)
irelease	= p5/(127*9)
amp		linsegr	0, iattack, 1, idecay, isustain, 1, isustain, irelease, 0

kgrainfreq	= igrainrate		; grains per second
kdistribution	= 0			; periodic grain distribution
idisttab	= -1			; (default) flat distribution used
                                        ; for grain distribution
async		= 0			; no sync input
kenv2amt	= 0			; no secondary enveloping
ienv2tab	= -1			; default secondary envelope (flat)
ienv_attack	= giSigmoRise		; default attack envelope (flat)
ienv_decay	= giSigmoFall		; default decay envelope (flat)
ksustain_amount	= 0.3			; time (in fraction of grain dur) at
                                        ; sustain level for each grain
ka_d_ratio	= 0.2 			; balance between attack and decay time
kduration	= igrainsize		; set grain duration in ms
kamp		= 0.85*0dbfs 		; amp
igainmasks	= -1 ; (default) no gain masking
kwavfreq	= igrainFreq		; fundamental frequency of source waveform
ksweepshape	= 0			; shape of frequency sweep (0=no sweep)
iwavfreqstarttab = -1			; default frequency sweep start
; (value in table = 1, which give
; no frequency modification)
iwavfreqendtab	= -1			; default frequency sweep end
; (value in table = 1, which give
; no frequency modification)
awavfm		= 0			; no FM input
ifmamptab	= -1			; default FM scaling (=1)
kfmenv		= -1			; default FM envelope (flat)
icosine		= gicosine		; cosine ftable
kTrainCps	= kgrainfreq		; set trainlet cps equal to grain
; rate for single-cycle trainlet in
                                        ; each grain
knumpartials	= 3			; number of partials in trainlet
kchroma		= 1			; balance of partials in trainlet
ichannelmasks	= -1			; (default) no channel masking,
                                        ; all grains to output 1
krandommask	= 0			; no random grain masking
kwaveform1	= gisine		; source waveforms
kwaveform2	= gisine		;
kwaveform3	= gisine		;
kwaveform4	= gisine		;
iwaveamptab	= -1			; mix of 4 source waveforms and
                                        ; trainlets (set to default)
asamplepos1	= 0			; phase offset for reading source waveform
asamplepos2	= 0			;
asamplepos3	= 0			;
asamplepos4	= 0			;
kwavekey1	= 1			; original key for source waveform
kwavekey2	= 1			;
kwavekey3	= 1			;
kwavekey4	= 1			;
imax_grains	= 100			; max grains per k period
iopcode_id	= 1			; id of opcode, linking partikkel
                                        ; to partikkelsync

a1  partikkel kgrainfreq, kdistribution, idisttab, async, kenv2amt, \
       ienv2tab,ienv_attack, ienv_decay, ksustain_amount, ka_d_ratio, \
       kduration, kamp, igainmasks, kwavfreq, ksweepshape, \
       iwavfreqstarttab, iwavfreqendtab, awavfm, ifmamptab, kfmenv, \
       icosine, kTrainCps, knumpartials, kchroma, ichannelmasks, \
       krandommask, kwaveform1, kwaveform2, kwaveform3, kwaveform4, \
       iwaveamptab, asamplepos1, asamplepos2, asamplepos3, asamplepos4, \
       kwavekey1, kwavekey2, kwavekey3, kwavekey4, imax_grains, iopcode_id

async1		partikkelsync	iopcode_id   ; clock pulse output of the 
                                             ; partikkel instance above
ksyncGravity 	line 0, p3, iMaxSync	     ; strength of synchronization
aphase2		init 0					
asyncPolarity	limit (int(aphase2*2)*2)-1, -1, 1
; use the phase of partikkelsync instance 2 to find sync 
; polarity for partikkel instance 2.
; If the phase of instance 2 is less than 0.5, we want to
; nudge it down when synchronizing,
; and if the phase is > 0.5 we want to nudge it upwards.
async1		= async1*ksyncGravity*asyncPolarity  ; prepare sync signal
                                                  ; with polarity and strength

kgrainfreq2	= igrainrate * iosc2Dev		; grains per second for second partikkel instance
iopcode_id2	= 2
a2 partikkel kgrainfreq2, kdistribution, idisttab, async1, kenv2amt, \
       ienv2tab, ienv_attack, ienv_decay, ksustain_amount, ka_d_ratio, \
       kduration, kamp, igainmasks, kwavfreq, ksweepshape, \
       iwavfreqstarttab, iwavfreqendtab, awavfm, ifmamptab, kfmenv, \
       icosine, kTrainCps, knumpartials, kchroma, ichannelmasks, \
       krandommask, kwaveform1, kwaveform2, kwaveform3, kwaveform4, \
       iwaveamptab, asamplepos1, asamplepos2, asamplepos3, \ 
       asamplepos4, kwavekey1, kwavekey2, kwavekey3, kwavekey4, \
       imax_grains, iopcode_id2

async2, aphase2	partikkelsync	iopcode_id2
; clock pulse and phase 
; output of the partikkel instance above,
; we will only use the phase

;outs	a1*amp, a2*amp
if (gi09on==1) then
						;insno,ic,ir,id 				
	AssignSend		        p1, 0.0, 0.5, gi09amp
	SendOut			        p1, a1*amp, a2*amp
endif
endin ; end ins 9



instr 11 ; Drillill
idur = p3

iatt = (idur*0.01)
iremainingafteratt= (idur-iatt)
irel  = (iremainingafteratt*0.01) 
irem = (iremainingafteratt - irel)
idec  = irem * 0.5  
islev = p5/127
kenv	xadsr iatt, idec, islev, irel

krnd random -25, 65
kcps = p4 + krnd	;freq, random scrntchs up sound a bit

iunwise = (p5*0.01)
kmod = iunwise - 0.1

asigL foscil iunwise, kcps, 1, kmod, kenv, 1
asigR	vco2  kenv * iunwise, kcps
if (gi11on==1) then
	AssignSend		        p1, 0.9, 0.2, gi11amp
	SendOut			        p1, asigL, asigR
endif
endin ; end ins 11



</CsInstruments>
<CsScore>

f211 0 -20 -41  12 .2 16 .8	;choose 12 at 20% probability, 16 at 80%

t 0 65 ; set tempo faster

; EFFECTS MATRIX
;	isend=p4
;	ibuss0=p5
;	igain0=p6
; Chorus to Reverb (210)
i100 0 0 200 210 0.0
; Chorus to Output (220)
;i100 0 0 200 220 0.0
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

; ins 1
; simple backup flourish/highlight
; 1:1 start 40.3

i1	40.36    	1.10    	554.300  	127
i1	40.364  	1.09    	880.00   	126
i1	41.454649	0.954649	493.869370	127
i1	41.454649	0.954649	830.585965	126
i1	40.363719	3.273016	659.217924	127
i1	42.409070	1.227664	880.00   	126
i1	42.409070	1.227664	554.300  	127
i1	43.636508	0.954649	987.738739	126
i1	43.636508	0.954649	622.253965	127
i1	43.636508	0.954649	740.0       	127
i1	44.590930	1.227438	880.00   	127
i1	44.590930	1.227438	659.217924	127
i1	44.590930	1.227438	554.300  	126
i1	45.81   	0.95    	493.869370	127
i1	45.82   	0.91    	830.585965	127
i1	45.82   	2.25    	659.217924	126
i1	46.772789	1.227438	880.00   	102
i1	46.772789	1.227438	554.300  	113
i1	48.000000	0.954875	740.0       	119
i1	48.000000	0.954875	987.738739	127
i1	48.000000	0.954875	622.253965	124
i1	48.954649	1.227438	880.00   	109
i1	48.954649	1.227438	554.300  	127
i1	50.181859	0.954875	493.869370	127
i1	50.181859	0.954875	830.585965	104
i1	48.954649	3.409297	659.217924	127
i1	51.136508	1.227438	554.300  	124
i1	51.136508	1.227438	880.00   	119
i1	52.363719	0.954649	622.253965	124
i1	52.363719	0.954649	987.738739	124
i1	52.363719	0.954649	740.0       	113
i1	53.318141	1.227664	880.00   	102
i1	53.318141	.		554.300  	124
i1	53.318141	.		659.217924	119
i1	54.545578	2.18    	830.585965	127
i1	54.545578	2.18    	740.0       	127
i1	54.545578	2.18    	987.738739	127
i1	54.545578	2.25    	625     	124
i1	57.545578	0.829705	493.869370	119
i1	56.727211	2.16     	554.300  	125
i1	56.727211	2.21     	880.00   	125
i1	56.727211	2.16     	659.217924	125
i1	56.727211	2.18     	740.0       	125
i1	58.363719	2.727438	415.292983	127
i1	58.909070	2.18     	830.585965	127
i1	58.909070	2.18     	740.0       	127
i1	58.909070	2.21     	987.738739	127
i1	58.909070	2.18     	622.253965	127
i1	61.090930	0.818367	554.300  	125
i1	61.090930	1.1     	370     	99
i1	61.090930	2.2     	880.00   	125
i1	61.090930	2.2     	666     	125
i1	61.090930	2.18    	741.0     	116
i1	61.909070	1.363946	554.300  	122
i1	62.18   	0.545805	493.9   	126
i1	62.727438	2.727438	415.292983	115
i1	63.272789	2.18    	830.585965	127
i1	63.272789	2.18     	740.0     	127
i1	63.272789	2.21     	987.738739	127
i1	63.272789	2.18     	622.253965	127
i1	65.454649	2.21     	369.994421	99
i1	65.454649	2.18     	554.300  	125
i1	65.454649	2.18     	880.00   	125
i1	65.454649	2.21     	659.217924	125
i1	65.454649	2.18     	740.0       	125
i1	67.636508	0.954649	659.217924	127
i1	67.636508	0.954649	493.869370	127
i1	67.636508	0.954649	830.585965	127
i1	68.590930	1.22		740.0       	102
i1	68.590930	1.23		880.00   	113
i1	68.590930	1.2		554.300  	102
i1	69.818141	0.954875	830.585965	119
i1	69.818141	0.954875	987.738739	127
i1	69.818141	0.95		630		124
i1	70.772789	1.227438	880.00   	109
i1	70.772789	1.227438	554.300  	127
i1	70.772789	1.227438	740.0       	127
i1	72.000000	0.954875	659.217924	127
i1	72.000000	0.954875	493.869370	127
i1	72.000000	0.954875	830.585965	127
i1	72.954649	1.227438	740.0       	102
i1	72.954649	1.227438	880.00   	113
i1	72.954649	1.227438	554.300  	102
i1	74.181859	0.954875	830.585965	119
i1	74.181859	0.954875	987.738739	127
i1	74.181859	0.954875	622.253965	124
i1	75.136508	1.227438	880.00   	109
i1	75.136508	1.227438	554.300  	127
i1	75.136508	1.227438	740.0      126
i1	76.363719	0.954649	493.869370	127
i1	76.363719	0.954649	830.585965	127
i1	76.363719	2.18     	659.217924	127
i1	77.318141	1.227664	880.00   	102
i1	77.318141	1.227664	554.3   	113
i1	78.545578	0.954649	740.0      120
i1	78.545578	0.954649	987.738739	127
i1	78.545578	0.95		625		124
i1	79.500000	1.227664	880.00   	109
i1	79.500000	1.227664	554.333990	127
i1	80.727438	0.954649	493.869370	127
i1	80.727438	0.954649	830.585965	104
i1	79.500000	3.409297	659.217924	127
i1	81.681859	1.227438	554.333990	124
i1	81.681859	1.227438	880.00   	119
i1	82.909070	0.954875	622.253965	124
i1	82.909070	.		987.738739	124
i1	82.909070	.		740.0      112
i1	83.863719	1.23		880.00   	102
i1	83.863719	1.227438	554.333990	124
i1	83.863719	1.227438	659.217924	119
i1	85.090930	0.954875	493.869370	127
i1	85.090930	0.954875	830.585965	127
i1	85.090930	2.18     		659.2		127
i1	86.045578	1.227438	880.00   	102
i1	86.045578	1.227438	554.3   	113
i1	87.272789	0.954875	740.0      119
i1	87.272789	0.954875	987.738739	127
i1	87.272789	0.954875	622.253965	124
i1	88.227438	1.227438	880.00   	109
i1	88.227438	1.227438	554.333990	127
i1	89.454649	0.954649	493.869370	127
i1	89.454649	0.954649	830.585965	104
i1	88.227438	3.409297	659.217924	127
i1	90.409070	1.227664	554.333990	124
i1	90.409070	1.23		880.00   	119
i1	91.636508	0.954649	622.253965	124
i1	91.636508	0.954649	987.8   	124
i1	91.636508	0.954649	740.0      113
i1	92.590930	1.227664	880.00   	102
i1	92.590930	1.227664	554.333990	124
i1	92.590930	1.227664	659.217924	119
i1	93.818367	0.954649	493.869370	127
i1	93.818367	0.954649	830.585965	127
i1	93.818367	2.181859	659.217924	127
i1	94.772789	1.227438	880.00   	102
i1	94.772789	.            	554.3    	113
i1	96.000000	0.954875	740.0                 119
i1	96.000000	0.954875	987.738739	127
i1	96.000000	0.954875	622.253965	124
i1	96.954649	1.227438	880.00   	109
i1	96.954649	.		554.333990	127
i1	98.181859	0.954875	493.869370	127
i1	98.181859	.		830.585965	104
i1	96.954649	3.409297	659.217924	127
i1	99.136508	1.23		554.333990	124
i1	99.136508	.		880.00   	119
i1	100.363719	0.954875	622.253965	124
i1	100.363719	.		987.738739	124
i1	100.363719	.		740.0		115
i1	101.318367	1.23		880.00   	102
i1	101.318367	.		554.333990	124
i1	101.318367	.		659.217924	119
i1	102.545578	0.954649	493.869370	127
i1	102.545578	.		830.585965	127
i1	102.545578	2.21 	    	661.		126
i1	103.500000	1.227664	880.00   	102
i1	103.500000	.		554.333990	113
i1	104.727438	0.954649	740.0      	119
i1	104.727438	.		987.738739	127
i1	104.727438	.		622.253965	124
i1	105.681859	1.227438	880.00   	109
i1	105.681859	1.227438	554.333990	127
i1	106.909070	0.954875	493.869370	127
i1	106.909070	0.954875	830.585965	104
i1	105.681859	3.409297	659.217924	127
i1	107.863719	1.227438	554.333990	124
i1	107.863719	1.227438	880.00   	119
i1	109.090930	0.954875	622.253965	124
i1	109.090930	.		987.738739	124
i1	109.090930	.		740.0		113
i1	110.045578	1.4		880.00   	102
i1	110.045578	.		554.333990	124
i1	110.045578	.		659.217924	119
i1	111.272789	2.21	     	830.585965	127
i1	111.272789	2.21     		740.0    	126
i1	111.272789	2.21     		988.0		127
i1	111.272789	2.21		622.253965	125
i1	114.272789	0.83		493.869370	119
i1	113.454649	2.21     		554.333990	125
i1	113.454649	2.19     		880.00   	125
i1	113.454649	2.19     		659.217924	125
i1	113.454649	2.19     		740.0		125
i1	115.090930	2.73		415.3		127
i1	115.636508	2.21     		830.59		127
i1	115.636508	2.19     		740.0      	124
i1	115.636508	2.19     		987.5		126
i1	115.636508	2.19     		622.253965	127
i1	117.80  		0.818367	554.333990	125
i1	.  		1.09		370.0		99
i1	.  		2.18    		880.00   	125
i1	.  		2.19    		659.2    	125
i1	.  		2.20	    	740.0      125
i1	118.636508	1.363719	554.333990	122
i1	118.90  	0.545578	493.869370	127
i1	119.454649	2.727438	415.292983	115
i1	120.000000	2.21	     	830.585965	127
i1	.		2.19	     	740.0      124
i1	.		2.18	     	987.738739	127
i1	.		2.18     		622.253965	127
i1	122.2		2.21  	   	369.994421	99
i1	.		2.21     		554.333990	125
i1	.		2.05     		880.00   	125
i1	.		2.18     		659.217924	125
i1	.		2.21     		740.0      124
i1	124.363719	0.954875	659.217924	127
i1	124.363719	0.954875	493.869370	127
i1	124.363719	0.954875	830.585965	127
i1	125.318367	1.227438	740.0      101
i1	125.318367	1.23		880.00   	113
i1	125.318367	1.227438	554.333990	102
i1	126.545578	0.954649	830.585965	119
i1	126.545578	0.954649	987.738739	127
i1	126.545578	0.954649	622.253965	124
i1	127.500000	1.227664	880.00   	109
i1	127.500000	1.227664	554.333990	127
i1	127.500000	1.227664	740.0      125
i1	128.727438	0.954649	659.217924	127
i1	128.727438	0.954649	493.869370	127
i1	128.727438	0.954649	830.585965	127
i1	129.681859	1.227664	740.0      112
i1	129.681859	1.227664	880.00   	113
i1	129.681859	1.227664	554.333990	102
i1	130.909297	0.954649	830.585965	119
i1	130.909297	0.954649	987.738739	127
i1	130.909297	0.954649	622.253965	124
i1	131.863719	1.227438	880.00   	109
i1	131.863719	1.227438	554.333990	127
i1	131.863719	1.227438	740.0		124
; 1:2
i1	168.0	   	2.18185 	830.585965	127
i1	168.1	   	2.1818  	740.0      126
i1	168.2	   	2.181	   	987.738739	127
i1	168.3	   	2.18	    	622.253965	127
i1	171.0	   	0.83	    	493.869370	119
i1	170.18		2.05	     	554.333990	125
i1	170.18		2.05	     	880.00   	125
i1	170.18		2.21	     	659.217924	125
i1	170.182	2.21	     	740.0      126
i1	171.82		2.73	415.292983	127
i1	172.36		2.21     	830.585965	127
i1	172.364	2.21     	740.0      127
i1	172.364	2.21     	987.738739	127
i1	172.364	2.05     	622.253965	127
i1	174.5		0.82		554.333990	125
i1	174.55		1.09	369.994421	99
i1	174.56		2.18     	880.00   	125
i1	174.57		2.18     	659.217924	125
i1	174.58		2.21     	740.0      125
i1	175.363719	1.363946	554.333990	122
i1	175.636508	0.545578	493.869370	127
i1	176.181859	2.727664	415.292983	115
i1	176.727438	2.21     	830.585965	127
i1	176.727438	2.21     	740.0      124
i1	176.727438	2.05     	987.738739	127
i1	176.73		2.18     	622.253965	127
i1	178.909297	2.18     	369.994421	99
i1	178.909297	2.21     	554.333990	125
i1	178.909297	2.21     	880.00   	125
i1	178.909297	2.21     	659.217924	125
i1	178.909297	2.18     	740.0      115
i1	181.091156	0.954649	659.217924	127
i1	181.091156	0.954649	493.869370	127
i1	181.091156	0.954649	830.585965	127
i1	182.045578	1.227438	740.0      100
i1	182.045578	1.227438	888.0   	113
i1	182.045578	1.227438	554.333990	102
i1	183.272789	0.954875	830.585965	119
i1	183.272789	0.954875	987.738739	127
i1	183.272789	0.954875	622.253965	124
i1	184.227438	1.227438	880.00   	109
i1	184.227438	1.227438	554.333990	127
i1	184.227438	1.227438	740.0      127
i1	185.454649	0.954875	659.217924	127
i1	185.454649	0.954875	493.869370	127
i1	185.454649	0.954875	830.585965	127
i1	186.409297	1.227438	740.0      102
i1	186.409297	1.227438	880.00   	113
i1	186.409297	1.227438	554.333990	102
i1	187.636508	0.954875	830.585965	119
i1	187.636508	0.954875	987.738739	127
i1	187.636508	0.954875	622.253965	124
i1	188.591156	1.227438	880.00   	109
i1	188.591156	1.227438	554.333990	127
i1	188.591156	1.227438	740.0      124
i1	189.818367	0.954649	659.217924	127
i1	189.818367	0.954649	493.869370	127
i1	189.818367	0.954649	830.585965	127
i1	190.772789	1.227664	740.0      104
i1	190.772789	1.227664	880.00   	113
i1	190.772789	1.227664	554.333990	102
i1	192.000227	0.954649	830.585965	119
i1	192.000227	0.954649	987.738739	127
i1	192.000227	0.954649	622.253965	124
i1	192.954649	1.227664	880.00   	109
i1	192.954649	1.227664	554.333990	127
i1	192.954649	1.227664	740.0      124
i1	194.182086	0.954649	659.217924	127
i1	194.182086	0.954649	493.869370	127
i1	194.182086	0.954649	830.585965	127
i1	195.136508	1.227438	740.0      101
i1	195.136508	1.227438	880.00   	113
i1	195.136508	1.227438	554.333990	102
i1	196.363719	0.954875	830.585965	119
i1	196.363719	0.954875	987.738739	127
i1	196.363719	0.954875	622.253965	124
i1	197.318367	1.227438	880.00   	109
i1	197.318367	1.227438	554.333990	127
i1	197.318367	1.227438	740.0      126
i1	198.545578	0.954875	659.217924	127
i1	198.545578	0.954875	493.869370	127
i1	198.545578	0.954875	830.585965	127
i1	199.500227	1.227438	740.0      103
i1	199.500227	1.227438	880.00   	113
i1	199.500227	1.227438	554.333990	102
i1	200.727438	0.954875	830.585965	119
i1	200.727438	.		987.738739	127
i1	200.727438	.		622.253965	124
i1	201.682086	1.227438	880.00   	109
i1	201.682086	1.227438	554.333990	127
i1	201.682086	1.227438	740.0      124
i1	202.909297	0.954649	659.217924	127
i1	202.909297	0.954649	493.869370	127
i1	202.909297	0.954649	830.585965	127
i1	203.863719	1.227664	740.0      101
i1	203.863719	1.227664	880.00   	113
i1	203.863719	1.227664	554.333990	102
i1	205.091156	1	       	830.6   		119
i1	205.091156	1       		987.7   		125
i1	205.091156	1       		622.3	   	124
i1	206.045578	1.227438	880.00   	109
i1	206.045578	.		554.333990	127
i1	206.045578	.		740.0      	125
i1	207.27   	2.05     		493.869370	127
i1	207.27   	2.18     		659.217924	127
i1	207.273 	2.21     		415.292983	124
i1	209.45  	0.27   		659.217924	127
i1	209.4649	0.3   		415.292983	127
i1	209.47  	.   		493.869370	127

; ins 2
; iChan StartTime Dur Pitch Vel
; 2:1 starts 10.9
#include "includes/i2sco.sco"

; 3:1
; Include score for i3
#include "includes/i3sco.sco"

; 30:1
; wav file of rendered i3
; broken into sections and
; then slightly speed up
i30 0 	 20		1.0
i30 20	 20		1.0
i30 40	 20		1.005
i30 60	 20		1.02
i30 80	  .		1.0
i30 100	  .		1.001
i30 120	  .		1.02
i30 140	  .		1.03
i30 160	  .		1.02
i30 180	  .		1.005
i30 200	  .		1.0

; 4:1 starts 19.5 vocal melody
; start end pitch att
; melody is here
i4	19.500000	0.136508	329.608962	127
i4	19.636281	0.136735	415.292983	127
i4	19.909070	0.136735	493.869370	127
i4	20.181859	0.136508	493.869370	127
i4	20.454649	0.409297	493.869370	127
i4	21.000000	0.273016	493.869370	127
i4	21.272789	0.136508	415.292983	127
i4	21.545578	0.545578	493.869370	127
i4	22.090930	0.136508	554.333990	127
i4	22.363719	0.136508	415.292983	127
i4	22.636281	0.136735	415.292983	127
i4	22.909070	0.273016	369.994421	127
i4	23.181859	0.136508	329.608962	127
i4	23.454649	0.136508	277.166995	127
i4	23.727211	0.545805	369.994421	127
i4	24.272789	0.818367	329.608962	127
i4	28.090930	0.273016	329.608962	127
i4	28.363719	0.272789	415.292983	127
i4	28.636281	0.136735	493.869370	126
i4	28.909070	0.136735	493.869370	127
i4	29.181859	0.409297	493.869370	127
i4	29.727211	0.273016	493.869370	127
i4	30.000000	0.136508	415.292983	127
i4	30.272789	0.545578	493.869370	127
i4	30.818141	0.273016	554.333990	127
i4	31.363719	0.136508	415.292983	127
i4	31.636281	0.273016	369.994421	126
i4	31.909070	0.136735	329.608962	126
i4	32.181859	0.136508	277.166995	127
i4	32.454649	0.545578	369.994421	127
i4	33.000000	0.818367	329.608962	126
i4	36.818141	0.273016	329.608962	127
i4	37.090930	0.273016	415.292983	127
i4	37.363719	0.136508	493.869370	127
i4	37.636508	0.272789	493.869370	127
i4	37.909070	0.136735	415.292983	121
i4	38.181859	0.136508	493.869370	127
i4	38.454649	0.272789	493.869370	127
i4	38.727211	0.136735	415.292983	121
i4	39.000000	0.55      	493.869370	127
i4	39.545578	0.136508	554.333990	127
i4	40.090930	0.136508	415.292983	127
i4	40.227211	0.136735	493.869370	121
i4	40.363719	0.136508	554.333990	127
i4	40.636508	0.136508	493.869370	114
i4	40.909070	0.136735	415.292983	127
i4	41.181859	0.55      	554.333990	103
i4	41.727211	1.091156	493.869370	109
i4	45.545578	0.136508	493.869370	127
i4	45.818141	0.273016	659.217924	127
i4	46.090930	0.136508	554.333990	127
i4	46.363719	0.136508	493.869370	127
i4	46.636508	0.409297	554.333990	127
i4	47.181859	0.273016	493.869370	127
i4	47.454649	0.136508	415.292983	127
i4	47.7       	0.55      	493.869370	127
i4	48.272789	0.136508	415.292983	127
i4	48.545578	0.272789	369.994421	127
i4	48.818141	0.136735	415.292983	127
i4	49.090930	0.273016	369.994421	127
i4	49.363719	0.136508	329.608962	127
i4	49.636508	0.136508	277.166995	127
i4	49.909070	0.545805	369.994421	127
i4	50.454649	0.818367	329.608962	127
i4	53.181859	0.136508	554.333990	127
i4	53.318141	0.136735	493.869370	114
i4	53.454649	0.272789	659.217924	127
i4	53.727211	0.273016	554.333990	126
i4	54.000000	0.273016	493.869370	127
i4	54.272789	0.545578	554.333990	127
i4	54.818141	1.363946	493.869370	126
i4	56.181859	0.545578	740.0     	99
i4	56.727211	0.818594	659.217924	127
i4	57.545578	0.272789	554.333990	127
i4	57.818141	0.273016	659.217924	127
i4	58.090930	0.273016	554.333990	126
i4	58.363719	0.273016	439.999998	126
i4	58.636508	0.545578	554.333990	127
i4	59.181859	1.363946	493.869370	126
i4	60.545578	0.545578	740.0     	104
i4	61.090930	0.818367	659.217924	127
i4	61.909070	0.273016	554.333990	127
i4	62.181859	0.273016	659.217924	127
i4	62.454649	0.273016	554.333990	119
i4	62.727438	0.272789	439.999998	126
i4	63.000000	0.545805	554.333990	127
i4	63.545578	1.363719	493.869370	126
i4	64.9    	0.55    	740.0   	127
i4	65.454649	0.818367	659.217924	127
i4	66.272789	0.273016	554.333990	127
i4	66.545578	0.272789	659.217924	127
i4	66.818141	0.273016	554.333990	126
i4	67.090930	0.545805	659.217924	127
i4	67.636508	0.272789	659.217924	127
i4	68.590930	0.273016	659.217924	127
i4	69.545578	0.272789	659.217924	127
i4	69.818141	0.273016	554.333990	125
i4	70.090930	0.368481	493.869370	127
i4	70.909070	0.273016	659.217924	127
i4	71.181859	0.136508	554.333990	125
i4	71.454649	0.136508	659.217924	127
i4	71.590930	0.300227	554.333990	125
i4	72.000000	0.345805	659.217924	127
i4	76.227438	0.136508	369.994421	127
i4	76.363719	0.273016	415.292983	127
i4	76.636508	0.136508	493.869370	127
i4	76.909070	0.136735	493.869370	126
i4	77.181859	0.409297	493.869370	127
i4	77.727438	0.272789	493.869370	127
i4	78.000000	0.136735	415.292983	127
i4	78.272789	0.545578	493.869370	127
i4	78.818141	0.273016	554.333990	126
i4	79.090930	0.136735	415.292983	127
i4	79.363719	0.136508	415.292983	127
i4	79.636508	0.272789	369.994421	127
i4	79.909070	0.136735	329.608962	126
i4	80.181859	0.136508	277.166995	127
i4	80.454649	0.682086	369.994421	127
i4	81.136508	0.681859	329.608962	114
i4	84.818141	0.136735	369.994421	126
i4	84.954649	0.136508	369.994421	114
i4	85.090930	0.273016	415.292983	127
i4	85.363719	0.136508	493.869370	127
i4	85.636508	0.136508	493.869370	127
i4	85.909070	0.409297	493.869370	126
i4	86.454649	0.273016	493.869370	127
i4	86.727438	0.136508	415.292983	126
i4	87.000000	0.545805	493.869370	127
i4	87.545578	0.545578	554.333990	127
i4	88.090930	0.136735	415.292983	126
i4	88.363719	0.273016	369.994421	108
i4	88.636508	0.136508	329.608962	119
i4	88.909070	0.136735	277.166995	127
i4	89.181859	0.545805	369.994421	114
i4	89.727438	1.227438	329.608962	99
i4	93.545578	0.136508	329.608962	127
i4	93.681859	0.136735	329.608962	114
i4	93.818367	0.272789	415.292983	127
i4	94.090930	0.136735	493.869370	127
i4	94.363719	0.136508	493.869370	127
i4	94.636508	0.409297	493.869370	127
i4	95.181859	0.273016	493.869370	127
i4	95.454649	0.136508	415.292983	127
i4	95.727438	0.545578	493.869370	127
i4	96.272789	0.545805	554.333990	127
i4	96.818367	0.272789	415.292983	127
i4	97.090930	0.136735	369.994421	127
i4	97.363719	0.273016	329.608962	126
i4	97.636508	0.136508	277.166995	127
i4	97.909070	0.545805	369.994421	119
i4	98.454649	1.363946	329.608962	126
i4	100.090930	0.136735	329.608962	127
i4	100.363719	0.273016	277.166995	127
i4	100.636508	0.545578	246.934685	127
i4	102.136508	0.136508	493.869370	127
i4	102.272789	0.136508	554.333990	127
i4	102.545578	0.273016	659.217924	127
i4	102.818367	0.136508	554.333990	127
i4	103.090930	0.136735	493.869370	126
i4	103.363719	0.545578	554.333990	127
i4	103.909070	0.273016	493.869370	127
i4	104.181859	0.136735	415.292983	126
i4	104.454649	0.545578	493.869370	127
i4	105.000000	0.273016	369.994421	99
i4	105.272789	0.136508	415.292983	126
i4	105.545578	0.409297	369.994421	126
i4	106.090930	0.273016	329.608962	126
i4	106.363719	0.136508	277.166995	126
i4	106.636508	0.545578	369.994421	127
i4	107.181859	1.636735	329.608962	126
i4	109.909070	0.136735	554.333990	127
i4	110.045578	0.136508	493.869370	114
i4	110.181859	0.273016	659.217924	127
i4	110.454649	0.273016	554.333990	126
i4	110.727438	0.272789	493.869370	127
i4	111.000000	0.545805	554.333990	127
i4	111.545578	1.363946	493.869370	126
i4	112.909297	0.545578	740.0       	99
i4	113.454649	0.818367	659.217924	127
i4	114.272789	0.273016	554.333990	127
i4	114.545578	0.273016	659.217924	127
i4	114.818367	0.272789	554.333990	126
i4	115.090930	0.273016	439.999998	126
i4	115.363719	0.545805	554.333990	127
i4	115.909297	1.363719	493.869370	126
i4	117.272789	0.545805	740.0       	104
i4	117.818367	0.818367	659.217924	127
i4	118.636508	0.273016	554.333990	127
i4	118.909297	0.272789	659.217924	127
i4	119.181859	0.273016	554.333990	119
i4	119.454649	0.273016	439.999998	126
i4	119.727438	0.545578	554.333990	127
i4	120.272789	1.363946	493.869370	126
i4	121.636508	0.545578	740.0       	127
i4	122.181859	0.818367	659.217924	127
i4	123.000000	0.273016	554.333990	127
i4	123.272789	0.273016	659.217924	127
i4	123.545578	0.273016	554.333990	126
i4	123.818367	0.545578	659.217924	127
i4	124.363719	0.409297	659.217924	125
i4	125.318367	0.409297	659.217924	.
i4	126.272789	0.273016	659.217924	.
i4	126.545578	0.136508	554.333990	.
i4	126.818367	0.545578	493.869370	.
i4	127.500000	0.136735	493.869370	.
i4	127.636508	0.273016	659.217924	.
i4	127.909297	0.136508	554.333990	.
i4	128.181859	0.409297	659.22		.
i4	128.727438	0.409297	.		.
i4	129.681859	0.409297	.		.
i4	130.636508	0.136508	.		.
i4	130.909297	0.136508	554.333990	.
i4	131.181859	0.682086	493.869370	.
i4	132.000000	0.136735	554.333990	.
i4	132.136508	0.273016	659.217924	.
i4	132.409297	0.136508	554.333990	.
i4	132.545578	0.409297	659.2		.
i4	133.090930	0.409297	.		.
i4	140.727438	0.273016	207.646491	126
i4	141.000227	0.136508	246.934685	127
i4	141.272789	0.136735	246.934685	126
i4	141.545578	0.545578	277.166995	127
i4	142.090930	1.909524	246.934685	119
i4	145.090930	0.273016	207.646491	126
i4	145.363719	0.136735	246.934685	126
i4	145.636508	0.273016	246.934685	127
i4	145.909297	0.545578	277.166995	127
i4	146.454649	1.909297	246.934685	126
i4	149.727438	0.136508	246.934685	126
i4	150.000227	0.272789	246.934685	127
i4	150.272789	0.136735	207.646491	119
i4	150.409297	0.272789	246.934685	126
i4	150.818367	0.136508	246.934685	127
i4	151.090930	0.273016	246.934685	127
i4	151.363719	0.136735	207.646491	119
i4	151.500227	0.136508	246.934685	126
i4	151.909297	0.136508	246.934685	126
i4	152.181859	0.273016	246.934685	127
i4	152.454649	0.136508	207.646491	114
i4	152.590930	0.273016	246.934685	114
i4	153.000227	0.136508	246.934685	126
i4	153.272789	0.273016	246.934685	127
i4	153.545578	0.136508	207.646491	108
i4	153.681859	0.136735	246.934685	127
i4	154.090930	0.136735	246.934685	126
i4	154.363719	0.273016	246.934685	127
i4	154.636508	0.136508	207.646491	114
i4	154.772789	0.273016	246.934685	127
i4	155.181859	0.136735	246.934685	127
i4	155.454649	0.273016	246.934685	127
i4	155.727438	0.136508	207.646491	114
i4	155.863719	0.136735	246.934685	126
i4	156.272789	0.136735	246.934685	127
i4	156.545578	0.273016	246.934685	127
i4	156.818367	0.136508	207.646491	114
i4	156.954649	0.273016	246.934685	126
i4	157.363719	0.136735	246.934685	127
i4	157.636508	0.273016	246.934685	127
i4	157.909297	0.136508	207.646491	126
i4	158.045578	0.136508	246.934685	127
i4	158.454649	0.409297	246.934685	127
i4	159.000227	0.545578	277.166995	127
i4	159.545578	0.409297	246.934685	127
i4	160.090930	0.545805	277.166995	127
i4	160.636508	0.818367	329.608962	126
i4	162.000227	0.409297	329.608962	127
i4	162.409297	0.272789	277.166995	127
i4	162.818367	0.409297	246.934685	127
i4	163.636508	0.409297	277.166995	127
i4	164.045578	0.409297	246.934685	127
i4	164.454649	0.409297	277.166995	127
i4	165.000227	0.954649	329.608962	126
i4	166.636508	0.136508	554.333990	127
i4	166.772789	0.136735	493.869370	114
i4	166.909297	0.272789	659.217924	127
i4	167.181859	0.273016	554.333990	126
i4	167.454649	0.273016	493.869370	127
i4	167.727438	0.545578	554.333990	127
i4	168.272789	1.363946	493.869370	126
i4	169.636508	0.545578	740.0       	99
i4	170.181859	0.818594	659.217924	127
i4	171.000227	0.272789	554.333990	127
i4	171.272789	0.273016	659.217924	127
i4	171.545578	0.273016	554.333990	126
i4	171.818367	0.273016	439.999998	126
i4	172.091156	0.545578	554.333990	127
i4	172.636508	1.363946	493.869370	126
i4	174.000227	0.545578	740.0       	104
i4	174.545578	0.818367	659.217924	127
i4	175.363719	0.273016	554.333990	127
i4	175.636508	0.273016	659.217924	127
i4	175.909297	0.272789	554.333990	119
i4	176.181859	0.273016	439.999998	126
i4	176.454649	0.545805	554.333990	127
i4	177.000227	1.363719	493.869370	126
i4	178.363719	0.545805	740.0       	127
i4	178.909297	0.818367	659.217924	127
i4	179.727438	0.273016	554.333990	127
i4	180.000227	0.272789	659.217924	127
i4	180.272789	0.273016	554.333990	126
i4	180.545578	0.545805	659.217924	127
i4	181.091156	0.272789	659.217924	127
i4	182.727438	0.545578	659.217924	127
i4	183.272789	0.136735	554.333990	99
i4	183.545578	0.409297	493.869370	125
i4	184.091156	0.272789	554.333990	127
i4	184.363719	0.273016	659.217924	127
i4	184.636508	0.273016	554.333990	127
i4	184.909297	0.41      	659.217924	127
i4	185.454649	0.41      	659.217924	127
i4	188.454649	0.273016	554.333990	127
i4	188.727438	0.273016	659.217924	127
i4	189.000227	0.272789	554.333990	119
i4	189.272789	0.545805	659.217924	127
i4	189.818367	0.273016	659.217924	127
i4	192.818367	0.273016	554.333990	127
i4	193.091156	0.272789	659.217924	127
i4	193.363719	0.273016	554.333990	127
i4	193.636508	0.41      	659.217924	127
i4	194.182086	0.42      	659.217924	127
i4	195.818367	0.545578	659.217924	127
i4	196.363719	0.136735	554.333990	107
i4	196.64		0.443537	493.869370	122
i4	197.182086	0.272789	554.333990	127
i4	197.454649	0.273016	659.217924	127
i4	197.727438	0.273016	554.333990	119
i4	198.000227	0.545578	659.217924	127
i4	198.545578	0.273016	659.217924	127
i4	201.545578	0.273016	554.333990	127
i4	201.818367	0.273016	659.217924	127
i4	202.091156	0.272789	554.333990	127
i4	202.36		0.41      	659.217924	127
i4	202.909297	0.42      	659.217924	127
i4	205.909297	0.273016	554.333990	127
i4	206.182086	0.272789	659.217924	127
i4	206.454649	0.273016	554.333990	119
i4	206.727438	0.545578	659.217924	127

; 5:1 start 10.9 ends 209
; ins 5
; Include score for i5
#include "includes/i5sco.sco"

; ins 6
; note the chords
; 6:1 10.9

i6	10.9	0.136508	415.292983	127
i6	10.9	0.136508	329.608962	127
i6	10.9	0.136508	246.934685	127
i6	11.181859	0.136508	415.292983	95
i6	11.181859	0.136508	329.608962	95
i6	11.181859	0.136508	246.934685	95
i6	11.454649	0.341043	415.292983	122
i6	11.454649	0.341043	329.608962	122
i6	11.454649	0.341043	246.934685	122
i6	11.863719	0.136508	277.166995	122
i6	11.863719	0.136508	329.608962	122
i6	11.863719	0.136508	439.999998	122
i6	12.136281	0.136735	329.608962	95
i6	12.136281	0.136735	439.999998	95
i6	12.136281	0.136735	277.166995	95
i6	12.409070	0.273016	439.999998	119
i6	12.409070	0.273016	277.166995	119
i6	12.409070	0.273016	329.608962	119
i6	12.681859	0.136508	277.166995	101
i6	12.681859	0.136508	329.608962	101
i6	12.818141	0.273016	439.999998	122
i6	12.818141	0.273016	329.608962	122
i6	13.090930	0.136508	369.994421	125
i6	13.090930	0.136508	493.869370	125
i6	13.090930	0.136508	311.126982	125
i6	13.363719	0.136508	369.994421	107
i6	13.363719	0.136508	493.869370	107
i6	13.363719	0.136508	311.126982	107
i6	13.636281	0.409524	311.126982	119
i6	13.636281	0.409524	493.869370	119
i6	13.636281	0.409524	369.994421	119
i6	14.045578	0.136508	439.999998	116
i6	14.045578	0.136508	329.608962	116
i6	14.045578	0.136508	277.166995	116
i6	14.318141	0.136735	439.999998	107
i6	14.318141	0.136735	277.166995	107
i6	14.318141	0.136735	329.608962	107
i6	14.590930	0.273016	277.166995	119
i6	14.590930	0.273016	439.999998	119
i6	14.590930	0.273016	329.608962	119
i6	14.863719	0.136508	277.166995	95
i6	14.863719	0.136508	329.608962	95
i6	15.000000	0.273016	439.999998	122
i6	15.000000	0.273016	329.608962	122
i6	15.136281	0.136735	277.166995	110
i6	15.272789	0.136508	415.292983	127
i6	15.272789	0.136508	329.608962	127
i6	15.272789	0.136508	246.934685	127
i6	15.545578	0.136508	415.292983	95
i6	15.545578	0.136508	329.608962	95
i6	15.545578	0.136508	246.934685	95
i6	15.818141	0.341270	415.292983	122
i6	15.818141	0.341270	329.608962	122
i6	15.818141	0.341270	246.934685	122
i6	16.227211	0.136735	277.166995	122
i6	16.227211	0.136735	329.608962	122
i6	16.227211	0.136735	439.999998	122
i6	16.500000	0.136508	329.608962	95
i6	16.500000	0.136508	439.999998	95
i6	16.500000	0.136508	277.166995	95
i6	16.772789	0.273016	439.999998	119
i6	16.772789	0.273016	277.166995	119
i6	16.772789	0.273016	329.608962	119
i6	17.045578	0.136508	277.166995	101
i6	17.045578	0.136508	329.608962	101
i6	17.181859	0.273016	439.999998	122
i6	17.181859	0.273016	329.608962	122
i6	17.454649	0.136508	369.994421	125
i6	17.454649	0.136508	493.869370	125
i6	17.454649	0.136508	311.126982	125
i6	17.727211	0.136735	369.994421	107
i6	17.727211	0.136735	493.869370	107
i6	17.727211	0.136735	311.126982	107
i6	18.000000	0.409297	311.126982	119
i6	18.000000	0.409297	493.869370	119
i6	18.000000	0.409297	369.994421	119
i6	18.409070	0.136735	439.999998	116
i6	18.409070	0.136735	329.608962	116
i6	18.409070	0.136735	277.166995	116
i6	18.681859	0.136508	439.999998	107
i6	18.681859	0.136508	277.166995	107
i6	18.681859	0.136508	329.608962	107
i6	18.954649	0.272789	277.166995	119
i6	18.954649	0.272789	439.999998	119
i6	18.954649	0.272789	329.608962	119
i6	19.227211	0.136735	277.166995	95
i6	19.227211	0.136735	329.608962	95
i6	19.363719	0.272789	439.999998	122
i6	19.363719	0.272789	329.608962	122
i6	19.500000	0.136508	277.166995	110
i6	19.636281	0.136735	415.292983	127
i6	19.636281	0.136735	329.608962	127
i6	19.636281	0.136735	246.934685	127
i6	19.909070	0.136735	415.292983	95
i6	19.909070	0.136735	329.608962	95
i6	19.909070	0.136735	246.934685	95
i6	20.181859	0.341043	415.292983	122
i6	20.181859	0.341043	329.608962	122
i6	20.181859	0.341043	246.934685	122
i6	20.590930	0.136508	277.166995	122
i6	20.590930	0.136508	329.608962	122
i6	20.590930	0.136508	439.999998	122
i6	20.863719	0.136508	329.608962	95
i6	20.863719	0.136508	439.999998	95
i6	20.863719	0.136508	277.166995	95
i6	21.136281	0.273016	439.999998	119
i6	21.136281	0.273016	277.166995	119
i6	21.136281	0.273016	329.608962	119
i6	21.409070	0.136735	277.166995	101
i6	21.409070	0.136735	329.608962	101
i6	21.545578	0.272789	439.999998	122
i6	21.545578	0.272789	329.608962	122
i6	21.818141	0.136735	369.994421	125
i6	21.818141	0.136735	493.869370	125
i6	21.818141	0.136735	311.126982	125
i6	22.090930	0.136508	369.994421	107
i6	22.090930	0.136508	493.869370	107
i6	22.090930	0.136508	311.126982	107
i6	22.363719	0.409297	311.126982	119
i6	22.363719	0.409297	493.869370	119
i6	22.363719	0.409297	369.994421	119
i6	22.772789	0.136508	439.999998	116
i6	22.772789	0.136508	329.608962	116
i6	22.772789	0.136508	277.166995	116
i6	23.045578	0.136508	439.999998	107
i6	23.045578	0.136508	277.166995	107
i6	23.045578	0.136508	329.608962	107
i6	23.318141	0.273016	277.166995	119
i6	23.318141	0.273016	439.999998	119
i6	23.318141	0.273016	329.608962	119
i6	23.590930	0.136508	277.166995	95
i6	23.590930	0.136508	329.608962	95
i6	23.727211	0.273016	439.999998	122
i6	23.727211	0.273016	329.608962	122
i6	23.863719	0.136508	277.166995	110
i6	24.000000	0.136508	415.292983	127
i6	24.000000	0.136508	329.608962	127
i6	24.000000	0.136508	246.934685	127
i6	24.272789	0.136508	415.292983	95
i6	24.272789	0.136508	329.608962	95
i6	24.272789	0.136508	246.934685	95
i6	24.545578	0.341043	415.292983	122
i6	24.545578	0.341043	329.608962	122
i6	24.545578	0.341043	246.934685	122
i6	24.954649	0.136508	277.166995	122
i6	24.954649	0.136508	329.608962	122
i6	24.954649	0.136508	439.999998	122
i6	25.227211	0.136735	329.608962	95
i6	25.227211	0.136735	439.999998	95
i6	25.227211	0.136735	277.166995	95
i6	25.500000	0.273016	439.999998	119
i6	25.500000	0.273016	277.166995	119
i6	25.500000	0.273016	329.608962	119
i6	25.772789	0.136508	277.166995	101
i6	25.772789	0.136508	329.608962	101
i6	25.909070	0.273016	439.999998	122
i6	25.909070	0.273016	329.608962	122
i6	26.181859	0.136508	369.994421	125
i6	26.181859	0.136508	493.869370	125
i6	26.181859	0.136508	311.126982	125
i6	26.454649	0.136508	369.994421	107
i6	26.454649	0.136508	493.869370	107
i6	26.454649	0.136508	311.126982	107
i6	26.727211	0.409297	311.126982	119
i6	26.727211	0.409297	493.869370	119
i6	26.727211	0.409297	369.994421	119
i6	27.136281	0.136735	439.999998	116
i6	27.136281	0.136735	329.608962	116
i6	27.136281	0.136735	277.166995	116
i6	27.409070	0.136735	439.999998	107
i6	27.409070	0.136735	277.166995	107
i6	27.409070	0.136735	329.608962	107
i6	27.681859	0.273016	277.166995	119
i6	27.681859	0.273016	439.999998	119
i6	27.681859	0.273016	329.608962	119
i6	27.954649	0.136508	277.166995	95
i6	27.954649	0.136508	329.608962	95
i6	28.090930	0.273016	439.999998	122
i6	28.090930	0.273016	329.608962	122
i6	28.227211	0.136735	277.166995	110
i6	28.363719	0.136508	415.292983	127
i6	28.363719	0.136508	329.608962	127
i6	28.363719	0.136508	246.934685	127
i6	28.636281	0.136735	415.292983	95
i6	28.636281	0.136735	329.608962	95
i6	28.636281	0.136735	246.934685	95
i6	28.909070	0.341270	415.292983	122
i6	28.909070	0.341270	329.608962	122
i6	28.909070	0.341270	246.934685	122
i6	29.318141	0.136735	277.166995	122
i6	29.318141	0.136735	329.608962	122
i6	29.318141	0.136735	439.999998	122
i6	29.590930	0.136508	329.608962	95
i6	29.590930	0.136508	439.999998	95
i6	29.590930	0.136508	277.166995	95
i6	29.863719	0.272789	439.999998	119
i6	29.863719	0.272789	277.166995	119
i6	29.863719	0.272789	329.608962	119
i6	30.136281	0.136735	277.166995	101
i6	30.136281	0.136735	329.608962	101
i6	30.272789	0.273016	439.999998	122
i6	30.272789	0.273016	329.608962	122
i6	30.545578	0.136508	369.994421	125
i6	30.545578	0.136508	493.869370	125
i6	30.545578	0.136508	311.126982	125
i6	30.818141	0.136735	369.994421	107
i6	30.818141	0.136735	493.869370	107
i6	30.818141	0.136735	311.126982	107
i6	31.090930	0.409297	311.126982	119
i6	31.090930	0.409297	493.869370	119
i6	31.090930	0.409297	369.994421	119
i6	31.500000	0.136508	439.999998	116
i6	31.500000	0.136508	329.608962	116
i6	31.500000	0.136508	277.166995	116
i6	31.772789	0.136508	439.999998	107
i6	31.772789	0.136508	277.166995	107
i6	31.772789	0.136508	329.608962	107
i6	32.045578	0.272789	277.166995	119
i6	32.045578	0.272789	439.999998	119
i6	32.045578	0.272789	329.608962	119
i6	32.318141	0.136735	277.166995	95
i6	32.318141	0.136735	329.608962	95
i6	32.454649	0.272789	439.999998	122
i6	32.454649	0.272789	329.608962	122
i6	32.590930	0.136508	277.166995	110
i6	32.727211	0.136735	415.292983	127
i6	32.727211	0.136735	329.608962	127
i6	32.727211	0.136735	246.934685	127
i6	33.000000	0.136508	415.292983	95
i6	33.000000	0.136508	329.608962	95
i6	33.000000	0.136508	246.934685	95
i6	33.272789	0.341043	415.292983	122
i6	33.272789	0.341043	329.608962	122
i6	33.272789	0.341043	246.934685	122
i6	33.681859	0.136508	277.166995	122
i6	33.681859	0.136508	329.608962	122
i6	33.681859	0.136508	439.999998	122
i6	33.954649	0.136508	329.608962	95
i6	33.954649	0.136508	439.999998	95
i6	33.954649	0.136508	277.166995	95
i6	34.227211	0.273016	439.999998	119
i6	34.227211	0.273016	277.166995	119
i6	34.227211	0.273016	329.608962	119
i6	34.500000	0.136508	277.166995	101
i6	34.500000	0.136508	329.608962	101
i6	34.636281	0.273016	439.999998	122
i6	34.636281	0.273016	329.608962	122
i6	34.909070	0.136735	369.994421	125
i6	34.909070	0.136735	493.869370	125
i6	34.909070	0.136735	311.126982	125
i6	35.181859	0.136508	369.994421	107
i6	35.181859	0.136508	493.869370	107
i6	35.181859	0.136508	311.126982	107
i6	35.454649	0.409297	311.126982	119
i6	35.454649	0.409297	493.869370	119
i6	35.454649	0.409297	369.994421	119
i6	35.863719	0.136508	439.999998	116
i6	35.863719	0.136508	329.608962	116
i6	35.863719	0.136508	277.166995	116
i6	36.136281	0.136735	439.999998	107
i6	36.136281	0.136735	277.166995	107
i6	36.136281	0.136735	329.608962	107
i6	36.409070	0.273016	277.166995	119
i6	36.409070	0.273016	439.999998	119
i6	36.409070	0.273016	329.608962	119
i6	36.681859	0.136508	277.166995	95
i6	36.681859	0.136508	329.608962	95
i6	36.818141	0.273016	439.999998	122
i6	36.818141	0.273016	329.608962	122
i6	36.954649	0.136508	277.166995	110
i6	37.090930	0.136508	415.292983	127
i6	37.090930	0.136508	329.608962	127
i6	37.090930	0.136508	246.934685	127
i6	37.363719	0.136508	415.292983	95
i6	37.363719	0.136508	329.608962	95
i6	37.363719	0.136508	246.934685	95
i6	37.636508	0.341043	415.292983	122
i6	37.636508	0.341043	329.608962	122
i6	37.636508	0.341043	246.934685	122
i6	38.045578	0.136508	277.166995	122
i6	38.045578	0.136508	329.608962	122
i6	38.045578	0.136508	439.999998	122
i6	38.318141	0.136735	329.608962	95
i6	38.318141	0.136735	439.999998	95
i6	38.318141	0.136735	277.166995	95
i6	38.590930	0.273016	439.999998	119
i6	38.590930	0.273016	277.166995	119
i6	38.590930	0.273016	329.608962	119
i6	38.863719	0.136508	277.166995	101
i6	38.863719	0.136508	329.608962	101
i6	39.000000	0.273016	439.999998	122
i6	39.000000	0.273016	329.608962	122
i6	39.272789	0.136508	369.994421	125
i6	39.272789	0.136508	493.869370	125
i6	39.272789	0.136508	311.126982	125
i6	39.545578	0.136508	369.994421	107
i6	39.545578	0.136508	493.869370	107
i6	39.545578	0.136508	311.126982	107
i6	39.818141	0.409297	311.126982	119
i6	39.818141	0.409297	493.869370	119
i6	39.818141	0.409297	369.994421	119
i6	40.227211	0.136735	439.999998	116
i6	40.227211	0.136735	329.608962	116
i6	40.227211	0.136735	277.166995	116
i6	40.500000	0.136735	439.999998	107
i6	40.500000	0.136735	277.166995	107
i6	40.500000	0.136735	329.608962	107
i6	40.772789	0.273016	277.166995	119
i6	40.772789	0.273016	439.999998	119
i6	40.772789	0.273016	329.608962	119
i6	41.045578	0.136508	277.166995	95
i6	41.045578	0.136508	329.608962	95
i6	41.181859	0.273016	439.999998	122
i6	41.181859	0.273016	329.608962	122
i6	41.318141	0.136735	277.166995	110
i6	41.454649	0.136508	415.292983	127
i6	41.454649	0.136508	329.608962	127
i6	41.454649	0.136508	246.934685	127
i6	41.727211	0.136735	415.292983	95
i6	41.727211	0.136735	329.608962	95
i6	41.727211	0.136735	246.934685	95
i6	42.000000	0.341270	415.292983	122
i6	42.000000	0.341270	329.608962	122
i6	42.000000	0.341270	246.934685	122
i6	42.409070	0.136735	277.166995	122
i6	42.409070	0.136735	329.608962	122
i6	42.409070	0.136735	439.999998	122
i6	42.681859	0.136508	329.608962	95
i6	42.681859	0.136508	439.999998	95
i6	42.681859	0.136508	277.166995	95
i6	42.954649	0.272789	439.999998	119
i6	42.954649	0.272789	277.166995	119
i6	42.954649	0.272789	329.608962	119
i6	43.227211	0.136735	277.166995	101
i6	43.227211	0.136735	329.608962	101
i6	43.363719	0.273016	439.999998	122
i6	43.363719	0.273016	329.608962	122
i6	43.636508	0.136508	369.994421	125
i6	43.636508	0.136508	493.869370	125
i6	43.636508	0.136508	311.126982	125
i6	43.909070	0.136735	369.994421	107
i6	43.909070	0.136735	493.869370	107
i6	43.909070	0.136735	311.126982	107
i6	44.181859	0.409297	311.126982	119
i6	44.181859	0.409297	493.869370	119
i6	44.181859	0.409297	369.994421	119
i6	44.590930	0.136508	439.999998	116
i6	44.590930	0.136508	329.608962	116
i6	44.590930	0.136508	277.166995	116
i6	44.863719	0.136508	439.999998	107
i6	44.863719	0.136508	277.166995	107
i6	44.863719	0.136508	329.608962	107
i6	45.136508	0.272789	277.166995	119
i6	45.136508	0.272789	439.999998	119
i6	45.136508	0.272789	329.608962	119
i6	45.409070	0.136735	277.166995	95
i6	45.409070	0.136735	329.608962	95
i6	45.545578	0.272789	439.999998	122
i6	45.545578	0.272789	329.608962	122
i6	45.681859	0.136508	277.166995	110
i6	45.818141	0.136735	415.292983	127
i6	45.818141	0.136735	329.608962	127
i6	45.818141	0.136735	246.934685	127
i6	46.090930	0.136508	415.292983	95
i6	46.090930	0.136508	329.608962	95
i6	46.090930	0.136508	246.934685	95
i6	46.363719	0.341043	415.292983	122
i6	46.363719	0.341043	329.608962	122
i6	46.363719	0.341043	246.934685	122
i6	46.772789	0.136508	277.166995	122
i6	46.772789	0.136508	329.608962	122
i6	46.772789	0.136508	439.999998	122
i6	47.045578	0.136508	329.608962	95
i6	47.045578	0.136508	439.999998	95
i6	47.045578	0.136508	277.166995	95
i6	47.318141	0.273016	439.999998	119
i6	47.318141	0.273016	277.166995	119
i6	47.318141	0.273016	329.608962	119
i6	47.590930	0.136508	277.166995	101
i6	47.590930	0.136508	329.608962	101
i6	47.727211	0.273016	439.999998	122
i6	47.727211	0.273016	329.608962	122
i6	48.000000	0.136735	369.994421	125
i6	48.000000	0.136735	493.869370	125
i6	48.000000	0.136735	311.126982	125
i6	48.272789	0.136508	369.994421	107
i6	48.272789	0.136508	493.869370	107
i6	48.272789	0.136508	311.126982	107
i6	48.545578	0.409297	311.126982	119
i6	48.545578	0.409297	493.869370	119
i6	48.545578	0.409297	369.994421	119
i6	48.954649	0.136508	439.999998	116
i6	48.954649	0.136508	329.608962	116
i6	48.954649	0.136508	277.166995	116
i6	49.227211	0.136735	439.999998	107
i6	49.227211	0.136735	277.166995	107
i6	49.227211	0.136735	329.608962	107
i6	49.500000	0.273016	277.166995	119
i6	49.500000	0.273016	439.999998	119
i6	49.500000	0.273016	329.608962	119
i6	49.772789	0.136508	277.166995	95
i6	49.772789	0.136508	329.608962	95
i6	49.909070	0.273016	439.999998	122
i6	49.909070	0.273016	329.608962	122
i6	50.045578	0.136508	277.166995	110
i6	50.181859	0.136508	415.292983	127
i6	50.181859	0.136508	329.608962	127
i6	50.181859	0.136508	246.934685	127
i6	50.454649	0.136508	415.292983	95
i6	50.454649	0.136508	329.608962	95
i6	50.454649	0.136508	246.934685	95
i6	50.727211	0.341270	415.292983	122
i6	50.727211	0.341270	329.608962	122
i6	50.727211	0.341270	246.934685	122
i6	51.136508	0.136508	277.166995	122
i6	51.136508	0.136508	329.608962	122
i6	51.136508	0.136508	439.999998	122
i6	51.409070	0.136735	329.608962	95
i6	51.409070	0.136735	439.999998	95
i6	51.409070	0.136735	277.166995	95
i6	51.681859	0.273016	439.999998	119
i6	51.681859	0.273016	277.166995	119
i6	51.681859	0.273016	329.608962	119
i6	51.954649	0.136508	277.166995	101
i6	51.954649	0.136508	329.608962	101
i6	52.090930	0.273016	439.999998	122
i6	52.090930	0.273016	329.608962	122
i6	52.363719	0.136508	369.994421	125
i6	52.363719	0.136508	493.869370	125
i6	52.363719	0.136508	311.126982	125
i6	52.636508	0.136508	369.994421	107
i6	52.636508	0.136508	493.869370	107
i6	52.636508	0.136508	311.126982	107
i6	52.909070	0.409297	311.126982	119
i6	52.909070	0.409297	493.869370	119
i6	52.909070	0.409297	369.994421	119
i6	53.318141	0.136735	439.999998	116
i6	53.318141	0.136735	329.608962	116
i6	53.318141	0.136735	277.166995	116
i6	53.590930	0.136508	439.999998	107
i6	53.590930	0.136508	277.166995	107
i6	53.590930	0.136508	329.608962	107
i6	53.863719	0.273016	277.166995	119
i6	53.863719	0.273016	439.999998	119
i6	53.863719	0.273016	329.608962	119
i6	54.136508	0.136508	277.166995	95
i6	54.136508	0.136508	329.608962	95
i6	54.272789	0.273016	439.999998	122
i6	54.272789	0.273016	329.608962	122
i6	54.409070	0.136735	277.166995	110
i6	54.545578	0.136508	246.934685	127
i6	54.545578	0.136508	311.126982	127
i6	54.545578	0.136508	415.292983	127
i6	54.818141	0.136735	246.934685	95
i6	54.818141	0.136735	311.126982	95
i6	54.818141	0.136735	415.292983	95
i6	55.090930	0.341270	246.934685	122
i6	55.090930	0.341270	311.126982	122
i6	55.090930	0.341270	415.292983	122
i6	55.500000	0.136735	311.126982	122
i6	55.500000	0.136735	246.934685	122
i6	55.500000	0.136735	415.292983	122
i6	55.772789	0.136508	246.934685	95
i6	55.772789	0.136508	311.126982	95
i6	55.772789	0.136508	415.292983	95
i6	56.045578	0.272789	246.934685	119
i6	56.045578	0.272789	311.126982	119
i6	56.045578	0.272789	415.292983	119
i6	56.318141	0.136735	246.934685	101
i6	56.318141	0.136735	311.126982	101
i6	56.454649	0.272789	415.292983	122
i6	56.454649	0.272789	311.126982	122
i6	56.727211	0.136735	277.166995	125
i6	56.727211	0.136735	439.999998	125
i6	56.727211	0.136735	369.994421	125
i6	57.000000	0.136735	277.166995	107
i6	57.000000	0.136735	439.999998	107
i6	57.000000	0.136735	369.994421	107
i6	57.272789	0.409297	277.166995	119
i6	57.272789	0.409297	439.999998	119
i6	57.272789	0.409297	369.994421	119
i6	57.681859	0.136508	439.999998	116
i6	57.681859	0.136508	277.166995	116
i6	57.681859	0.136508	369.994421	116
i6	57.954649	0.136508	439.999998	107
i6	57.954649	0.136508	277.166995	107
i6	57.954649	0.136508	369.994421	107
i6	58.227211	0.273016	277.166995	119
i6	58.227211	0.273016	439.999998	119
i6	58.227211	0.273016	369.994421	119
i6	58.500000	0.136735	277.166995	95
i6	58.500000	0.136735	369.994421	95
i6	58.636508	0.272789	369.994421	122
i6	58.636508	0.272789	439.999998	122
i6	58.772789	0.136508	277.166995	110
i6	58.909070	0.136735	246.934685	127
i6	58.909070	0.136735	311.126982	127
i6	58.909070	0.136735	415.292983	127
i6	59.181859	0.136508	246.934685	95
i6	59.181859	0.136508	311.126982	95
i6	59.181859	0.136508	415.292983	95
i6	59.454649	0.341043	246.934685	122
i6	59.454649	0.341043	311.126982	122
i6	59.454649	0.341043	415.292983	122
i6	59.863719	0.136508	311.126982	122
i6	59.863719	0.136508	246.934685	122
i6	59.863719	0.136508	415.292983	122
i6	60.136508	0.136508	246.934685	95
i6	60.136508	0.136508	311.126982	95
i6	60.136508	0.136508	415.292983	95
i6	60.409070	0.273016	246.934685	119
i6	60.409070	0.273016	311.126982	119
i6	60.409070	0.273016	415.292983	119
i6	60.681859	0.136508	246.934685	101
i6	60.681859	0.136508	311.126982	101
i6	60.818141	0.273016	415.292983	122
i6	60.818141	0.273016	311.126982	122
i6	61.090930	0.136508	277.166995	125
i6	61.090930	0.136508	439.999998	125
i6	61.090930	0.136508	369.994421	125
i6	61.363719	0.136508	277.166995	107
i6	61.363719	0.136508	439.999998	107
i6	61.363719	0.136508	369.994421	107
i6	61.636508	0.409297	277.166995	119
i6	61.636508	0.409297	439.999998	119
i6	61.636508	0.409297	369.994421	119
i6	62.045578	0.136508	439.999998	116
i6	62.045578	0.136508	277.166995	116
i6	62.045578	0.136508	369.994421	116
i6	62.318141	0.136735	439.999998	107
i6	62.318141	0.136735	277.166995	107
i6	62.318141	0.136735	369.994421	107
i6	62.590930	0.273016	277.166995	119
i6	62.590930	0.273016	439.999998	119
i6	62.590930	0.273016	369.994421	119
i6	62.863719	0.136508	277.166995	95
i6	62.863719	0.136508	369.994421	95
i6	63.000000	0.273016	369.994421	122
i6	63.000000	0.273016	439.999998	122
i6	63.136508	0.136508	277.166995	110
i6	63.272789	0.136508	246.934685	127
i6	63.272789	0.136508	311.126982	127
i6	63.272789	0.136508	415.292983	127
i6	63.545578	0.136508	246.934685	95
i6	63.545578	0.136508	311.126982	95
i6	63.545578	0.136508	415.292983	95
i6	63.818141	0.341270	246.934685	122
i6	63.818141	0.341270	311.126982	122
i6	63.818141	0.341270	415.292983	122
i6	64.227438	0.136508	311.126982	122
i6	64.227438	0.136508	246.934685	122
i6	64.227438	0.136508	415.292983	122
i6	64.500000	0.136735	246.934685	95
i6	64.500000	0.136735	311.126982	95
i6	64.500000	0.136735	415.292983	95
i6	64.772789	0.273016	246.934685	119
i6	64.772789	0.273016	311.126982	119
i6	64.772789	0.273016	415.292983	119
i6	65.045578	0.136508	246.934685	101
i6	65.045578	0.136508	311.126982	101
i6	65.181859	0.273016	415.292983	122
i6	65.181859	0.273016	311.126982	122
i6	65.454649	0.136508	277.166995	125
i6	65.454649	0.136508	439.999998	125
i6	65.454649	0.136508	369.994421	125
i6	65.727438	0.136508	277.166995	107
i6	65.727438	0.136508	439.999998	107
i6	65.727438	0.136508	369.994421	107
i6	66.000000	0.409297	277.166995	119
i6	66.000000	0.409297	439.999998	119
i6	66.000000	0.409297	369.994421	119
i6	66.409070	0.136735	439.999998	116
i6	66.409070	0.136735	277.166995	116
i6	66.409070	0.136735	369.994421	116
i6	66.681859	0.136508	439.999998	107
i6	66.681859	0.136508	277.166995	107
i6	66.681859	0.136508	369.994421	107
i6	66.954649	0.273016	277.166995	119
i6	66.954649	0.273016	439.999998	119
i6	66.954649	0.273016	369.994421	119
i6	67.227438	0.136508	277.166995	95
i6	67.227438	0.136508	369.994421	95
i6	67.363719	0.273016	369.994421	122
i6	67.363719	0.273016	439.999998	122
i6	67.500000	0.136735	277.166995	110
i6	67.636508	0.136508	415.292983	127
i6	67.636508	0.136508	329.608962	127
i6	67.636508	0.136508	246.934685	127
i6	67.909070	0.136735	415.292983	95
i6	67.909070	0.136735	329.608962	95
i6	67.909070	0.136735	246.934685	95
i6	68.181859	0.341043	415.292983	122
i6	68.181859	0.341043	329.608962	122
i6	68.181859	0.341043	246.934685	122
i6	68.590930	0.136735	277.166995	122
i6	68.590930	0.136735	329.608962	122
i6	68.590930	0.136735	439.999998	122
i6	68.863719	0.136508	329.608962	95
i6	68.863719	0.136508	439.999998	95
i6	68.863719	0.136508	277.166995	95
i6	69.136508	0.272789	439.999998	119
i6	69.136508	0.272789	277.166995	119
i6	69.136508	0.272789	329.608962	119
i6	69.409070	0.136735	277.166995	101
i6	69.409070	0.136735	329.608962	101
i6	69.545578	0.272789	439.999998	122
i6	69.545578	0.272789	329.608962	122
i6	69.818141	0.136735	369.994421	125
i6	69.818141	0.136735	493.869370	125
i6	69.818141	0.136735	311.126982	125
i6	70.090930	0.136735	369.994421	107
i6	70.090930	0.136735	493.869370	107
i6	70.090930	0.136735	311.126982	107
i6	70.363719	0.409297	311.126982	119
i6	70.363719	0.409297	493.869370	119
i6	70.363719	0.409297	369.994421	119
i6	70.772789	0.136508	439.999998	116
i6	70.772789	0.136508	329.608962	116
i6	70.772789	0.136508	277.166995	116
i6	71.045578	0.136508	439.999998	107
i6	71.045578	0.136508	277.166995	107
i6	71.045578	0.136508	329.608962	107
i6	71.318141	0.273016	277.166995	119
i6	71.318141	0.273016	439.999998	119
i6	71.318141	0.273016	329.608962	119
i6	71.590930	0.136735	277.166995	95
i6	71.590930	0.136735	329.608962	95
i6	71.727438	0.272789	439.999998	122
i6	71.727438	0.272789	329.608962	122
i6	71.863719	0.136508	277.166995	110
i6	72.000000	0.136735	415.292983	127
i6	72.000000	0.136735	329.608962	127
i6	72.000000	0.136735	246.934685	127
i6	72.272789	0.136508	415.292983	95
i6	72.272789	0.136508	329.608962	95
i6	72.272789	0.136508	246.934685	95
i6	72.545578	0.341043	415.292983	122
i6	72.545578	0.341043	329.608962	122
i6	72.545578	0.341043	246.934685	122
i6	72.954649	0.136508	277.166995	122
i6	72.954649	0.136508	329.608962	122
i6	72.954649	0.136508	439.999998	122
i6	73.227438	0.136508	329.608962	95
i6	73.227438	0.136508	439.999998	95
i6	73.227438	0.136508	277.166995	95
i6	73.500000	0.273016	439.999998	119
i6	73.500000	0.273016	277.166995	119
i6	73.500000	0.273016	329.608962	119
i6	73.772789	0.136508	277.166995	101
i6	73.772789	0.136508	329.608962	101
i6	73.909070	0.273016	439.999998	122
i6	73.909070	0.273016	329.608962	122
i6	74.181859	0.136508	369.994421	125
i6	74.181859	0.136508	493.869370	125
i6	74.181859	0.136508	311.126982	125
i6	74.454649	0.136508	369.994421	107
i6	74.454649	0.136508	493.869370	107
i6	74.454649	0.136508	311.126982	107
i6	74.727438	0.409297	311.126982	119
i6	74.727438	0.409297	493.869370	119
i6	74.727438	0.409297	369.994421	119
i6	75.136508	0.136508	439.999998	116
i6	75.136508	0.136508	329.608962	116
i6	75.136508	0.136508	277.166995	116
i6	75.409070	0.136735	439.999998	107
i6	75.409070	0.136735	277.166995	107
i6	75.409070	0.136735	329.608962	107
i6	75.681859	0.273016	277.166995	119
i6	75.681859	0.273016	439.999998	119
i6	75.681859	0.273016	329.608962	119
i6	75.954649	0.136508	277.166995	95
i6	75.954649	0.136508	329.608962	95
i6	76.090930	0.273016	439.999998	122
i6	76.090930	0.273016	329.608962	122
i6	76.227438	0.136508	277.166995	110
i6	76.363719	0.136508	415.292983	127
i6	76.363719	0.136508	329.608962	127
i6	76.363719	0.136508	246.934685	127
i6	76.636508	0.136508	415.292983	95
i6	76.636508	0.136508	329.608962	95
i6	76.636508	0.136508	246.934685	95
i6	76.909070	0.341270	415.292983	122
i6	76.909070	0.341270	329.608962	122
i6	76.909070	0.341270	246.934685	122
i6	77.318141	0.136735	277.166995	122
i6	77.318141	0.136735	329.608962	122
i6	77.318141	0.136735	439.999998	122
i6	77.590930	0.136735	329.608962	95
i6	77.590930	0.136735	439.999998	95
i6	77.590930	0.136735	277.166995	95
i6	77.863719	0.273016	439.999998	119
i6	77.863719	0.273016	277.166995	119
i6	77.863719	0.273016	329.608962	119
i6	78.136508	0.136508	277.166995	101
i6	78.136508	0.136508	329.608962	101
i6	78.272789	0.273016	439.999998	122
i6	78.272789	0.273016	329.608962	122
i6	78.545578	0.136508	369.994421	125
i6	78.545578	0.136508	493.869370	125
i6	78.545578	0.136508	311.126982	125
i6	78.818141	0.136735	369.994421	107
i6	78.818141	0.136735	493.869370	107
i6	78.818141	0.136735	311.126982	107
i6	79.090930	0.409297	311.126982	119
i6	79.090930	0.409297	493.869370	119
i6	79.090930	0.409297	369.994421	119
i6	79.500000	0.136735	439.999998	116
i6	79.500000	0.136735	329.608962	116
i6	79.500000	0.136735	277.166995	116
i6	79.772789	0.136508	439.999998	107
i6	79.772789	0.136508	277.166995	107
i6	79.772789	0.136508	329.608962	107
i6	80.045578	0.272789	277.166995	119
i6	80.045578	0.272789	439.999998	119
i6	80.045578	0.272789	329.608962	119
i6	80.318141	0.136735	277.166995	95
i6	80.318141	0.136735	329.608962	95
i6	80.454649	0.273016	439.999998	122
i6	80.454649	0.273016	329.608962	122
i6	80.590930	0.136735	277.166995	110
i6	80.727438	0.136508	415.292983	127
i6	80.727438	0.136508	329.608962	127
i6	80.727438	0.136508	246.934685	127
i6	81.000000	0.136735	415.292983	95
i6	81.000000	0.136735	329.608962	95
i6	81.000000	0.136735	246.934685	95
i6	81.272789	0.341043	415.292983	122
i6	81.272789	0.341043	329.608962	122
i6	81.272789	0.341043	246.934685	122
i6	81.681859	0.136508	277.166995	122
i6	81.681859	0.136508	329.608962	122
i6	81.681859	0.136508	439.999998	122
i6	81.954649	0.136508	329.608962	95
i6	81.954649	0.136508	439.999998	95
i6	81.954649	0.136508	277.166995	95
i6	82.227438	0.272789	439.999998	119
i6	82.227438	0.272789	277.166995	119
i6	82.227438	0.272789	329.608962	119
i6	82.500000	0.136735	277.166995	101
i6	82.500000	0.136735	329.608962	101
i6	82.636508	0.272789	439.999998	122
i6	82.636508	0.272789	329.608962	122
i6	82.909070	0.136735	369.994421	125
i6	82.909070	0.136735	493.869370	125
i6	82.909070	0.136735	311.126982	125
i6	83.181859	0.136508	369.994421	107
i6	83.181859	0.136508	493.869370	107
i6	83.181859	0.136508	311.126982	107
i6	83.454649	0.409297	311.126982	119
i6	83.454649	0.409297	493.869370	119
i6	83.454649	0.409297	369.994421	119
i6	83.863719	0.136508	439.999998	116
i6	83.863719	0.136508	329.608962	116
i6	83.863719	0.136508	277.166995	116
i6	84.136508	0.136508	439.999998	107
i6	84.136508	0.136508	277.166995	107
i6	84.136508	0.136508	329.608962	107
i6	84.409070	0.273016	277.166995	119
i6	84.409070	0.273016	439.999998	119
i6	84.409070	0.273016	329.608962	119
i6	84.681859	0.136508	277.166995	95
i6	84.681859	0.136508	329.608962	95
i6	84.818141	0.273016	439.999998	122
i6	84.818141	0.273016	329.608962	122
i6	84.954649	0.136508	277.166995	110
i6	85.090930	0.136735	415.292983	127
i6	85.090930	0.136735	329.608962	127
i6	85.090930	0.136735	246.934685	127
i6	85.363719	0.136508	415.292983	95
i6	85.363719	0.136508	329.608962	95
i6	85.363719	0.136508	246.934685	95
i6	85.636508	0.341043	415.292983	122
i6	85.636508	0.341043	329.608962	122
i6	85.636508	0.341043	246.934685	122
i6	86.045578	0.136508	277.166995	122
i6	86.045578	0.136508	329.608962	122
i6	86.045578	0.136508	439.999998	122
i6	86.318141	0.136735	329.608962	95
i6	86.318141	0.136735	439.999998	95
i6	86.318141	0.136735	277.166995	95
i6	86.590930	0.273016	439.999998	119
i6	86.590930	0.273016	277.166995	119
i6	86.590930	0.273016	329.608962	119
i6	86.863719	0.136508	277.166995	101
i6	86.863719	0.136508	329.608962	101
i6	87.000000	0.273016	439.999998	122
i6	87.000000	0.273016	329.608962	122
i6	87.272789	0.136508	369.994421	125
i6	87.272789	0.136508	493.869370	125
i6	87.272789	0.136508	311.126982	125
i6	87.545578	0.136508	369.994421	107
i6	87.545578	0.136508	493.869370	107
i6	87.545578	0.136508	311.126982	107
i6	87.818367	0.409297	311.126982	119
i6	87.818367	0.409297	493.869370	119
i6	87.818367	0.409297	369.994421	119
i6	88.227438	0.136508	439.999998	116
i6	88.227438	0.136508	329.608962	116
i6	88.227438	0.136508	277.166995	116
i6	88.500000	0.136735	439.999998	107
i6	88.500000	0.136735	277.166995	107
i6	88.500000	0.136735	329.608962	107
i6	88.772789	0.273016	277.166995	119
i6	88.772789	0.273016	439.999998	119
i6	88.772789	0.273016	329.608962	119
i6	89.045578	0.136508	277.166995	95
i6	89.045578	0.136508	329.608962	95
i6	89.181859	0.273016	439.999998	122
i6	89.181859	0.273016	329.608962	122
i6	89.318367	0.136508	277.166995	110
i6	89.454649	0.136508	415.292983	127
i6	89.454649	0.136508	329.608962	127
i6	89.454649	0.136508	246.934685	127
i6	89.727438	0.136508	415.292983	95
i6	89.727438	0.136508	329.608962	95
i6	89.727438	0.136508	246.934685	95
i6	90.000000	0.341270	415.292983	122
i6	90.000000	0.341270	329.608962	122
i6	90.000000	0.341270	246.934685	122
i6	90.409070	0.136735	277.166995	122
i6	90.409070	0.136735	329.608962	122
i6	90.409070	0.136735	439.999998	122
i6	90.681859	0.136735	329.608962	95
i6	90.681859	0.136735	439.999998	95
i6	90.681859	0.136735	277.166995	95
i6	90.954649	0.273016	439.999998	119
i6	90.954649	0.273016	277.166995	119
i6	90.954649	0.273016	329.608962	119
i6	91.227438	0.136508	277.166995	101
i6	91.227438	0.136508	329.608962	101
i6	91.363719	0.273016	439.999998	122
i6	91.363719	0.273016	329.608962	122
i6	91.636508	0.136508	369.994421	125
i6	91.636508	0.136508	493.869370	125
i6	91.636508	0.136508	311.126982	125
i6	91.909070	0.136735	369.994421	107
i6	91.909070	0.136735	493.869370	107
i6	91.909070	0.136735	311.126982	107
i6	92.181859	0.409297	311.126982	119
i6	92.181859	0.409297	493.869370	119
i6	92.181859	0.409297	369.994421	119
i6	92.590930	0.136735	439.999998	116
i6	92.590930	0.136735	329.608962	116
i6	92.590930	0.136735	277.166995	116
i6	92.863719	0.136508	439.999998	107
i6	92.863719	0.136508	277.166995	107
i6	92.863719	0.136508	329.608962	107
i6	93.136508	0.272789	277.166995	119
i6	93.136508	0.272789	439.999998	119
i6	93.136508	0.272789	329.608962	119
i6	93.409070	0.136735	277.166995	95
i6	93.409070	0.136735	329.608962	95
i6	93.545578	0.273016	439.999998	122
i6	93.545578	0.273016	329.608962	122
i6	93.681859	0.136735	277.166995	110
i6	93.818367	0.136508	415.292983	127
i6	93.818367	0.136508	329.608962	127
i6	93.818367	0.136508	246.934685	127
i6	94.090930	0.136735	415.292983	95
i6	94.090930	0.136735	329.608962	95
i6	94.090930	0.136735	246.934685	95
i6	94.363719	0.341043	415.292983	122
i6	94.363719	0.341043	329.608962	122
i6	94.363719	0.341043	246.934685	122
i6	94.772789	0.136508	277.166995	122
i6	94.772789	0.136508	329.608962	122
i6	94.772789	0.136508	439.999998	122
i6	95.045578	0.136508	329.608962	95
i6	95.045578	0.136508	439.999998	95
i6	95.045578	0.136508	277.166995	95
i6	95.318367	0.272789	439.999998	119
i6	95.318367	0.272789	277.166995	119
i6	95.318367	0.272789	329.608962	119
i6	95.590930	0.136735	277.166995	101
i6	95.590930	0.136735	329.608962	101
i6	95.727438	0.272789	439.999998	122
i6	95.727438	0.272789	329.608962	122
i6	96.000000	0.136735	369.994421	125
i6	96.000000	0.136735	493.869370	125
i6	96.000000	0.136735	311.126982	125
i6	96.272789	0.136508	369.994421	107
i6	96.272789	0.136508	493.869370	107
i6	96.272789	0.136508	311.126982	107
i6	96.545578	0.409297	311.126982	119
i6	96.545578	0.409297	493.869370	119
i6	96.545578	0.409297	369.994421	119
i6	96.954649	0.136508	439.999998	116
i6	96.954649	0.136508	329.608962	116
i6	96.954649	0.136508	277.166995	116
i6	97.227438	0.136508	439.999998	107
i6	97.227438	0.136508	277.166995	107
i6	97.227438	0.136508	329.608962	107
i6	97.500000	0.273016	277.166995	119
i6	97.500000	0.273016	439.999998	119
i6	97.500000	0.273016	329.608962	119
i6	97.772789	0.136508	277.166995	95
i6	97.772789	0.136508	329.608962	95
i6	97.909070	0.273016	439.999998	122
i6	97.909070	0.273016	329.608962	122
i6	98.045578	0.136508	277.166995	110
i6	98.181859	0.136735	415.292983	127
i6	98.181859	0.136735	329.608962	127
i6	98.181859	0.136735	246.934685	127
i6	98.454649	0.136508	415.292983	95
i6	98.454649	0.136508	329.608962	95
i6	98.454649	0.136508	246.934685	95
i6	98.727438	0.341043	415.292983	122
i6	98.727438	0.341043	329.608962	122
i6	98.727438	0.341043	246.934685	122
i6	99.136508	0.136508	277.166995	122
i6	99.136508	0.136508	329.608962	122
i6	99.136508	0.136508	439.999998	122
i6	99.409070	0.136735	329.608962	95
i6	99.409070	0.136735	439.999998	95
i6	99.409070	0.136735	277.166995	95
i6	99.681859	0.273016	439.999998	119
i6	99.681859	0.273016	277.166995	119
i6	99.681859	0.273016	329.608962	119
i6	99.954649	0.136508	277.166995	101
i6	99.954649	0.136508	329.608962	101
i6	100.090930	0.273016	439.999998	122
i6	100.090930	0.273016	329.608962	122
i6	100.363719	0.136508	369.994421	125
i6	100.363719	0.136508	493.869370	125
i6	100.363719	0.136508	311.126982	125
i6	100.636508	0.136508	369.994421	107
i6	100.636508	0.136508	493.869370	107
i6	100.636508	0.136508	311.126982	107
i6	100.909070	0.409524	311.126982	119
i6	100.909070	0.409524	493.869370	119
i6	100.909070	0.409524	369.994421	119
i6	101.318367	0.136508	439.999998	116
i6	101.318367	0.136508	329.608962	116
i6	101.318367	0.136508	277.166995	116
i6	101.590930	0.136735	439.999998	107
i6	101.590930	0.136735	277.166995	107
i6	101.590930	0.136735	329.608962	107
i6	101.863719	0.273016	277.166995	119
i6	101.863719	0.273016	439.999998	119
i6	101.863719	0.273016	329.608962	119
i6	102.136508	0.136508	277.166995	95
i6	102.136508	0.136508	329.608962	95
i6	102.272789	0.273016	439.999998	122
i6	102.272789	0.273016	329.608962	122
i6	102.409070	0.136735	277.166995	110
i6	102.545578	0.136508	415.292983	127
i6	102.545578	0.136508	329.608962	127
i6	102.545578	0.136508	246.934685	127
i6	102.818367	0.136508	415.292983	95
i6	102.818367	0.136508	329.608962	95
i6	102.818367	0.136508	246.934685	95
i6	103.090930	0.341270	415.292983	122
i6	103.090930	0.341270	329.608962	122
i6	103.090930	0.341270	246.934685	122
i6	103.500000	0.136735	277.166995	122
i6	103.500000	0.136735	329.608962	122
i6	103.500000	0.136735	439.999998	122
i6	103.772789	0.136508	329.608962	95
i6	103.772789	0.136508	439.999998	95
i6	103.772789	0.136508	277.166995	95
i6	104.045578	0.273016	439.999998	119
i6	104.045578	0.273016	277.166995	119
i6	104.045578	0.273016	329.608962	119
i6	104.318367	0.136508	277.166995	101
i6	104.318367	0.136508	329.608962	101
i6	104.454649	0.273016	439.999998	122
i6	104.454649	0.273016	329.608962	122
i6	104.727438	0.136508	369.994421	125
i6	104.727438	0.136508	493.869370	125
i6	104.727438	0.136508	311.126982	125
i6	105.000000	0.136735	369.994421	107
i6	105.000000	0.136735	493.869370	107
i6	105.000000	0.136735	311.126982	107
i6	105.272789	0.409297	311.126982	119
i6	105.272789	0.409297	493.869370	119
i6	105.272789	0.409297	369.994421	119
i6	105.681859	0.136735	439.999998	116
i6	105.681859	0.136735	329.608962	116
i6	105.681859	0.136735	277.166995	116
i6	105.954649	0.136508	439.999998	107
i6	105.954649	0.136508	277.166995	107
i6	105.954649	0.136508	329.608962	107
i6	106.227438	0.272789	277.166995	119
i6	106.227438	0.272789	439.999998	119
i6	106.227438	0.272789	329.608962	119
i6	106.500000	0.136735	277.166995	95
i6	106.500000	0.136735	329.608962	95
i6	106.636508	0.272789	439.999998	122
i6	106.636508	0.272789	329.608962	122
i6	106.772789	0.136508	277.166995	110
i6	106.909070	0.136735	415.292983	127
i6	106.909070	0.136735	329.608962	127
i6	106.909070	0.136735	246.934685	127
i6	107.181859	0.136735	415.292983	95
i6	107.181859	0.136735	329.608962	95
i6	107.181859	0.136735	246.934685	95
i6	107.454649	0.341043	415.292983	122
i6	107.454649	0.341043	329.608962	122
i6	107.454649	0.341043	246.934685	122
i6	107.863719	0.136508	277.166995	122
i6	107.863719	0.136508	329.608962	122
i6	107.863719	0.136508	439.999998	122
i6	108.136508	0.136508	329.608962	95
i6	108.136508	0.136508	439.999998	95
i6	108.136508	0.136508	277.166995	95
i6	108.409070	0.273016	439.999998	119
i6	108.409070	0.273016	277.166995	119
i6	108.409070	0.273016	329.608962	119
i6	108.681859	0.136735	277.166995	101
i6	108.681859	0.136735	329.608962	101
i6	108.818367	0.272789	439.999998	122
i6	108.818367	0.272789	329.608962	122
i6	109.090930	0.136735	369.994421	125
i6	109.090930	0.136735	493.869370	125
i6	109.090930	0.136735	311.126982	125
i6	109.363719	0.136508	369.994421	107
i6	109.363719	0.136508	493.869370	107
i6	109.363719	0.136508	311.126982	107
i6	109.636508	0.409297	311.126982	119
i6	109.636508	0.409297	493.869370	119
i6	109.636508	0.409297	369.994421	119
i6	110.045578	0.136508	439.999998	116
i6	110.045578	0.136508	329.608962	116
i6	110.045578	0.136508	277.166995	116
i6	110.318367	0.136508	439.999998	107
i6	110.318367	0.136508	277.166995	107
i6	110.318367	0.136508	329.608962	107
i6	110.590930	0.273016	277.166995	119
i6	110.590930	0.273016	439.999998	119
i6	110.590930	0.273016	329.608962	119
i6	110.863719	0.136508	277.166995	95
i6	110.863719	0.136508	329.608962	95
i6	111.000000	0.273016	439.999998	122
i6	111.000000	0.273016	329.608962	122
i6	111.136508	0.136508	277.166995	110
i6	111.272789	0.136735	246.934685	127
i6	111.272789	0.136735	311.126982	127
i6	111.272789	0.136735	415.292983	127
i6	111.545578	0.136508	246.934685	95
i6	111.545578	0.136508	311.126982	95
i6	111.545578	0.136508	415.292983	95
i6	111.818367	0.341043	246.934685	122
i6	111.818367	0.341043	311.126982	122
i6	111.818367	0.341043	415.292983	122
i6	112.227438	0.136508	311.126982	122
i6	112.227438	0.136508	246.934685	122
i6	112.227438	0.136508	415.292983	122
i6	112.500000	0.136735	246.934685	95
i6	112.500000	0.136735	311.126982	95
i6	112.500000	0.136735	415.292983	95
i6	112.772789	0.273016	246.934685	119
i6	112.772789	0.273016	311.126982	119
i6	112.772789	0.273016	415.292983	119
i6	113.045578	0.136508	246.934685	101
i6	113.045578	0.136508	311.126982	101
i6	113.181859	0.273016	415.292983	122
i6	113.181859	0.273016	311.126982	122
i6	113.454649	0.136508	277.166995	125
i6	113.454649	0.136508	439.999998	125
i6	113.454649	0.136508	369.994421	125
i6	113.727438	0.136508	277.166995	107
i6	113.727438	0.136508	439.999998	107
i6	113.727438	0.136508	369.994421	107
i6	114.000000	0.409524	277.166995	119
i6	114.000000	0.409524	439.999998	119
i6	114.000000	0.409524	369.994421	119
i6	114.409297	0.136508	439.999998	116
i6	114.409297	0.136508	277.166995	116
i6	114.409297	0.136508	369.994421	116
i6	114.681859	0.136735	439.999998	107
i6	114.681859	0.136735	277.166995	107
i6	114.681859	0.136735	369.994421	107
i6	114.954649	0.273016	277.166995	119
i6	114.954649	0.273016	439.999998	119
i6	114.954649	0.273016	369.994421	119
i6	115.227438	0.136508	277.166995	95
i6	115.227438	0.136508	369.994421	95
i6	115.363719	0.273016	369.994421	122
i6	115.363719	0.273016	439.999998	122
i6	115.500000	0.136735	277.166995	110
i6	115.636508	0.136508	246.934685	127
i6	115.636508	0.136508	311.126982	127
i6	115.636508	0.136508	415.292983	127
i6	115.909297	0.136508	246.934685	95
i6	115.909297	0.136508	311.126982	95
i6	115.909297	0.136508	415.292983	95
i6	116.181859	0.341270	246.934685	122
i6	116.181859	0.341270	311.126982	122
i6	116.181859	0.341270	415.292983	122
i6	116.590930	0.136735	311.126982	122
i6	116.590930	0.136735	246.934685	122
i6	116.590930	0.136735	415.292983	122
i6	116.863719	0.136508	246.934685	95
i6	116.863719	0.136508	311.126982	95
i6	116.863719	0.136508	415.292983	95
i6	117.136508	0.273016	246.934685	119
i6	117.136508	0.273016	311.126982	119
i6	117.136508	0.273016	415.292983	119
i6	117.409297	0.136508	246.934685	101
i6	117.409297	0.136508	311.126982	101
i6	117.545578	0.273016	415.292983	122
i6	117.545578	0.273016	311.126982	122
i6	117.818367	0.136508	277.166995	125
i6	117.818367	0.136508	439.999998	125
i6	117.818367	0.136508	369.994421	125
i6	118.090930	0.136735	277.166995	107
i6	118.090930	0.136735	439.999998	107
i6	118.090930	0.136735	369.994421	107
i6	118.363719	0.409297	277.166995	119
i6	118.363719	0.409297	439.999998	119
i6	118.363719	0.409297	369.994421	119
i6	118.772789	0.136735	439.999998	116
i6	118.772789	0.136735	277.166995	116
i6	118.772789	0.136735	369.994421	116
i6	119.045578	0.136508	439.999998	107
i6	119.045578	0.136508	277.166995	107
i6	119.045578	0.136508	369.994421	107
i6	119.318367	0.272789	277.166995	119
i6	119.318367	0.272789	439.999998	119
i6	119.318367	0.272789	369.994421	119
i6	119.590930	0.136735	277.166995	95
i6	119.590930	0.136735	369.994421	95
i6	119.727438	0.272789	369.994421	122
i6	119.727438	0.272789	439.999998	122
i6	119.863719	0.136508	277.166995	110
i6	120.000000	0.136735	246.934685	127
i6	120.000000	0.136735	311.126982	127
i6	120.000000	0.136735	415.292983	127
i6	120.272789	0.136735	246.934685	95
i6	120.272789	0.136735	311.126982	95
i6	120.272789	0.136735	415.292983	95
i6	120.545578	0.341043	246.934685	122
i6	120.545578	0.341043	311.126982	122
i6	120.545578	0.341043	415.292983	122
i6	120.954649	0.136508	311.126982	122
i6	120.954649	0.136508	246.934685	122
i6	120.954649	0.136508	415.292983	122
i6	121.227438	0.136508	246.934685	95
i6	121.227438	0.136508	311.126982	95
i6	121.227438	0.136508	415.292983	95
i6	121.500000	0.273016	246.934685	119
i6	121.500000	0.273016	311.126982	119
i6	121.500000	0.273016	415.292983	119
i6	121.772789	0.136735	246.934685	101
i6	121.772789	0.136735	311.126982	101
i6	121.909297	0.272789	415.292983	122
i6	121.909297	0.272789	311.126982	122
i6	122.181859	0.136735	277.166995	125
i6	122.181859	0.136735	439.999998	125
i6	122.181859	0.136735	369.994421	125
i6	122.454649	0.136508	277.166995	107
i6	122.454649	0.136508	439.999998	107
i6	122.454649	0.136508	369.994421	107
i6	122.727438	0.409297	277.166995	119
i6	122.727438	0.409297	439.999998	119
i6	122.727438	0.409297	369.994421	119
i6	123.136508	0.136508	439.999998	116
i6	123.136508	0.136508	277.166995	116
i6	123.136508	0.136508	369.994421	116
i6	123.409297	0.136508	439.999998	107
i6	123.409297	0.136508	277.166995	107
i6	123.409297	0.136508	369.994421	107
i6	123.681859	0.273016	277.166995	119
i6	123.681859	0.273016	439.999998	119
i6	123.681859	0.273016	369.994421	119
i6	123.954649	0.136508	277.166995	95
i6	123.954649	0.136508	369.994421	95
i6	124.090930	0.273016	369.994421	122
i6	124.090930	0.273016	439.999998	122
i6	124.227438	0.136508	277.166995	110
i6	124.363719	0.136508	415.292983	127
i6	124.363719	0.136508	329.608962	127
i6	124.363719	0.136508	246.934685	127
i6	124.636508	0.136508	415.292983	95
i6	124.636508	0.136508	329.608962	95
i6	124.636508	0.136508	246.934685	95
i6	124.909297	0.341043	415.292983	122
i6	124.909297	0.341043	329.608962	122
i6	124.909297	0.341043	246.934685	122
i6	125.318367	0.136508	277.166995	122
i6	125.318367	0.136508	329.608962	122
i6	125.318367	0.136508	439.999998	122
i6	125.590930	0.136735	329.608962	95
i6	125.590930	0.136735	439.999998	95
i6	125.590930	0.136735	277.166995	95
i6	125.863719	0.273016	439.999998	119
i6	125.863719	0.273016	277.166995	119
i6	125.863719	0.273016	329.608962	119
i6	126.136508	0.136508	277.166995	101
i6	126.136508	0.136508	329.608962	101
i6	126.272789	0.273016	439.999998	122
i6	126.272789	0.273016	329.608962	122
i6	126.545578	0.136508	369.994421	125
i6	126.545578	0.136508	493.869370	125
i6	126.545578	0.136508	311.126982	125
i6	126.818367	0.136508	369.994421	107
i6	126.818367	0.136508	493.869370	107
i6	126.818367	0.136508	311.126982	107
i6	127.090930	0.409297	311.126982	119
i6	127.090930	0.409297	493.869370	119
i6	127.090930	0.409297	369.994421	119
i6	127.500000	0.136735	439.999998	116
i6	127.500000	0.136735	329.608962	116
i6	127.500000	0.136735	277.166995	116
i6	127.772789	0.136735	439.999998	107
i6	127.772789	0.136735	277.166995	107
i6	127.772789	0.136735	329.608962	107
i6	128.045578	0.273016	277.166995	119
i6	128.045578	0.273016	439.999998	119
i6	128.045578	0.273016	329.608962	119
i6	128.318367	0.136508	277.166995	95
i6	128.318367	0.136508	329.608962	95
i6	128.454649	0.273016	439.999998	122
i6	128.454649	0.273016	329.608962	122
i6	128.590930	0.136735	277.166995	110
i6	128.727438	0.136508	415.292983	127
i6	128.727438	0.136508	329.608962	127
i6	128.727438	0.136508	246.934685	127
i6	129.000000	0.136735	415.292983	95
i6	129.000000	0.136735	329.608962	95
i6	129.000000	0.136735	246.934685	95
i6	129.272789	0.341270	415.292983	122
i6	129.272789	0.341270	329.608962	122
i6	129.272789	0.341270	246.934685	122
i6	129.681859	0.136735	277.166995	122
i6	129.681859	0.136735	329.608962	122
i6	129.681859	0.136735	439.999998	122
i6	129.954649	0.136508	329.608962	95
i6	129.954649	0.136508	439.999998	95
i6	129.954649	0.136508	277.166995	95
i6	130.227438	0.272789	439.999998	119
i6	130.227438	0.272789	277.166995	119
i6	130.227438	0.272789	329.608962	119
i6	130.500000	0.136735	277.166995	101
i6	130.500000	0.136735	329.608962	101
i6	130.636508	0.273016	439.999998	122
i6	130.636508	0.273016	329.608962	122
i6	130.909297	0.136508	369.994421	125
i6	130.909297	0.136508	493.869370	125
i6	130.909297	0.136508	311.126982	125
i6	131.181859	0.136735	369.994421	107
i6	131.181859	0.136735	493.869370	107
i6	131.181859	0.136735	311.126982	107
i6	131.454649	0.409297	311.126982	119
i6	131.454649	0.409297	493.869370	119
i6	131.454649	0.409297	369.994421	119
i6	131.863719	0.136508	439.999998	116
i6	131.863719	0.136508	329.608962	116
i6	131.863719	0.136508	277.166995	116
i6	132.136508	0.136508	439.999998	107
i6	132.136508	0.136508	277.166995	107
i6	132.136508	0.136508	329.608962	107
i6	132.409297	0.272789	277.166995	119
i6	132.409297	0.272789	439.999998	119
i6	132.409297	0.272789	329.608962	119
i6	132.681859	0.136735	277.166995	95
i6	132.681859	0.136735	329.608962	95
i6	132.818367	0.272789	439.999998	122
i6	132.818367	0.272789	329.608962	122
i6	132.954649	0.136508	277.166995	110
; 6:2 starts 150
i6	150.5   	0.136508	415.292983	126
i6	150.545578	0.136508	329.61  	125
i6	150.545578	0.136508	246.934685	126
i6	150.81  	0.136508	415.292983	95
i6	150.818		0.136508	329.608962	95
i6	150.82		0.136508	246.934685	95
i6	151.090930	0.341270	415.292983	122
i6	151.090930	0.341270	329.608962	122
i6	151.090930	0.341270	246.934685	122
i6	151.500227	0.136508	277.166995	122
i6	151.500227	0.136508	329.608962	122
i6	151.500227	0.136508	439.999998	122
i6	151.772789	0.136735	329.608962	95
i6	151.772789	0.136735	439.999998	95
i6	151.772789	0.136735	277.166995	95
i6	152.045578	0.273016	439.999998	119
i6	152.045578	0.273016	277.166995	119
i6	152.045578	0.273016	329.608962	119
i6	152.318367	0.136508	277.166995	101
i6	152.318367	0.136508	329.608962	101
i6	152.454649	0.273016	439.999998	122
i6	152.454649	0.273016	329.608962	122
i6	152.727438	0.136508	369.994421	125
i6	152.727438	0.136508	493.869370	125
i6	152.727438	0.136508	311.126982	125
i6	153.000227	0.136508	369.994421	107
i6	153.000227	0.136508	493.869370	107
i6	153.000227	0.136508	311.126982	107
i6	153.272789	0.409297	311.126982	119
i6	153.272789	0.409297	493.869370	119
i6	153.272789	0.409297	369.994421	119
i6	153.681859	0.136735	439.999998	116
i6	153.681859	0.136735	329.608962	116
i6	153.681859	0.136735	277.166995	116
i6	153.954649	0.136508	439.999998	107
i6	153.954649	0.136508	277.166995	107
i6	153.954649	0.136508	329.608962	107
i6	154.227438	0.273016	277.166995	119
i6	154.227438	0.273016	439.999998	119
i6	154.227438	0.273016	329.608962	119
i6	154.500227	0.136508	277.166995	95
i6	154.500227	0.136508	329.608962	95
i6	154.636508	0.273016	439.999998	122
i6	154.636508	0.273016	329.608962	122
i6	154.772789	0.136735	277.166995	110
i6	154.909297	0.136508	415.292983	127
i6	154.909297	0.136508	329.608962	127
i6	154.909297	0.136508	246.934685	127
i6	155.181859	0.136735	415.292983	95
i6	155.181859	0.136735	329.608962	95
i6	155.181859	0.136735	246.934685	95
i6	155.454649	0.341270	415.292983	122
i6	155.454649	0.341270	329.608962	122
i6	155.454649	0.341270	246.934685	122
i6	155.863719	0.136735	277.166995	122
i6	155.863719	0.136735	329.608962	122
i6	155.863719	0.136735	439.999998	122
i6	156.136508	0.136508	329.608962	95
i6	156.136508	0.136508	439.999998	95
i6	156.136508	0.136508	277.166995	95
i6	156.409297	0.272789	439.999998	119
i6	156.409297	0.272789	277.166995	119
i6	156.409297	0.272789	329.608962	119
i6	156.681859	0.136735	277.166995	101
i6	156.681859	0.136735	329.608962	101
i6	156.818367	0.272789	439.999998	122
i6	156.818367	0.272789	329.608962	122
i6	157.090930	0.136735	369.994421	125
i6	157.090930	0.136735	493.869370	125
i6	157.090930	0.136735	311.126982	125
i6	157.363719	0.136735	369.994421	107
i6	157.363719	0.136735	493.869370	107
i6	157.363719	0.136735	311.126982	107
i6	157.636508	0.409297	311.126982	119
i6	157.636508	0.409297	493.869370	119
i6	157.636508	0.409297	369.994421	119
i6	158.045578	0.136508	439.999998	116
i6	158.045578	0.136508	329.608962	116
i6	158.045578	0.136508	277.166995	116
i6	158.318367	0.136508	439.999998	107
i6	158.318367	0.136508	277.166995	107
i6	158.318367	0.136508	329.608962	107
i6	158.590930	0.273016	277.166995	119
i6	158.590930	0.273016	439.999998	119
i6	158.590930	0.273016	329.608962	119
i6	158.863719	0.136735	277.166995	95
i6	158.863719	0.136735	329.608962	95
i6	159.000227	0.272789	439.999998	122
i6	159.000227	0.272789	329.608962	122
i6	159.136508	0.136508	277.166995	110
i6	159.272789	0.136735	415.292983	127
i6	159.272789	0.136735	329.608962	127
i6	159.272789	0.136735	246.934685	127
i6	159.545578	0.136508	415.292983	95
i6	159.545578	0.136508	329.608962	95
i6	159.545578	0.136508	246.934685	95
i6	159.818367	0.341043	415.292983	122
i6	159.818367	0.341043	329.608962	122
i6	159.818367	0.341043	246.934685	122
i6	160.227438	0.136508	277.166995	122
i6	160.227438	0.136508	329.608962	122
i6	160.227438	0.136508	439.999998	122
i6	160.500227	0.136508	329.608962	95
i6	160.500227	0.136508	439.999998	95
i6	160.500227	0.136508	277.166995	95
i6	160.772789	0.273016	439.999998	119
i6	160.772789	0.273016	277.166995	119
i6	160.772789	0.273016	329.608962	119
i6	161.045578	0.136508	277.166995	101
i6	161.045578	0.136508	329.608962	101
i6	161.181859	0.273016	439.999998	122
i6	161.181859	0.273016	329.608962	122
i6	161.454649	0.136735	369.994421	125
i6	161.454649	0.136735	493.869370	125
i6	161.454649	0.136735	311.126982	125
i6	161.727438	0.136508	369.994421	107
i6	161.727438	0.136508	493.869370	107
i6	161.727438	0.136508	311.126982	107
i6	162.000227	0.409297	311.126982	119
i6	162.000227	0.409297	493.869370	119
i6	162.000227	0.409297	369.994421	119
i6	162.409297	0.136508	439.999998	116
i6	162.409297	0.136508	329.608962	116
i6	162.409297	0.136508	277.166995	116
i6	162.681859	0.136735	439.999998	107
i6	162.681859	0.136735	277.166995	107
i6	162.681859	0.136735	329.608962	107
i6	162.954649	0.273016	277.166995	119
i6	162.954649	0.273016	439.999998	119
i6	162.954649	0.273016	329.608962	119
i6	163.227438	0.136508	277.166995	95
i6	163.227438	0.136508	329.608962	95
i6	163.363719	0.273016	439.999998	122
i6	163.363719	0.273016	329.608962	122
i6	163.500227	0.136508	277.166995	110
i6	163.636508	0.136508	415.292983	127
i6	163.636508	0.136508	329.608962	127
i6	163.636508	0.136508	246.934685	127
i6	163.909297	0.136508	415.292983	95
i6	163.909297	0.136508	329.608962	95
i6	163.909297	0.136508	246.934685	95
i6	164.181859	0.341270	415.292983	122
i6	164.181859	0.341270	329.608962	122
i6	164.181859	0.341270	246.934685	122
i6	164.591156	0.136508	277.166995	122
i6	164.591156	0.136508	329.608962	122
i6	164.591156	0.136508	439.999998	122
i6	164.863719	0.136735	329.608962	95
i6	164.863719	0.136735	439.999998	95
i6	164.863719	0.136735	277.166995	95
i6	165.136508	0.273016	439.999998	119
i6	165.136508	0.273016	277.166995	119
i6	165.136508	0.273016	329.608962	119
i6	165.409297	0.136508	277.166995	101
i6	165.409297	0.136508	329.608962	101
i6	165.545578	0.273016	439.999998	122
i6	165.545578	0.273016	329.608962	122
i6	165.818367	0.136508	369.994421	125
i6	165.818367	0.136508	493.869370	125
i6	165.818367	0.136508	311.126982	125
i6	166.091156	0.136508	369.994421	107
i6	166.091156	0.136508	493.869370	107
i6	166.091156	0.136508	311.126982	107
i6	166.363719	0.409297	311.126982	119
i6	166.363719	0.409297	493.869370	119
i6	166.363719	0.409297	369.994421	119
i6	166.772789	0.136735	439.999998	116
i6	166.772789	0.136735	329.608962	116
i6	166.772789	0.136735	277.166995	116
i6	167.045578	0.136508	439.999998	107
i6	167.045578	0.136508	277.166995	107
i6	167.045578	0.136508	329.608962	107
i6	167.318367	0.273016	277.166995	119
i6	167.318367	0.273016	439.999998	119
i6	167.318367	0.273016	329.608962	119
i6	167.591156	0.136508	277.166995	95
i6	167.591156	0.136508	329.608962	95
i6	167.727438	0.273016	439.999998	122
i6	167.727438	0.273016	329.608962	122
i6	167.863719	0.136735	277.166995	110
i6	168.000227	0.136508	246.934685	127
i6	168.000227	0.136508	311.126982	127
i6	168.000227	0.136508	415.292983	127
i6	168.272789	0.136735	246.934685	95
i6	168.272789	0.136735	311.126982	95
i6	168.272789	0.136735	415.292983	95
i6	168.545578	0.341043	246.934685	122
i6	168.545578	0.341043	311.126982	122
i6	168.545578	0.341043	415.292983	122
i6	168.954649	0.136735	311.126982	122
i6	168.954649	0.136735	246.934685	122
i6	168.954649	0.136735	415.292983	122
i6	169.227438	0.136508	246.934685	95
i6	169.227438	0.136508	311.126982	95
i6	169.227438	0.136508	415.292983	95
i6	169.500227	0.272789	246.934685	119
i6	169.500227	0.272789	311.126982	119
i6	169.500227	0.272789	415.292983	119
i6	169.772789	0.136735	246.934685	101
i6	169.772789	0.136735	311.126982	101
i6	169.909297	0.272789	415.292983	122
i6	169.909297	0.272789	311.126982	122
i6	170.181859	0.136735	277.166995	125
i6	170.181859	0.136735	439.999998	125
i6	170.181859	0.136735	369.994421	125
i6	170.454649	0.136735	277.166995	107
i6	170.454649	0.136735	439.999998	107
i6	170.454649	0.136735	369.994421	107
i6	170.727438	0.409297	277.166995	119
i6	170.727438	0.409297	439.999998	119
i6	170.727438	0.409297	369.994421	119
i6	171.136508	0.136508	439.999998	116
i6	171.136508	0.136508	277.166995	116
i6	171.136508	0.136508	369.994421	116
i6	171.409297	0.136508	439.999998	107
i6	171.409297	0.136508	277.166995	107
i6	171.409297	0.136508	369.994421	107
i6	171.681859	0.273016	277.166995	119
i6	171.681859	0.273016	439.999998	119
i6	171.681859	0.273016	369.994421	119
i6	171.954649	0.136735	277.166995	95
i6	171.954649	0.136735	369.994421	95
i6	172.091156	0.272789	369.994421	122
i6	172.091156	0.272789	439.999998	122
i6	172.227438	0.136508	277.166995	110
i6	172.363719	0.136735	246.934685	127
i6	172.363719	0.136735	311.126982	127
i6	172.363719	0.136735	415.292983	127
i6	172.636508	0.136508	246.934685	95
i6	172.636508	0.136508	311.126982	95
i6	172.636508	0.136508	415.292983	95
i6	172.909297	0.341043	246.934685	122
i6	172.909297	0.341043	311.126982	122
i6	172.909297	0.341043	415.292983	122
i6	173.318367	0.136508	311.126982	122
i6	173.318367	0.136508	246.934685	122
i6	173.318367	0.136508	415.292983	122
i6	173.591156	0.136508	246.934685	95
i6	173.591156	0.136508	311.126982	95
i6	173.591156	0.136508	415.292983	95
i6	173.863719	0.273016	246.934685	119
i6	173.863719	0.273016	311.126982	119
i6	173.863719	0.273016	415.292983	119
i6	174.136508	0.136508	246.934685	101
i6	174.136508	0.136508	311.126982	101
i6	174.272789	0.273016	415.292983	122
i6	174.272789	0.273016	311.126982	122
i6	174.545578	0.136508	277.166995	125
i6	174.545578	0.136508	439.999998	125
i6	174.545578	0.136508	369.994421	125
i6	174.818367	0.136508	277.166995	107
i6	174.818367	0.136508	439.999998	107
i6	174.818367	0.136508	369.994421	107
i6	175.091156	0.409297	277.166995	119
i6	175.091156	0.409297	439.999998	119
i6	175.091156	0.409297	369.994421	119
i6	175.500227	0.136508	439.999998	116
i6	175.500227	0.136508	277.166995	116
i6	175.500227	0.136508	369.994421	116
i6	175.772789	0.136735	439.999998	107
i6	175.772789	0.136735	277.166995	107
i6	175.772789	0.136735	369.994421	107
i6	176.045578	0.273016	277.166995	119
i6	176.045578	0.273016	439.999998	119
i6	176.045578	0.273016	369.994421	119
i6	176.318367	0.136508	277.166995	95
i6	176.318367	0.136508	369.994421	95
i6	176.454649	0.273016	369.994421	122
i6	176.454649	0.273016	439.999998	122
i6	176.591156	0.136508	277.166995	110
i6	176.727438	0.136508	246.934685	127
i6	176.727438	0.136508	311.126982	127
i6	176.727438	0.136508	415.292983	127
i6	177.000227	0.136508	246.934685	95
i6	177.000227	0.136508	311.126982	95
i6	177.000227	0.136508	415.292983	95
i6	177.272789	0.341270	246.934685	122
i6	177.272789	0.341270	311.126982	122
i6	177.272789	0.341270	415.292983	122
i6	177.681859	0.136735	311.126982	122
i6	177.681859	0.136735	246.934685	122
i6	177.681859	0.136735	415.292983	122
i6	177.954649	0.136735	246.934685	95
i6	177.954649	0.136735	311.126982	95
i6	177.954649	0.136735	415.292983	95
i6	178.227438	0.273016	246.934685	119
i6	178.227438	0.273016	311.126982	119
i6	178.227438	0.273016	415.292983	119
i6	178.500227	0.136508	246.934685	101
i6	178.500227	0.136508	311.126982	101
i6	178.636508	0.273016	415.292983	122
i6	178.636508	0.273016	311.126982	122
i6	178.909297	0.136508	277.166995	125
i6	178.909297	0.136508	439.999998	125
i6	178.909297	0.136508	369.994421	125
i6	179.181859	0.136735	277.166995	107
i6	179.181859	0.136735	439.999998	107
i6	179.181859	0.136735	369.994421	107
i6	179.454649	0.409297	277.166995	119
i6	179.454649	0.409297	439.999998	119
i6	179.454649	0.409297	369.994421	119
i6	179.863719	0.136735	439.999998	116
i6	179.863719	0.136735	277.166995	116
i6	179.863719	0.136735	369.994421	116
i6	180.136508	0.136508	439.999998	107
i6	180.136508	0.136508	277.166995	107
i6	180.136508	0.136508	369.994421	107
i6	180.409297	0.272789	277.166995	119
i6	180.409297	0.272789	439.999998	119
i6	180.409297	0.272789	369.994421	119
i6	180.681859	0.136735	277.166995	95
i6	180.681859	0.136735	369.994421	95
i6	180.818367	0.273016	369.994421	122
i6	180.818367	0.273016	439.999998	122
i6	180.954649	0.136735	277.166995	110
i6	181.091156	0.136508	415.292983	127
i6	181.091156	0.136508	329.608962	127
i6	181.091156	0.136508	246.934685	127
i6	181.363719	0.136735	415.292983	95
i6	181.363719	0.136735	329.608962	95
i6	181.363719	0.136735	246.934685	95
i6	181.636508	0.341043	415.292983	122
i6	181.636508	0.341043	329.608962	122
i6	181.636508	0.341043	246.934685	122
i6	182.045578	0.136508	277.166995	122
i6	182.045578	0.136508	329.608962	122
i6	182.045578	0.136508	439.999998	122
i6	182.318367	0.136508	329.608962	95
i6	182.318367	0.136508	439.999998	95
i6	182.318367	0.136508	277.166995	95
i6	182.591156	0.272789	439.999998	119
i6	182.591156	0.272789	277.166995	119
i6	182.591156	0.272789	329.608962	119
i6	182.863719	0.136735	277.166995	101
i6	182.863719	0.136735	329.608962	101
i6	183.000227	0.272789	439.999998	122
i6	183.000227	0.272789	329.608962	122
i6	183.272789	0.136735	369.994421	125
i6	183.272789	0.136735	493.869370	125
i6	183.272789	0.136735	311.126982	125
i6	183.545578	0.136508	369.994421	107
i6	183.545578	0.136508	493.869370	107
i6	183.545578	0.136508	311.126982	107
i6	183.818367	0.409297	311.126982	119
i6	183.818367	0.409297	493.869370	119
i6	183.818367	0.409297	369.994421	119
i6	184.227438	0.136508	439.999998	116
i6	184.227438	0.136508	329.608962	116
i6	184.227438	0.136508	277.166995	116
i6	184.500227	0.136508	439.999998	107
i6	184.500227	0.136508	277.166995	107
i6	184.500227	0.136508	329.608962	107
i6	184.772789	0.273016	277.166995	119
i6	184.772789	0.273016	439.999998	119
i6	184.772789	0.273016	329.608962	119
i6	185.045578	0.136508	277.166995	95
i6	185.045578	0.136508	329.608962	95
i6	185.181859	0.273016	439.999998	122
i6	185.181859	0.273016	329.608962	122
i6	185.318367	0.136508	277.166995	110
i6	185.454649	0.136735	415.292983	127
i6	185.454649	0.136735	329.608962	127
i6	185.454649	0.136735	246.934685	127
i6	185.727438	0.136508	415.292983	95
i6	185.727438	0.136508	329.608962	95
i6	185.727438	0.136508	246.934685	95
i6	186.000227	0.341043	415.292983	122
i6	186.000227	0.341043	329.608962	122
i6	186.000227	0.341043	246.934685	122
i6	186.409297	0.136508	277.166995	122
i6	186.409297	0.136508	329.608962	122
i6	186.409297	0.136508	439.999998	122
i6	186.682086	0.136508	329.608962	95
i6	186.682086	0.136508	439.999998	95
i6	186.682086	0.136508	277.166995	95
i6	186.954649	0.273016	439.999998	119
i6	186.954649	0.273016	277.166995	119
i6	186.954649	0.273016	329.608962	119
i6	187.227438	0.136508	277.166995	101
i6	187.227438	0.136508	329.608962	101
i6	187.363719	0.273016	439.999998	122
i6	187.363719	0.273016	329.608962	122
i6	187.636508	0.136508	369.994421	125
i6	187.636508	0.136508	493.869370	125
i6	187.636508	0.136508	311.126982	125
i6	187.909297	0.136508	369.994421	107
i6	187.909297	0.136508	493.869370	107
i6	187.909297	0.136508	311.126982	107
i6	188.182086	0.409297	311.126982	119
i6	188.182086	0.409297	493.869370	119
i6	188.182086	0.409297	369.994421	119
i6	188.59  	0.136508	439.999998	116
i6	188.59  	0.136508	329.608962	116
i6	188.591 	0.136508	277.166995	116
i6	188.863 	0.137   	439.999998	107
i6	188.8637	0.137   	277.166995	107
i6	188.864 	0.137   	329.608962	107
i6	189.136508	0.273016	277.166995	119
i6	189.136508	0.273016	439.999998	112
i6	189.136508	0.273016	329.608962	119
i6	189.409297	0.136508	277.166995	95
i6	189.409297	0.136508	329.608962	95
i6	189.545578	0.273016	439.999998	122
i6	189.545578	0.273016	329.608962	122
i6	189.682086	0.136508	277.166995	110
i6	189.818367	0.136508	415.292983	127
i6	189.818367	0.136508	329.608962	127
i6	189.818367	0.136508	246.934685	127
i6	190.091156	0.136508	415.292983	95
i6	190.091156	0.136508	329.608962	95
i6	190.091156	0.136508	246.934685	95
i6	190.363719	0.341270	415.292983	122
i6	190.363719	0.341270	329.608962	122
i6	190.363719	0.341270	246.934685	122
i6	190.772789	0.136735	277.166995	122
i6	190.772789	0.136735	329.608962	122
i6	190.772789	0.136735	439.999998	122
i6	191.045578	0.136735	329.608962	95
i6	191.045578	0.136735	439.999998	95
i6	191.045578	0.136735	277.166995	95
i6	191.318367	0.273016	439.999998	119
i6	191.318367	0.273016	277.166995	119
i6	191.318367	0.273016	329.608962	119
i6	191.591156	0.136508	277.166995	101
i6	191.591156	0.136508	329.608962	101
i6	191.727438	0.273016	439.999998	122
i6	191.727438	0.273016	329.608962	122
i6	192.000227	0.136508	369.994421	125
i6	192.000227	0.136508	493.869370	125
i6	192.000227	0.136508	311.126982	125
i6	192.272789	0.136735	369.994421	107
i6	192.272789	0.136735	493.869370	107
i6	192.272789	0.136735	311.126982	107
i6	192.545578	0.409297	311.126982	119
i6	192.545578	0.409297	493.869370	119
i6	192.545578	0.409297	369.994421	119
i6	192.954649	0.136735	439.999998	116
i6	192.954649	0.136735	329.608962	116
i6	192.954649	0.136735	277.166995	116
i6	193.227438	0.136508	439.999998	107
i6	193.227438	0.136508	277.166995	107
i6	193.227438	0.136508	329.608962	107
i6	193.500227	0.272789	277.166995	119
i6	193.500227	0.272789	439.999998	119
i6	193.500227	0.272789	329.608962	119
i6	193.772789	0.136735	277.166995	95
i6	193.772789	0.136735	329.608962	95
i6	193.909297	0.273016	439.999998	122
i6	193.909297	0.273016	329.608962	122
i6	194.045578	0.136735	277.166995	110
i6	194.182086	0.136508	415.292983	127
i6	194.182086	0.136508	329.608962	127
i6	194.182086	0.136508	246.934685	127
i6	194.454649	0.136735	415.292983	95
i6	194.454649	0.136735	329.608962	95
i6	194.454649	0.136735	246.934685	95
i6	194.727438	0.341043	415.292983	122
i6	194.727438	0.341043	329.608962	122
i6	194.727438	0.341043	246.934685	122
i6	195.136508	0.136508	277.166995	122
i6	195.136508	0.136508	329.608962	122
i6	195.136508	0.136508	439.999998	122
i6	195.409297	0.136508	329.608962	95
i6	195.409297	0.136508	439.999998	95
i6	195.409297	0.136508	277.166995	95
i6	195.682086	0.272789	439.999998	119
i6	195.682086	0.272789	277.166995	119
i6	195.682086	0.272789	329.608962	119
i6	195.954649	0.136735	277.166995	101
i6	195.954649	0.136735	329.608962	101
i6	196.091156	0.272789	439.999998	122
i6	196.091156	0.272789	329.608962	122
i6	196.363719	0.136735	369.994421	125
i6	196.363719	0.136735	493.869370	125
i6	196.363719	0.136735	311.126982	125
i6	196.636508	0.136508	369.994421	107
i6	196.636508	0.136508	493.869370	107
i6	196.636508	0.136508	311.126982	107
i6	196.909297	0.409297	311.126982	119
i6	196.909297	0.409297	493.869370	119
i6	196.909297	0.409297	369.994421	119
i6	197.318367	0.136508	439.999998	116
i6	197.318367	0.136508	329.608962	116
i6	197.318367	0.136508	277.166995	116
i6	197.591156	0.136508	439.999998	107
i6	197.591156	0.136508	277.166995	107
i6	197.591156	0.136508	329.608962	107
i6	197.863719	0.273016	277.166995	119
i6	197.863719	0.273016	439.999998	119
i6	197.863719	0.273016	329.608962	119
i6	198.136508	0.136508	277.166995	95
i6	198.136508	0.136508	329.608962	95
i6	198.272789	0.273016	439.999998	122
i6	198.272789	0.273016	329.608962	122
i6	198.409297	0.136508	277.166995	110
i6	198.545578	0.136735	415.292983	127
i6	198.545578	0.136735	329.608962	127
i6	198.545578	0.136735	246.934685	127
i6	198.818367	0.136508	415.292983	95
i6	198.818367	0.136508	329.608962	95
i6	198.818367	0.136508	246.934685	95
i6	199.091156	0.341043	415.292983	122
i6	199.091156	0.341043	329.608962	122
i6	199.091156	0.341043	246.934685	122
i6	199.500227	0.136508	277.166995	122
i6	199.500227	0.136508	329.608962	122
i6	199.500227	0.136508	439.999998	122
i6	199.772789	0.136735	329.608962	95
i6	199.772789	0.136735	439.999998	95
i6	199.772789	0.136735	277.166995	95
i6	200.045578	0.273016	439.999998	119
i6	200.045578	0.273016	277.166995	119
i6	200.045578	0.273016	329.608962	119
i6	200.318367	0.136508	277.166995	101
i6	200.318367	0.136508	329.608962	101
i6	200.454649	0.273016	439.999998	122
i6	200.454649	0.273016	329.608962	122
i6	200.727438	0.136508	369.994421	125
i6	200.727438	0.136508	493.869370	125
i6	200.727438	0.136508	311.126982	125
i6	201.000227	0.136508	369.994421	107
i6	201.000227	0.136508	493.869370	107
i6	201.000227	0.136508	311.126982	107
i6	201.272789	0.409524	311.126982	119
i6	201.272789	0.409524	493.869370	119
i6	201.272789	0.409524	369.994421	119
i6	201.682086	0.136508	439.999998	116
i6	201.682086	0.136508	329.608962	116
i6	201.682086	0.136508	277.166995	116
i6	201.954649	0.136735	439.999998	107
i6	201.954649	0.136735	277.166995	107
i6	201.954649	0.136735	329.608962	107
i6	202.227438	0.273016	277.166995	119
i6	202.227438	0.273016	439.999998	119
i6	202.227438	0.273016	329.608962	119
i6	202.500227	0.136508	277.166995	95
i6	202.500227	0.136508	329.608962	95
i6	202.636508	0.273016	439.999998	122
i6	202.636508	0.273016	329.608962	122
i6	202.772789	0.136735	277.166995	110
i6	202.909297	0.136508	415.292983	127
i6	202.909297	0.136508	329.608962	127
i6	202.909297	0.136508	246.934685	127
i6	203.182086	0.136508	415.292983	95
i6	203.182086	0.136508	329.608962	95
i6	203.182086	0.136508	246.934685	95
i6	203.454649	0.341270	415.292983	122
i6	203.454649	0.341270	329.608962	122
i6	203.454649	0.341270	246.934685	122
i6	203.863719	0.136735	277.166995	122
i6	203.863719	0.136735	329.608962	122
i6	203.863719	0.136735	439.999998	122
i6	204.136508	0.136508	329.608962	95
i6	204.136508	0.136508	439.999998	95
i6	204.136508	0.136508	277.166995	95
i6	204.409297	0.273016	439.999998	119
i6	204.409297	0.273016	277.166995	119
i6	204.409297	0.273016	329.608962	119
i6	204.682086	0.136508	277.166995	101
i6	204.682086	0.136508	329.608962	101
i6	204.818367	0.273016	439.999998	122
i6	204.818367	0.273016	329.608962	122
i6	205.091156	0.136508	369.994421	125
i6	205.091156	0.136508	493.869370	125
i6	205.091156	0.136508	311.126982	125
i6	205.363719	0.136735	369.994421	107
i6	205.363719	0.136735	493.869370	107
i6	205.363719	0.136735	311.126982	107
i6	205.636508	0.409297	311.126982	119
i6	205.636508	0.409297	493.869370	119
i6	205.636508	0.409297	369.994421	119
i6	206.045578	0.136735	439.999998	116
i6	206.045578	0.136735	329.608962	116
i6	206.045578	0.136735	277.166995	116
i6	206.318367	0.136508	439.999998	107
i6	206.318367	0.136508	277.166995	107
i6	206.318367	0.136508	329.608962	107
i6	206.591156	0.272789	277.166995	119
i6	206.591156	0.272789	439.999998	119
i6	206.591156	0.272789	329.608962	119
i6	206.863719	0.136735	277.166995	95
i6	206.863719	0.136735	329.608962	95
i6	207.000227	0.272789	439.999998	122
i6	207.000227	0.272789	329.608962	122
i6	207.136508	0.136508	277.166995	110
i6	207.272789	0.420862	246.934685	127
i6	207.272789	0.425397	329.608962	127
i6	207.818367	0.084354	246.934685	127
i6	207.818367	0.091156	329.608962	127
i6	207.954649	0.079819	246.934685	91
i6	208.091156	0.079592	329.608962	127
i6	208.091156	0.084354	246.934685	127
i6	208.227438	0.084354	246.934685	84
i6	208.363719	0.091156	329.608962	127
i6	208.363719	0.095692	246.934685	127
i6	208.500227	0.084354	246.934685	99
i6	208.636508	0.073016	246.934685	127
i6	208.636508	0.079819	329.608962	127
i6	208.772789	0.084354	246.934685	91
i6	208.909297	0.073016	246.934685	127
i6	208.909297	0.073016	329.608962	127
i6	209.045578	0.091156	246.934685	84
i6	209.182086	0.079592	246.934685	127
i6	209.182086	0.084127	329.608962	127
i6	209.318367	0.084354	246.934685	84
i6	209.454649	0.273016	329.608962	127
i6	209.454649	0.273016	246.934685	127

; ins 7 
; is this the horn or the piano?
; 7:1 starts 10.9 ends 209.4

i7	10.9	0.136508	164.804481	127
i7	10.9	0.136508	207.646491	127
i7	10.9	0.136508	246.934685	127
i7	11.181859	0.136508	164.804481	95
i7	11.181859	0.136508	207.646491	95
i7	11.181859	0.136508	246.934685	127
i7	11.454649	0.272789	246.934685	127
i7	11.454649	0.341043	164.804481	122
i7	11.454649	0.341043	207.646491	122
i7	11.727211	0.136735	246.934685	84
i7	11.863719	0.136508	219.00    	122
i7	11.863719	0.136508	164.804481	122
i7	11.863719	0.136508	246.934685	127
i7	12.136281	0.136735	219.00    	95
i7	12.136281	0.136735	164.804481	95
i7	12.136281	0.136735	246.934685	127
i7	12.409070	0.273016	164.804481	119
i7	12.409070	0.273016	219.00    	119
i7	12.409070	0.273016	246.934685	127
i7	12.681859	0.136508	164.804481	101
i7	12.681859	0.136508	246.934685	99
i7	12.818141	0.136735	246.934685	127
i7	12.818141	0.273016	164.804481	122
i7	12.818141	0.273016	219.00    	122
i7	12.954649	0.136508	246.934685	94
i7	13.090930	0.136508	246.934685	127
i7	13.090930	0.136508	184.997211	125
i7	13.090930	0.136508	246.934685	127
i7	13.363719	0.136508	246.934685	122
i7	13.363719	0.136508	184.997211	107
i7	13.363719	0.136508	246.934685	127
i7	13.636281	0.273016	246.934685	127
i7	13.636281	0.409524	184.997211	119
i7	13.636281	0.409524	246.934685	127
i7	14.045578	0.136508	164.804481	116
i7	14.045578	0.136508	219.00    	116
i7	14.045578	0.136508	246.934685	127
i7	14.318141	0.136735	164.804481	107
i7	14.318141	0.136735	219.00    	107
i7	14.318141	0.136735	246.934685	127
i7	14.590930	0.273016	164.804481	119
i7	14.590930	0.273016	219.00    	119
i7	14.590930	0.273016	246.934685	127
i7	14.863719	0.136508	164.804481	95
i7	14.863719	0.136508	246.934685	99
i7	15.000000	0.136508	246.934685	127
i7	15.000000	0.273016	164.804481	122
i7	15.000000	0.273016	219.00    	122
i7	15.136281	0.136735	246.934685	106
i7	15.272789	0.136508	164.804481	127
i7	15.272789	0.136508	207.646491	127
i7	15.272789	0.136508	246.934685	127
i7	15.545578	0.136508	164.804481	95
i7	15.545578	0.136508	207.646491	95
i7	15.545578	0.136508	246.934685	127
i7	15.818141	0.273016	246.934685	127
i7	15.818141	0.341270	164.804481	122
i7	15.818141	0.341270	207.646491	122
i7	16.090930	0.136508	246.934685	84
i7	16.227211	0.136735	219.00    	122
i7	16.227211	0.136735	164.804481	122
i7	16.227211	0.136735	246.934685	127
i7	16.500000	0.136508	219.00    	95
i7	16.500000	0.136508	164.804481	95
i7	16.500000	0.136508	246.934685	127
i7	16.772789	0.273016	164.804481	119
i7	16.772789	0.273016	219.999999	119
i7	16.772789	0.273016	246.934685	127
i7	17.045578	0.136508	164.804481	101
i7	17.045578	0.136508	246.934685	99
i7	17.181859	0.136508	246.934685	127
i7	17.181859	0.273016	164.804481	122
i7	17.181859	0.273016	219.00    	122
i7	17.318141	0.136735	246.934685	94
i7	17.454649	0.136508	246.934685	127
i7	17.454649	0.136508	184.997211	125
i7	17.454649	0.136508	246.934685	127
i7	17.727211	0.136735	246.934685	122
i7	17.727211	0.136735	184.997211	107
i7	17.727211	0.136735	246.934685	127
i7	18.000000	0.273016	246.934685	127
i7	18.000000	0.409297	184.997211	119
i7	18.000000	0.409297	246.934685	127
i7	18.409070	0.136735	164.804481	116
i7	18.409070	0.136735	219.00    	116
i7	18.409070	0.136735	246.934685	127
i7	18.681859	0.136508	164.804481	107
i7	18.681859	0.136508	219.00    	107
i7	18.681859	0.136508	246.934685	127
i7	18.954649	0.272789	164.804481	119
i7	18.954649	0.272789	219.00    	119
i7	18.954649	0.272789	246.934685	127
i7	19.227211	0.136735	164.804481	95
i7	19.227211	0.136735	246.934685	99
i7	19.363719	0.136508	246.934685	127
i7	19.363719	0.272789	164.804481	122
i7	19.363719	0.272789	219.00    	122
i7	19.500000	0.136508	246.934685	106
i7	19.636281	0.136735	164.804481	127
i7	19.636281	0.136735	207.646491	127
i7	19.636281	0.136735	246.934685	127
i7	19.909070	0.136735	164.804481	95
i7	19.909070	0.136735	207.646491	95
i7	19.909070	0.136735	246.934685	127
i7	20.181859	0.273016	246.934685	127
i7	20.181859	0.341043	164.804481	122
i7	20.181859	0.341043	207.646491	122
i7	20.454649	0.136508	246.934685	84
i7	20.590930	0.136508	219.00    	122
i7	20.590930	0.136508	164.804481	122
i7	20.590930	0.136508	246.934685	127
i7	20.863719	0.136508	219.00    	95
i7	20.863719	0.136508	164.804481	95
i7	20.863719	0.136508	246.934685	127
i7	21.136281	0.273016	164.804481	119
i7	21.136281	0.273016	219.00    	119
i7	21.136281	0.273016	246.934685	127
i7	21.409070	0.136735	164.804481	101
i7	21.409070	0.136735	246.934685	99
i7	21.545578	0.136508	246.934685	127
i7	21.545578	0.272789	164.804481	122
i7	21.545578	0.272789	219.00    	122
i7	21.681859	0.136508	246.934685	94
i7	21.818141	0.136735	246.934685	127
i7	21.818141	0.136735	184.997211	125
i7	21.818141	0.136735	246.934685	127
i7	22.090930	0.136508	246.934685	122
i7	22.090930	0.136508	184.997211	107
i7	22.090930	0.136508	246.934685	127
i7	22.363719	0.272789	246.934685	127
i7	22.363719	0.409297	184.997211	119
i7	22.363719	0.409297	246.934685	127
i7	22.772789	0.136508	164.804481	116
i7	22.772789	0.136508	219.00    	116
i7	22.772789	0.136508	246.934685	127
i7	23.045578	0.136508	164.804481	107
i7	23.045578	0.136508	219.00    	107
i7	23.045578	0.136508	246.934685	127
i7	23.318141	0.273016	164.804481	119
i7	23.318141	0.273016	219.00    	119
i7	23.318141	0.273016	246.934685	127
i7	23.590930	0.136508	164.804481	95
i7	23.590930	0.136508	246.934685	99
i7	23.727211	0.136735	246.934685	127
i7	23.727211	0.273016	164.804481	122
i7	23.727211	0.273016	219.00    	122
i7	23.863719	0.136508	246.934685	106
i7	24.000000	0.136508	164.804481	127
i7	24.000000	0.136508	207.646491	127
i7	24.000000	0.136508	246.934685	127
i7	24.272789	0.136508	164.804481	95
i7	24.272789	0.136508	207.646491	95
i7	24.272789	0.136508	246.934685	127
i7	24.545578	0.272789	246.934685	127
i7	24.545578	0.341043	164.804481	122
i7	24.545578	0.341043	207.646491	122
i7	24.818141	0.136735	246.934685	84
i7	24.954649	0.136508	219.00    	122
i7	24.954649	0.136508	164.804481	122
i7	24.954649	0.136508	246.934685	127
i7	25.227211	0.136735	219.00    	95
i7	25.227211	0.136735	164.804481	95
i7	25.227211	0.136735	246.934685	127
i7	25.500000	0.273016	164.804481	119
i7	25.500000	0.273016	219.00    	119
i7	25.500000	0.273016	246.934685	127
i7	25.772789	0.136508	164.804481	101
i7	25.772789	0.136508	246.934685	99
i7	25.909070	0.136735	246.934685	127
i7	25.909070	0.273016	164.804481	122
i7	25.909070	0.273016	219.00    	122
i7	26.045578	0.136508	246.934685	94
i7	26.181859	0.136508	246.934685	127
i7	26.181859	0.136508	184.997211	125
i7	26.181859	0.136508	246.934685	127
i7	26.454649	0.136508	246.934685	122
i7	26.454649	0.136508	184.997211	107
i7	26.454649	0.136508	246.934685	127
i7	26.727211	0.273016	246.934685	127
i7	26.727211	0.409297	184.997211	119
i7	26.727211	0.409297	246.934685	127
i7	27.136281	0.136735	164.804481	116
i7	27.136281	0.136735	219.00    	116
i7	27.136281	0.136735	246.934685	127
i7	27.409070	0.136735	164.804481	107
i7	27.409070	0.136735	219.00    	107
i7	27.409070	0.136735	246.934685	127
i7	27.681859	0.273016	164.804481	119
i7	27.681859	0.273016	219.00    	119
i7	27.681859	0.273016	246.934685	127
i7	27.954649	0.136508	164.804481	95
i7	27.954649	0.136508	246.934685	99
i7	28.090930	0.136508	246.934685	127
i7	28.090930	0.273016	164.804481	122
i7	28.090930	0.273016	219.00    	122
i7	28.227211	0.136735	246.934685	106
i7	28.363719	0.136508	164.804481	127
i7	28.363719	0.136508	207.646491	127
i7	28.363719	0.136508	246.934685	127
i7	28.636281	0.136735	164.804481	95
i7	28.636281	0.136735	207.646491	95
i7	28.636281	0.136735	246.934685	127
i7	28.909070	0.273016	246.934685	127
i7	28.909070	0.341270	164.804481	122
i7	28.909070	0.341270	207.646491	122
i7	29.181859	0.136508	246.934685	84
i7	29.318141	0.136735	219.00    	122
i7	29.318141	0.136735	164.804481	122
i7	29.318141	0.136735	246.934685	127
i7	29.590930	0.136508	219.00    	95
i7	29.590930	0.136508	164.804481	95
i7	29.590930	0.136508	246.934685	127
i7	29.863719	0.272789	164.804481	119
i7	29.863719	0.272789	219.00    	119
i7	29.863719	0.272789	246.934685	127
i7	30.136281	0.136735	164.804481	101
i7	30.136281	0.136735	246.934685	99
i7	30.272789	0.136508	246.934685	127
i7	30.272789	0.273016	164.804481	122
i7	30.272789	0.273016	219.00    	122
i7	30.409070	0.136735	246.934685	94
i7	30.545578	0.136508	246.934685	127
i7	30.545578	0.136508	184.997211	125
i7	30.545578	0.136508	246.934685	127
i7	30.818141	0.136735	246.934685	122
i7	30.818141	0.136735	184.997211	107
i7	30.818141	0.136735	246.934685	127
i7	31.090930	0.273016	246.934685	127
i7	31.090930	0.409297	184.997211	119
i7	31.090930	0.409297	246.934685	127
i7	31.500000	0.136508	164.804481	116
i7	31.500000	0.136508	219.00    	116
i7	31.500000	0.136508	246.934685	127
i7	31.772789	0.136508	164.804481	107
i7	31.772789	0.136508	219.00    	107
i7	31.772789	0.136508	246.934685	127
i7	32.045578	0.272789	164.804481	119
i7	32.045578	0.272789	219.00    	119
i7	32.045578	0.272789	246.934685	127
i7	32.318141	0.136735	164.804481	95
i7	32.318141	0.136735	246.934685	99
i7	32.454649	0.136508	246.934685	127
i7	32.454649	0.272789	164.804481	122
i7	32.454649	0.272789	219.00    	122
i7	32.590930	0.136508	246.934685	106
i7	32.727211	0.136735	164.804481	127
i7	32.727211	0.136735	207.646491	127
i7	32.727211	0.136735	246.934685	127
i7	33.000000	0.136508	164.804481	95
i7	33.000000	0.136508	207.646491	95
i7	33.000000	0.136508	246.934685	127
i7	33.272789	0.273016	246.934685	127
i7	33.272789	0.341043	164.804481	122
i7	33.272789	0.341043	207.646491	122
i7	33.545578	0.136508	246.934685	84
i7	33.681859	0.136508	219.00    	122
i7	33.681859	0.136508	164.804481	122
i7	33.681859	0.136508	246.934685	127
i7	33.954649	0.136508	219.00    	95
i7	33.954649	0.136508	164.804481	95
i7	33.954649	0.136508	246.934685	127
i7	34.227211	0.273016	164.804481	119
i7	34.227211	0.273016	219.00    	119
i7	34.227211	0.273016	246.934685	127
i7	34.500000	0.136508	164.804481	101
i7	34.500000	0.136508	246.934685	99
i7	34.636281	0.136735	246.934685	127
i7	34.636281	0.273016	164.804481	122
i7	34.636281	0.273016	219.00    	122
i7	34.772789	0.136508	246.934685	94
i7	34.909070	0.136735	246.934685	127
i7	34.909070	0.136735	184.997211	125
i7	34.909070	0.136735	246.934685	127
i7	35.181859	0.136508	246.934685	122
i7	35.181859	0.136508	184.997211	107
i7	35.181859	0.136508	246.934685	127
i7	35.454649	0.272789	246.934685	127
i7	35.454649	0.409297	184.997211	119
i7	35.454649	0.409297	246.934685	127
i7	35.863719	0.136508	164.804481	116
i7	35.863719	0.136508	219.00    	116
i7	35.863719	0.136508	246.934685	127
i7	36.136281	0.136735	164.804481	107
i7	36.136281	0.136735	219.00    	107
i7	36.136281	0.136735	246.934685	127
i7	36.409070	0.273016	164.804481	119
i7	36.409070	0.273016	219.00    	119
i7	36.409070	0.273016	246.934685	127
i7	36.681859	0.136508	164.804481	95
i7	36.681859	0.136508	246.934685	99
i7	36.818141	0.136735	246.934685	127
i7	36.818141	0.273016	164.804481	122
i7	36.818141	0.273016	219.00    	122
i7	36.954649	0.136508	246.934685	106
i7	37.090930	0.136508	164.804481	127
i7	37.090930	0.136508	207.646491	127
i7	37.090930	0.136508	246.934685	127
i7	37.363719	0.136508	164.804481	95
i7	37.363719	0.136508	207.646491	95
i7	37.363719	0.136508	246.934685	127
i7	37.636508	0.272789	246.934685	127
i7	37.636508	0.341043	164.804481	122
i7	37.636508	0.341043	207.646491	122
i7	37.909070	0.136735	246.934685	84
i7	38.045578	0.136508	219.00    	122
i7	38.045578	0.136508	164.804481	122
i7	38.045578	0.136508	246.934685	127
i7	38.318141	0.136735	219.00    	95
i7	38.318141	0.136735	164.804481	95
i7	38.318141	0.136735	246.934685	127
i7	38.590930	0.273016	164.804481	119
i7	38.590930	0.273016	219.00    	119
i7	38.590930	0.273016	246.934685	127
i7	38.863719	0.136508	164.804481	101
i7	38.863719	0.136508	246.934685	99
i7	39.000000	0.136735	246.934685	127
i7	39.000000	0.273016	164.804481	122
i7	39.000000	0.273016	219.00    	122
i7	39.136508	0.136508	246.934685	94
i7	39.272789	0.136508	246.934685	127
i7	39.272789	0.136508	184.997211	125
i7	39.272789	0.136508	246.934685	127
i7	39.545578	0.136508	246.934685	122
i7	39.545578	0.136508	184.997211	107
i7	39.545578	0.136508	246.934685	127
i7	39.818141	0.273016	246.934685	127
i7	39.818141	0.409297	184.997211	119
i7	39.818141	0.409297	246.934685	127
i7	40.227211	0.136735	164.804481	116
i7	40.227211	0.136735	219.00    	116
i7	40.227211	0.136735	246.934685	127
i7	40.500000	0.136735	164.804481	107
i7	40.500000	0.136735	219.00    	107
i7	40.500000	0.136735	246.934685	127
i7	40.772789	0.273016	164.804481	119
i7	40.772789	0.273016	219.00    	119
i7	40.772789	0.273016	246.934685	127
i7	41.045578	0.136508	164.804481	95
i7	41.045578	0.136508	246.934685	99
i7	41.181859	0.136508	246.934685	127
i7	41.181859	0.273016	164.804481	122
i7	41.181859	0.273016	219.00    	122
i7	41.318141	0.136735	246.934685	106
i7	41.454649	0.136508	164.804481	127
i7	41.454649	0.136508	207.646491	127
i7	41.454649	0.136508	246.934685	127
i7	41.727211	0.136735	164.804481	95
i7	41.727211	0.136735	207.646491	95
i7	41.727211	0.136735	246.934685	127
i7	42.000000	0.273016	246.934685	127
i7	42.000000	0.341270	164.804481	122
i7	42.000000	0.341270	207.646491	122
i7	42.272789	0.136508	246.934685	84
i7	42.409070	0.136735	219.00    	122
i7	42.409070	0.136735	164.804481	122
i7	42.409070	0.136735	246.934685	127
i7	42.681859	0.136508	219.00    	95
i7	42.681859	0.136508	164.804481	95
i7	42.681859	0.136508	246.934685	127
i7	42.954649	0.272789	164.804481	119
i7	42.954649	0.272789	219.00    	119
i7	42.954649	0.272789	246.934685	127
i7	43.227211	0.136735	164.804481	101
i7	43.227211	0.136735	246.934685	99
i7	43.363719	0.136508	246.934685	127
i7	43.363719	0.273016	164.804481	122
i7	43.363719	0.273016	219.00    	122
i7	43.500000	0.136735	246.934685	94
i7	43.636508	0.136508	246.934685	127
i7	43.636508	0.136508	184.997211	125
i7	43.636508	0.136508	246.934685	127
i7	43.909070	0.136735	246.934685	122
i7	43.909070	0.136735	184.997211	107
i7	43.909070	0.136735	246.934685	127
i7	44.181859	0.273016	246.934685	127
i7	44.181859	0.409297	184.997211	119
i7	44.181859	0.409297	246.934685	127
i7	44.590930	0.136508	164.804481	116
i7	44.590930	0.136508	219.00    	116
i7	44.590930	0.136508	246.934685	127
i7	44.863719	0.136508	164.804481	107
i7	44.863719	0.136508	219.00    	107
i7	44.863719	0.136508	246.934685	127
i7	45.136508	0.272789	164.804481	119
i7	45.136508	0.272789	219.00    	119
i7	45.136508	0.272789	246.934685	127
i7	45.409070	0.136735	164.804481	95
i7	45.409070	0.136735	246.934685	99
i7	45.545578	0.136508	246.934685	127
i7	45.545578	0.272789	164.804481	122
i7	45.545578	0.272789	219.00    	122
i7	45.681859	0.136508	246.934685	106
i7	45.818141	0.136735	164.804481	127
i7	45.818141	0.136735	207.646491	127
i7	45.818141	0.136735	246.934685	127
i7	46.090930	0.136508	164.804481	95
i7	46.090930	0.136508	207.646491	95
i7	46.090930	0.136508	246.934685	127
i7	46.363719	0.273016	246.934685	127
i7	46.363719	0.341043	164.804481	122
i7	46.363719	0.341043	207.646491	122
i7	46.636508	0.136508	246.934685	84
i7	46.772789	0.136508	219.00    	122
i7	46.772789	0.136508	164.804481	122
i7	46.772789	0.136508	246.934685	127
i7	47.045578	0.136508	219.00    	95
i7	47.045578	0.136508	164.804481	95
i7	47.045578	0.136508	246.934685	127
i7	47.318141	0.273016	164.804481	119
i7	47.318141	0.273016	219.00    	119
i7	47.318141	0.273016	246.934685	127
i7	47.590930	0.136508	164.804481	101
i7	47.590930	0.136508	246.934685	99
i7	47.73		0.136735	246.934685	127
i7	47.73		0.27		164.804481	122
i7	47.73		0.27		219.00    	122
i7	47.863719	0.136508	246.934685	94
i7	48.000000	0.136735	246.934685	127
i7	48.000000	0.136735	185.0		125
i7	48.000000	0.136735	246.934685	127
i7	48.272789	0.136508	246.934685	122
i7	48.272789	0.136508	184.997211	107
i7	48.272789	0.136508	246.934685	127
i7	48.55		0.27		246.9		127
i7	48.55		0.41		184.9		119
i7	48.55		0.41		246.9		127
i7	48.954649	0.136508	164.804481	116
i7	48.954649	0.136508	219.00    	116
i7	48.954649	0.136508	246.934685	127
i7	49.227211	0.136735	164.804481	107
i7	49.227211	0.136735	219.00    	107
i7	49.227211	0.136735	246.934685	127
i7	49.500000	0.273016	164.804481	119
i7	49.500000	0.273016	219.00    	119
i7	49.500000	0.273016	246.934685	127
i7	49.772789	0.136508	164.804481	95
i7	49.772789	0.136508	246.934685	99
i7	49.909070	0.136735	246.934685	127
i7	49.909070	0.273016	164.804481	122
i7	49.909070	0.273016	219.00    	122
i7	50.045578	0.136508	246.934685	106
i7	50.181859	0.136508	164.804481	127
i7	50.181859	0.136508	207.646491	127
i7	50.181859	0.136508	246.934685	127
i7	50.454649	0.136508	164.804481	95
i7	50.454649	0.136508	207.646491	95
i7	50.454649	0.136508	246.934685	127
i7	50.727211	0.273016	246.934685	127
i7	50.727211	0.341270	164.804481	122
i7	50.727211	0.341270	207.646491	122
i7	51.000000	0.136735	246.934685	84
i7	51.136508	0.136508	219.00    	122
i7	51.136508	0.136508	164.804481	122
i7	51.136508	0.136508	246.934685	127
i7	51.409070	0.136735	219.00    	95
i7	51.409070	0.136735	164.804481	95
i7	51.409070	0.136735	246.934685	127
i7	51.681859	0.273016	164.804481	119
i7	51.681859	0.273016	219.00    	119
i7	51.681859	0.273016	246.934685	127
i7	51.954649	0.136508	164.804481	101
i7	51.954649	0.136508	246.934685	99
i7	52.090930	0.136508	246.934685	127
i7	52.090930	0.273016	164.804481	122
i7	52.090930	0.273016	219.00    	122
i7	52.227211	0.136735	246.934685	94
i7	52.363719	0.136508	246.934685	127
i7	52.363719	0.136508	184.997211	125
i7	52.363719	0.136508	246.934685	127
i7	52.636508	0.136508	246.934685	122
i7	52.636508	0.136508	184.997211	107
i7	52.636508	0.136508	246.934685	127
i7	52.909070	0.273016	246.934685	127
i7	52.909070	0.409297	184.997211	119
i7	52.909070	0.409297	246.934685	127
i7	53.318141	0.136735	164.804481	116
i7	53.318141	0.136735	219.00    	116
i7	53.318141	0.136735	246.934685	127
i7	53.590930	0.136508	164.804481	107
i7	53.590930	0.136508	219.00    	107
i7	53.590930	0.136508	246.934685	127
i7	53.863719	0.273016	164.804481	119
i7	53.863719	0.273016	219.00    	119
i7	53.863719	0.273016	246.934685	127
i7	54.136508	0.136508	164.804481	95
i7	54.136508	0.136508	246.934685	99
i7	54.272789	0.136508	246.934685	127
i7	54.272789	0.273016	164.804481	122
i7	54.272789	0.273016	219.00    	122
i7	54.409070	0.136735	246.934685	106
i7	54.545578	0.136508	155.563491	127
i7	54.545578	0.136508	207.646491	127
i7	54.545578	0.136508	246.934685	127
i7	54.818141	0.136735	155.563491	95
i7	54.818141	0.136735	207.646491	95
i7	54.818141	0.136735	246.934685	127
i7	55.090930	0.273016	246.934685	127
i7	55.090930	0.341270	155.563491	122
i7	55.090930	0.341270	207.646491	122
i7	55.363719	0.136508	246.934685	84
i7	55.500000	0.136735	207.646491	122
i7	55.500000	0.136735	155.563491	122
i7	55.500000	0.136735	246.934685	127
i7	55.772789	0.136508	207.646491	95
i7	55.772789	0.136508	155.563491	95
i7	55.772789	0.136508	246.934685	127
i7	56.045578	0.272789	207.646491	119
i7	56.045578	0.272789	155.563491	119
i7	56.045578	0.272789	246.934685	127
i7	56.318141	0.136735	155.563491	101
i7	56.318141	0.136735	246.934685	99
i7	56.454649	0.136508	246.934685	127
i7	56.454649	0.272789	155.563491	122
i7	56.454649	0.272789	207.646491	122
i7	56.590930	0.136508	246.934685	94
i7	56.727211	0.136735	164.804481	107
i7	56.727211	0.136735	219.00    	127
i7	56.727211	0.136735	219.00    	127
i7	56.727211	0.136735	184.997211	125
i7	57.000000	0.136735	164.804481	107
i7	57.000000	0.136735	219.00    	127
i7	57.000000	0.136735	219.00    	122
i7	57.000000	0.136735	184.997211	107
i7	57.272789	0.273016	164.804481	119
i7	57.272789	0.273016	219.00    	127
i7	57.272789	0.409297	219.00    	127
i7	57.272789	0.409297	184.997211	119
i7	57.681859	0.136508	219.00    	127
i7	57.681859	0.136508	184.997211	116
i7	57.681859	0.136508	164.804481	116
i7	57.954649	0.136508	219.00    	127
i7	57.954649	0.136508	184.997211	107
i7	57.954649	0.136508	164.804481	107
i7	58.227211	0.273016	219.00    	127
i7	58.227211	0.273016	184.997211	119
i7	58.227211	0.273016	164.804481	119
i7	58.500000	0.136735	219.00    	99
i7	58.500000	0.136735	164.804481	95
i7	58.636508	0.136508	219.00    	127
i7	58.636508	0.272789	164.804481	122
i7	58.636508	0.272789	184.997211	122
i7	58.772789	0.136508	219.00    	106
i7	58.909070	0.136735	155.563491	127
i7	58.909070	0.136735	207.646491	127
i7	58.909070	0.136735	246.934685	127
i7	59.181859	0.136508	155.563491	95
i7	59.181859	0.136508	207.646491	95
i7	59.181859	0.136508	246.934685	127
i7	59.454649	0.272789	246.934685	127
i7	59.454649	0.341043	155.563491	122
i7	59.454649	0.341043	207.646491	122
i7	59.727211	0.136735	246.934685	84
i7	59.863719	0.136508	207.646491	122
i7	59.863719	0.136508	155.563491	122
i7	59.863719	0.136508	246.934685	127
i7	60.136508	0.136508	207.646491	95
i7	60.136508	0.136508	155.563491	95
i7	60.136508	0.136508	246.934685	127
i7	60.409070	0.273016	207.646491	119
i7	60.409070	0.273016	155.563491	119
i7	60.409070	0.273016	246.934685	127
i7	60.681859	0.136508	155.563491	101
i7	60.681859	0.136508	246.934685	99
i7	60.818141	0.136735	246.934685	127
i7	60.818141	0.273016	155.563491	122
i7	60.818141	0.273016	207.646491	122
i7	60.954649	0.136508	246.934685	94
i7	61.090930	0.136508	164.804481	107
i7	61.090930	0.136508	219.00    	127
i7	61.090930	0.136508	219.00    	127
i7	61.090930	0.136508	184.997211	125
i7	61.363719	0.136508	164.804481	107
i7	61.363719	0.136508	219.00    	127
i7	61.363719	0.136508	219.00    	122
i7	61.363719	0.136508	184.997211	107
i7	61.636508	0.272789	164.804481	119
i7	61.636508	0.272789	219.00    	127
i7	61.636508	0.409297	219.00    	127
i7	61.636508	0.409297	184.997211	119
i7	62.045578	0.136508	219.00    	127
i7	62.045578	0.136508	184.997211	116
i7	62.045578	0.136508	164.804481	116
i7	62.318141	0.136735	219.00    	127
i7	62.318141	0.136735	184.997211	107
i7	62.318141	0.136735	164.804481	107
i7	62.590930	0.273016	219.00    	127
i7	62.590930	0.273016	184.997211	119
i7	62.590930	0.273016	164.804481	119
i7	62.863719	0.136508	219.00    	99
i7	62.863719	0.136508	164.804481	95
i7	63.000000	0.136735	219.999999	127
i7	63.000000	0.273016	164.804481	122
i7	63.000000	0.273016	184.997211	122
i7	63.136508	0.136508	219.00    	106
i7	63.272789	0.136508	155.563491	127
i7	63.272789	0.136508	207.646491	127
i7	63.272789	0.136508	246.934685	127
i7	63.545578	0.136508	155.563491	95
i7	63.545578	0.136508	207.646491	95
i7	63.545578	0.136508	246.934685	127
i7	63.818141	0.273016	246.934685	127
i7	63.818141	0.341270	155.563491	122
i7	63.818141	0.341270	207.646491	122
i7	64.090930	0.136735	246.934685	84
i7	64.227438	0.136508	207.646491	122
i7	64.227438	0.136508	155.563491	122
i7	64.227438	0.136508	246.934685	127
i7	64.500000	0.136735	207.646491	95
i7	64.500000	0.136735	155.563491	95
i7	64.500000	0.136735	246.934685	127
i7	64.772789	0.273016	207.646491	119
i7	64.772789	0.273016	155.563491	119
i7	64.772789	0.273016	246.934685	127
i7	65.045578	0.136508	155.563491	101
i7	65.045578	0.136508	246.934685	99
i7	65.181859	0.136508	246.934685	127
i7	65.181859	0.273016	155.563491	122
i7	65.181859	0.273016	207.646491	122
i7	65.318141	0.136735	246.934685	94
i7	65.454649	0.136508	164.804481	107
i7	65.454649	0.136508	219.00    	127
i7	65.454649	0.136508	219.00    	127
i7	65.454649	0.136508	184.997211	125
i7	65.727438	0.136508	164.804481	107
i7	65.727438	0.136508	219.00    	127
i7	65.727438	0.136508	219.00    	122
i7	65.727438	0.136508	184.997211	107
i7	66.000000	0.273016	164.804481	119
i7	66.000000	0.273016	219.00    	127
i7	66.000000	0.409297	219.00    	127
i7	66.000000	0.409297	184.997211	119
i7	66.409070	0.136735	219.00    	127
i7	66.409070	0.136735	184.997211	116
i7	66.409070	0.136735	164.804481	116
i7	66.681859	0.136508	219.00    	127
i7	66.681859	0.136508	184.997211	107
i7	66.681859	0.136508	164.804481	107
i7	66.954649	0.273016	219.00    	127
i7	66.954649	0.273016	184.997211	119
i7	66.954649	0.273016	164.804481	119
i7	67.227438	0.136508	219.00    	99
i7	67.227438	0.136508	164.804481	95
i7	67.363719	0.136508	219.00    	127
i7	67.363719	0.273016	164.804481	122
i7	67.363719	0.273016	184.997211	122
i7	67.500000	0.136735	219.00    	106
i7	67.636508	0.136508	164.804481	127
i7	67.636508	0.136508	207.646491	127
i7	67.636508	0.136508	246.934685	127
i7	67.909070	0.136735	164.804481	95
i7	67.909070	0.136735	207.646491	95
i7	67.909070	0.136735	246.934685	127
i7	68.181859	0.273016	246.934685	127
i7	68.181859	0.341043	164.804481	122
i7	68.181859	0.341043	207.646491	122
i7	68.454649	0.136508	246.934685	84
i7	68.590930	0.136735	219.00    	122
i7	68.590930	0.136735	164.804481	122
i7	68.590930	0.136735	246.934685	127
i7	68.863719	0.136508	219.00    	95
i7	68.863719	0.136508	164.804481	95
i7	68.863719	0.136508	246.934685	127
i7	69.136508	0.272789	164.804481	119
i7	69.136508	0.272789	219.00    	119
i7	69.136508	0.272789	246.934685	127
i7	69.409070	0.136735	164.804481	101
i7	69.409070	0.136735	246.934685	99
i7	69.545578	0.136508	246.934685	127
i7	69.545578	0.272789	164.804481	122
i7	69.545578	0.272789	219.00    	122
i7	69.681859	0.136508	246.934685	94
i7	69.818141	0.136735	246.934685	127
i7	69.818141	0.136735	184.997211	125
i7	69.818141	0.136735	246.934685	127
i7	70.090930	0.136735	246.934685	122
i7	70.090930	0.136735	184.997211	107
i7	70.090930	0.136735	246.934685	127
i7	70.363719	0.273016	246.934685	127
i7	70.363719	0.409297	184.997211	119
i7	70.363719	0.409297	246.934685	127
i7	70.772789	0.136508	164.804481	116
i7	70.772789	0.136508	219.00    	116
i7	70.772789	0.136508	246.934685	127
i7	71.045578	0.136508	164.804481	107
i7	71.045578	0.136508	219.00    	107
i7	71.045578	0.136508	246.934685	127
i7	71.318141	0.273016	164.804481	119
i7	71.318141	0.273016	219.00    	119
i7	71.318141	0.273016	246.934685	127
i7	71.590930	0.136735	164.804481	95
i7	71.590930	0.136735	246.934685	99
i7	71.727438	0.136508	246.934685	127
i7	71.727438	0.272789	164.804481	122
i7	71.727438	0.272789	219.00    	122
i7	71.863719	0.136508	246.934685	106
i7	72.000000	0.136735	164.804481	127
i7	72.000000	0.136735	207.646491	127
i7	72.000000	0.136735	246.934685	127
i7	72.272789	0.136508	164.804481	95
i7	72.272789	0.136508	207.646491	95
i7	72.272789	0.136508	246.934685	127
i7	72.545578	0.272789	246.934685	127
i7	72.545578	0.341043	164.804481	122
i7	72.545578	0.341043	207.646491	122
i7	72.818141	0.136735	246.934685	84
i7	72.954649	0.136508	219.00    	122
i7	72.954649	0.136508	164.804481	122
i7	72.954649	0.136508	246.934685	127
i7	73.227438	0.136508	219.00    	95
i7	73.227438	0.136508	164.804481	95
i7	73.227438	0.136508	246.934685	127
i7	73.500000	0.273016	164.804481	119
i7	73.500000	0.273016	219.00    	119
i7	73.500000	0.273016	246.934685	127
i7	73.772789	0.136508	164.804481	101
i7	73.772789	0.136508	246.934685	99
i7	73.909070	0.136735	246.934685	127
i7	73.909070	0.273016	164.804481	122
i7	73.909070	0.273016	219.00    	122
i7	74.045578	0.136508	246.934685	94
i7	74.181859	0.136508	246.934685	127
i7	74.181859	0.136508	184.997211	125
i7	74.181859	0.136508	246.934685	127
i7	74.454649	0.136508	246.934685	122
i7	74.454649	0.136508	184.997211	107
i7	74.454649	0.136508	246.934685	127
i7	74.727438	0.272789	246.934685	127
i7	74.727438	0.409297	184.997211	119
i7	74.727438	0.409297	246.934685	127
i7	75.136508	0.136508	164.804481	116
i7	75.136508	0.136508	219.00    	116
i7	75.136508	0.136508	246.934685	127
i7	75.409070	0.136735	164.804481	107
i7	75.409070	0.136735	219.00    	107
i7	75.409070	0.136735	246.934685	127
i7	75.681859	0.273016	164.804481	119
i7	75.681859	0.273016	219.00    	119
i7	75.681859	0.273016	246.934685	127
i7	75.954649	0.136508	164.804481	95
i7	75.954649	0.136508	246.934685	99
i7	76.090930	0.136735	246.934685	127
i7	76.090930	0.273016	164.804481	122
i7	76.090930	0.273016	219.00    	122
i7	76.227438	0.136508	246.934685	106
i7	76.363719	0.136508	164.804481	127
i7	76.363719	0.136508	207.646491	127
i7	76.363719	0.136508	246.934685	127
i7	76.636508	0.136508	164.804481	95
i7	76.636508	0.136508	207.646491	95
i7	76.636508	0.136508	246.934685	127
i7	76.909070	0.273016	246.934685	127
i7	76.909070	0.341270	164.804481	122
i7	76.909070	0.341270	207.646491	122
i7	77.181859	0.136508	246.934685	84
i7	77.318141	0.136735	219.00    	122
i7	77.318141	0.136735	164.804481	122
i7	77.318141	0.136735	246.934685	127
i7	77.590930	0.136735	219.00    	95
i7	77.590930	0.136735	164.804481	95
i7	77.590930	0.136735	246.934685	127
i7	77.863719	0.273016	164.804481	119
i7	77.863719	0.273016	219.00    	119
i7	77.863719	0.273016	246.934685	127
i7	78.136508	0.136508	164.804481	101
i7	78.136508	0.136508	246.934685	99
i7	78.272789	0.136508	246.934685	127
i7	78.272789	0.273016	164.804481	122
i7	78.272789	0.273016	219.00    	122
i7	78.409070	0.136735	246.934685	94
i7	78.545578	0.136508	246.934685	127
i7	78.545578	0.136508	184.997211	125
i7	78.545578	0.136508	246.934685	127
i7	78.818141	0.136735	246.934685	122
i7	78.818141	0.136735	184.997211	107
i7	78.818141	0.136735	246.934685	127
i7	79.090930	0.273016	246.934685	127
i7	79.090930	0.409297	184.997211	119
i7	79.090930	0.409297	246.934685	127
i7	79.500000	0.136735	164.804481	116
i7	79.500000	0.136735	219.00    	116
i7	79.500000	0.136735	246.934685	127
i7	79.772789	0.136508	164.804481	107
i7	79.772789	0.136508	219.00    	107
i7	79.772789	0.136508	246.934685	127
i7	80.045578	0.272789	164.804481	119
i7	80.045578	0.272789	219.00    	119
i7	80.045578	0.272789	246.934685	127
i7	80.318141	0.136735	164.804481	95
i7	80.318141	0.136735	246.934685	99
i7	80.454649	0.136508	246.934685	127
i7	80.454649	0.273016	164.804481	122
i7	80.454649	0.273016	219.00    	122
i7	80.590930	0.136735	246.934685	106
i7	80.727438	0.136508	164.804481	127
i7	80.727438	0.136508	207.646491	127
i7	80.727438	0.136508	246.934685	127
i7	81      	0.136735	164.804481	95
i7	81	        0.136735	207.646491	95
i7	81      	0.136735	246.934685	127
i7	81.272789	0.273016	246.934685	127
i7	81.272789	0.341043	164.804481	122
i7	81.272789	0.341043	207.646491	122
i7	81.545578	0.136508	246.934685	84
i7	81.7	0.136508	219.00    	122
i7	81.7	0.136508	164.804481	122
i7	81.7	0.136508	246.934685	127
i7	81.954649	0.136508	219.00    	95
i7	81.954649	0.136508	164.804481	95
i7	81.954649	0.136508	246.934685	127
i7	82.227438	0.272789	164.804481	119
i7	82.227438	0.272789	219.00    	119
i7	82.227438	0.272789	246.934685	127
i7	82.500000	0.136735	164.804481	101
i7	82.500000	0.136735	246.934685	99
i7	82.636508	0.136508	246.934685	127
i7	82.636508	0.272789	164.804481	122
i7	82.636508	0.272789	219.00    	122
i7	82.772789	0.136508	246.934685	94
i7	82.909070	0.136735	246.934685	127
i7	82.909070	0.136735	184.997211	125
i7	82.909070	0.136735	246.934685	127
i7	83.181859	0.136508	246.934685	122
i7	83.181859	0.136508	184.997211	107
i7	83.181859	0.136508	246.934685	127
i7	83.454649	0.273016	246.934685	127
i7	83.454649	0.409297	184.997211	119
i7	83.454649	0.409297	246.934685	127
i7	83.863719	0.136508	164.804481	116
i7	83.863719	0.136508	219.00    	116
i7	83.863719	0.136508	246.934685	127
i7	84.136508	0.136508	164.804481	107
i7	84.136508	0.136508	219.00    	107
i7	84.136508	0.136508	246.934685	127
i7	84.409070	0.273016	164.804481	119
i7	84.409070	0.273016	219.00    	119
i7	84.409070	0.273016	246.934685	127
i7	84.681859	0.136508	164.804481	95
i7	84.681859	0.136508	246.934685	99
i7	84.818141	0.136735	246.934685	127
i7	84.818141	0.273016	164.804481	122
i7	84.818141	0.273016	219.00    	122
i7	84.954649	0.136508	246.934685	106
i7	85.090930	0.136735	164.804481	127
i7	85.090930	0.136735	207.646491	127
i7	85.090930	0.136735	246.934685	127
i7	85.363719	0.136508	164.804481	95
i7	85.363719	0.136508	207.646491	95
i7	85.363719	0.136508	246.934685	127
i7	85.636508	0.272789	246.934685	127
i7	85.636508	0.341043	164.804481	122
i7	85.636508	0.341043	207.646491	122
i7	85.909070	0.136735	246.934685	84
i7	86.0		0.136508	219.00    	122
i7	86.0		0.136508	164.804481	122
i7	86.1		0.136508	246.934685	127
i7	86.318141	0.136735	219.00    	95
i7	86.318141	0.136735	164.804481	95
i7	86.318141	0.136735	246.934685	127
i7	86.590930	0.273016	164.804481	119
i7	86.590930	0.273016	219.00    	119
i7	86.590930	0.273016	246.934685	127
i7	86.863719	0.136508	164.804481	101
i7	86.863719	0.136508	246.934685	99
i7	87.000000	0.136735	246.934685	127
i7	87.000000	0.273016	164.804481	122
i7	87.000000	0.273016	219.00    	122
i7	87.136508	0.136508	246.934685	94
i7	87.272789	0.136508	246.934685	127
i7	87.272789	0.136508	184.997211	125
i7	87.272789	0.136508	246.934685	127
i7	87.545578	0.136508	246.934685	122
i7	87.545578	0.136508	184.997211	107
i7	87.545578	0.136508	246.934685	127
i7	87.818367	0.272789	246.934685	127
i7	87.818367	0.409297	184.997211	119
i7	87.818367	0.409297	246.934685	127
i7	88.227438	0.136508	164.804481	116
i7	88.227438	0.136508	219.00    	116
i7	88.227438	0.136508	246.934685	127
i7	88.500000	0.136735	164.804481	107
i7	88.500000	0.136735	219.00    	107
i7	88.500000	0.136735	246.934685	127
i7	88.772789	0.273016	164.804481	119
i7	88.772789	0.273016	219.00    	119
i7	88.772789	0.273016	246.934685	127
i7	89.045578	0.136508	164.804481	95
i7	89.045578	0.136508	246.934685	99
i7	89.181859	0.136735	246.934685	127
i7	89.181859	0.273016	164.804481	122
i7	89.181859	0.273016	219.00    	122
i7	89.318367	0.136508	246.934685	106
i7	89.454649	0.136508	164.804481	127
i7	89.454649	0.136508	207.646491	127
i7	89.454649	0.136508	246.934685	127
i7	89.727438	0.136508	164.804481	95
i7	89.727438	0.136508	207.646491	95
i7	89.727438	0.136508	246.934685	127
i7	90.000000	0.273016	246.934685	127
i7	90.000000	0.341270	164.804481	122
i7	90.000000	0.341270	207.646491	122
i7	90.272789	0.136508	246.934685	84
i7	90.409070	0.136735	219.00    	122
i7	90.409070	0.136735	164.804481	122
i7	90.409070	0.136735	246.934685	127
i7	90.681859	0.136735	219.00    	95
i7	90.681859	0.136735	164.804481	95
i7	90.681859	0.136735	246.934685	127
i7	90.954649	0.273016	164.804481	119
i7	90.954649	0.273016	219.00    	119
i7	90.954649	0.273016	246.934685	127
i7	91.227438	0.136508	164.804481	101
i7	91.227438	0.136508	246.934685	99
i7	91.363719	0.136508	246.934685	127
i7	91.363719	0.273016	164.804481	122
i7	91.363719	0.273016	219.00    	122
i7	91.500000	0.136735	246.934685	94
i7	91.636508	0.136508	246.934685	127
i7	91.636508	0.136508	184.997211	125
i7	91.636508	0.136508	246.934685	127
i7	91.909070	0.136735	246.934685	122
i7	91.909070	0.136735	184.997211	107
i7	91.909070	0.136735	246.934685	127
i7	92.181859	0.273016	246.934685	127
i7	92.181859	0.409297	184.997211	119
i7	92.181859	0.409297	246.934685	127
i7	92.590930	0.136735	164.804481	116
i7	92.590930	0.136735	219.00    	116
i7	92.590930	0.136735	246.934685	127
i7	92.863719	0.136508	164.804481	107
i7	92.863719	0.136508	219.00    	107
i7	92.863719	0.136508	246.934685	127
i7	93.136508	0.272789	164.804481	119
i7	93.136508	0.272789	219.00    	119
i7	93.136508	0.272789	246.934685	127
i7	93.409070	0.136735	164.804481	95
i7	93.409070	0.136735	246.934685	99
i7	93.545578	0.136508	246.934685	127
i7	93.545578	0.273016	164.804481	122
i7	93.545578	0.273016	219.00    	122
i7	93.681859	0.136735	246.934685	106
i7	93.818367	0.136508	164.804481	127
i7	93.818367	0.136508	207.646491	127
i7	93.818367	0.136508	246.934685	127
i7	94.090930	0.136735	164.804481	95
i7	94.090930	0.136735	207.646491	95
i7	94.090930	0.136735	246.934685	127
i7	94.363719	0.273016	246.934685	127
i7	94.363719	0.341043	164.804481	122
i7	94.363719	0.341043	207.646491	122
i7	94.636508	0.136508	246.934685	84
i7	94.772789	0.136508	219.00    	122
i7	94.772789	0.136508	164.804481	122
i7	94.772789	0.136508	246.934685	127
i7	95.045578	0.136508	219.00    	95
i7	95.045578	0.136508	164.804481	95
i7	95.045578	0.136508	246.934685	127
i7	95.318367	0.272789	164.804481	119
i7	95.318367	0.272789	219.00    	119
i7	95.318367	0.272789	246.934685	127
i7	95.590930	0.136735	164.804481	101
i7	95.590930	0.136735	246.934685	99
i7	95.727438	0.136508	246.934685	127
i7	95.727438	0.272789	164.804481	122
i7	95.727438	0.272789	219.00    	122
i7	95.863719	0.136508	246.934685	94
i7	96.000000	0.136735	246.934685	127
i7	96.000000	0.136735	184.997211	125
i7	96.000000	0.136735	246.934685	127
i7	96.272789	0.136508	246.934685	122
i7	96.272789	0.136508	184.997211	107
i7	96.272789	0.136508	246.934685	127
i7	96.545578	0.273016	246.934685	127
i7	96.545578	0.409297	184.997211	119
i7	96.545578	0.409297	246.934685	127
i7	96.954649	0.136508	164.804481	116
i7	96.954649	0.136508	219.00    	116
i7	96.954649	0.136508	246.934685	127
i7	97.227438	0.136508	164.804481	107
i7	97.227438	0.136508	219.00    	107
i7	97.227438	0.136508	246.934685	127
i7	97.500000	0.273016	164.804481	119
i7	97.500000	0.273016	219.00    	119
i7	97.500000	0.273016	246.934685	127
i7	97.772789	0.136508	164.804481	95
i7	97.772789	0.136508	246.934685	99
i7	97.909070	0.136735	246.934685	127
i7	97.909070	0.273016	164.804481	122
i7	97.909070	0.273016	219.00    	122
i7	98.045578	0.136508	246.934685	106
i7	98.181859	0.136735	164.804481	127
i7	98.181859	0.136735	207.646491	127
i7	98.181859	0.136735	246.934685	127
i7	98.454649	0.136508	164.804481	95
i7	98.454649	0.136508	207.646491	95
i7	98.454649	0.136508	246.934685	127
i7	98.727438	0.272789	246.934685	127
i7	98.727438	0.341043	164.804481	122
i7	98.727438	0.341043	207.646491	122
i7	99.000000	0.136735	246.934685	84
i7	99.136508	0.136508	219.00    	122
i7	99.136508	0.136508	164.804481	122
i7	99.136508	0.136508	246.934685	127
i7	99.409070	0.136735	219.00    	95
i7	99.409070	0.136735	164.804481	95
i7	99.409070	0.136735	246.934685	127
i7	99.681859	0.273016	164.804481	119
i7	99.681859	0.273016	219.00    	119
i7	99.681859	0.273016	246.934685	127
i7	99.954649	0.136508	164.804481	101
i7	99.954649	0.136508	246.934685	99
i7	100.090930	0.136735	246.934685	127
i7	100.090930	0.273016	164.804481	122
i7	100.090930	0.273016	219.00    	122
i7	100.227438	0.136508	246.934685	94
i7	100.363719	0.136508	246.934685	127
i7	100.363719	0.136508	184.997211	125
i7	100.363719	0.136508	246.934685	127
i7	100.636508	0.136508	246.934685	122
i7	100.636508	0.136508	184.997211	107
i7	100.636508	0.136508	246.934685	127
i7	100.9		0.273016	246.934685	127
i7	100.9		0.409524	184.997211	119
i7	100.91		0.41		246.9		125
i7	101.31		0.136508	164.804481	116
i7	101.32		0.136508	219.00    	116
i7	101.32		0.136508	246.934685	127
i7	101.59		0.136735	164.804481	107
i7	101.59		0.136735	219.00    	107
i7	101.59		0.136735	246.934685	127
i7	101.863719	0.273016	164.804481	119
i7	101.863719	0.273016	219.00    	119
i7	101.863719	0.273016	246.934685	127
i7	102.136508	0.136508	164.804481	95
i7	102.136508	0.136508	246.934685	99
i7	102.272789	0.136508	246.934685	127
i7	102.272789	0.273016	164.804481	122
i7	102.272789	0.273016	219.00    	122
i7	102.409070	0.136735	246.934685	106
i7	102.545578	0.136508	164.804481	127
i7	102.545578	0.136508	207.646491	127
i7	102.545578	0.136508	246.934685	127
i7	102.818367	0.136508	164.804481	95
i7	102.818367	0.136508	207.646491	95
i7	102.818367	0.136508	246.934685	127
i7	103.090930	0.273016	246.934685	127
i7	103.090930	0.341270	164.804481	122
i7	103.090930	0.341270	207.646491	122
i7	103.363719	0.136508	246.934685	84
i7	103.500000	0.136735	219.00    	122
i7	103.500000	0.136735	164.804481	122
i7	103.500000	0.136735	246.934685	127
i7	103.772789	0.136508	219.00    	95
i7	103.772789	0.136508	164.804481	95
i7	103.772789	0.136508	246.934685	127
i7	104.045578	0.273016	164.804481	119
i7	104.045578	0.273016	219.00    	119
i7	104.045578	0.273016	246.934685	127
i7	104.318367	0.136508	164.804481	101
i7	104.318367	0.136508	246.934685	99
i7	104.454649	0.136508	246.934685	127
i7	104.454649	0.273016	164.804481	122
i7	104.454649	0.273016	219.00    	122
i7	104.590930	0.136735	246.934685	94
i7	104.727438	0.136508	246.934685	127
i7	104.727438	0.136508	184.997211	125
i7	104.727438	0.136508	246.934685	127
i7	105.000000	0.136735	246.934685	122
i7	105.000000	0.136735	184.997211	107
i7	105.000000	0.136735	246.934685	127
i7	105.272789	0.273016	246.934685	127
i7	105.272789	0.409297	184.997211	119
i7	105.272789	0.409297	246.934685	127
i7	105.681859	0.136735	164.804481	116
i7	105.681859	0.136735	219.00    	116
i7	105.681859	0.136735	246.934685	127
i7	105.954649	0.136508	164.804481	107
i7	105.954649	0.136508	219.00    	107
i7	105.954649	0.136508	246.934685	127
i7	106.227438	0.272789	164.804481	119
i7	106.227438	0.272789	219.00    	119
i7	106.227438	0.272789	246.934685	127
i7	106.500000	0.136735	164.804481	95
i7	106.500000	0.136735	246.934685	99
i7	106.636508	0.136508	246.934685	127
i7	106.636508	0.272789	164.804481	122
i7	106.636508	0.272789	219.00    	122
i7	106.772789	0.136508	246.934685	106
i7	106.909070	0.136735	164.804481	127
i7	106.909070	0.136735	207.646491	127
i7	106.909070	0.136735	246.934685	127
i7	107.181859	0.136735	164.804481	95
i7	107.181859	0.136735	207.646491	95
i7	107.181859	0.136735	246.934685	127
i7	107.454649	0.273016	246.934685	127
i7	107.454649	0.341043	164.804481	122
i7	107.454649	0.341043	207.646491	122
i7	107.727438	0.136508	246.934685	84
i7	107.863719	0.136508	219.00    	122
i7	107.863719	0.136508	164.804481	122
i7	107.863719	0.136508	246.934685	127
i7	108.136508	0.136508	219.00    	95
i7	108.136508	0.136508	164.804481	95
i7	108.136508	0.136508	246.934685	127
i7	108.409070	0.273016	164.804481	119
i7	108.409070	0.273016	219.00    	119
i7	108.409070	0.273016	246.934685	127
i7	108.681859	0.136735	164.804481	101
i7	108.681859	0.136735	246.934685	99
i7	108.818367	0.136508	246.934685	127
i7	108.818367	0.272789	164.804481	122
i7	108.818367	0.272789	219.00    	122
i7	108.954649	0.136508	246.934685	94
i7	109.090930	0.136735	246.934685	127
i7	109.090930	0.136735	184.997211	125
i7	109.090930	0.136735	246.934685	127
i7	109.363719	0.136508	246.934685	122
i7	109.363719	0.136508	184.997211	107
i7	109.363719	0.136508	246.934685	127
i7	109.636508	0.272789	246.934685	127
i7	109.636508	0.409297	184.997211	119
i7	109.636508	0.409297	246.934685	127
i7	110.045578	0.136508	164.804481	116
i7	110.045578	0.136508	219.00    	116
i7	110.045578	0.136508	246.934685	127
i7	110.318367	0.136508	164.804481	107
i7	110.318367	0.136508	219.00    	107
i7	110.318367	0.136508	246.934685	127
i7	110.590930	0.273016	164.804481	119
i7	110.590930	0.273016	219.00    	119
i7	110.590930	0.273016	246.934685	127
i7	110.863719	0.136508	164.804481	95
i7	110.863719	0.136508	246.934685	99
i7	111.000000	0.136735	246.934685	127
i7	111.000000	0.273016	164.804481	122
i7	111.000000	0.273016	219.00    	122
i7	111.136508	0.136508	246.934685	106
i7	111.272789	0.136735	155.563491	127
i7	111.272789	0.136735	207.646491	127
i7	111.272789	0.136735	246.934685	127
i7	111.545578	0.136508	155.563491	95
i7	111.545578	0.136508	207.646491	95
i7	111.545578	0.136508	246.934685	127
i7	111.818367	0.272789	246.934685	127
i7	111.818367	0.341043	155.563491	122
i7	111.818367	0.341043	207.646491	122
i7	112.090930	0.136735	246.934685	84
i7	112.227438	0.136508	207.646491	122
i7	112.227438	0.136508	155.563491	122
i7	112.227438	0.136508	246.934685	127
i7	112.500000	0.136735	207.646491	95
i7	112.500000	0.136735	155.563491	95
i7	112.500000	0.136735	246.934685	127
i7	112.772789	0.273016	207.646491	119
i7	112.772789	0.273016	155.563491	119
i7	112.772789	0.273016	246.934685	127
i7	113.045578	0.136508	155.563491	101
i7	113.045578	0.136508	246.934685	99
i7	113.181859	0.136735	246.934685	127
i7	113.181859	0.273016	155.563491	122
i7	113.181859	0.273016	207.646491	122
i7	113.318367	0.136508	246.934685	94
i7	113.454649	0.136508	164.804481	107
i7	113.454649	0.136508	219.00    	127
i7	113.454649	0.136508	219.00    	127
i7	113.454649	0.136508	184.997211	125
i7	113.727438	0.136508	164.804481	107
i7	113.727438	0.136508	219.00    	127
i7	113.727438	0.136508	219.00    	122
i7	113.727438	0.136508	184.997211	107
i7	114.000000	0.273016	164.804481	119
i7	114.000000	0.273016	219.00    	127
i7	114.000000	0.409524	219.00    	127
i7	114.000000	0.409524	184.997211	119
i7	114.409297	0.136508	219.00    	127
i7	114.409297	0.136508	184.997211	116
i7	114.409297	0.136508	164.804481	116
i7	114.681859	0.136735	219.00    	127
i7	114.681859	0.136735	184.997211	107
i7	114.681859	0.136735	164.804481	107
i7	114.954649	0.273016	219.00    	127
i7	114.954649	0.273016	184.997211	119
i7	114.954649	0.273016	164.804481	119
i7	115.227438	0.136508	219.00    	99
i7	115.227438	0.136508	164.804481	95
i7	115.363719	0.136508	219.00    	127
i7	115.363719	0.273016	164.804481	122
i7	115.363719	0.273016	184.997211	122
i7	115.500000	0.136735	219.00    	106
i7	115.636508	0.136508	155.563491	127
i7	115.636508	0.136508	207.646491	127
i7	115.636508	0.136508	246.934685	127
i7	115.909297	0.136508	155.563491	95
i7	115.909297	0.136508	207.646491	95
i7	115.909297	0.136508	246.934685	127
i7	116.181859	0.273016	246.934685	127
i7	116.181859	0.341270	155.563491	122
i7	116.181859	0.341270	207.646491	122
i7	116.454649	0.136508	246.934685	84
i7	116.590930	0.136735	207.646491	122
i7	116.590930	0.136735	155.563491	122
i7	116.590930	0.136735	246.934685	127
i7	116.863719	0.136508	207.646491	95
i7	116.863719	0.136508	155.563491	95
i7	116.863719	0.136508	246.934685	127
i7	117.136508	0.273016	207.646491	119
i7	117.136508	0.273016	155.563491	119
i7	117.136508	0.273016	246.934685	127
i7	117.409297	0.136508	155.563491	101
i7	117.409297	0.136508	246.934685	99
i7	117.545578	0.136508	246.934685	127
i7	117.545578	0.273016	155.563491	122
i7	117.545578	0.273016	207.646491	122
i7	117.681859	0.136735	246.934685	94
i7	117.818367	0.136508	164.804481	107
i7	117.818367	0.136508	219.00    	127
i7	117.818367	0.136508	219.00    	127
i7	117.818367	0.136508	184.997211	125
i7	118.090930	0.136735	164.804481	107
i7	118.090930	0.136735	219.00    	127
i7	118.090930	0.136735	219.00    	122
i7	118.090930	0.136735	184.997211	107
i7	118.363719	0.273016	164.804481	119
i7	118.363719	0.273016	219.00    	127
i7	118.363719	0.409297	219.00    	127
i7	118.363719	0.409297	184.997211	119
i7	118.772789	0.136735	219.00    	127
i7	118.772789	0.136735	184.997211	116
i7	118.772789	0.136735	164.804481	116
i7	119.045578	0.136508	219.00    	127
i7	119.045578	0.136508	184.997211	107
i7	119.045578	0.136508	164.804481	107
i7	119.318367	0.272789	219.00    	127
i7	119.318367	0.272789	184.997211	119
i7	119.318367	0.272789	164.804481	119
i7	119.590930	0.136735	219.00    	99
i7	119.590930	0.136735	164.804481	95
i7	119.727438	0.136508	219.00    	127
i7	119.727438	0.272789	164.804481	122
i7	119.727438	0.272789	184.997211	122
i7	119.863719	0.136508	219.00    	106
i7	120.000000	0.136735	155.563491	127
i7	120.000000	0.136735	207.646491	127
i7	120.000000	0.136735	246.934685	127
i7	120.272789	0.136735	155.563491	95
i7	120.272789	0.136735	207.646491	95
i7	120.272789	0.136735	246.934685	127
i7	120.545578	0.273016	246.934685	127
i7	120.545578	0.341043	155.563491	122
i7	120.545578	0.341043	207.646491	122
i7	120.818367	0.136508	246.934685	84
i7	120.954649	0.136508	207.646491	122
i7	120.954649	0.136508	155.563491	122
i7	120.954649	0.136508	246.934685	127
i7	121.227438	0.136508	207.646491	95
i7	121.227438	0.136508	155.563491	95
i7	121.227438	0.136508	246.934685	127
i7	121.500000	0.273016	207.646491	119
i7	121.500000	0.273016	155.563491	119
i7	121.500000	0.273016	246.934685	127
i7	121.772789	0.136735	155.563491	101
i7	121.772789	0.136735	246.934685	99
i7	121.909297	0.136508	246.934685	127
i7	121.909297	0.272789	155.563491	122
i7	121.909297	0.272789	207.646491	122
i7	122.045578	0.136508	246.934685	94
i7	122.181859	0.136735	164.804481	107
i7	122.181859	0.136735	219.00    	127
i7	122.181859	0.136735	219.00    	127
i7	122.181859	0.136735	184.997211	125
i7	122.454649	0.136508	164.804481	107
i7	122.454649	0.136508	219.00    	127
i7	122.454649	0.136508	219.00    	122
i7	122.454649	0.136508	184.997211	107
i7	122.727438	0.272789	164.804481	119
i7	122.727438	0.272789	219.00    	127
i7	122.727438	0.409297	219.00    	127
i7	122.727438	0.409297	184.997211	119
i7	123.136508	0.136508	219.00    	127
i7	123.136508	0.136508	184.997211	116
i7	123.136508	0.136508	164.804481	116
i7	123.409297	0.136508	219.00    	127
i7	123.409297	0.136508	184.997211	107
i7	123.409297	0.136508	164.804481	107
i7	123.681859	0.273016	219.00    	127
i7	123.681859	0.273016	184.997211	119
i7	123.681859	0.273016	164.804481	119
i7	123.954649	0.136508	219.00    	99
i7	123.954649	0.136508	164.804481	95
i7	124.090930	0.136735	219.00    	127
i7	124.090930	0.273016	164.804481	122
i7	124.090930	0.273016	184.997211	122
i7	124.227438	0.136508	219.00    	106
i7	124.363719	0.136508	164.804481	127
i7	124.363719	0.136508	207.646491	127
i7	124.363719	0.136508	246.934685	127
i7	124.636508	0.136508	164.804481	95
i7	124.636508	0.136508	207.646491	95
i7	124.636508	0.136508	246.934685	127
i7	124.909297	0.272789	246.934685	127
i7	124.909297	0.341043	164.804481	122
i7	124.909297	0.341043	207.646491	122
i7	125.181859	0.136735	246.90  	84
i7	125.318367	0.136508	219.00    	122
i7	125.318367	0.136508	164.80  	122
i7	125.318367	0.136508	246.90  	127
i7	125.590930	0.136735	219.00    	95
i7	125.590930	0.136735	164.804481	95
i7	125.590930	0.136735	246.934685	127
i7	125.863719	0.273016	164.804481	119
i7	125.863719	0.273016	220.00     119
i7	125.863719	0.273016	246.934685	127
i7	126.136508	0.136508	164.804481	101
i7	126.136508	0.136508	246.934685	99
i7	126.272789	0.136735	246.934685	127
i7	126.272789	0.273016	164.804481	122
i7	126.272789	0.273016	220.00     122
i7	126.409297	0.136508	246.934685	94
i7	126.545578	0.136508	246.934685	127
i7	126.545578	0.136508	184.997211	125
i7	126.545578	0.136508	246.934685	127
i7	126.818367	0.136508	246.934685	122
i7	126.818367	0.136508	184.997211	107
i7	126.818367	0.136508	246.934685	127
i7	127.090930	0.273016	246.934685	127
i7	127.090930	0.409297	184.997211	119
i7	127.090930	0.409297	246.934685	127
i7	127.500000	0.136735	164.804481	116
i7	127.500000	0.136735	220.00     116
i7	127.500000	0.136735	246.934685	127
i7	127.772789	0.136735	164.8   	107
i7	127.772789	0.136735	220.00     107
i7	127.772789	0.136735	246.93  	127
i7	128.045578	0.27    	164.80  	119
i7	128.045578	0.27    	220.00  	119
i7	128.045578	0.27    	246.90   	127
i7	128.318367	0.14    	164.80   	95
i7	128.318367	0.14	    246.93  	99
i7	128.454649	0.14    	246.93  	127
i7	128.454649	0.273016	164.80  	122
i7	128.454649	0.273016	220.00  	122
i7	128.590930	0.136735	246.934685	106
i7	128.727438	0.136508	164.804481	127
i7	128.727438	0.136508	207.646491	127
i7	128.727438	0.136508	246.934685	127
i7	129.000000	0.136735	164.804481	95
i7	129.000000	0.136735	207.646491	95
i7	129.000000	0.136735	246.934685	127
i7	129.272789	0.273016	246.934685	127
i7	129.272789	0.341270	164.804481	122
i7	129.272789	0.341270	207.646491	122
i7	129.545578	0.136508	246.934685	84
i7	129.681859	0.136735	219.999999	122
i7	129.681859	0.136735	164.804481	122
i7	129.681859	0.136735	246.934685	127
i7	129.954649	0.136508	219.999999	95
i7	129.954649	0.136508	164.80  	99
i7	129.954649	0.136508	246.90  	127
i7	130.227438	0.272789	164.80	    119
i7	130.227438	0.272789	220.00  	119
i7	130.227438	0.272789	246.934685	127
i7	130.500000	0.136735	164.804481	101
i7	130.500000	0.136735	246.934685	99
i7	130.636508	0.136508	246.934685	127
i7	130.636508	0.273016	164.804481	122
i7	130.636508	0.273016	219.999999	122
i7	130.772789	0.136735	246.934685	94
i7	130.909297	0.136508	246.934685	127
i7	130.909297	0.136508	185.0   	125
i7	130.909297	0.136508	246.934685	127
i7	131.181859	0.136735	246.934685	122
i7	131.181859	0.136735	185.0   	107
i7	131.181859	0.136735	246.934685	127
i7	131.454649	0.273016	246.934685	127
i7	131.454649	0.409297	184.997211	119
i7	131.454649	0.409297	246.934685	127
i7	131.863719	0.136508	164.804481	116
i7	131.863719	0.136508	219.999999	116
i7	131.863719	0.136508	246.934685	127
i7	132.136508	0.136508	164.804481	107
i7	132.136508	0.136508	219.999999	107
i7	132.136508	0.136508	246.934685	127
i7	132.409297	0.272789	164.804481	119
i7	132.409297	0.272789	219.999999	119
i7	132.409297	0.272789	246.934685	127
i7	132.681859	0.136735	164.804481	95
i7	132.681859	0.136735	246.934685	99
i7	132.818367	0.136508	246.934685	127
i7	132.818367	0.272789	164.804481	122
i7	132.818367	0.272789	219.999999	122
i7	132.954649	0.136508	246.934685	106
i7	150.545578	0.136508	164.804481	127
i7	150.545578	0.136508	207.646491	127
i7	150.545578	0.136508	246.934685	127
i7	150.818367	0.136508	164.804481	95
i7	150.818367	0.136508	207.646491	95
i7	150.818367	0.136508	246.934685	127
i7	151.090930	0.273016	246.934685	127
i7	151.090930	0.341270	164.804481	122
i7	151.090930	0.341270	207.646491	122
i7	151.363719	0.136735	246.934685	84
i7	151.500227	0.136508	219.999999	122
i7	151.500227	0.136508	164.804481	122
i7	151.500227	0.136508	246.934685	127
i7	151.772789	0.136735	219.999999	95
i7	151.772789	0.136735	164.804481	95
i7	151.772789	0.136735	246.934685	127
i7	152.045578	0.273016	164.804481	119
i7	152.045578	0.273016	219.999999	119
i7	152.045578	0.273016	246.934685	127
i7	152.318367	0.136508	164.804481	101
i7	152.318367	0.136508	246.934685	99
i7	152.454649	0.136508	246.934685	127
i7	152.454649	0.273016	164.804481	122
i7	152.454649	0.273016	219.999999	122
i7	152.590930	0.136735	246.934685	94
i7	152.727438	0.136508	246.934685	127
i7	152.727438	0.136508	184.997211	125
i7	152.727438	0.136508	246.934685	127
i7	153.000227	0.136508	246.934685	122
i7	153.000227	0.136508	184.997211	107
i7	153.000227	0.136508	246.934685	127
i7	153.272789	0.273016	246.934685	127
i7	153.272789	0.409297	184.997211	119
i7	153.272789	0.409297	246.934685	127
i7	153.681859	0.136735	164.804481	116
i7	153.681859	0.136735	219.999999	116
i7	153.681859	0.136735	246.934685	127
i7	153.954649	0.136508	164.804481	107
i7	153.954649	0.136508	219.999999	107
i7	153.954649	0.136508	246.934685	127
i7	154.227438	0.273016	164.804481	119
i7	154.227438	0.273016	219.999999	119
i7	154.227438	0.273016	246.934685	127
i7	154.500227	0.136508	164.804481	95
i7	154.500227	0.136508	246.934685	99
i7	154.636508	0.136508	246.934685	127
i7	154.636508	0.273016	164.804481	122
i7	154.636508	0.273016	219.999999	122
i7	154.772789	0.136735	246.934685	106
i7	154.909297	0.136508	164.804481	127
i7	154.909297	0.136508	207.646491	127
i7	154.909297	0.136508	246.934685	127
i7	155.181859	0.136735	164.804481	95
i7	155.181859	0.136735	207.646491	95
i7	155.181859	0.136735	246.934685	127
i7	155.454649	0.273016	246.934685	127
i7	155.454649	0.341270	164.804481	122
i7	155.454649	0.341270	207.646491	122
i7	155.727438	0.136508	246.934685	84
i7	155.863719	0.136735	219.999999	122
i7	155.863719	0.136735	164.804481	122
i7	155.863719	0.136735	246.934685	127
i7	156.136508	0.136508	219.999999	95
i7	156.136508	0.136508	164.804481	95
i7	156.136508	0.136508	246.934685	127
i7	156.409297	0.272789	164.804481	119
i7	156.409297	0.272789	219.999999	119
i7	156.409297	0.272789	246.934685	127
i7	156.681859	0.136735	164.804481	101
i7	156.681859	0.136735	246.934685	99
i7	156.818367	0.136508	246.934685	127
i7	156.818367	0.272789	164.804481	122
i7	156.818367	0.272789	219.999999	122
i7	156.954649	0.136508	246.934685	94
i7	157.090930	0.136735	246.934685	127
i7	157.090930	0.136735	184.997211	125
i7	157.090930	0.136735	246.934685	127
i7	157.363719	0.136735	246.934685	122
i7	157.363719	0.136735	184.997211	107
i7	157.363719	0.136735	246.934685	127
i7	157.636508	0.273016	246.934685	127
i7	157.636508	0.409297	184.997211	119
i7	157.636508	0.409297	246.934685	127
i7	158.045578	0.136508	164.804481	116
i7	158.045578	0.136508	219.999999	116
i7	158.045578	0.136508	246.934685	127
i7	158.318367	0.136508	164.804481	107
i7	158.318367	0.136508	219.999999	107
i7	158.318367	0.136508	246.934685	127
i7	158.590930	0.273016	164.804481	119
i7	158.590930	0.273016	219.999999	119
i7	158.590930	0.273016	246.934685	127
i7	158.863719	0.136735	164.804481	95
i7	158.863719	0.136735	246.934685	99
i7	159.000227	0.136508	246.934685	127
i7	159.000227	0.272789	164.804481	122
i7	159.000227	0.272789	219.999999	122
i7	159.136508	0.136508	246.934685	106
i7	159.272789	0.136735	164.804481	127
i7	159.272789	0.136735	207.646491	127
i7	159.272789	0.136735	246.934685	127
i7	159.545578	0.136508	164.804481	95
i7	159.545578	0.136508	207.646491	95
i7	159.545578	0.136508	246.934685	127
i7	159.818367	0.272789	246.934685	127
i7	159.818367	0.341043	164.804481	122
i7	159.818367	0.341043	207.646491	122
i7	160.090930	0.136735	246.934685	84
i7	160.227438	0.136508	219.999999	122
i7	160.227438	0.136508	164.804481	122
i7	160.227438	0.136508	246.934685	127
i7	160.500227	0.136508	219.999999	95
i7	160.500227	0.136508	164.804481	95
i7	160.500227	0.136508	246.934685	127
i7	160.772789	0.273016	164.804481	119
i7	160.772789	0.273016	219.999999	119
i7	160.772789	0.273016	246.934685	127
i7	161.045578	0.136508	164.804481	101
i7	161.045578	0.136508	246.934685	99
i7	161.181859	0.136735	246.934685	127
i7	161.181859	0.273016	164.804481	122
i7	161.181859	0.273016	219.999999	122
i7	161.318367	0.136508	246.934685	94
i7	161.454649	0.136735	246.934685	127
i7	161.454649	0.136735	184.997211	125
i7	161.454649	0.136735	246.934685	127
i7	161.727438	0.136508	246.934685	122
i7	161.727438	0.136508	184.997211	107
i7	161.727438	0.136508	246.934685	127
i7	162.000227	0.272789	246.934685	127
i7	162.000227	0.409297	184.997211	119
i7	162.000227	0.409297	246.934685	127
i7	162.409297	0.136508	164.804481	116
i7	162.409297	0.136508	219.999999	116
i7	162.409297	0.136508	246.934685	127
i7	162.681859	0.136735	164.804481	107
i7	162.681859	0.136735	219.999999	107
i7	162.681859	0.136735	246.934685	127
i7	162.954649	0.273016	164.804481	119
i7	162.954649	0.273016	219.999999	119
i7	162.954649	0.273016	246.934685	127
i7	163.227438	0.136508	164.804481	95
i7	163.227438	0.136508	246.934685	99
i7	163.363719	0.136735	246.934685	127
i7	163.363719	0.273016	164.804481	122
i7	163.363719	0.273016	219.999999	122
i7	163.500227	0.136508	246.934685	106
i7	163.636508	0.136508	164.804481	127
i7	163.636508	0.136508	207.646491	127
i7	163.636508	0.136508	246.934685	127
i7	163.909297	0.136508	164.804481	95
i7	163.909297	0.136508	207.646491	95
i7	163.909297	0.136508	246.934685	127
i7	164.181859	0.273016	246.934685	127
i7	164.181859	0.341270	164.804481	122
i7	164.181859	0.341270	207.646491	122
i7	164.454649	0.136735	246.934685	84
i7	164.591156	0.136508	219.999999	122
i7	164.591156	0.136508	164.804481	122
i7	164.591156	0.136508	246.934685	127
i7	164.863719	0.136735	219.999999	95
i7	164.863719	0.136735	164.804481	95
i7	164.863719	0.136735	246.934685	127
i7	165.136508	0.273016	164.804481	119
i7	165.136508	0.273016	219.999999	119
i7	165.136508	0.273016	246.934685	127
i7	165.409297	0.136508	164.804481	101
i7	165.409297	0.136508	246.934685	99
i7	165.545578	0.136508	246.934685	127
i7	165.545578	0.273016	164.804481	122
i7	165.545578	0.273016	219.999999	122
i7	165.681859	0.136735	246.934685	94
i7	165.818367	0.136508	246.934685	127
i7	165.818367	0.136508	184.997211	125
i7	165.818367	0.136508	246.934685	127
i7	166.091156	0.136508	246.934685	122
i7	166.091156	0.136508	184.997211	107
i7	166.091156	0.136508	246.934685	127
i7	166.363719	0.273016	246.934685	127
i7	166.363719	0.409297	184.997211	119
i7	166.363719	0.409297	246.934685	127
i7	166.772789	0.136735	164.804481	116
i7	166.772789	0.136735	219.999999	116
i7	166.772789	0.136735	246.934685	127
i7	167.045578	0.136508	164.804481	107
i7	167.045578	0.136508	219.999999	107
i7	167.045578	0.136508	246.934685	127
i7	167.318367	0.273016	164.804481	119
i7	167.318367	0.273016	219.999999	119
i7	167.318367	0.273016	246.934685	127
i7	167.591156	0.136508	164.804481	95
i7	167.591156	0.136508	246.934685	99
i7	167.727438	0.136508	246.934685	127
i7	167.727438	0.273016	164.804481	122
i7	167.727438	0.273016	219.999999	122
i7	167.863719	0.136735	246.934685	106
i7	168.000227	0.136508	155.563491	127
i7	168.000227	0.136508	207.646491	127
i7	168.000227	0.136508	246.934685	127
i7	168.272789	0.136735	155.563491	95
i7	168.272789	0.136735	207.646491	95
i7	168.272789	0.136735	246.934685	127
i7	168.545578	0.273016	246.934685	127
i7	168.545578	0.341043	155.563491	122
i7	168.545578	0.341043	207.646491	122
i7	168.818367	0.136508	246.934685	84
i7	168.954649	0.136735	207.646491	122
i7	168.954649	0.136735	155.563491	122
i7	168.954649	0.136735	246.934685	127
i7	169.227438	0.136508	207.646491	95
i7	169.227438	0.136508	155.563491	95
i7	169.227438	0.136508	246.934685	127
i7	169.500227	0.272789	207.646491	119
i7	169.500227	0.272789	155.563491	119
i7	169.500227	0.272789	246.934685	127
i7	169.772789	0.136735	155.563491	101
i7	169.772789	0.136735	246.934685	99
i7	169.909297	0.136508	246.934685	127
i7	169.909297	0.272789	155.563491	122
i7	169.909297	0.272789	207.646491	122
i7	170.045578	0.136508	246.934685	94
i7	170.181859	0.136735	164.804481	107
i7	170.181859	0.136735	219.999999	127
i7	170.181859	0.136735	219.999999	127
i7	170.181859	0.136735	184.997211	125
i7	170.454649	0.136735	164.804481	107
i7	170.454649	0.136735	219.999999	127
i7	170.454649	0.136735	219.999999	122
i7	170.454649	0.136735	184.997211	107
i7	170.727438	0.273016	164.804481	119
i7	170.727438	0.273016	219.999999	127
i7	170.727438	0.409297	219.999999	127
i7	170.727438	0.409297	184.997211	119
i7	171.136508	0.136508	219.999999	127
i7	171.136508	0.136508	184.997211	116
i7	171.136508	0.136508	164.804481	116
i7	171.409297	0.136508	219.999999	127
i7	171.409297	0.136508	184.997211	107
i7	171.409297	0.136508	164.804481	107
i7	171.681859	0.273016	219.999999	127
i7	171.681859	0.273016	184.997211	119
i7	171.681859	0.273016	164.804481	119
i7	171.954649	0.136735	219.999999	99
i7	171.954649	0.136735	164.804481	95
i7	172.091156	0.136508	219.999999	127
i7	172.091156	0.272789	164.804481	122
i7	172.091156	0.272789	184.997211	122
i7	172.227438	0.136508	219.999999	106
i7	172.363719	0.136735	155.563491	127
i7	172.363719	0.136735	207.646491	127
i7	172.363719	0.136735	246.934685	127
i7	172.636508	0.136508	155.563491	95
i7	172.636508	0.136508	207.646491	95
i7	172.636508	0.136508	246.934685	127
i7	172.9	0.272789	246.934685	127
i7	172.9	0.341043	155.563491	122
i7	172.9	0.341043	207.646491	122
i7	173.181859	0.136735	246.934685	84
i7	173.318367	0.136508	207.646491	122
i7	173.318367	0.136508	155.563491	122
i7	173.318367	0.136508	246.934685	127
i7	173.591156	0.136508	207.646491	95
i7	173.591156	0.136508	155.563491	95
i7	173.591156	0.136508	246.934685	127
i7	173.863719	0.273016	207.646491	119
i7	173.863719	0.273016	155.563491	119
i7	173.863719	0.273016	246.934685	127
i7	174.136508	0.136508	155.563491	101
i7	174.136508	0.136508	246.934685	99
i7	174.272789	0.136735	246.934685	127
i7	174.272789	0.273016	155.563491	122
i7	174.272789	0.273016	207.646491	122
i7	174.409297	0.136508	246.934685	94
i7	174.545578	0.136508	164.804481	107
i7	174.545578	0.136508	219.999999	127
i7	174.545578	0.136508	219.999999	127
i7	174.545578	0.136508	184.997211	125
i7	174.818367	0.136508	164.804481	107
i7	174.818367	0.136508	219.999999	127
i7	174.818367	0.136508	219.999999	122
i7	174.818367	0.136508	184.997211	107
i7	175.091156	0.272789	164.804481	119
i7	175.091156	0.272789	219.999999	127
i7	175.091156	0.409297	219.999999	127
i7	175.091156	0.409297	184.997211	119
i7	175.5	0.136508	219.999999	127
i7	175.5	0.136508	184.997211	116
i7	175.5	0.136508	164.804481	116
i7	175.772789	0.136735	219.999999	127
i7	175.772789	0.136735	184.997211	107
i7	175.772789	0.136735	164.804481	107
i7	176.045578	0.273016	219.999999	127
i7	176.045578	0.273016	184.997211	119
i7	176.045578	0.273016	164.804481	119
i7	176.318367	0.136508	219.999999	99
i7	176.318367	0.136508	164.804481	95
i7	176.454649	0.136735	219.999999	127
i7	176.454649	0.273016	164.804481	122
i7	176.454649	0.273016	184.997211	122
i7	176.591156	0.136508	219.999999	106
i7	176.727438	0.136508	155.563491	127
i7	176.727438	0.136508	207.646491	127
i7	176.727438	0.136508	246.934685	127
i7	177.000227	0.136508	155.563491	95
i7	177.000227	0.136508	207.646491	95
i7	177.000227	0.136508	246.934685	127
i7	177.272789	0.273016	246.934685	127
i7	177.272789	0.341270	155.563491	122
i7	177.272789	0.341270	207.646491	122
i7	177.545578	0.136508	246.934685	84
i7	177.681859	0.136735	207.646491	122
i7	177.681859	0.136735	155.563491	122
i7	177.681859	0.136735	246.934685	127
i7	177.954649	0.136735	207.646491	95
i7	177.954649	0.136735	155.563491	95
i7	177.954649	0.136735	246.934685	127
i7	178.227438	0.273016	207.646491	119
i7	178.227438	0.273016	155.563491	119
i7	178.227438	0.273016	246.934685	127
i7	178.500227	0.136508	155.563491	101
i7	178.500227	0.136508	246.934685	99
i7	178.636508	0.136508	246.934685	127
i7	178.636508	0.273016	155.563491	122
i7	178.636508	0.273016	207.646491	122
i7	178.772789	0.136735	246.934685	94
i7	178.909297	0.136508	164.804481	107
i7	178.909297	0.136508	219.999999	127
i7	178.909297	0.136508	219.999999	127
i7	178.909297	0.136508	184.997211	125
i7	179.181859	0.136735	164.804481	107
i7	179.181859	0.136735	219.999999	127
i7	179.181859	0.136735	219.999999	122
i7	179.181859	0.136735	184.997211	107
i7	179.454649	0.273016	164.804481	119
i7	179.454649	0.273016	219.999999	127
i7	179.454649	0.409297	219.999999	127
i7	179.454649	0.409297	184.997211	119
i7	179.863719	0.136735	219.999999	127
i7	179.863719	0.136735	184.997211	116
i7	179.863719	0.136735	164.804481	116
i7	180.136508	0.136508	219.999999	127
i7	180.136508	0.136508	184.997211	107
i7	180.136508	0.136508	164.804481	107
i7	180.409297	0.272789	219.999999	127
i7	180.409297	0.272789	184.997211	119
i7	180.409297	0.272789	164.804481	119
i7	180.681859	0.136735	219.999999	99
i7	180.681859	0.136735	164.804481	95
i7	180.818367	0.136508	219.999999	127
i7	180.818367	0.273016	164.804481	122
i7	180.818367	0.273016	184.997211	122
i7	180.954649	0.136735	219.999999	106
i7	181.091156	0.136508	164.804481	127
i7	181.091156	0.136508	207.646491	127
i7	181.091156	0.136508	246.934685	127
i7	181.363719	0.136735	164.804481	95
i7	181.363719	0.136735	207.646491	95
i7	181.363719	0.136735	246.934685	127
i7	181.636508	0.273016	246.934685	127
i7	181.636508	0.341043	164.804481	122
i7	181.636508	0.341043	207.646491	122
i7	181.909297	0.136508	246.934685	84
i7	182.045578	0.136508	219.999999	122
i7	182.045578	0.136508	164.804481	122
i7	182.045578	0.136508	246.934685	127
i7	182.318367	0.136508	219.999999	95
i7	182.318367	0.136508	164.804481	95
i7	182.318367	0.136508	246.934685	127
i7	182.591156	0.272789	164.804481	119
i7	182.591156	0.272789	219.999999	119
i7	182.591156	0.272789	246.934685	127
i7	182.863719	0.136735	164.804481	101
i7	182.863719	0.136735	246.934685	99
i7	183.000227	0.136508	246.934685	127
i7	183.000227	0.272789	164.804481	122
i7	183.000227	0.272789	219.999999	122
i7	183.136508	0.136508	246.934685	94
i7	183.272789	0.136735	246.934685	127
i7	183.272789	0.136735	184.997211	125
i7	183.272789	0.136735	246.934685	127
i7	183.545578	0.136508	246.934685	122
i7	183.545578	0.136508	184.997211	107
i7	183.545578	0.136508	246.934685	127
i7	183.818367	0.273016	246.934685	127
i7	183.818367	0.409297	184.997211	119
i7	183.818367	0.409297	246.934685	127
i7	184.227438	0.136508	164.804481	116
i7	184.227438	0.136508	219.999999	116
i7	184.227438	0.136508	246.934685	127
i7	184.500227	0.136508	164.804481	107
i7	184.500227	0.136508	219.999999	107
i7	184.500227	0.136508	246.934685	127
i7	184.772789	0.273016	164.804481	119
i7	184.772789	0.273016	219.999999	119
i7	184.772789	0.273016	246.934685	127
i7	185.045578	0.136508	164.804481	95
i7	185.045578	0.136508	246.934685	99
i7	185.181859	0.136735	246.934685	127
i7	185.181859	0.273016	164.804481	122
i7	185.181859	0.273016	219.999999	122
i7	185.318367	0.136508	246.934685	106
i7	185.454649	0.136735	164.804481	127
i7	185.454649	0.136735	207.646491	127
i7	185.454649	0.136735	246.934685	127
i7	185.727438	0.136508	164.804481	95
i7	185.727438	0.136508	207.646491	95
i7	185.727438	0.136508	246.934685	127
i7	186.000227	0.272789	246.934685	127
i7	186.000227	0.341043	164.804481	122
i7	186.000227	0.341043	207.646491	122
i7	186.272789	0.136735	246.934685	84
i7	186.409297	0.136508	219.999999	122
i7	186.409297	0.136508	164.804481	122
i7	186.409297	0.136508	246.934685	127
i7	186.682086	0.136508	219.999999	95
i7	186.682086	0.136508	164.804481	95
i7	186.682086	0.136508	246.934685	127
i7	186.954649	0.273016	164.804481	119
i7	186.954649	0.273016	219.999999	119
i7	186.954649	0.273016	246.934685	127
i7	187.227438	0.136508	164.804481	101
i7	187.227438	0.136508	246.934685	99
i7	187.363719	0.136735	246.934685	127
i7	187.363719	0.273016	164.804481	122
i7	187.363719	0.273016	219.999999	122
i7	187.500227	0.136508	246.934685	94
i7	187.636508	0.136508	246.934685	127
i7	187.636508	0.136508	184.997211	125
i7	187.636508	0.136508	246.934685	127
i7	187.909297	0.136508	246.934685	122
i7	187.909297	0.136508	184.997211	107
i7	187.909297	0.136508	246.934685	127
i7	188.182086	0.272789	246.934685	127
i7	188.182086	0.409297	184.997211	119
i7	188.182086	0.409297	246.934685	127
i7	188.591156	0.136508	164.804481	116
i7	188.591156	0.136508	219.999999	116
i7	188.591156	0.136508	246.934685	127
i7	188.863719	0.136735	164.804481	107
i7	188.863719	0.136735	219.999999	107
i7	188.863719	0.136735	246.934685	127
i7	189.136508	0.273016	164.804481	119
i7	189.136508	0.273016	219.999999	119
i7	189.136508	0.273016	246.934685	127
i7	189.409297	0.136508	164.804481	95
i7	189.409297	0.136508	246.934685	99
i7	189.545578	0.136735	246.934685	127
i7	189.545578	0.273016	164.804481	122
i7	189.545578	0.273016	219.999999	122
i7	189.682086	0.136508	246.934685	106
i7	189.818367	0.136508	164.804481	127
i7	189.818367	0.136508	207.646491	127
i7	189.818367	0.136508	246.934685	127
i7	190.091156	0.136508	164.804481	95
i7	190.091156	0.136508	207.646491	95
i7	190.091156	0.136508	246.934685	127
i7	190.363719	0.273016	246.934685	127
i7	190.363719	0.341270	164.804481	122
i7	190.363719	0.341270	207.646491	122
i7	190.636508	0.136508	246.934685	84
i7	190.772789	0.136735	219.999999	122
i7	190.772789	0.136735	164.804481	122
i7	190.772789	0.136735	246.934685	127
i7	191.045578	0.136735	219.999999	95
i7	191.045578	0.136735	164.804481	95
i7	191.045578	0.136735	246.934685	127
i7	191.318367	0.273016	164.804481	119
i7	191.318367	0.273016	219.999999	119
i7	191.318367	0.273016	246.934685	127
i7	191.591156	0.136508	164.804481	101
i7	191.591156	0.136508	246.934685	99
i7	191.727438	0.136508	246.934685	127
i7	191.727438	0.273016	164.804481	122
i7	191.727438	0.273016	219.999999	122
i7	191.863719	0.136735	246.934685	94
i7	192.000227	0.136508	246.934685	127
i7	192.000227	0.136508	184.997211	125
i7	192.000227	0.136508	246.934685	127
i7	192.272789	0.136735	246.934685	122
i7	192.272789	0.136735	184.997211	107
i7	192.272789	0.136735	246.934685	127
i7	192.545578	0.273016	246.934685	127
i7	192.545578	0.409297	184.997211	119
i7	192.545578	0.409297	246.934685	127
i7	192.954649	0.136735	164.804481	116
i7	192.954649	0.136735	219.999999	116
i7	192.954649	0.136735	246.934685	127
i7	193.227438	0.136508	164.804481	107
i7	193.227438	0.136508	219.999999	107
i7	193.227438	0.136508	246.934685	127
i7	193.500227	0.272789	164.804481	119
i7	193.500227	0.272789	219.999999	119
i7	193.500227	0.272789	246.934685	127
i7	193.772789	0.136735	164.804481	95
i7	193.772789	0.136735	246.934685	99
i7	193.909297	0.136508	246.934685	127
i7	193.909297	0.273016	164.804481	122
i7	193.909297	0.273016	219.999999	122
i7	194.045578	0.136735	246.934685	106
i7	194.182086	0.136508	164.804481	127
i7	194.182086	0.136508	207.646491	127
i7	194.182086	0.136508	246.934685	127
i7	194.454649	0.136735	164.804481	95
i7	194.454649	0.136735	207.646491	95
i7	194.454649	0.136735	246.934685	127
i7	194.727438	0.273016	246.934685	127
i7	194.727438	0.341043	164.804481	122
i7	194.727438	0.341043	207.646491	122
i7	195.000227	0.136508	246.934685	84
i7	195.136508	0.136508	219.999999	122
i7	195.136508	0.136508	164.804481	122
i7	195.136508	0.136508	246.934685	127
i7	195.409297	0.136508	219.999999	95
i7	195.409297	0.136508	164.804481	95
i7	195.409297	0.136508	246.934685	127
i7	195.682086	0.272789	164.804481	119
i7	195.682086	0.272789	219.999999	119
i7	195.682086	0.272789	246.934685	127
i7	195.954649	0.136735	164.804481	101
i7	195.954649	0.136735	246.934685	99
i7	196.091156	0.136508	246.934685	127
i7	196.091156	0.272789	164.804481	122
i7	196.091156	0.272789	219.999999	122
i7	196.227438	0.136508	246.934685	94
i7	196.363719	0.136735	246.934685	127
i7	196.363719	0.136735	184.997211	125
i7	196.363719	0.136735	246.934685	127
i7	196.636508	0.136508	246.934685	122
i7	196.636508	0.136508	184.997211	107
i7	196.636508	0.136508	246.934685	127
i7	196.909297	0.273016	246.934685	127
i7	196.909297	0.409297	184.997211	119
i7	196.909297	0.409297	246.934685	127
i7	197.318367	0.136508	164.804481	116
i7	197.318367	0.136508	219.999999	116
i7	197.318367	0.136508	246.934685	127
i7	197.591156	0.136508	164.804481	107
i7	197.591156	0.136508	219.999999	107
i7	197.591156	0.136508	246.934685	127
i7	197.863719	0.273016	164.804481	119
i7	197.863719	0.273016	219.999999	119
i7	197.863719	0.273016	246.934685	127
i7	198.136508	0.136508	164.804481	95
i7	198.136508	0.136508	246.934685	99
i7	198.272789	0.136735	246.934685	127
i7	198.272789	0.273016	164.804481	122
i7	198.272789	0.273016	219.999999	122
i7	198.409297	0.136508	246.934685	106
i7	198.545578	0.136735	164.804481	127
i7	198.545578	0.136735	207.646491	127
i7	198.545578	0.136735	246.934685	127
i7	198.818367	0.136508	164.804481	95
i7	198.818367	0.136508	207.646491	95
i7	198.818367	0.136508	246.934685	127
i7	199.091156	0.272789	246.934685	127
i7	199.091156	0.341043	164.804481	122
i7	199.091156	0.341043	207.646491	122
i7	199.363719	0.136735	246.934685	84
i7	199.500227	0.136508	219.999999	122
i7	199.500227	0.136508	164.804481	122
i7	199.500227	0.136508	246.934685	127
i7	199.772789	0.136735	219.999999	95
i7	199.772789	0.136735	164.804481	95
i7	199.772789	0.136735	246.934685	127
i7	200.045578	0.273016	164.804481	119
i7	200.045578	0.273016	219.999999	119
i7	200.045578	0.273016	246.934685	127
i7	200.318367	0.136508	164.804481	101
i7	200.318367	0.136508	246.934685	99
i7	200.454649	0.136735	246.934685	127
i7	200.454649	0.273016	164.804481	122
i7	200.454649	0.273016	219.999999	122
i7	200.591156	0.136508	246.934685	94
i7	200.727438	0.136508	246.934685	127
i7	200.727438	0.136508	184.997211	125
i7	200.727438	0.136508	246.934685	127
i7	201.00		0.137		246.934685	122
i7	201.00		0.137		184.997211	107
i7	201.00		0.137		246.934685	127
i7	201.272789	0.273016	246.934685	127
i7	201.272789	0.409524	184.997211	119
i7	201.272789	0.409524	246.934685	127
i7	201.682086	0.136508	164.804481	116
i7	201.682086	0.136508	219.999999	116
i7	201.682086	0.136508	246.934685	127
i7	201.954649	0.136735	164.804481	107
i7	201.954649	0.136735	219.999999	107
i7	201.954649	0.136735	246.934685	127
i7	202.227438	0.273016	164.804481	119
i7	202.227438	0.273016	219.999999	119
i7	202.227438	0.273016	246.934685	127
i7	202.500227	0.136508	164.804481	95
i7	202.500227	0.136508	246.934685	99
i7	202.636508	0.136508	246.934685	127
i7	202.636508	0.273016	164.804481	122
i7	202.636508	0.273016	219.999999	122
i7	202.772789	0.136735	246.934685	106
i7	202.909297	0.136508	164.804481	127
i7	202.909297	0.136508	207.646491	127
i7	202.909297	0.136508	246.934685	127
i7	203.182086	0.136508	164.804481	95
i7	203.182086	0.136508	207.646491	95
i7	203.182086	0.136508	246.934685	127
i7	203.454649	0.273016	246.934685	127
i7	203.454649	0.341270	164.804481	122
i7	203.454649	0.341270	207.646491	122
i7	203.727438	0.136508	246.934685	84
i7	203.863719	0.136735	219.999999	122
i7	203.863719	0.136735	164.804481	122
i7	203.863719	0.136735	246.934685	127
i7	204.136508	0.136508	219.999999	95
i7	204.136508	0.136508	164.804481	95
i7	204.136508	0.136508	246.934685	127
i7	204.409297	0.273016	164.804481	119
i7	204.409297	0.273016	219.999999	119
i7	204.409297	0.273016	246.934685	127
i7	204.682086	0.136508	164.804481	101
i7	204.682086	0.136508	246.934685	99
i7	204.818367	0.136508	246.934685	127
i7	204.818367	0.273016	164.804481	122
i7	204.818367	0.273016	219.999999	122
i7	204.954649	0.136735	246.934685	94
i7	205.091156	0.136508	246.934685	127
i7	205.091156	0.136508	184.997211	125
i7	205.091156	0.136508	246.934685	127
i7	205.363719	0.136735	246.934685	122
i7	205.363719	0.136735	184.997211	107
i7	205.363719	0.136735	246.934685	127
i7	205.636508	0.273016	246.934685	127
i7	205.636508	0.409297	184.997211	119
i7	205.636508	0.409297	246.934685	127
i7	206.045578	0.136735	164.804481	116
i7	206.045578	0.136735	219.999999	116
i7	206.045578	0.136735	246.934685	127
i7	206.318367	0.136508	164.804481	107
i7	206.318367	0.136508	219.999999	107
i7	206.318367	0.136508	246.934685	127
i7	206.591156	0.272789	164.804481	119
i7	206.591156	0.272789	219.999999	119
i7	206.591156	0.272789	246.934685	127
i7	206.863719	0.136735	164.804481	95
i7	206.863719	0.136735	246.934685	99
i7	207.000227	0.136508	246.934685	127
i7	207.000227	0.272789	164.804481	122
i7	207.000227	0.272789	219.999999	122
i7	207.136508	0.136508	246.934685	106
i7	207.272789	0.409524	82.402241	115
i7	207.818367	0.136508	82.402241	127
i7	207.954649	0.136735	82.402241	110
i7	208.091156	0.136508	82.402241	127
i7	208.227438	0.136508	82.402241	113
i7	208.363719	0.136735	82.402241	127
i7	208.500227	0.136508	82.402241	110
i7	208.636508	0.136508	82.402241	127
i7	208.772789	0.136735	82.402241	99
i7	208.909297	0.136508	82.402241	127
i7	209.045578	0.136735	82.402241	91
i7	209.182086	0.136508	82.402241	127
i7	209.318367	0.136508	82.402241	69
i7	209.454649	0.136735	82.402241	115

; 8:1 starts 11.45
; ins 8
; flourishy mario paint trill

i8	11.45   	0.272789	1318.435849	127
i8	11.45	    0.277324	 659.217924	127
i8	11.70    	0.034467	1244.507929	125
i8	11.76   	0.05	1108.667979	108
i8	11.80   	0.05	987.738739	    92
i8	11.83   	0.05	932.274929	    76
i8	11.86   	0.05	830.585965	    59
i8	12.27   	0.1     	554.3	       127
i8	12.273  	0.1     	1108.667979	127
i8	12.409070	0.1     	1108.667979	116
i8	12.409070	0.1     	554.30  	   116
i8	12.545578	0.1     	1108.667979	127
i8	12.545578	0.1     	554.300  	    127
i8	12.681859	0.1     	554.300  	    119
i8	12.681859	0.1     	1108.667979	119
i8	12.954649	0.1     	1244.507929	127
i8	12.954649	0.1     	622.253965	127
i8	13.227211	0.1     	622.253965	127
i8	13.227211	0.1     	1244.507929	127
i8	13.500000	0.1     	622.253965	127
i8	13.500000	0.1     	1244.507929	127
i8	13.636281	0.1     	1244.507929	116
i8	13.636281	0.1     	622.253965	116
i8	13.772789	0.1     	1244.507929	127
i8	13.772789	0.1     	622.253965	127
i8	13.909070	0.136735	1174.625937	119
i8	13.909070	0.136735	587.312968	119
i8	14.045578	0.1     	1108.667979	113
i8	14.045578	0.1     	554.300  	113
i8	15.818141	0.273016	1318.435849	127
i8	15.818141	0.277551	659.217924	127
i8	16.090930	0.034240	1244.507929	125
i8	16.124943	0.034467	1108.667979	108
i8	16.159184	0.034240	987.738739	92
i8	16.193197	0.034240	932.274929	76
i8	16.227211	0.034467	830.585965	59
i8	16.636281	0.1     	554.300  	127
i8	16.636281	0.1     	1108.667979	127
i8	16.772789	0.1     	1108.667979	116
i8	16.772789	0.1     	554.300  	116
i8	16.909070	0.1     	1108.667979	127
i8	16.909070	0.1     	554.300  	127
i8	17.045578	0.1     	554.300  	119
i8	17.045578	0.1     	1108.667979	119
i8	17.318141	0.1     	1244.507929	127
i8	17.318141	0.1     	622.253965	127
i8	17.590930	0.1     	622.253965	127
i8	17.590930	0.1     	1244.507929	127
i8	17.863719	0.1     	622.253965	127
i8	17.863719	0.1     	1244.507929	127
i8	18.000000	0.1     	1244.507929	116
i8	18.000000	0.1     	622.253965	116
i8	18.136281	0.1     	1244.507929	127
i8	18.136281	0.1     	622.253965	127
i8	18.272789	0.136508	1174.625937	119
i8	18.272789	0.136508	587.312968	119
i8	18.409070	0.1     	1108.667979	113
i8	18.409070	0.1     	554.300  	113
; 8:2
i8	124.909297	0.272789	1318.435849	127
i8	124.909297	0.277324	659.217924	127
i8	125.181859	0.034467	1244.507929	125
i8	125.216100	0.034240	1108.667979	108
i8	125.250113	0.034240	987.738739	92
i8	125.284127	0.034467	932.274929	76
i8	125.318367	0.034240	830.585965	59
i8	125.727438	0.1     	554.300  	127
i8	125.727438	0.1     	1108.67  	127
i8	125.863719	0.1     	1108.67 	116
i8	125.863719	0.1     	554.30     	116
i8	126.000000	0.1     	1108.667979	127
i8	126.000000	0.1     	554.300  	127
i8	126.136508	0.1     	554.300  	119
i8	126.136508	0.1     	1108.667979	119
i8	126.409297	0.1     	1244.507929	127
i8	126.409297	0.1     	622.253965	127
i8	126.681859	0.1     	622.253965	127
i8	126.681859	0.1     	1244.507929	127
i8	126.954649	0.1     	622.253965	127
i8	126.954649	0.1     	1244.507929	127
i8	127.090930	0.1     	1244.507929	116
i8	127.090930	0.1     	622.253965	116
i8	127.227438	0.1     	1244.507929	127
i8	127.227438	0.1     	622.253965	127
i8	127.363719	0.136508	1174.625937	119
i8	127.363719	0.136508	587.312968	119
i8	127.500000	0.1     	1108.667979	113
i8	127.500000	0.1     	554.300  	113
i8	129.272789	0.273016	1318.435849	127
i8	129.272789	0.277551	659.217924	127
i8	129.545578	0.034240	1244.507929	125
i8	129.579592	0.034467	1108.667979	108
i8	129.613832	0.034240	987.738739	92
i8	129.647846	0.034240	932.274929	76
i8	129.681859	0.034467	830.585965	59
i8	130.090930	0.1     	554.300  	127
i8	130.090930	0.1     	1108.667979	127
i8	130.227438	0.1     	1108.667979	116
i8	130.227438	0.1     	554.300  	116
i8	130.363719	0.1     	1108.667979	127
i8	130.363719	0.1     	554.300  	127
i8	130.500000	0.102721	554.300  	119
i8	130.500000	0.102721	1108.667979	119
i8	130.772789	0.1     	1244.507929	127
i8	130.772789	0.1     	622.253965	127
i8	131.045578	0.1     	622.253965	127
i8	131.045578	0.1     	1244.507929	127
i8	131.318367	0.1     	622.253965	127
i8	131.318367	0.1     	1244.507929	127
i8	131.454649	0.1     	1244.507929	116
i8	131.454649	0.1     	622.253965	116
i8	131.590930	0.1     	1244.507929	127
i8	131.590930	0.1     	622.253965	127
i8	131.727438	0.136508	1174.625937	119
i8	131.727438	0.136508	587.312968	119
i8	131.86  	0.1     	1108.667979	113
i8	131.87  	0.1     	554.300  	113
; 8:3 starts 181
i8	181.6		0.273016	1318.435849	127
i8	181.6		0.278   	659.217924 	127
i8	181.909297	0.034240	1244.507929	125
i8	181.943311	0.034240	1108.667979	108
i8	181.977324	0.034467	987.738739	92
i8	182.01		0.034240	932.274929	76
i8	182.04		0.034240	830.585965	59
i8	182.454649	0.1     	554.300  	127
i8	182.454649	0.1     	1108.667979	127
i8	182.591156	0.1     	1108.667979	116
i8	182.591156	0.1     	554.300  	116
i8	182.727438	0.1     	1108.667979	127
i8	182.727438	0.1     	554.300  	127
i8	182.863719	0.1     	554.300  	119
i8	182.863719	0.1     	1108.667979	119
i8	183.136508	0.1     	1244.507929	127
i8	183.136508	0.1     	622.253965	127
i8	183.409297	0.1     	622.253965	127
i8	183.409297	0.1     	1244.507929	127
i8	183.681859	0.1	622.253965	127
i8	183.681859	0.11	1244.507929	127
i8	183.818367	0.1     	1244.507929	116
i8	183.818367	0.1     	622.253965	116
i8	183.954649	0.1     	1244.507929	127
i8	183.954649	0.1     	622.253965	127
i8	184.091156	0.136508	1174.625937	119
i8	184.091156	0.136508	587.312968	119
i8	184.227438	0.1     	1108.667979	113
i8	184.227438	0.1     	554.300  	113
i8	186.000227	0.272789	1318.435849	127
i8	186.000227	0.277324	659.217924	127
i8	186.272789	0.034467	1244.507929	125
i8	186.307029	0.034240	1108.667979	108
i8	186.341043	0.034240	987.738739	92
i8	186.375057	0.034467	932.274929	76
i8	186.409297	0.034240	830.585965	59
i8	186.818367	0.1     	554.300  	127
i8	186.818367	0.1     	1108.667979	127
i8	186.954649	0.1     	1108.667979	116
i8	186.954649	0.1     	554.300  	116
i8	187.091156	0.1     	1108.667979	127
i8	187.091156	0.1     	554.300  	127
i8	187.227438	0.1     	554.300  	119
i8	187.227438	0.1     	1108.667979	119
i8	187.500227	0.1     	1244.507929	127
i8	187.500227	0.1     	622.253965	127
i8	187.772789	0.1     	622.253965	127
i8	187.772789	0.1     	1244.507929	127
i8	188.045578	0.1     	622.253965	127
i8	188.045578	0.1     	1244.507929	127
i8	188.182086	0.1     	1244.507929	116
i8	188.182086	0.1     	622.253965	116
i8	188.318367	0.1     	1244.507929	127
i8	188.318367	0.1     	622.253965	127
i8	188.454649	0.136735	1174.625937	119
i8	188.454649	0.136735	587.312968	119
i8	188.591156	0.1     	1108.667979	113
i8	188.591156	0.1     	554.300  	113
i8	190.363719	0.273016	1318.435849	127
i8	190.363719	0.277551	659.217924	127
i8	190.636508	0.034240	1244.507929	125
i8	190.670522	0.034467	1108.667979	108
i8	190.704762	0.034240	987.738739	92
i8	190.738776	0.034240	932.274929	76
i8	190.772789	0.034467	830.585965	59
i8	191.182086	0.1     	554.300  	127
i8	191.182086	0.1     	1108.667979	127
i8	191.318367	0.1     	1108.667979	116
i8	191.318367	0.1     	554.300  	116
i8	191.454649	0.1     	1108.667979	127
i8	191.454649	0.1     	554.300  	127
i8	191.591156	0.1     	554.300  	119
i8	191.591156	0.1     	1108.667979	119
i8	191.863719	0.1     	1244.507929	127
i8	191.863719	0.1     	622.253965	127
i8	192.136508	0.1     	622.253965	127
i8	192.136508	0.1     	1244.507929	127
i8	192.409297	0.1     	622.253965	127
i8	192.409297	0.1     	1244.507929	127
i8	192.545578	0.1     	1244.507929	116
i8	192.545578	0.1     	622.253965	116
i8	192.682086	0.1     	1244.507929	127
i8	192.682086	0.1     	622.253965	127
i8	192.818367	0.136508	1174.625937	119
i8	192.818367	0.136508	587.312968	119
i8	192.954649	0.1     	1108.667979	113
i8	192.954649	0.1     	554.300  	113
i8	194.73  	0.273016	1318.435849	127
i8	194.73  	0.277551	659.217924	127
i8	195.00  	0.034240	1244.507929	125
i8	195.03  	0.034240	1108.667979	108
i8	195.06  	0.034467	987.738739	92
i8	195.1     	0.034240	932.274929	76
i8	195.136508	0.034240	830.585965	59
i8	195.545578	0.1     	554.300  	127
i8	195.545578	0.1     	1108.667979	127
i8	195.682086	0.1     	1108.667979	116
i8	195.682086	0.1     	554.300  	116
i8	195.818367	0.1     	1108.667979	127
i8	195.818367	0.1     	554.300  	127
i8	195.954649	0.1     	554.300  	119
i8	195.954649	0.1     	1108.667979	119
i8	196.227438	0.1     	1244.507929	127
i8	196.227438	0.1     	622.253965	127
i8	196.500227	0.1     	622.253965	127
i8	196.500227	0.1     	1244.507929	127
i8	196.772789	0.1     	622.253965	127
i8	196.772789	0.1     	1244.507929	127
i8	196.909297	0.1     	1244.507929	116
i8	196.909297	0.1     	622.253965	116
i8	197.045578	0.1     	1244.507929	127
i8	197.045578	0.1     	622.253965	127
i8	197.182086	0.136508	1174.625937	119
i8	197.182086	0.136508	587.312968	119
i8	197.318367	0.1     	1108.667979	113
i8	197.318367	0.1     	554.300  	113
i8	199.091156	0.272789	1318.435849	127
i8	199.091156	0.277324	659.217924	127
i8	199.363719	0.034467	1244.507929	125
i8	199.397959	0.034240	1108.667979	108
i8	199.431973	0.034240	987.738739	92
i8	199.465986	0.034467	932.274929	76
i8	199.500227	0.034240	830.585965	59
i8	199.909297	0.1     	554.300  	127
i8	199.909297	0.1     	1108.667979	127
i8	200.045578	0.1     	1108.667979	116
i8	200.045578	0.1     	554.300  	116
i8	200.182086	0.1     	1108.667979	127
i8	200.182086	0.1     	554.300  	127
i8	200.318367	0.1     	554.300  	119
i8	200.318367	0.1     	1108.667979	119
i8	200.591156	0.1     	1244.507929	127
i8	200.591156	0.1     	622.253965	127
i8	200.863719	0.1     	622.253965	127
i8	200.863719	0.1     	1244.507929	127
i8	201.136508	0.1     	622.253965	127
i8	201.136508	0.1     	1244.507929	127
i8	201.272789	0.1     	1244.507929	116
i8	201.272789	0.1     	622.253965	116
i8	201.409297	0.1     	1244.507929	127
i8	201.409297	0.1     	622.253965	127
i8	201.545578	0.136735	1174.625937	119
i8	201.545578	0.136735	587.312968	119
i8	201.682086	0.1     	1108.667979	113
i8	201.682086	0.1     	554.300  	113
i8	203.454649	0.273016	1318.435849	127
i8	203.454649	0.277551	659.217924	127
i8	203.727438	0.034240	1244.507929	125
i8	203.761451	0.034467	1108.667979	108
i8	203.795692	0.034240	987.738739	92
i8	203.829705	0.034240	932.274929	76
i8	203.863719	0.034467	830.585965	59
i8	204.272789	0.1     	554.300  	127
i8	204.272789	0.1     	1108.667979	127
i8	204.409297	0.1     	1108.667979	116
i8	204.409297	0.1     	554.300  	116
i8	204.545578	0.1     	1108.667979	127
i8	204.545578	0.1     	554.300  	127
i8	204.682086	0.1     	554.300  	119
i8	204.682086	0.1     	1108.667979	119
i8	204.954649	0.1     	1244.507929	127
i8	204.954649	0.1     	622.253965	127
i8	205.227438	0.1     	622.253965	127
i8	205.227438	0.1     	1244.507929	127
i8	205.500227	0.1     	622.253965	127
i8	205.500227	0.1     	1244.507929	127
i8	205.636508	0.1     	1244.507929	116
i8	205.636508	0.1     	622.253965	116
i8	205.772789	0.102721	1244.507929	127
i8	205.772789	0.102721	622.253965	127
i8	205.909297	0.136508	1174.625937	119
i8	205.909297	0.136508	587.312968	119
i8	206.045578	0.1     	1108.667979	113
i8	206.045578	0.1     	554.300  	113
i8	207.272789	0.273016	659.217924	127
i8	207.272789	0.273016	1318.435849	127
i8	207.545578	0.068481	1244.507929	118
i8	207.613832	0.068481	1108.667979	104
i8	207.682086	0.068254	987.738739	90
i8	207.750113	0.068481	932.274929	76
i8	209.454649	0.273016	1318.435849	127
i8	209.454649	0.273016	2636.871698	127

; 9:1 starts 67.6 compliment to horn
; ins 9

i9	67.6    	0.15    	415.29  	127
i9	67.636508	0.15    	329.608962	121
i9	67.909070	0.14      	415.292983	127
i9	67.909070	0.14      	329.608962	121
i9	68.181859	0.15    	329.608962	127
i9	68.181859	0.15    	415.292983	127
i9	68.590930	0.14      	369.994421	117
i9	68.590930	0.14      	440.0     	121
i9	68.863719	0.15    	369.994421	121
i9	68.863719	0.15    	439.999998	127
i9	69.136508	0.29     	369.994421	121
i9	69.136508	0.29     	439.999998	127
i9	69.409070	0.14      	440.0    	87
i9	69.409070	0.14      	369.994421	87
i9	69.545578	0.15    	369.994421	127
i9	69.545578	0.15    	439.999998	127
i9	69.818141	0.14      	493.869370	121
i9	69.818141	0.14      	415.292983	121
i9	70.090930	0.14      	415.292983	127
i9	70.090930	0.14      	492.10  	121
i9	70.363719	0.28       494.10  	127
i9	70.363719	0.28       415.292983	121
i9	70.772789	0.15    	439.999998	121
i9	70.772789	0.15    	369.994421	121
i9	71.045578	0.15    	439.999998	121
i9	71.045578	0.15    	369.994421	127
i9	71.318141	0.29       442.5   	127
i9	71.318141	0.28       369.994421	127
i9	71.590930	0.14      	439.999998	127
i9	71.590930	0.14      	369.994421	127
i9	71.727438	0.15    	439.999998	127
i9	71.727438	0.15    	369.994421	121
i9	72.000000	0.14      	415.292983	121
i9	72.000000	0.14      	329.608962	127
i9	72.272789	0.15    	415.292983	127
i9	72.272789	0.15    	329.608962	127
i9	72.545578	0.15    	329.608962	121
i9	72.545578	0.15    	415.292983	121
i9	72.954649	0.15    	369.994421	117
i9	72.954649	0.15    	439.999998	127
i9	73.227438	0.15    	369.994421	127
i9	73.227438	0.15    	439.999998	127
i9	73.500000	0.29       369.994421	127
i9	73.500000	0.29       440.8   	127
i9	73.772789	0.15    	439.999998	87
i9	73.772789	0.15    	369.994421	87
i9	73.909070	0.14      	369.994421	127
i9	73.909070	0.14      	439.999998	127
i9	74.181859	0.15    	493.869370	127
i9	74.181859	0.15    	415.292983	127
i9	74.454649	0.15    	415.292983	127
i9	74.454649	0.15    	493.869370	127
i9	74.727438	0.29     	493.869370	121
i9	74.727438	0.29     	415.292983	121
i9	75.136508	0.15    	439.999998	127
i9	75.136508	0.15    	369.994421	121
i9	75.409070	0.14      	439.999998	127
i9	75.409070	0.14      	369.994421	127
i9	75.681859	0.28       440.0   	121
i9	75.681859	0.29       370.0   	127
i9	75.954649	0.15    	440.0   	127
i9	75.954649	0.15    	369.994421	121
i9	76.090930	0.14      	439.999998	127
i9	76.090930	0.14      	369.994421	127
i9	80.727438	0.15    	329.608962	121
i9	80.727438	0.15    	246.934685	121
i9	81.000000	0.14      	246.934685	122
i9	81.000000	0.14      	329.608962	122
i9	81.272789	0.15    	246.934685	121
i9	81.272789	0.15    	329.608962	121
i9	81.681859	0.15    	329.608962	121
i9	81.681859	0.15    	277.166995	121
i9	81.954649	0.15    	329.608962	119
i9	81.954649	0.15    	277.166995	119
i9	82.227438	0.29     	277.166995	121
i9	82.227438	0.29     	329.608962	121
i9	82.500000	0.14      	329.608962	87
i9	82.500000	0.14      	277.166995	87
i9	82.636508	0.15    	329.608962	127
i9	82.636508	0.15    	277.166995	127
i9	82.909070	0.14      	311.126982	127
i9	82.909070	0.14      	369.994421	127
i9	83.181859	0.15    	369.994421	113
i9	83.181859	0.15    	311.126982	113
i9	83.454649	0.42       311.13  	119
i9	83.454649	0.42      	369.994421	118
i9	83.863719	0.15    	277.166995	127
i9	83.863719	0.15    	329.608962	127
i9	84.136508	0.15    	277.166995	116
i9	84.136508	0.15    	329.608962	116
i9	83.863719	0.57      	219.999999	127
i9	84.409070	0.29       329.608962	121
i9	84.409070	0.29       277.166995	127
i9	84.409070	0.42      	207.75  	127
i9	84.681859	0.15    	329.608962	84
i9	84.681859	0.15    	277.166995	84
i9	84.818141	0.14      	277.166995	127
i9	84.818141	0.14      	329.608962	127
i9	84.818141	0.29       	184.997211	127
i9	85.090930	2.1        	164.804481	127
i9	89.454649	0.15    	329.608962	121
i9	89.454649	0.15    	246.934685	127
i9	89.727438	0.15    	246.934685	122
i9	89.727438	0.15    	329.608962	122
i9	90.000000	0.14      	246.934685	127
i9	90.000000	0.14      	329.608962	127
i9	90.409070	0.14      	329.608962	127
i9	90.409070	0.14      	277.166995	127
i9	90.681859	0.14      	329.608962	119
i9	90.681859	0.14      	277.166995	119
i9	90.954649	0.28       277.166995	121
i9	90.954649	0.28       329.6   	127
i9	91.227438	0.15    	329.65  	88
i9	91.227438	0.15    	277.166995	87
i9	91.363719	0.15    	329.608962	127
i9	91.363719	0.15    	277.166995	127
i9	91.636508	0.15    	311.126982	123
i9	91.636508	0.15    	369.994421	127
i9	91.909070	0.14      	369.994421	113
i9	91.909070	0.14      	311.126982	113
i9	92.181859	0.42      	311.13  	119
i9	92.181859	0.42      	369.994421	119
i9	92.590930	0.14      	277.166995	127
i9	92.590930	0.14      	329.608962	127
i9	92.863719	0.15    	277.166995	116
i9	92.863719	0.15    	329.608962	116
i9	92.590930	0.55    	220.0   	127
i9	93.136508	0.29     	329.608962	123
i9	93.136508	0.29     	277.166995	127
i9	93.136508	0.42       208.0   	127
i9	93.409070	0.14      	329.608962	84
i9	93.409070	0.14      	277.166995	84
i9	93.545578	0.15    	277.166995	127
i9	93.545578	0.15    	329.608962	127
i9	93.545578	0.29      	184.997211	123
i9	93.818367	2.1     	164.804481	127
; 9:2
i9	98.181859	0.14      	329.608962	127
i9	98.12   	0.14      	246.934685	127
i9	98.454649	0.15    	246.934685	122
i9	98.46   	0.15    	329.608962	122
i9	98.727438	0.15    	246.934685	123
i9	98.73   	0.15    	329.608962	127
i9	99.136508	0.15    	330.0   	127
i9	99.14   	0.15    	277.166995	127
i9	99.41   	0.14      	329.608962	119
i9	99.42   	0.14      	277.166995	119
i9	99.681859	0.28      	277.2   	127
i9	99.69   	0.28      	329.608962	123
i9	99.954649	0.15    	329.608962	87
i9	99.96   	0.15    	277.166995	87
i9	100.09  	0.14      	329.608962	127
i9	100.10  	0.14      	277.166995	127
i9	100.36  	0.15    	311.13  	125
i9	100.37  	0.15    	369.994421	127
i9	100.636508	0.15    	369.994421	113
i9	100.636508	0.15    	311.13		113
i9	100.909070	0.42		311.13		119
i9	100.909070	0.43		370.0		115
i9	101.318367	0.15    	277.166995	127
i9	101.318367	0.15    	329.608962	127
i9	101.590930	0.14      	277.17  	126
i9	101.590930	0.14      	329.608962	116
i9	101.318367	0.56		220.00		127
i9	101.863719	0.28       329.608962	127
i9	101.863719	0.28       277.17		120
i9	101.863719	0.42       207.65		121
i9	102.136508	0.15    	329.608962	84
i9	102.136508	0.15    	277.166995	84
i9	102.272789	0.15    	277.166995	127
i9	102.272789	0.15    	329.608962	127
i9	102.272789	0.28       184.997211	127
i9	102.545578	0.29       329.608962	127
i9	102.545578	0.29       247.00		126
i9	102.818367	0.11	329.608962	125
i9	102.818367	0.12	246.934685	125
i9	102.545578	2.15	164.804481	127
; 9:3
i9	106.9    	0.14      	329.608962	127
i9	106.909070	0.14      	246.934685	123
i9	107.181859	0.14      	246.934685	122
i9	107.181859	0.14      	329.608962	122
i9	107.454649	0.15    	246.934685	123
i9	107.454649	0.15    	329.608962	123
i9	107.863719	0.15    	329.608962	123
i9	107.863719	0.15    	277.166995	123
i9	108.136508	0.15    	329.608962	119
i9	108.136508	0.15    	277.166995	119
i9	108.409070	0.28       	277.166995	123
i9	108.409070	0.29       	329.608962	123
i9	108.681859	0.14      	329.608962	87
i9	108.681859	0.14      	277.166995	87
i9	108.818367	0.15    	329.608962	127
i9	108.818367	0.15    	277.166995	123
i9	109.090930	0.14      	311.126982	123
i9	109.090930	0.14      	369.994421	123
i9	109.363719	0.15    	369.994421	113
i9	109.363719	0.15    	311.126982	113
i9	109.636508	0.42               	311.126982	119
i9	109.636508	0.42               	369.994421	119
i9	110.045578	0.15    	277.166995	127
i9	110.045578	0.15    	329.608962	127
i9	110.318367	0.15    	277.166995	116
i9	110.318367	0.15    	329.60  	116
i9	110.590930	0.29      	329.61  	126
i9	110.590930	0.29       277.17  	123
i9	110.863719	0.15    	329.608962	84
i9	110.9   	0.15    	277.166995	84
i9	111.000000	0.14      	277.166995	123
i9	111.000000	0.14      	329.608962	123
; 9:4
i9	137.454649	0.15    	207.646491	92
i9	137.454649	0.15    	246.934685	123
i9	137.590930	0.14      	207.646491	46
i9	137.590930	0.14      	246.934685	81
i9	137.727438	0.15    	207.646491	92
i9	137.727438	0.15    	246.934685	123
i9	137.863719	0.14      	207.646491	52
i9	137.863719	0.14      	246.934685	87
i9	138.000227	0.15    	207.646491	92
i9	138.000227	0.15    	246.934685	123
i9	138.136508	0.15    	207.646491	49
i9	138.136508	0.15    	246.934685	84
i9	138.272789	0.14      	207.646491	92
i9	138.272789	0.14      	246.934685	123
i9	138.409297	0.15    	219.999999	92
i9	138.409297	0.15    	246.934685	127
i9	138.681859	0.14      	219.999999	92
i9	138.681859	0.14      	246.934685	127
i9	138.954649	0.15    	219.999999	88
i9	138.954649	0.15    	246.934685	123
i9	139.090930	0.14      	219.999999	75
i9	139.090930	0.14      	246.934685	110
i9	139.227438	0.15    	219.999999	92
i9	139.227438	0.15    	246.934685	127
i9	139.363719	0.14      	219.999999	72
i9	139.363719	0.14      	246.934685	107
i9	139.500227	0.15    	246.934685	127
i9	139.500227	0.15    	219.999999	92
i9	139.636508	0.15    	184.997211	92
i9	139.636508	0.15    	246.934685	127
i9	139.909297	0.15    	184.997211	92
i9	139.909297	0.15    	246.934685	127
i9	140.181859	0.14      	184.997211	92
i9	140.181859	0.14      	246.934685	123
i9	140.318367	0.15    	184.997211	72
i9	140.318367	0.15    	246.934685	107
i9	140.454649	0.15    	184.997211	92
i9	140.454649	0.15    	246.934685	123
i9	140.590930	0.14      	219.999999	80
i9	140.590930	0.14      	246.934685	115
i9	140.863719	0.14      	219.999999	92
i9	140.863719	0.14      	246.934685	123
i9	141.136508	0.15    	220     	92
i9	141.136508	0.15    	246.934685	127
i9	141.272789	0.14      	220     	63
i9	141.272789	0.14      	246.934685	98
i9	141.409297	0.15    	220     	92
i9	141.409297	0.15    	246.934685	127
i9	141.545578	0.15    	220     	72
i9	141.545578	0.15    	246.934685	107
i9	141.681859	0.14      	246.934685	127
i9	141.681859	0.14      	220     	92
i9	141.818367	0.15    	207.646491	92
i9	141.818367	0.15    	246.934685	127
i9	141.954649	0.15    	207.646491	46
i9	141.954649	0.15    	246.934685	81
i9	142.090930	0.14      	207.646491	92
i9	142.090930	0.14      	246.934685	127
i9	142.227438	0.15    	207.646491	52
i9	142.227438	0.15    	246.934685	87
i9	142.363719	0.14      	207.646491	92
i9	142.363719	0.14      	246.934685	127
i9	142.500227	0.15    	207.646491	49
i9	142.500227	0.15    	246.934685	84
i9	142.636508	0.15    	207.646491	92
i9	142.636508	0.15    	246.934685	127
i9	142.772789	0.14      	225     	92
i9	142.772789	0.14      	246.934685	123
i9	143.045578	0.15    	219.999999	92
i9	143.045578	0.15    	246.934685	127
i9	143.318367	0.15    	219.999999	88
i9	143.318367	0.15    	246.934685	123
i9	143.454649	0.15    	219.999999	75
i9	143.454649	0.15    	246.934685	110
i9	143.590930	0.14      	219.999999	92
i9	143.590930	0.14      	246.934685	123
i9	143.727438	0.15    	219.999999	72
i9	143.727438	0.15    	246.934685	107
i9	143.863719	0.14      	246.934685	127
i9	143.863719	0.14      	219.999999	92
i9	144.000227	0.15    	184.997211	92
i9	144.000227	0.15    	246.934685	127
i9	144.272789	0.14      	184.997211	92
i9	144.272789	0.14      	246.934685	127
i9	144.545578	0.15    	184.997211	92
i9	144.545578	0.15    	246.934685	127
i9	144.681859	0.14      	184.997211	72
i9	144.681859	0.14      	246.934685	107
i9	144.818367	0.15    	184.997211	92
i9	144.818367	0.15    	246.934685	127
i9	144.954649	0.15    	219.999999	80
i9	144.954649	0.15    	246.934685	115
i9	145.227438	0.15    	219.999999	92
i9	145.227438	0.15    	246.934685	127
i9	145.500227	0.15    	219.999999	92
i9	145.500227	0.15    	246.934685	127
i9	145.636508	0.15    	219.999999	63
i9	145.636508	0.15    	246.934685	98
i9	145.772789	0.14      	219.999999	92
i9	145.772789	0.14      	246.934685	123
i9	145.909297	0.15    	219.999999	72
i9	145.909297	0.15    	246.934685	107
i9	146.045578	0.15    	246.934685	127
i9	146.045578	0.15    	219.999999	92
i9	146.181859	0.14      	207.646491	92
i9	146.181859	0.14      	246.934685	123
i9	146.318367	0.15    	207.646491	46
i9	146.318367	0.15    	246.934685	81
i9	146.454649	0.15    	207.646491	92
i9	146.454649	0.15    	246.934685	127
i9	146.590930	0.14      	207.646491	52
i9	146.590930	0.14      	246.934685	87
i9	146.727438	0.15    	207.646491	92
i9	146.727438	0.15    	246.934685	127
i9	146.863719	0.14      	207.646491	49
i9	146.863719	0.14      	246.934685	84
i9	147.000227	0.15    	207.646491	92
i9	147.000227	0.15    	246.934685	127
i9	147.136508	0.15    	219.999999	92
i9	147.136508	0.15    	246.934685	127
i9	147.409297	0.15    	220     	92
i9	147.409297	0.15    	246.934685	123
i9	147.681859	0.14      	220     	88
i9	147.681859	0.14      	246.934685	123
i9	147.818367	0.15    	219.999999	75
i9	147.818367	0.15    	246.934685	110
i9	147.954649	0.15    	219.999999	92
i9	147.954649	0.15    	246.934685	123
i9	148.090930	0.14      	221.0   	72
i9	148.090930	0.14      	246.934685	107
i9	148.227438	0.15    	246.9   	127
i9	148.227438	0.15    	230     	92
i9	148.363719	0.14      	184.997211	92
i9	148.363719	0.14      	246.934685	123
i9	148.636508	0.15    	184.997211	92
i9	148.636508	0.15    	246.934685	127
i9	148.909297	0.15    	184.997211	92
i9	148.909297	0.15    	246.934685	127
i9	149.045578	0.15    	184.997211	72
i9	149.045578	0.15    	246.934685	107
i9	149.181859	0.14      	184.997211	92
i9	149.181859	0.14      	246.934685	127
i9	149.318367	0.15    	219.999999	80
i9	149.318367	0.15    	246.934685	115
i9	149.590930	0.14      	219.999999	92
i9	149.590930	0.14      	246.934685	127
i9	149.863719	0.14      	219.999999	92
i9	149.863719	0.14      	246.934685	127
i9	150.000227	0.15    	219.999999	63
i9	150.000227	0.15    	246.934685	98
i9	150.15    	0.15    	219.999999	92
i9	150.15    	0.15    	246.934685	127
i9	150.29     	0.14      	219.999999	72
i9	150.29     	0.14      	246.934685	107
i9	150.42               	0.15    	246.934685	127
i9	150.42               	0.15    	219.999999	92
i9	150.545578	0.15    	329.608962	127
i9	150.545578	0.15    	246.934685	127
i9	150.818367	0.15    	246.934685	122
i9	150.818367	0.15    	329.608962	122
i9	151.090930	0.14      	246.934685	127
i9	151.090930	0.14      	329.608962	127
i9	151.500227	0.15    	329.608962	127
i9	151.500227	0.15    	277.166995	127
i9	151.772789	0.14      	329.608962	119
i9	151.772789	0.14      	277.166995	119
i9	152.045578	0.29      	277.166995	127
i9	152.045578	0.28      	329.608962	127
i9	152.318367	0.15    	329.608962	87
i9	152.318367	0.15    	277.166995	87
i9	152.454649	0.15    	329.608962	127
i9	152.454649	0.15    	277.166995	123
i9	152.727438	0.15    	311.126982	127
i9	152.727438	0.15    	369.994421	127
i9	153.000227	0.15    	369.994421	113
i9	153.000227	0.15    	311.126982	113
i9	153.272789	0.42      	311.126982	119
i9	153.272789	0.42      	369.994421	119
i9	153.681859	0.14      	277.2   	127
i9	153.681859	0.14      	329.608962	127
i9	153.954649	0.15      	277.166995	116
i9	153.954649	0.15      	329.608962	116
i9	153.681859	0.57      	219.999999	123
i9	154.227438	0.28      	329.608962	127
i9	154.227438	0.29      	277.166995	127
i9	154.227438	0.42      	207.646491	127
i9	154.500227	0.15    	329.608962	84
i9	154.500227	0.15    	277.166995	84
i9	154.636508	0.15    	277.166995	127
i9	154.636508	0.15    	329.608962	127
i9	154.636508	0.28      	184.997211	127
i9	154.909297	0.15    	329.608962	127
i9	154.909297	0.15    	246.934685	127
i9	155.181859	0.14      	246.934685	122
i9	155.181859	0.14      	329.608962	122
i9	155.454649	0.15    	246.934685	127
i9	155.454649	0.15    	329.608962	127
i9	155.863719	0.14      	329.608962	127
i9	155.863719	0.14      	277.166995	123
i9	156.136508	0.15    	329.608962	119
i9	156.136508	0.15    	277.166995	119
i9	156.409297	0.29     	277.166995	127
i9	156.409297	0.29     	329.608962	127
i9	156.681859	0.14      	329.608962	87
i9	156.681859	0.14      	277.166995	87
i9	156.818367	0.15    	329.608962	127
i9	156.818367	0.15    	277.166995	127
i9	154.909297	2.13      	164.804481	123
i9	157.090930	0.14      	311.126982	127
i9	157.090930	0.14      	369.994421	127
i9	157.363719	0.14      	369.994421	113
i9	157.363719	0.14      	311.126982	113
i9	157.636508	0.42               	311.126982	119
i9	157.636508	0.42               	369.994421	119
i9	158.045578	0.15    	277.166995	127
i9	158.045578	0.15    	329.608962	123
i9	158.318367	0.15    	277.166995	116
i9	158.318367	0.15    	329.608962	116
i9	158.045578	0.56      	227.0    	127
i9	158.590930	0.28      	329.608962	127
i9	158.590930	0.28      	277.166995	127
i9	158.590930	0.39      	207.646491	123
i9	158.863719	0.14      	329.608962	84
i9	158.863719	0.14      	277.166995	84
i9	159.000227	0.15    	277.166995	127
i9	159.000227	0.15    	329.608962	122
i9	159.000227	0.29     	184.997211	127
i9	159.272789	0.14      	329.608962	122
i9	159.272789	0.14      	246.934685	122
i9	159.545578	0.15    	246.934685	122
i9	159.545578	0.15    	329.608962	122
i9	159.818367	0.15    	246.934685	122
i9	159.818367	0.15    	329.608962	127
i9	160.227438	0.15    	329.608962	127
i9	160.227438	0.15    	277.166995	127
i9	160.500227	0.15    	329.608962	119
i9	160.500227	0.15    	277.166995	119
i9	160.772789	0.28      	277.166995	127
i9	160.772789	0.28      	329.608962	127
i9	161.045578	0.15    	329.608962	87
i9	161.045578	0.15    	277.166995	87
i9	161.181859	0.14      	329.608962	122
i9	161.181859	0.14      	277.166995	127
i9	161.454649	0.14      	311.126982	127
i9	161.454649	0.14      	370.00   	127
i9	161.727438	0.15    	372.00  	113
i9	161.727438	0.15    	311.126982	113
i9	162.000227	0.42               	311.126982	119
i9	162.000227	0.42               	369.994421	119
i9	162.409297	0.15    	277.166995	122
i9	162.409297	0.15    	329.608962	127
i9	162.681859	0.14      	277.166995	116
i9	162.681859	0.14      	329.608962	116
i9	162.409297	0.54      	219.999999	127
i9	162.954649	0.29       	329.608962	127
i9	162.954649	0.29       	277.166995	122
i9	162.954649	0.42               	207.646491	127
i9	163.227438	0.15    	329.608962	84
i9	163.227438	0.15    	277.166995	84
i9	163.363719	0.14      	277.166995	127
i9	163.363719	0.14      	329.608962	127
i9	163.363719	0.29       	184.997211	122
i9	163.636508	0.15    	329.608962	127
i9	163.636508	0.15    	246.934685	127
i9	163.909297	0.15    	246.934685	122
i9	163.909297	0.15    	329.608962	122
i9	164.181859	0.14      	246.934685	127
i9	164.181859	0.14      	329.608962	127
i9	164.591156	0.15    	329.608962	127
i9	164.591156	0.15    	277.166995	127
i9	164.863719	0.14      	329.608962	119
i9	164.863719	0.14      	276.0   	119
i9	165.136508	0.28       278.0    	127
i9	165.136508	0.28       329.608962	127
i9	165.409297	0.15    	329.608962	87
i9	165.409297	0.15    	277.166995	87
i9	165.545578	0.15    	329.608962	122
i9	165.545578	0.15    	277.166995	127
i9	163.636508	2.11      	164.804481	127
i9	165.818367	0.15      	311.126982	127
i9	165.818367	0.15    	369.994421	127
i9	166.091156	0.15    	369.994421	113
i9	166.091156	0.15    	311.126982	113
i9	166.363719	0.43      	312.126982	119
i9	166.363719	0.42       372.00  	119
i9	166.772789	0.14      	277.166995	127
i9	166.772789	0.14      	329.608962	122
i9	167.045578	0.15    	277.166995	116
i9	167.045578	0.15    	329.608962	116
i9	167.318367	0.28      	329.608962	127
i9	167.318367	0.29      	278.17  	127
i9	167.591156	0.15    	329.608962	84
i9	167.591156	0.15    	277.166995	86
i9	167.727438	0.15    	277.166995	127
i9	167.727438	0.15    	329.608962	127

; ins 10
; rapid bass drums
; 10:1 starts 2.1 ends 3:30 ins 10
; Include score for i10
#include "includes/i10sco.sco"

; 11:1 starts 63.2
; ins 11

i11	63.272789	0.5     	311.126982	119
i11	63.272789	0.5     	415.292983	119
i11	65.454649	0.5     	277.166995	127
i11	65.454649	0.5     	369.994421	127
i11	67.636508	3.818367	659.217924	110
i11	71.454649	0.136508	554.333990	124
i11	71.590930	0.136735	493.869370	104
i11	71.727438	0.136508	554.333990	124
i11	71.863719	0.136508	493.869370	109
i11	72.000000	3.818367	659.217924	113
i11	75.818141	0.5     	554.333990	119
i11	76.363719	0.5     	493.869370	115
; 11:2 111.3
i11	111.27  	0.5     	311.126982	119
i11	111.3   	0.5     	415.292983	119
i11	113.454649	0.5     	277.166995	127
i11	113.454649	0.5     	369.994421	127
i11	115.636508	0.5     	311.126982	119
i11	115.636508	0.5     	415.292983	119
i11	117.818367	0.6     	277.166995	127
i11	117.818367	0.5     	369.994421	126
i11	120.000000	0.545805	311.126982	119
i11	120.000000	0.545805	415.292983	119
i11	122.181859	0.545805	277.166995	127
i11	122.181859	0.545805	369.994421	127
i11	124.363719	3.818367	659.217924	110
i11	128.181859	0.136735	554.333990	124
i11	128.318367	0.136508	493.869370	104
i11	128.454649	0.136508	554.333990	124
i11	128.590930	0.136735	493.869370	109
i11	128.727438	3.818367	659.217924	113
i11	132.545578	0.545578	554.333990	119
i11	133.090930	0.545805	493.869370	115
i11	168.000227	0.545578	311.126982	119
i11	168.000227	0.545578	415.292983	119
i11	170.181859	0.545805	277.166995	127
i11	170.181859	0.545805	369.994421	127
i11	172.363719	0.545805	311.126982	119
i11	172.363719	0.545805	415.292983	119
i11	174.545578	0.545805	277.166995	127
i11	174.545578	0.545805	369.994421	127
i11	176.727438	0.545578	311.126982	119
i11	176.727438	0.545578	415.292983	119
i11	178.909297	0.545578	277.166995	127
i11	178.909297	0.545578	369.994421	127
i11	181.091156	3.818367	659.217924	110
i11	184.909297	0.136508	554.333990	124
i11	185.045578	0.136508	493.869370	104
i11	185.181859	0.136735	554.333990	124
i11	185.318367	0.136508	493.869370	109
i11	185.454649	3.818367	659.217924	113
i11	189.272789	0.545805	554.333990	119
i11	189.818367	3.818367	659.217924	110
i11	193.636508	0.136508	554.333990	124
i11	193.772789	0.136735	493.869370	104
i11	193.909297	0.136508	554.333990	124
i11	194.045578	0.136735	493.869370	109
i11	194.182086	3.818367	659.217924	113
i11	198.000227	0.545578	554.333990	119
i11	198.545578	3.818367	659.217924	110
i11	202.363719	0.136735	554.333990	124
i11	202.500227	0.136508	493.869370	104
i11	202.636508	0.136508	554.333990	124
i11	202.772789	0.136735	493.869370	109
i11	202.909297	3.818367	659.217924	113
i11	206.727438	0.545578	554.333990	119
i11	207.272789	2.21     	659.217924	115
i11	209.454649	0.277551	659.217924	107

e
</CsScore>
</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>1046</x>
 <y>661</y>
 <width>643</width>
 <height>193</height>
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
