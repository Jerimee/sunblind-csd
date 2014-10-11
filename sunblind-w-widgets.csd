<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
-+id_artist=Jerimee
;-odac     ;;;realtime audio out
--env:SSDIR+=assets/ ; needed for instrument 30
;-v ;verbose?
;-+skip_seconds=60
;-iadc    ;;;uncomment -iadc if RT audio input is needed too
-o sunblind-justi3mfckr.wav -W ;;; for file output any platform
;-o sunblind3.ogg --ogg
</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 128 ;64 ;128 ;64 ;32
nchnls = 2
0dbfs = 4 ; 1 is standard and probably better advised

; Include the file with all the various tables
#include "includes/suntables.inc"
; Include user-defined opcodes
#include "includes/sunopcodes.inc"

; G L O B A L S
;;;;;;;;;;;;;;;;;;;;;
giFirstThree 				= 1
giSecondThree				= 1
giLastFive 				= 1
giTwoThreeFiveandSeven	= 1
giFourAndNine				= 1
gievenon					= 1
gioddon  					= 1
	

gi01on = 1     *gioddon*giFirstThree                                   		/* inst 1 sco is a simple backup highlight */
gi02on = 1     *gievenon*giTwoThreeFiveandSeven*giFirstThree 		/* inst 2 sco is a faint tap percusive in time with i3 */
gi03on = 1     *gioddon*giTwoThreeFiveandSeven*giFirstThree 		/* inst 3 sco is a repetative rhythm in time with i2 */

gi04on = 1     *gievenon*giSecondThree*giFourAndNine			/* inst 4 sco is a vocal */
gi05on = 1     *gioddon*giTwoThreeFiveandSeven*giSecondThree		/* inst 5 sco is repetative bass line */
gi06on = 1     *gievenon*giSecondThree					/* inst 6 sco is repeated simple melody */

gi07on = 1     *gioddon*giTwoThreeFiveandSeven*giLastFive 		/* inst 7 sco is a drum rhythm*/
gi08on = 1     *gievenon*giLastFive					/* inst 8 sco is flourishy mario paint trill */
gi09on = 1     *gioddon*giLastFive*giFourAndNine			/* inst 9 sco is */

gi10on = 1     *gievenon*giLastFive					/* inst 10 sco is rapid drums */

gi11on = 1    *gioddon*giLastFive					/* inst 11 sco is */

gi12on = 1    								/* inst 30 sco is ten 20 second chunks for WavPlayer */

giamp   = 0.25 ; global volume control
; can we turn it down beyond 0? NO, somehow it actually gets louder if you do that!
gi01amp = giamp - 0.075
gi02amp = giamp + 0.4
gi03amp = giamp - 0.1
gi04amp = giamp + 0.1
gi05amp = giamp 
gi06amp = giamp - 0.1
gi07amp = giamp  
gi08amp = giamp - 0.2 
gi09amp = giamp * 0.15
gi10amp = giamp + 0.2
gi11amp = giamp - 0.2
gi12amp = giamp 

gicount = 0 ; I don't know how to do a counter without a global var


; I N S T R U M E N T   D E F I N I T I O N S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

connect	 "bqdhorn", "Out", "mycomb", "In" 

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

instr mycomb ; 6 (for now)
	aIn   inleta "In" 
   	#include "instruments/mycomb.inc"
   	aoutL = ((aRes*iamp)*0.55)+(aIn*0.45)
   	aoutR = ((aRes*iamp)*0.3)+(aIn*0.7)
	ksecthr invalue "secondthree"
	#include "includes/kon.inc"
	if ((gi06on==1) && (ksecthr==1) && (kthisoneon==1)) then     
		AssignSend		p1, 0.1, 0.4, gi06amp
		SendOut			p1, aoutL, aoutR
	endif
endin

instr 1 ; Moog Fleur
	#include "instruments/moogfleur.inc"
	kfirthr invalue "firstthree"
	#include "includes/kon.inc"
	if ((gi01on==1) && (kfirthr==1) && (kthisoneon==1)) then
		AssignSend	p1, 0.2, 0.45, gi01amp
		SendOut	 p1, asig*kpanl, asig*kpanr
	endif
endin ; end ins 1

instr 2 ; sine_bass_wave
	ipitch = p4
	ivel = p5
	kfirthr invalue "firstthree"
	kon invalue "allexcept"
	aSubOut subinstr "sine_bass_wave", ivel, ipitch
	if ((gi02on==1) && (kfirthr==1) && ((kon==2)||(kon==0))) then  
		AssignSendNamed	    	p1, 5, 0.75, gi02amp
		SendOutNamed	        	p1, aSubOut, aSubOut
	endif
endin ; end ins 2

instr 3 
	ipitch = p4
	ivel = p5
	kon invalue "allexcept"
	kfirthr invalue "firstthree"
	aSubOutL, aSubOutR subinstr "sweepy", ivel, ipitch
	if ((gi03on==1) && (kfirthr==1) && ((kon==3)||(kon==0))) then  
		AssignSend		        p1, 0.15, 0.1, gi03amp
		SendOut		        p1, aSubOutL, aSubOutR
	endif
endin ; end ins 3

instr 4 ; vocal
;;;;;;;;;;;;;;;;;;;;;;;;;

kcps 	= (p4 > 15 ? p4 : cpspch(p4))
ivel 	= p5/127
idur 	= p3
iatt 	= idur * 0.05
irel 	= idur * 0.2
idec 	= idur * 0.2
islev 	= ivel * 0.6
kenv	xadsr iatt, idec, islev, irel

; this number is divided below
; should be between 2 and 12 (ish)
; high numbers less chorus/delay
; low numbers more
irange		= 4 
if (ivel > 0.99) then
	ispread 	= 0.002 ; delay tight
	idifinit	= 0.999  ; notes tight
	iBoostAmp  = 2
	;Sfoo    strcpy "TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTtite"
else
	irange		= irange/1.2 
	ispread 	= 0.009 ; loose
	idifinit	= 0.98
	iBoostAmp  = 1 ; ie no boost
	;Sfoo    strcpy "                          loooose"
endif
   	;puts Sfoo, 1

; dif of notes
idif01			= idifinit
idif02			= idif01 * idif01
idif03			= idif01 * idif02
idif04			= idif01 * idif03

asig			vco2  kenv, kcps*0.9999, 		12 ; triangle
asigLow			vco2  kenv, kcps*idif01,		8
asigLower		vco2  kenv, kcps*idif02,		12 ; triangle
asigLowrr		vco2  kenv, kcps*idif03, 		8
asigLowest		vco2  kenv, kcps*idif04, 		12 ; triangle
asig			= asig*1.2 ; will this make it louder?
asigLowest		= asigLowest*1.5 ; will this make it louder?

ispread01 	= ispread							; 0.004  or 0.006
ispread02 	= ispread01	- (ispread/irange)	; 0.0035 or 0.00525
ispread03 	= ispread02	- (ispread/irange)	; 0.0030 or 0.0045
ispread04 	= ispread03	- (ispread/irange)	; 0.0025 or 0.00375
adelLow			delay   asigLow, 		ispread01;
adelLower		delay   asigLower, 	ispread02;
adelLowerer	delay   asigLowrr, 	ispread03;
adelLowest		delay   asigLowest, 	ispread04;

aout 			= asig+adelLow+adelLower+adelLowerer+adelLowest
;aout 			= adelLowest

kon invalue "allexcept"
ksecthr invalue "secondthree"
if ((gi04on==1) && (ksecthr==1) && ((kon==4)||(kon==0))) then  
	AssignSend		p1, 2, 0.2, gi04amp * iBoostAmp
	SendOut  		p1, aout, aout
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

; temp
; kmodv 	invalue "bdqhornmode"

iindex = 6
;p5 is never less than 95
kcrossfade = (p5-95)/2.5 ;gisine*10 ;kmodv*3 ; 3? 0?
ivibedepth = 0.015
kviberate  = 3

ifn1 = gisine
ifn2 = gisine
ifn3 = gisine ;giharpsichord
ifn4 = giFmrhode ;giSigmoFall

ivibefn  = ifn2 ;  giSigmoFall;gisine

;fmrhode — Uses FM synthesis to create a Fender Rhodes electric piano sound. 
asig     fmrhode   iamplitude, kHz, iindex, kcrossfade, ivibedepth, kviberate, ifn1, ifn2, ifn3, ifn4, ivibefn

	kon invalue "allexcept"
	ksecthr invalue "secondthree"
	;#include "includes/kon.inc"
	if ((gi05on==1) && (ksecthr==1) && ((kon==5)||(kon==0))) then  
		;note that Reverb is high
		AssignSend		        p1, 5, 0.3, gi05amp
		SendOut			        p1, asig*aenv, asig*aenv
	endif
endin ; end ins 5

instr 6, bqdhorn
;;;;;;;;;;;;;;;
/*  modal synthesis using biquad filters as oscillators
    Example by Scott Lindroth 2007 */

    ipi 		= $M_PI ;3.1415926
    idenom		= sr*0.5
    icps     	= p4
    ipan 		= 0.7
    iamp    	= 0.01
    kmod	 	invalue "bdqhornmode"
	if (kmod < 1) then
	    kModes = 3 		; "jjr"
	    ;printk 	5,kmod,20
   	elseif ((kmod >= 1) && (kmod < 2)) then
	   	kModes 	= 1 		; pot lid
	    ;printk 	5,kmod,40
   	elseif ((kmod >= 2) && (kmod < 3)) then
	   	kModes 	= 2 		; wood bar
	    ;printk 	5,kmod,60
	elseif (kmod >= 3) then
	   	kModes 	= 4 		; proroxy
	    ;printk 	5,kmod,80
	endif
    ifilterw 	= p5

    apulse    mpulse iamp, 0

    ; filter gain
    iamp1 = 4 + (p5/254) 
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

    if (kModes == 1) goto modes1
    if (kModes == 2) goto modes2
    if (kModes == 3) goto modes3
    if (kModes == 4) goto modes4
    
    modes2:
    if ((p5 == 127)||(p5 == 95)) goto modes4
    if1     = icps * 1            ;uniform wood bar
    if2     = icps * 2.572
    if3     = icps * 4.644
    if4     = icps * 6.984
    if5     = icps * 9.723
    if6     = icps * 12.0
    ;Sfoo    strcpy "wood bar"
    ;puts Sfoo, 1
    goto nextPart
    
    modes1:
    if ((p5 == 127)||(p5 == 95)) goto modes4
    if1    = icps * 1            ;pot lid
    if2    = icps * 6.27
    if3    = icps * 3.2
    if4    = icps * 9.92
    if5    = icps * 14.15
    if6    = icps * 6.23
    ;Sfoo    strcpy "pot lid"
    ;puts Sfoo, 1
    goto nextPart

    modes3:
    if ((p5 == 127)||(p5 == 95)) goto modes4
    if1     = icps * 1           ;jerimee
    if2     = icps * 1.6
    if3     = icps * 1.3
    if4     = icps * 1.9
    if5     = icps * 4
    if6     = icps * 1.5
    ;Sfoo    strcpy "jjr"
    ;puts Sfoo, 1
    goto nextPart
    
    modes4:
    if1     = icps * 3.1            ;proroxy
    if2     = icps * 6.27
    if3     = icps * 3.1
    if4     = icps * 9.92
    if5     = icps * 12.5
    if6     = icps * 6.27
    ;Sfoo    strcpy "proroxy"
    ;puts Sfoo, 1
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

asin1 biquad  apulse * iamp1, 1, 0, -1, 1, ib11, ib21
asin2 biquad  apulse * iamp2, 1, 0, -1, 1, ib12, ib22
asin3 biquad  apulse * iamp3, 1, 0,  0, 1, ib13, ib23
asin4 biquad  apulse * iamp4, 1, 0, -1, 1, ib14, ib24
asin5 biquad  apulse * iamp5, 1, 0, -1, 1, ib15, ib25
asin6 biquad  apulse * iamp6, 1, 0, -1, 1, ib16, ib26

afin  = (asin1 + asin2 + asin3 + asin4 + asin5 + asin6)
outleta "Out", afin
endin ; end ins 6

instr 7 
	ipitch = p4
	ivel = p5
	aSubOutL, aSubOutR subinstr "x11", ivel, ipitch
	#include "includes/kon.inc"
	kthithr invalue "thirdthree"
	if ((gi07on==1) && (kthithr==1) && (kthisoneon==1)) then  
	        AssignSend		   	p1, 0.9, 0.075, gi07amp
	        SendOut	 		p1, aSubOutL, aSubOutR
	endif
endin ; end ins 7

instr 8 ; Sunshape
;;;;;;;;;;;;;;;;;;;;

	imaxamp    =20
	ilineend = (p5/5)
	itiny =  0.00001
	ipfo = p4

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
	aenv   expseg   0.8, idurf, 1.0, idurf,0.4, idurf,1.0, idurf,0.3, idurf, 0.9, idurf,0.6, idurf,0.2, idurf,0.05, idurf,itiny,idurf

	;wobble the env by reducing it
	;the krandoInverted number just happens to fit

	afinalenv = aenv-krandoInverted
    adeclick   linseg      0.0, itiny, 1.0, p3 - 0.06, 1.0, itiny, 0.0
    adone = afinalout * adeclick * imaxamp*afinalenv
	#include "includes/kon.inc"
	kthithr invalue "thirdthree"
	if ((gi08on==1) && (kthithr==1) && (kthisoneon==1)) then 								
		AssignSend		        p1, 0.8, 0.2, gi08amp
		SendOut			        p1, adone, (adone*0.92)
	endif
endin ; end ins 8

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

	kthithr invalue "thirdthree"
	#include "includes/kon.inc"
	if ((gi09on==1) && (kthithr==1) && (kthisoneon==1)) then 
		AssignSend		p1, 0.1, 0.1, gi09amp
		SendOut		p1, asig*aenvL, asig*aenvR
	endif 
endin ; end instr 9

; Instrument #10 - Demonstrates the subinstr opcode.

instr 10
	ivel = p5
	ipitch = p4
	abasic subinstr "mymarimba", ivel, ipitch
	kfinthr invalue "finalthree"
	#include "includes/kon.inc"
	if ((gi10on==1) && (kfinthr==1) && (kthisoneon==1)) then 
	  AssignSendNamed		  	p1, 0.5, 0.7, gi10amp
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

instr 11 ; Elephant Drillill
	idur 	= p3
	iatt 	= (idur*0.15)
	idec  	= (idur*0.25) 
	isus	= (idur*0.25) 
	irel  	= (idur*0.35) 
	islev 	= p5/127
	;kenv	xadsr iatt, idec, islev, irel
	kenv expseg 0.001, \
	iatt, islev*0.95, \
	idec, islev*0.8, \
	isus, islev*0.9, \
	irel, 0.001 
	
	krnd random -23, 62
	kcps = p4 + krnd	;freq, random scrntchs up sound a bit

	asig	vco2  kenv, kcps
	kon invalue "allexcept"
	kfinthr invalue "finalthree"
	if ((gi11on==1) && (kfinthr==1) && ((kon==11)||(kon==0))) then 
		AssignSend		        p1, 10, 3.1, gi11amp
		SendOut			        p1, asig, asig
	endif
endin ; end ins 11

instr	12 ; WavPlayer
	iSkip   	init p2           ; inskip into file (in seconds)
	idur		= p3  
	kSpeed  	init p4           ; playback speed
	iLoop  		init 0           ; looping switch (0=off 1=on)
	iselect    = p5
		
	kenv     linseg 	0, \
						idur*.2, 3, \ ; triple vol
						idur*.4, 2, \ ; double vol
						idur*.4, 0
	
	; read audio from disk using diskin2 opcode
	if (p5 = 3) then
		a1,a2     diskin2  "sunblind-justi3.wav", kSpeed, iSkip, iLoop
	elseif (p5 = 4) then
		a1,a2     diskin2  "sunblind-justi4.wav", kSpeed, iSkip, iLoop
	endif
	;outs      a1*kenv,a2*kenv          ; send audio to outputs
	
	kfinthr invalue "finalthree"
	kon invalue "allexcept"
	if ((gi12on==1) && (kfinthr==1) && ((kon==12)||(kon==0))) then  
		AssignSend		        p1, 0.1, 0.15, gi12amp
		SendOut		        p1, a1*kenv, a2*kenv
	endif
endin ; end 12

instr	122 ; another WavPlayer
	idur		= p3  
	kSpeed  	init p4           ; playback speed
	iSkip   	init p2           ; inskip into file (in seconds)
	iLoop  		init 0           ; looping switch (0=off 1=on)
	iselect    = p5
	ixcf 		= p6
		
	kenv     linseg 0, idur*.15, 1, idur*.6, 1, idur*.25, 0
	
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
		; provides amplitude for poscil
		asig balance afil, asin
	endif
	;#include "includes/kon.inc"
	;if ((kthisoneon==1)) then
		AssignSend		        p1, 0.0001, 0.0001, 0.4
		SendOut		        p1, asig*kenv, afil2*kenv
	;endif
endin ; end 122

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
i100 0 0 200 220 0.0
; Reverb (210) to Output (220)
i100 0 0 210 220 0.28

; 200:1 MASTER EFFECT CONTROLS
; Chorus.
; Insno Start   Dur Delay   Divisor of Delay
i200   0       -1      15      10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Reverb.
; Insno Start   Dur Delay   Pitch mod   Cutoff
i210   0       -1      0.75    0.009       17000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Master output. 
i220   0       -1  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

; mycomb
i "mycomb" 	0 200 0.45

; ins 1
; simple backup flourish/highlight
; 1:1 start 40.3
; 1:2 168
#include "includes/i1sco.sco"

; ins 2
; iChan StartTime Dur Pitch Vel
; 2:1 starts 10.9
#include "includes/i2sco.sco"

; 3:1
#include "includes/i3sco.sco"

; 4:1 starts 19.5 vocal melody
; melody is here
#include "includes/i4sco.sco"

; 5:1 start 10.9 ends 209
; ins 5
#include "includes/i5sco.sco"

; ins 6
; note the chords
; 6:1 10.9
#include "includes/i6sco.sco"

; ins 7 
; is this the horn or the piano?
; 7:1 starts 10.9 ends 209.4
#include "includes/i7sco.sco"

; 8:1 starts 11.45
; ins 8
; flourishy mario paint trill
#include "includes/i8sco.sco"

; 9:1 starts 67.6 compliment to horn
; ins 9
; 9:2 98.181 
; 9:3 starts 106.9
; 9:4 137.45 
#include "includes/i9sco.sco"

; ins 10
; rapid bass drums
; 10:1 starts 2.1 ends 3:30 ins 10
; Include score for i10
#include "includes/i10sco.sco"

; 11:1 starts 63.2 ends 76
; 11:2 111.3
; ins 11
#include "includes/i11sco.sco"

; 12:1
; wav file of rendered i3
; broken into sections and
; then slightly speed up

i12 0 	 20		1.0		3
i12 20	 6		1.0		3
i12 26	 34		1.005	3
i12 60	 20		1.012	3
i12 80	  .		1.001	3
i12 100	 31		1.001	3
i12 131	 9		1.02	3
i12 140	 20		1.03	3
; i12 160	  .		1.015	3
i12 180	 20		1.0055	3
i12 200	 .		1.0		3

; 12:2
; this amplifies the vocal by laying on the wav track
i12	19.5	15		1		4
i12	26		50		1		4
i12	36		1		1.01	4
i12	58		4		1.002	4
i12	63		2.5		1.012	4
i12	66		2		1.01	4
i12	69.1  	20		1		4
i12	79		3		1.06	4
i12	85		5.5		1.032	4
i12	97.1	3		1.05	4
i12	100.1	30		1		4
i12	105.1	3.1		1.05	4
i12	120		20		1		4

i12	149.75	17.26	1		4 ; try to make this even louder
i12	149.75	17.26	1		4
i12	149.75	17.26	1.02	4

i12	168		15		1.01	4

; now with more cacophony
i122 	30		20	1	5	950
i122 	100		30	.	.	1010
i122   	120		8.9	.	.	950
i122 	162		15	.	.	650
i122 	200		10	.	.	900

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
 <bgcolor mode="background">
  <r>240</r>
  <g>221</g>
  <b>229</b>
 </bgcolor>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>firstthree</objectName>
  <x>90</x>
  <y>51</y>
  <width>20</width>
  <height>20</height>
  <uuid>{66201786-d546-446a-937a-7e60e85c3bd4}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>15</x>
  <y>50</y>
  <width>80</width>
  <height>25</height>
  <uuid>{b38dac41-5c01-4737-9d6f-63d9844400b9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>First three on?</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>secondthree</objectName>
  <x>90</x>
  <y>74</y>
  <width>25</width>
  <height>25</height>
  <uuid>{d8810105-c843-4cdb-a00e-59395f05d21b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>15</x>
  <y>76</y>
  <width>80</width>
  <height>25</height>
  <uuid>{1f2a90ac-2b60-4984-858e-548f84257725}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>2nd three on?</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>15</x>
  <y>102</y>
  <width>80</width>
  <height>25</height>
  <uuid>{159e635d-7bd6-484f-a422-0a40e471cee5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>3rd three on?</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>15</x>
  <y>126</y>
  <width>80</width>
  <height>25</height>
  <uuid>{b7aa6d02-c1d7-45f1-b319-a187059f07c6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Final three on?</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>finalthree</objectName>
  <x>92</x>
  <y>127</y>
  <width>20</width>
  <height>20</height>
  <uuid>{cf659137-9158-4fb6-9481-bba3f086ca31}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBCheckBox">
  <objectName>thirdthree</objectName>
  <x>91</x>
  <y>103</y>
  <width>20</width>
  <height>20</height>
  <uuid>{35002d2b-900a-40c6-8bab-378d98b5b042}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <selected>true</selected>
  <label/>
  <pressedValue>1</pressedValue>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBKnob">
  <objectName>bdqhornmode</objectName>
  <x>50</x>
  <y>170</y>
  <width>50</width>
  <height>50</height>
  <uuid>{40e6a407-3700-479f-b110-a819bc332bd3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>4.00000000</maximum>
  <value>1.32000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>0.01000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>148</x>
  <y>20</y>
  <width>120</width>
  <height>200</height>
  <uuid>{58499c6c-a91f-45a5-9c32-7815262070eb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>Turn off all except</label>
  <alignment>left</alignment>
  <font>Georgia</font>
  <fontsize>14</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBDropdown">
  <objectName>allexcept</objectName>
  <x>165</x>
  <y>50</y>
  <width>80</width>
  <height>30</height>
  <uuid>{d9b643a4-5f7a-4171-8f2e-242632828389}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <bsbDropdownItemList>
   <bsbDropdownItem>
    <name>_x (all on)</name>
    <value>0</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i1</name>
    <value>1</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i2</name>
    <value>2</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i3</name>
    <value>3</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i4</name>
    <value>4</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i5</name>
    <value>5</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i6</name>
    <value>6</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i7</name>
    <value>7</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i8</name>
    <value>8</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i9</name>
    <value>9</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i10</name>
    <value>10</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i11</name>
    <value>11</value>
    <stringvalue/>
   </bsbDropdownItem>
   <bsbDropdownItem>
    <name>i12</name>
    <value>12</value>
    <stringvalue/>
   </bsbDropdownItem>
  </bsbDropdownItemList>
  <selectedIndex>3</selectedIndex>
  <randomizable group="0">false</randomizable>
 </bsbObject>
</bsbPanel>
<bsbPresets>
</bsbPresets>
