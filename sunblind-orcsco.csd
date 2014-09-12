<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
;-odac     ;;;realtime audio out
;-+skip_seconds=60
;-iadc    ;;;uncomment -iadc if RT audio input is needed too
;-o sunblind.wav -W ;;; for file output any platform
-o sunblind.ogg --ogg
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 128 ;64 ;32
nchnls = 2
0dbfs = 4 ; 1 is standard and probably better advised

; Include the file with all the various tables
#include "suntables.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; U S E R - D E F I N E D   O P C O D E S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        
opcode 	AssignSend, 0, iiii
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
insno,ic,ir,id 	    xin
inum			        = floor(insno)
;print                 inum, ic, ir, id
MixerSetLevel	 	    inum, 200, ic
MixerSetLevel	 	    inum, 210, ir
MixerSetLevel	 	    inum, 220, id
endop

opcode			        SendOut, 0, iaa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
insno, aleft, aright	xin
inum                    =                       floor(insno)
MixerSend               aleft, inum, 200, 0
MixerSend               aright, inum, 200, 1
MixerSend               aleft, inum, 210, 0
MixerSend               aright, inum, 210, 1
MixerSend               aleft, inum, 220, 0
MixerSend               aright, inum, 220, 1
                        ;print                   inum
                        endop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; G L O B A L S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

giFirstThree = 1
giSecondThree = 1
giLastFive = 1
gitwothreefiveandseven=1
giFourAndNine=1
gievenon = 1
gioddon  = 1
	/***********
	/* inst 1 sco is a simple backup highlight */
gi01on = 1     *gioddon*giFirstThree
	
	/***********
	/* inst 2 sco is a faint tap percusive 
	/* in time with i3 */
gi02on = 1     *gievenon*gitwothreefiveandseven*giFirstThree

	/***********
	/* inst 3 sco is a repetative rhythm */
	/* in time with i2 */
gi03on = 1     *gioddon*gitwothreefiveandseven*giFirstThree 

	/***********
	/* inst 4 sco is a vocal? */
gi04on = 1     *gievenon*giSecondThree*giFourAndNine

	/***********
	/* inst 5 sco is */
gi05on = 1     *gioddon*gitwothreefiveandseven*giSecondThree

	/***********
	/* inst 6 sco is */
gi06on = 1     *gievenon*giSecondThree

	/***********
	/* inst 7 sco is percusive? */
gi07on = 1     *gioddon*gitwothreefiveandseven*giLastFive

	/***********
	/* inst 8 sco is flourishy mario paint trill */
gi08on = 1     *gievenon*giLastFive

	/***********
	/* inst 9 sco is */
gi09on = 1     *gioddon*giLastFive*giFourAndNine

	/***********
	/* inst 10 sco is rapid drums */
gi10on = 1     *gievenon*giLastFive

	/***********
	/* inst 11 sco is */
gi11on = 1    *gioddon*giLastFive

giamp   = 0.27
gi01amp = giamp + 0.73
gi02amp = giamp
gi03amp = giamp - 0.2
gi04amp = giamp - 0.10
gi05amp = giamp + 0.30
gi06amp = giamp
gi07amp = giamp + 1.5 ; can we turn it up beyond 1? yes
gi08amp = giamp - 0.15
gi09amp = giamp
gi10amp = giamp + 0.1
gi11amp = giamp - 0.22

gicount = 0 ; I don't know how to do a counter without a global var
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I N S T R U M E N T   D E F I N I T I O N S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; M I X E R   L E V E L S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

instr 100 ; Mixer level
	isend=p4
	ibuss0=p5
	igain0=p6
	MixerSetLevel           isend, ibuss0, igain0
endin

instr 1 ; moog!
kfreq  = p4
kfiltq = p5/141
kfiltrate = 0.0002
kvibf  = 5
kvamp  = .01
;low volume is needed
asig moog .15, kfreq, kfiltq, kfiltrate, kvibf, kvamp, gimandpluk, giimpuls20, gisine
     ;outs asig, asig
; Hans Mikelson cos pan left to right
kpan      linseg     0, p3, 1 ;move left to right
kpan  =  kpan*$M_PI_2 ;range 0-1 becomes 0-pi/2
kpanl     =          cos(kpan)
kpanr     =          sin(kpan)
if (gi01on==1) then
	AssignSend	p1, 0.2, 0.45, gi01amp
	SendOut	 p1, asig*kpanl, asig*kpanr
endif
endin ; end ins 1

instr 2 ; Sine Wave Bass Line 
ifreq = (p4 > 15 ? p4 : cpspch(p4))

imyrandom random 0, 9
irdvd = imyrandom / 4
iroll = (60 * int(irdvd)) ; %20 of the time output is 100, %40 out is 50, %40 out is 0
;printks2 "Roll! A fifth of the time it is 120, 2/5 out is 60, and 2/5 out is 0. And the roll is %d!!\n", iroll
ifreq = ifreq + iroll

iamp = ampdb((p5-40)/2)
kenv adsr 0.1, 0.1, 0.5, 0.3
aout init 0
ksampnum init 0
kcount = 0
iperiod = sr / ifreq
i2pi = 3.14159 * 2

loopStart:
	kphase = (ksampnum % iperiod) / iperiod
	knewval = sin(kphase * i2pi)
	vaset knewval, kcount,aout
	ksampnum = ksampnum + 1

loop_lt kcount, 1, ksmps, loopStart

iTurnItDown = 0.02
aout = aout*iamp*kenv*iTurnItDown

if (gi02on==1) then  
						;insno,ic,ir,id 	
AssignSend		        p1, 0.3, 0.2, gi02amp
SendOut			        p1, aout, aout
endif
endin ; end ins 2

instr 3 ; Sweepy 
  idur     = p3
  ibegfreq = 1 ; bgng of sweep freq
  iendfreq = p4*2 ; ending of sweep frequency
  ibw      = 70  ; bandwidth of filters in Hz
  ifreq    = 200 ; frequency of gbuzz that is to be filtered
  iamp     = (p5/127)*2 ; amplitude to scale output by
  ires     = 1 ; coefficient to scale amount of reson in output
  iresr    = 1 ; coefficient to scale amount of resonr in output
 ; Frequency envelope for reson cutoff
  kfreq    linseg ibegfreq, idur * .5, iendfreq, idur * .5, ibegfreq
 ; Amplitude envelope to prevent clicking
  kenv     linseg 0, .05, iamp, idur - .1, iamp, .05, 0
  iharms   = (sr*.4)/ifreq  ; Num of harmonics for gbuzz scaled to avoid aliasing
  asig     gbuzz 1, ifreq, iharms, 1, .9, 1 ; "Sawtooth" waveform
  ain      = kenv * asig  ; output scaled by amp envelope
  ares     reson ain, kfreq, ibw, 1
  aresr    resonr ain, kfreq, ibw, 1
  iampRrnd random 0.1, 0.9
  iampR =   1 - iampRrnd

aoutL init 0
aoutR init 0  
;reson Left
aoutL = ares * ires
;resonr Right 
aoutR = aresr * iresr * iampR
;outs asig, asig
if (gi03on==1) then  
	AssignSend		        p1, 25, 0.15, gi03amp
	SendOut			        p1, aoutL, aoutR
endif
endin ; end 3

instr 4 ; bzzy
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

instr 5   ; Rhodes elec piano
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rhodes elec piano model originally by Perry Cook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pset           0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0

;cigoto condition, label 
cigoto (p5==127), dorand  
  igoto norand

dorand:
  ikrnd random -4, 6
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

instr 6 ; synth
;;;;;;;;;;;;;;;
/*  modal synthesis using biquad filters as oscillators
    Example by Scott Lindroth 2007 */

    ipi = 3.1415926
    idenom = sr*0.5
    ipulseSpd = 0
    icps     = p4
    ipan = 0.5
    iamp    = 0.00003
    iModes = 2
    ifilterw = p5

    apulse    mpulse iamp, 0

    ;icps    = cpspch( icps )

    ; filter gain
    iamp1 = 600 
    iamp2 = 800 +  ifilterw
    iamp3 = 900 +  ifilterw
    iamp4 = 800 +  ifilterw
    iamp5 = 700
    iamp6 = 500

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

asin1     biquad  apulse * iamp1, 1, 0, -1, 1, ib11, ib21
asin2       biquad  apulse * iamp2, 1, 0, -1, 1, ib12, ib22
asin3       biquad  apulse * iamp3, 1, 0, -1, 1, ib13, ib23
asin4       biquad  apulse * iamp4, 1, 0, -1, 1, ib14, ib24
asin5       biquad  apulse * iamp5, 1, 0, -1, 1, ib15, ib25
asin6       biquad  apulse * iamp6, 1, 0, -1, 1, ib16, ib26
afin = (asin1 + asin2 + asin3 + asin4 + asin5 + asin6)

    ;outs        
    aL = afin*sqrt(ipan)
    aR = afin*sqrt(1- ipan)
if (gi06on==1) then    
  AssignSend		        p1, 0.2, 0.2, gi06amp
  SendOut			        p1, aL, aR
endif
endin ; end ins 6

instr 7 ; polynomial nonlinear phasor
;This instrument crossfades between a 
;pure sine and one distorted with x^11

idur   = p3
ivelval = p5/127.9 ; turn vel into a value < 1
iamp   = ivelval 
ifreq  = p4
itableA = giffitch3 
itableB = gitonewheel3

aenv	linseg	0, .001, iamp, idur - .051, iamp, .05, 0	; declicking envelope
aosc	phasor	ifreq	; create a linear phasor
aout3	tablei	aosc, itableA, 1 ; read a sine table without the linear phasor
apd11	polynomial aosc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ; distort the phasor with x^11
aout11	tablei	apd11, itableA, 1	 ; read a sine table with the nonlinear phasor
aout3B	tablei	aosc, itableB, 1 ; read a sine table without the linear phasor
aout11B	tablei	apd11, itableB, 1	 ; read a sine table with the nonlinear phasor
kamount	linseg	1.0, 0.05, 0.9, 1.0, 0.0	 ; crossfade between two outputs

aoA	= aout3*kamount + aout11*(1.0 - kamount)
aoB = aout3B*kamount + aout11B*(1.0 - kamount)
aout = (aoA*0.699)+(aoB*0.299)

irando = birnd(0.25)+0.5 ; generate a random number from 0.25 to 0.75
irandoInverted = 1 - irando

;aL = aenv*aout*iamp*irando
; take iamp out below because iamp is
; now in the envelope aenv
aL = aenv*aout*irando
aR = aenv*aout*irandoInverted

if (gi07on==1) then  
        AssignSend		        p1, 0.09, 0.09, gi07amp
        SendOut			    p1, aL, aR
endif
endin ; end ins 7

instr 8 ; powershape
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

instr 9 ; partikkel
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

instr 10 ; marimba
  ifreq = p4
  ihrd = 0.1
  ipos = 0.561
  imp = gimarmstk1
  kvibf = 6.0
  kvamp = 0.05
  ivibfn = gisine
  idec = 0.6
  idoubles = (p5/(127*2))*90
  itriples = (p5/(127*4))*90

  a1 marimba 2, ifreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec, idoubles, itriples

;outs a1, a1
if (gi10on==1) then
						;insno,ic,ir,id 						
AssignSend		        p1, 0.05, 0.15, gi10amp
SendOut			        p1, a1, a1
endif
endin ; ins 10

instr 11 ; drill trill
idur = p3

iatt = (idur*0.01)
iremainingafteratt= (idur-iatt)
irel  = (iremainingafteratt*0.01) 
irem = (iremainingafteratt - irel)
idec  = irem * 0.5  
islev = p5/127
kenv	xadsr iatt, idec, islev, irel

krnd random -25, 65
;krnd random 0, 4
kcps = p4 + krnd	;freq, random scrntchs up sound a bit

iunwise = (p5*0.01)
kmod = iunwise - 0.1
;print iunwise
asigL foscil iunwise, kcps, 1, kmod, kenv, 1
asigR	vco2  kenv * iunwise, kcps
if (gi11on==1) then
	AssignSend		        p1, 0.9, 0.2, gi11amp
	SendOut			        p1, asigL, asigR
endif
endin ; end ins 11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; B U S S   E F F E C T S 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

instr 200               ; Chorus by J. Lato
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; p4 = delay in milliseconds
; p5 = divisor of p4
; Chorus effect from http://www.jlpublishing.com/Csound.htm
; Some of its parameters are accesible through the score.
a1 MixerReceive            200, 0
a2 MixerReceive            200, 1
idlyml=p4      ;delay in milliseconds
k1                      poscil                  idlyml/p5, 1, 2
ar1l vdelay3   a1, idlyml/5+k1, 900    ;delayed sound 1
ar1r                    vdelay3                 a2, idlyml/5+k1, 900    ;delayed sound 1
k2                      poscil                  idlyml/p5, .995, 2
ar2l                    vdelay3                 a1, idlyml/5+k2, 700    ;delayed sound 2
ar2r                    vdelay3                 a2, idlyml/5+k2, 700    ;delayed sound 2
k3                      poscil                  idlyml/p5, 1.05, 2
ar3l                    vdelay3                 a1, idlyml/5+k3, 700    ;delayed sound 3
ar3r                    vdelay3                 a2, idlyml/5+k3, 700    ;delayed sound 3
k4                      poscil                  idlyml/p5, 1, 2
ar4l                    vdelay3                 a1, idlyml/5+k4, 900    ;delayed sound 4
ar4r                    vdelay3                 a2, idlyml/5+k4, 900    ;delayed sound 4
aoutl                   = (a1+ar1l+ar2l+ar3l+ar4l)*.5
aoutr                   = (a2+ar1r+ar2r+ar3r+ar4r)*.5
                        ; To the reverb unit
                        MixerSend               aoutl, 200, 210, 0
                        MixerSend               aoutr, 200, 210, 1
                        ; To the output mixer
                        MixerSend               aoutl, 200, 220, 0
                        MixerSend               aoutr, 200, 220, 1
                        endin

instr 210 ; Reverb by Sean Costello / J. Lato
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
idelay =                       p4      
ipitchmod =                       p5  
icutoff =                       p6              
ainL                    MixerReceive            210, 0
ainR                    MixerReceive            210, 1
aoutL, aoutR            reverbsc                ainL, ainR, idelay, icutoff, sr, ipitchmod, 0
                        ; To the master output.
                        MixerSend               aoutL, 210, 220, 0
                        MixerSend               aoutR, 210, 220, 1
                        endin
                        
instr 220               ; Master output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; p4 = level
; p5 = fadein + fadeout
; Applies a bass enhancement, compression and fadeout
; to the whole piece, outputs signals, and clears the mixer.
; Receive audio from the master mixer buss.
a1                      MixerReceive            220, 0
a2                      MixerReceive            220, 1
                        ; Enhance the bass.
al1                     butterlp                a1, 100
al2                     butterlp                a2, 100
a1                      =                       al1 * 1.5 + a1
a2                      =                       al2 * 1.5 + a2
                        ; Remove DC bias.
a1blocked               dcblock                 a1
a2blocked               dcblock                 a2
                        ; Apply compression.
a1 compress                a1, a1, 0, 48, 60, 3, .01, .05, .05
a2 compress                a2, a2, 0, 48, 60, 3, .01, .05, .05
                        ; Output audio.
outs                    a1blocked, a2blocked
; Reset the busses for the next kperiod.
MixerClear
endin

</CsInstruments>
<CsScore>
t 0 64

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

; MASTER EFFECT CONTROLS
; Chorus.
; Insno Start   Dur Delay   Divisor of Delay
i 200   0       -1      10      30
; Reverb.
; Insno Start   Dur Delay   Pitch mod   Cutoff
i 210   0       -1      0.74    0.007       17000
; Master output.
; Insno Start   Dur   
i 220   0       -1      

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
i1	53.318141	1.227664	554.300  	124
i1	53.318141	1.227664	659.217924	119
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
i1	68.590930	1.227438	740.0       	102
i1	68.590930	1.227438	880.00   	113
i1	68.590930	1.227438	554.300  	102
i1	69.818141	0.954875	830.585965	119
i1	69.818141	0.954875	987.738739	127
i1	69.818141	0.954875	622.253965	124
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
i1	78.545578	0.954649	622.253965	124
i1	79.500000	1.227664	880.00   	109
i1	79.500000	1.227664	554.333990	127
i1	80.727438	0.954649	493.869370	127
i1	80.727438	0.954649	830.585965	104
i1	79.500000	3.409297	659.217924	127
i1	81.681859	1.227438	554.333990	124
i1	81.681859	1.227438	880.00   	119
i1	82.909070	0.954875	622.253965	124
i1	82.909070	0.954875	987.738739	124
i1	82.909070	0.954875	740.0      112
i1	83.863719	1.227438	880.00   	102
i1	83.863719	1.227438	554.333990	124
i1	83.863719	1.227438	659.217924	119
i1	85.090930	0.954875	493.869370	127
i1	85.090930	0.954875	830.585965	127
i1	85.090930	2.18     	659.217924	127
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
i1	90.409070	1.227664	880.00   	119
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
i1	94.772789	1.227438	554.3   	113
i1	96.000000	0.954875	740.0      119
i1	96.000000	0.954875	987.738739	127
i1	96.000000	0.954875	622.253965	124
i1	96.954649	1.227438	880.00   	109
i1	96.954649	1.227438	554.333990	127
i1	98.181859	0.954875	493.869370	127
i1	98.181859	0.954875	830.585965	104
i1	96.954649	3.409297	659.217924	127
i1	99.136508	1.227438	554.333990	124
i1	99.136508	1.227438	880.00   	119
i1	100.363719	0.954875	622.253965	124
i1	100.363719	0.954875	987.738739	124
i1	100.363719	0.954875	740.0      113
i1	101.318367	1.227438	880.00   	102
i1	101.318367	1.227438	554.333990	124
i1	101.318367	1.227438	659.217924	119
i1	102.545578	0.954649	493.869370	127
i1	102.545578	0.954649	830.585965	127
i1	102.545578	2.21     	659.217924	127
i1	103.500000	1.227664	880.00   	102
i1	103.500000	1.227664	554.333990	113
i1	104.727438	0.954649	740.0      119
i1	104.727438	0.954649	987.738739	127
i1	104.727438	0.954649	622.253965	124
i1	105.681859	1.227438	880.00   	109
i1	105.681859	1.227438	554.333990	127
i1	106.909070	0.954875	493.869370	127
i1	106.909070	0.954875	830.585965	104
i1	105.681859	3.409297	659.217924	127
i1	107.863719	1.227438	554.333990	124
i1	107.863719	1.227438	880.00   	119
i1	109.090930	0.954875	622.253965	124
i1	109.090930	0.954875	987.738739	124
i1	109.090930	0.954875	740.0      113
i1	110.045578	1.227438	880.00   	102
i1	110.045578	1.227438	554.333990	124
i1	110.045578	1.227438	659.217924	119
i1	111.272789	2.21     	830.585965	127
i1	111.272789	2.21     	740.0      126
i1	111.272789	2.21     	987.738739	127
i1	111.272789	2.21     	622.253965	127
i1	114.272789	0.829705	493.869370	119
i1	113.454649	2.21     	554.333990	125
i1	113.454649	2.19     	880.00   	125
i1	113.454649	2.19     	659.217924	125
i1	113.454649	2.19     	740.0      125
i1	115.090930	2.727664	415.292983	127
i1	115.636508	2.21     	830.585965	127
i1	115.636508	2.19     	740.0      124
i1	115.636508	2.19     	987.738739	127
i1	115.636508	2.19     	622.253965	127
i1	117.80  	0.818367	554.333990	125
i1	117.80  	1.091156	369.994421	99
i1	117.80  	2.18    	880.00   	125
i1	117.80  	2.19    	659.2    	125
i1	117.80  	2.20    	740.0      125
i1	118.636508	1.363719	554.333990	122
i1	118.90  	0.545578	493.869370	127
i1	119.454649	2.727438	415.292983	115
i1	120.000000	2.21     	830.585965	127
i1	120.000000	2.19     	740.0      124
i1	120.000000	2.18     	987.738739	127
i1	120.000000	2.18     	622.253965	127
i1	122.181859	2.21     	369.994421	99
i1	122.181859	2.21     	554.333990	125
i1	122.181859	2.05     	880.00   	125
i1	122.181859	2.18     	659.217924	125
i1	122.181859	2.21     	740.0      124
i1	124.363719	0.954875	659.217924	127
i1	124.363719	0.954875	493.869370	127
i1	124.363719	0.954875	830.585965	127
i1	125.318367	1.227438	740.0      101
i1	125.318367	1.227438	880.00   	113
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
i1	131.863719	1.227438	740.0      127
; 1:2
i1	168.0   	2.18185 	830.585965	127
i1	168.1   	2.1818  	740.0      126
i1	168.2   	2.181   	987.738739	127
i1	168.3   	2.18    	622.253965	127
i1	171.000227	0.83    	493.869370	119
i1	170.181859	2.05     	554.333990	125
i1	170.181859	2.05     	880.00   	125
i1	170.181859	2.21     	659.217924	125
i1	170.181859	2.21     	740.0      126
i1	171.818367	2.727438	415.292983	127
i1	172.363719	2.21     	830.585965	127
i1	172.363719	2.21     	740.0      127
i1	172.363719	2.21     	987.738739	127
i1	172.363719	2.05     	622.253965	127
i1	174.545578	0.818367	554.333990	125
i1	174.545578	1.091156	369.994421	99
i1	175.636508	0.545578	493.869370	127
i1	174.545578	2.18     	880.00   	125
i1	174.545578	2.18     	659.217924	125
i1	174.545578	2.21     	740.0      125
i1	175.363719	1.363946	554.333990	122
i1	176.181859	2.727664	415.292983	115
i1	176.727438	2.21     	830.585965	127
i1	176.727438	2.21     	740.0      124
i1	176.727438	2.05     	987.738739	127
i1	176.727438	2.18     	622.253965	127
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
i1	200.727438	0.954875	987.738739	127
i1	200.727438	0.954875	622.253965	124
i1	201.682086	1.227438	880.00   	109
i1	201.682086	1.227438	554.333990	127
i1	201.682086	1.227438	740.0      124
i1	202.909297	0.954649	659.217924	127
i1	202.909297	0.954649	493.869370	127
i1	202.909297	0.954649	830.585965	127
i1	203.863719	1.227664	740.0      101
i1	203.863719	1.227664	880.00   	113
i1	203.863719	1.227664	554.333990	102
i1	205.091156	1       	830.6   	119
i1	205.091156	1       	987.7   	125
i1	205.091156	1       	622.3   	124
i1	206.045578	1.227438	880.00   	109
i1	206.045578	1.227438	554.333990	127
i1	206.045578	1.227438	740.0      125
i1	207.27   	2.05     	493.869370	127
i1	207.27   	2.18     	659.217924	127
i1	207.273 	2.21     	415.292983	124
i1	209.45  	0.273   	659.217924	127
i1	209.4649	0.273   	415.292983	127
i1	209.47  	0.273   	493.869370	127


; ins 2
; iChan StartTime Dur Pitch Vel
; 2:1 starts 10.9

i2	10.909070	0.136508	41.2     	127
i2	11.181859	0.136508	41.2     	123
i2	11.454649	0.272789	41.2     	127
i2	11.863719	0.136508	55.000000	127
i2	12.136281	0.136735	55.000000	117
i2	12.409070	0.136735	55.000000	114
i2	12.545578	0.272789	55.000000	123
i2	13.090930	0.136508	61.733671	127
i2	13.363719	0.136508	61.733671	127
i2	13.636281	0.273016	61.733671	127
i2	14.045578	0.136508	55.000000	127
i2	14.318141	0.136735	55.000000	127
i2	14.590930	0.136508	55.000000	127
i2	14.727211	0.273016	55.000000	127
i2	15.272789	0.136508	41.2     	127
i2	15.545578	0.136508	41.2     	123
i2	15.818141	0.273016	41.2     	127
i2	16.227211	0.136735	55.000000	127
i2	16.500000	0.136508	55.000000	117
i2	16.772789	0.136508	55.000000	114
i2	16.909070	0.273016	55.000000	123
i2	17.454649	0.136508	61.733671	127
i2	17.727211	0.136735	61.733671	127
i2	18.000000	0.273016	61.733671	127
i2	18.409070	0.136735	55.000000	127
i2	18.681859	0.137   	55.000000	127
i2	18.954649	0.137   	55.000000	127
i2	19.090930	0.273016	55.000000	127
i2	19.636281	0.136735	41.2     	127
i2	19.909070	0.136735	41.2     	123
i2	20.181859	0.273016	41.2     	127
i2	20.590930	0.136508	55.000000	127
i2	20.863719	0.136508	55.000000	117
i2	21.136281	0.136735	55.000000	114
i2	21.272789	0.273016	55.000000	123
i2	21.818141	0.136735	61.733671	127
i2	22.090930	0.136508	61.733671	127
i2	22.363719	0.272789	61.733671	127
i2	22.772789	0.136508	55.000000	127
i2	23.045578	0.136508	55.000000	127
i2	23.318141	0.136735	55.000000	127
i2	23.454649	0.272789	55.000000	127
i2	24.000000	0.136508	41.2     	127
i2	24.272789	0.136508	41.2     	123
i2	24.545578	0.272789	41.2     	127
i2	24.90   	0.136508	55.000000	127
i2	25.20   	0.136735	55.000000	117
i2	25.50   	0.136508	55.000000	114
i2	25.64   	0.273016	55.000000	123
i2	26.18   	0.136508	61.733671	127
i2	26.454649	0.136508	61.733671	127
i2	26.727211	0.273016	61.733671	127
i2	27.136281	0.136735	55.000000	127
i2	27.409070	0.136735	55.000000	127
i2	27.681859	0.136508	55.000000	127
i2	27.818141	0.273016	55.000000	127
i2	28.363719	0.136508	41.2     	127
i2	28.636281	0.136735	41.2     	123
i2	28.909070	0.273016	41.2     	127
i2	29.318141	0.136735	55.000000	127
i2	29.590930	0.136508	55.000000	117
i2	29.863719	0.136508	55.000000	114
i2	30.000000	0.273016	55.000000	123
i2	30.545578	0.136508	61.733671	127
i2	30.818141	0.136735	61.733671	127
i2	31.090930	0.273016	61.733671	127
i2	31.500000	0.136508	55.000000	127
i2	31.772789	0.136508	55.000000	127
i2	32.045578	0.136508	55.000000	127
i2	32.181859	0.273016	55.000000	127
i2	32.727211	0.136735	41.2     	127
i2	33.000000	0.136508	41.2     	123
i2	33.272789	0.273016	41.2     	127
i2	33.681859	0.136508	55.000000	127
i2	33.954649	0.136508	55.000000	117
i2	34.227211	0.136735	55.000000	114
i2	34.363719	0.272789	55.000000	123
i2	34.909070	0.136735	61.733671	127
i2	35.181859	0.136508	61.733671	127
i2	35.454649	0.272789	61.733671	127
i2	35.863719	0.136508	55.000000	127
i2	36.136281	0.136735	55.000000	127
i2	36.409070	0.136735	55.000000	127
i2	36.545578	0.272789	55.000000	127
i2	37.090930	0.136508	41.2     	127
i2	37.363719	0.136508	41.2     	123
i2	37.636508	0.272789	41.2     	127
i2	38.045578	0.136508	55.000000	127
i2	38.318141	0.136735	55.000000	117
i2	38.590930	0.136508	55.000000	114
i2	38.727211	0.273016	55.000000	123
i2	39.272789	0.136508	61.733671	127
i2	39.545578	0.136508	61.733671	127
i2	39.818141	0.273016	61.733671	127
i2	40.227211	0.136735	55.000000	127
i2	40.500000	0.136735	55.000000	127
i2	40.772789	0.136508	55.000000	127
i2	40.909070	0.273016	55.000000	127
i2	41.454649	0.136508	41.2     	127
i2	41.727211	0.136735	41.2     	123
i2	42.000000	0.273016	41.2     	127
i2	42.409070	0.136735	55.000000	127
i2	42.681859	0.136508	55.000000	117
i2	42.954649	0.136508	55.000000	114
i2	43.090930	0.273016	55.000000	123
i2	43.636508	0.136508	61.733671	127
i2	43.909070	0.136735	61.733671	127
i2	44.181859	0.273016	61.733671	127
i2	44.590930	0.136508	55.000000	127
i2	44.863719	0.136508	55.000000	127
i2	45.136508	0.136508	55.000000	127
i2	45.272789	0.273016	55.000000	127
i2	45.818141	0.136735	41.2     	127
i2	46.090930	0.136508	41.2     	123
i2	46.363719	0.273016	41.2     	127
i2	46.772789	0.136508	55.000000	127
i2	47.045578	0.136508	55.000000	117
i2	47.318141	0.136735	55.000000	114
i2	47.454649	0.272789	55.000000	123
i2	48.000000	0.136735	61.733671	127
i2	48.272789	0.136508	61.733671	127
i2	48.545578	0.272789	61.733671	127
i2	48.954649	0.136508	55.000000	127
i2	49.227211	0.136735	55.000000	127
i2	49.500000	0.136735	55.000000	127
i2	49.636508	0.272789	55.000000	127
i2	50.181859	0.136508	41.2     	127
i2	50.454649	0.136508	41.2     	123
i2	50.727211	0.273016	41.2     	127
i2	51.136508	0.136508	55.0    	127
i2	51.409070	0.136735	55.0    	117
i2	51.681859	0.136508	55.0    	114
i2	51.818141	0.273016	55.0    	123
i2	52.363719	0.136508	61.733671	127
i2	52.636508	0.136508	61.733671	127
i2	52.909070	0.273016	61.733671	127
i2	53.318141	0.136735	55.000000	127
i2	53.590930	0.136508	55.000000	127
i2	53.863719	0.136508	55.000000	127
i2	54.000000	0.273016	55.000000	127
i2	54.545578	0.136508	61.733671	127
i2	54.818141	0.136735	61.733671	123
i2	55.090930	0.273016	61.733671	127
i2	55.363719	0.136508	55.000000	98
i2	55.500000	0.136735	61.733671	127
i2	55.772789	0.136508	61.733671	117
i2	56.045578	0.136508	61.733671	114
i2	56.181859	0.409297	61.733671	123
i2	56.727211	0.136735	55.000000	127
i2	57.000000	0.136735	55.000000	127
i2	57.272789	0.273016	55.000000	127
i2	57.545578	0.136508	41.2     	101
i2	57.681859	0.136508	55.000000	127
i2	57.954649	0.136508	55.000000	127
i2	58.227211	0.136735	55.000000	127
i2	58.363719	0.136508	55.000000	127
i2	58.500000	0.136735	51.911623	104
i2	58.636508	0.136508	55.000000	72
i2	58.772789	0.136508	58.267183	98
i2	58.909070	0.136735	61.733671	127
i2	59.181859	0.136508	61.733671	123
i2	59.454649	0.272789	61.733671	127
i2	59.727211	0.136735	55.000000	98
i2	59.863719	0.136508	61.733671	127
i2	60.136508	0.136508	61.733671	117
i2	60.409070	0.136735	61.733671	114
i2	60.545578	0.409297	61.733671	123
i2	61.090930	0.136508	55.000000	127
i2	61.363719	0.136508	55.000000	127
i2	61.636508	0.272789	55.000000	127
i2	61.909070	0.136735	41.2     	101
i2	62.045578	0.136508	55.000000	127
i2	62.318141	0.136735	55.000000	127
i2	62.590930	0.136735	55.000000	127
i2	62.727438	0.204762	55.000000	127
i2	63.000000	0.136735	55.000000	72
i2	63.136508	0.136508	58.267183	98
i2	63.272789	0.136508	61.733671	127
i2	63.545578	0.136508	61.733671	123
i2	63.818141	0.273016	61.733671	127
i2	64.090930	0.136735	55.000000	98
i2	64.227438	0.136508	61.733671	127
i2	64.500000	0.136735	61.733671	117
i2	64.772789	0.136508	61.733671	114
i2	64.9	   0.4    	61.7       	123
i2	65.454649	0.136508	55.000000	127
i2	65.727438	0.136508	55.000000	127
i2	66.000000	0.273016	55.000000	127
i2	66.272789	0.136508	41.2     	101
i2	66.409070	0.136735	55.000000	127
i2	66.681859	0.136508	55.000000	127
i2	66.954649	0.136508	55.000000	127
i2	67.090930	0.341270	55.000000	127
i2	67.500000	0.136735	55.000000	113
i2	67.636508	0.136508	41.2     	127
i2	67.909070	0.136735	41.2     	123
i2	68.181859	0.273016	41.2     	127
i2	68.590930	0.136735	55.000000	127
i2	68.863719	0.136508	55.000000	117
i2	69.136508	0.136508	55.000000	114
i2	69.272789	0.273016	55.000000	123
i2	69.818141	0.136735	61.7    	127
i2	70.090930	0.136735	61.7    	127
i2	70.363719	0.273016	61.7    	127
i2	70.772789	0.136508	55.000000	127
i2	71.045578	0.136508	55.000000	127
i2	71.318141	0.136735	55.000000	127
i2	71.454649	0.273016	55.000000	127
i2	72.000000	0.136735	41.2     	127
i2	72.272789	0.136508	41.2     	123
i2	72.545578	0.272789	41.2     	127
i2	72.954649	0.136508	55.000000	127
i2	73.227438	0.136508	55.000000	117
i2	73.500000	0.136735	55.000000	114
i2	73.636508	0.272789	55.000000	123
i2	74.181859	0.136508	61.733671	127
i2	74.454649	0.136508	61.733671	127
i2	74.727438	0.272789	61.733671	127
i2	75.136508	0.136508	55.000000	127
i2	75.409070	0.136735	55.000000	127
i2	75.681859	0.136508	55.000000	127
i2	75.818141	0.273016	55.000000	127
i2	76.363719	0.136508	41.2     	127
i2	76.636508	0.136508	41.2     	123
i2	76.909070	0.273016	41.2     	127
i2	77.318141	0.136735	55.000000	127
i2	77.590930	0.136735	55.000000	117
i2	77.863719	0.136508	55.000000	114
i2	78.000000	0.273016	55.000000	123
i2	78.545578	0.136508	61.733671	127
i2	78.818141	0.136735	61.733671	127
i2	79.090930	0.273016	61.733671	127
i2	79.500000	0.136735	55.000000	127
i2	79.772789	0.136508	55.000000	127
i2	80.045578	0.136508	55.000000	127
i2	80.181859	0.273016	55.000000	127
i2	80.727438	0.136508	41.2     	127
i2	81.000000	0.136735	41.2     	123
i2	81.272789	0.273016	41.2     	127
i2	81.681859	0.136508	55.000000	127
i2	81.954649	0.136508	55.000000	117
i2	82.227438	0.136508	55.000000	114
i2	82.363719	0.273016	55.000000	123
i2	82.909070	0.136735	61.733671	127
i2	83.181859	0.136508	61.733671	127
i2	83.454649	0.273016	61.733671	127
i2	83.863719	0.136508	55.000000	127
i2	84.136508	0.136508	55.000000	127
i2	84.409070	0.136735	55.000000	127
i2	84.545578	0.272789	55.000000	127
i2	85.090930	0.136735	41.2     	127
i2	85.363719	0.136508	41.2     	123
i2	85.636508	0.272789	41.2     	127
i2	86.045578	0.136508	55.000000	127
i2	86.318141	0.136735	55.000000	117
i2	86.590930	0.136735	55.000000	114
i2	86.727438	0.272789	55.000000	123
i2	87.272789	0.136508	61.733671	127
i2	87.545578	0.136508	61.733671	127
i2	87.818367	0.272789	61.733671	127
i2	88.227438	0.136508	55.000000	127
i2	88.500000	0.136735	55.000000	127
i2	88.772789	0.136508	55.000000	127
i2	88.909070	0.273016	55.000000	127
i2	89.454649	0.136508	41.2     	127
i2	89.727438	0.136508	41.2     	123
i2	90.000000	0.273016	41.2     	127
i2	90.409070	0.136735	55.000000	127
i2	90.681859	0.136735	55.000000	117
i2	90.954649	0.136508	55.000000	114
i2	91.090930	0.273016	55.000000	123
i2	91.636508	0.136508	61.733671	127
i2	91.909070	0.136735	61.733671	127
i2	92.181859	0.273016	61.733671	127
i2	92.590930	0.136735	55.000000	127
i2	92.863719	0.136508	55.000000	127
i2	93.136508	0.136508	55.000000	127
i2	93.272789	0.273016	55.000000	127
i2	93.818367	0.136508	41.2     	127
i2	94.090930	0.136735	41.2     	123
i2	94.363719	0.273016	41.2     	127
i2	94.772789	0.136508	55.000000	127
i2	95.045578	0.136508	55.000000	117
i2	95.318367	0.136508	55.000000	114
i2	95.454649	0.273016	55.000000	123
i2	96.000000	0.136735	61.733671	127
i2	96.272789	0.136508	61.733671	127
i2	96.545578	0.273016	61.733671	127
i2	96.954649	0.136508	55.000000	127
i2	97.227438	0.136508	55.000000	127
i2	97.500000	0.136735	55.000000	127
i2	97.636508	0.272789	55.000000	127
i2	98.181859	0.136735	41.2     	127
i2	98.454649	0.136508	41.2     	123
i2	98.727438	0.272789	41.2     	127
i2	99.136508	0.136508	55.000000	127
i2	99.409070	0.136735	55.000000	117
i2	99.681859	0.136735	55.000000	114
i2	99.818367	0.272789	55.000000	123
i2	100.363719	0.136508	61.733671	127
i2	100.636508	0.136508	61.733671	127
i2	100.909070	0.273016	61.733671	127
i2	101.318367	0.136508	55.000000	127
i2	101.590930	0.136735	55.000000	127
i2	101.863719	0.136508	55.000000	127
i2	102.000000	0.273016	55.000000	127
i2	102.545578	0.136508	41.2     	127
i2	102.818367	0.136508	41.2     	123
i2	103.090930	0.273016	41.2     	127
i2	103.500000	0.136735	55.000000	127
i2	103.772789	0.136508	55.000000	117
i2	104.045578	0.136508	55.000000	114
i2	104.181859	0.273016	55.000000	123
i2	104.727438	0.136508	61.733671	127
i2	105.000000	0.136735	61.733671	127
i2	105.272789	0.273016	61.733671	127
i2	105.681859	0.136735	55.000000	127
i2	105.954649	0.136508	55.000000	127
i2	106.227438	0.136508	55.000000	127
i2	106.363719	0.273016	55.000000	127
i2	106.909070	0.136735	41.2     	127
i2	107.181859	0.136735	41.2     	123
i2	107.454649	0.273016	41.2     	127
i2	107.863719	0.136508	55.000000	127
i2	108.136508	0.136508	55.000000	117
i2	108.409070	0.136735	55.000000	114
i2	108.545578	0.273016	55.000000	123
i2	109.090930	0.136735	61.733671	127
i2	109.363719	0.136508	61.733671	127
i2	109.636508	0.272789	61.733671	127
i2	110.045578	0.136508	55.000000	127
i2	110.318367	0.136508	55.000000	127
i2	110.6   	0.136735	55.000000	127
i2	110.7   	0.272789	55.000000	127
i2	111.272789	0.136735	61.733671	127
i2	111.545578	0.136508	61.733671	123
i2	111.818367	0.272789	61.733671	127
i2	112.090930	0.136735	55.000000	98
i2	112.227438	0.136508	61.733671	127
i2	112.500000	0.136735	61.733671	117
i2	112.772789	0.136735	61.733671	114
i2	112.909297	0.409297	61.733671	123
i2	113.454649	0.136508	55.000000	127
i2	113.727438	0.136508	55.000000	127
i2	114.000000	0.273016	55.000000	127
i2	114.272789	0.136735	41.2     	101
i2	114.409297	0.136508	55.000000	127
i2	114.681859	0.136735	55.000000	127
i2	114.954649	0.136508	55.000000	127
i2	115.090930	0.136735	55.000000	127
i2	115.227438	0.136508	51.911623	104
i2	115.363719	0.136508	55.000000	72
i2	115.500000	0.136735	58.267183	98
i2	115.636508	0.136508	61.733671	127
i2	115.909297	0.136508	61.733671	123
i2	116.181859	0.273016	61.733671	127
i2	116.454649	0.136508	55.000000	98
i2	116.590930	0.136735	61.733671	127
i2	116.863719	0.136508	61.733671	117
i2	117.136508	0.136508	61.733671	114
i2	117.272789	0.409297	61.733671	123
i2	117.818367	0.136508	55.000000	127
i2	118.090930	0.136735	55.000000	127
i2	118.363719	0.273016	55.000000	127
i2	118.636508	0.136508	41.2     	101
i2	118.772789	0.136735	55.000000	127
i2	119.045578	0.136508	55.000000	127
i2	119.318367	0.136508	55.000000	127
i2	119.454649	0.204762	55.000000	127
i2	119.727438	0.136508	55.000000	72
i2	119.863719	0.136508	58.267183	98
i2	120.000000	0.136735	61.733671	127
i2	120.272789	0.136735	61.733671	123
i2	120.545578	0.273016	61.733671	127
i2	120.818367	0.136508	55.000000	98
i2	120.954649	0.136508	61.733671	127
i2	121.227438	0.136508	61.733671	117
i2	121.500000	0.136735	61.733671	114
i2	121.636508	0.409297	61.733671	123
i2	122.181859	0.136735	55.000000	127
i2	122.454649	0.136508	55.000000	127
i2	122.727438	0.272789	55.000000	127
i2	123.000000	0.136735	41.2     	101
i2	123.136508	0.136508	55.000000	127
i2	123.409297	0.136508	55.000000	127
i2	123.681859	0.136735	55.000000	127
i2	123.818367	0.341043	55.000000	127
i2	124.227438	0.136508	55.000000	113
i2	124.363719	0.136508	41.2     	127
i2	124.636508	0.136508	41.2     	123
i2	124.909297	0.272789	41.2     	127
i2	125.318367	0.136508	55.000000	127
i2	125.590930	0.136735	55.000000	117
i2	125.863719	0.136508	55.000000	114
i2	126.000000	0.273016	55.000000	123
i2	126.545578	0.136508	61.733671	127
i2	126.818367	0.136508	61.733671	127
i2	127.090930	0.273016	61.733671	127
i2	127.500000	0.136735	55.000000	127
i2	127.772789	0.136735	55.000000	127
i2	128.045578	0.136508	55.000000	127
i2	128.181859	0.273016	55.000000	127
i2	128.727438	0.136508	41.2     	127
i2	129.000000	0.136735	41.2     	123
i2	129.272789	0.273016	41.2     	127
i2	129.681859	0.136735	55.000000	127
i2	129.954649	0.136508	55.000000	117
i2	130.227438	0.136508	55.000000	114
i2	130.363719	0.273016	55.000000	123
i2	130.909297	0.136508	61.733671	127
i2	131.181859	0.136735	61.733671	127
i2	131.454649	0.273016	61.733671	127
i2	131.863719	0.136508	55.000000	127
i2	132.136508	0.136508	55.000000	127
i2	132.409297	0.136508	55.000000	127
i2	132.545578	0.273016	55.000000	127
i2	133.090930	0.136735	41.2     	127
i2	133.363719	0.136508	41.2     	123
i2	133.636508	0.273016	41.2     	127
i2	134.045578	0.136508	55.000000	127
i2	134.318367	0.136508	55.000000	117
i2	134.590930	0.136735	55.000000	114
i2	134.727438	0.272789	55.000000	123
i2	135.272789	0.136735	61.733671	127
i2	135.545578	0.136508	61.733671	127
i2	135.818367	0.272789	61.733671	127
i2	136.227438	0.136508	55.000000	127
i2	136.500227	0.136508	55.000000	127
i2	136.772789	0.136735	55.000000	127
i2	136.909297	0.272789	55.000000	127
i2	137.454649	0.136508	41.2     	127
i2	137.727438	0.136508	41.2     	123
i2	138.000227	0.272789	41.2     	127
i2	138.409297	0.136508	55.000000	127
i2	138.681859	0.136735	55.000000	117
i2	138.954649	0.136508	55.000000	114
i2	139.090930	0.273016	55.000000	123
i2	139.636508	0.136508	61.733671	127
i2	139.909297	0.136508	61.733671	127
i2	140.181859	0.273016	61.733671	127
i2	140.590930	0.136735	55.000000	127
i2	140.863719	0.136735	55.000000	127
i2	141.136508	0.136508	55.000000	127
i2	141.272789	0.273016	55.000000	127
i2	141.818367	0.136508	41.2     	127
i2	142.090930	0.136735	41.2     	123
i2	142.363719	0.273016	41.2     	127
i2	142.772789	0.136735	55.000000	127
i2	143.045578	0.136508	55.000000	117
i2	143.318367	0.136508	55.000000	114
i2	143.454649	0.273016	55.000000	123
i2	144.000227	0.136508	61.733671	127
i2	144.272789	0.136735	61.733671	127
i2	144.545578	0.273016	61.733671	127
i2	144.954649	0.136508	55.000000	127
i2	145.227438	0.136508	55.000000	127
i2	145.500227	0.136508	55.000000	127
i2	145.636508	0.273016	55.000000	127
i2	146.181859	0.136735	41.2     	127
i2	146.454649	0.136508	41.2     	123
i2	146.727438	0.273016	41.2     	127
i2	147.136508	0.136508	55.000000	127
i2	147.409297	0.136508	55.000000	117
i2	147.681859	0.136735	55.000000	114
i2	147.818367	0.272789	55.000000	123
i2	148.363719	0.136735	61.733671	127
i2	148.636508	0.136508	61.733671	127
i2	148.909297	0.272789	61.733671	127
i2	149.318367	0.136508	55.000000	127
i2	149.590930	0.136735	55.000000	127
i2	149.863719	0.136735	55.000000	127
i2	150.000227	0.272789	55.000000	127
i2	150.545578	0.136508	41.2     	127
i2	150.818367	0.136508	41.2     	123
i2	151.090930	0.273016	41.2     	127
i2	151.500227	0.136508	55.000000	127
i2	151.772789	0.136735	55.000000	117
i2	152.045578	0.136508	55.000000	114
i2	152.181859	0.273016	55.000000	123
i2	152.727438	0.136508	61.733671	127
i2	153.000227	0.136508	61.733671	127
i2	153.272789	0.273016	61.733671	127
i2	153.681859	0.136735	55.000000	126
i2	153.954649	0.136508	55.000000	126
i2	154.227438	0.136508	55.000000	127
i2	154.363719	0.273016	55.000000	127
i2	154.909297	0.136508	41.2     	127
i2	155.181859	0.136735	41.2     	123
i2	155.454649	0.273016	41.2     	127
i2	155.863719	0.136735	55.000000	127
i2	156.136508	0.136508	55.000000	117
i2	156.409297	0.136508	55.000000	114
i2	156.545578	0.273016	55.000000	123
i2	157.090930	0.136735	61.733671	127
i2	157.363719	0.136735	61.733671	127
i2	157.636508	0.273016	61.733671	127
i2	158.045578	0.136508	55.000000	127
i2	158.318367	0.136508	55.000000	127
i2	158.590930	0.136735	55.000000	127
i2	158.727438	0.273016	55.000000	127
i2	159.272789	0.136735	41.2     	127
i2	159.545578	0.136508	41.2     	123
i2	159.818367	0.272789	41.2     	127
i2	160.227438	0.136508	55.000000	127
i2	160.500227	0.136508	55.000000	117
i2	160.772789	0.136735	55.000000	114
i2	160.909297	0.272789	55.000000	123
i2	161.454649	0.136735	61.733671	127
i2	161.727438	0.136508	61.733671	127
i2	162.000227	0.272789	61.733671	127
i2	162.409297	0.136508	55.000000	127
i2	162.681859	0.136735	55.000000	127
i2	162.954649	0.136735	55.000000	127
i2	163.091156	0.272789	55.000000	127
i2	163.636508	0.136508	41.2     	127
i2	163.909297	0.136508	41.2     	123
i2	164.181859	0.273016	41.2     	127
i2	164.591156	0.136508	55.000000	127
i2	164.863719	0.136735	55.000000	117
i2	165.136508	0.136508	55.000000	114
i2	165.272789	0.273016	55.000000	123
i2	165.818367	0.136508	61.733671	127
i2	166.091156	0.136508	61.733671	127
i2	166.363719	0.273016	61.733671	127
i2	166.772789	0.136735	55.000000	127
i2	167.045578	0.136508	55.000000	127
i2	167.318367	0.136508	55.000000	127
i2	167.454649	0.273016	55.000000	127
i2	168.000227	0.136508	61.733671	127
i2	168.272789	0.136735	61.733671	123
i2	168.545578	0.273016	61.733671	127
i2	168.818367	0.136508	55.000000	98
i2	168.954649	0.136735	61.733671	127
i2	169.227438	0.136508	61.733671	117
i2	169.500227	0.136508	61.733671	114
i2	169.636508	0.409297	61.733671	123
i2	170.181859	0.136735	55.000000	127
i2	170.454649	0.136735	55.000000	127
i2	170.727438	0.273016	55.000000	127
i2	171.000227	0.136508	41.2     	101
i2	171.136508	0.136508	55.000000	127
i2	171.409297	0.136508	55.000000	127
i2	171.681859	0.136735	55.000000	127
i2	171.818367	0.136508	55.000000	127
i2	171.954649	0.136735	51.911623	104
i2	172.091156	0.136508	55.000000	72
i2	172.227438	0.136508	58.267183	98
i2	172.363719	0.136735	61.733671	127
i2	172.636508	0.136508	61.733671	123
i2	172.909297	0.272789	61.733671	127
i2	173.181859	0.136735	55.000000	98
i2	173.318367	0.136508	61.733671	127
i2	173.591156	0.136508	61.733671	117
i2	173.863719	0.136735	61.733671	114
i2	174.000227	0.409297	61.733671	123
i2	174.545578	0.136508	55.000000	127
i2	174.818367	0.136508	55.000000	127
i2	175.091156	0.272789	55.000000	127
i2	175.363719	0.136735	41.2     	101
i2	175.500227	0.136508	55.000000	127
i2	175.772789	0.136735	55.000000	127
i2	176.045578	0.136508	55.000000	127
i2	176.181859	0.204989	55.000000	127
i2	176.454649	0.136735	55.000000	72
i2	176.591156	0.136508	58.267183	98
i2	176.727438	0.136508	61.733671	127
i2	177.000227	0.136508	61.733671	123
i2	177.272789	0.273016	61.733671	127
i2	177.545578	0.136508	55.000000	98
i2	177.681859	0.136735	61.733671	127
i2	177.954649	0.136735	61.733671	117
i2	178.227438	0.136508	61.733671	114
i2	178.363719	0.409297	61.733671	123
i2	178.909297	0.136508	55.000000	127
i2	179.181859	0.136735	55.000000	127
i2	179.454649	0.273016	55.000000	127
i2	179.727438	0.136508	41.2     	101
i2	179.863719	0.136735	55.000000	127
i2	180.136508	0.136508	55.000000	127
i2	180.409297	0.136508	55.000000	127
i2	180.545578	0.341270	55.000000	127
i2	180.954649	0.136735	55.000000	113
i2	181.091156	0.136508	41.2     	127
i2	181.363719	0.136735	41.2     	123
i2	181.636508	0.273016	41.2     	127
i2	182.045578	0.136508	55.000000	127
i2	182.318367	0.136508	55.000000	117
i2	182.591156	0.136508	55.000000	114
i2	182.727438	0.273016	55.000000	123
i2	183.272789	0.136735	61.733671	127
i2	183.545578	0.136508	61.733671	127
i2	183.818367	0.273016	61.733671	127
i2	184.227438	0.136508	55.000000	127
i2	184.500227	0.136508	55.000000	127
i2	184.772789	0.136735	55.000000	127
i2	184.909297	0.272789	55.000000	127
i2	185.454649	0.136735	41.2     	127
i2	185.727438	0.136508	41.2     	123
i2	186.000227	0.272789	41.2     	127
i2	186.409297	0.136508	55.000000	127
i2	186.682086	0.136508	55.000000	117
i2	186.954649	0.136735	55.000000	114
i2	187.091156	0.272789	55.000000	123
i2	187.636508	0.136508	61.733671	127
i2	187.909297	0.136508	61.733671	127
i2	188.182086	0.272789	61.733671	127
i2	188.591156	0.136508	55.000000	127
i2	188.863719	0.136735	55.000000	127
i2	189.136508	0.136508	55.000000	127
i2	189.272789	0.273016	55.000000	127
i2	189.818367	0.136508	41.2     	127
i2	190.091156	0.136508	41.2     	123
i2	190.363719	0.273016	41.2     	127
i2	190.772789	0.136735	55.000000	127
i2	191.045578	0.136735	55.000000	117
i2	191.318367	0.136508	55.000000	114
i2	191.454649	0.273016	55.000000	123
i2	192.000227	0.136508	61.733671	127
i2	192.272789	0.136735	61.733671	127
i2	192.545578	0.273016	61.733671	127
i2	192.954649	0.136735	55.000000	127
i2	193.227438	0.136508	55.000000	127
i2	193.500227	0.136508	55.000000	127
i2	193.636508	0.273016	55.000000	127
i2	194.182086	0.136508	41.2     	127
i2	194.454649	0.136735	41.2     	123
i2	194.727438	0.273016	41.2     	127
i2	195.136508	0.136508	55.000000	127
i2	195.409297	0.136508	55.000000	117
i2	195.682086	0.136508	55.000000	114
i2	195.818367	0.273016	55.000000	123
i2	196.363719	0.136735	61.733671	127
i2	196.636508	0.136508	61.733671	127
i2	196.909297	0.273016	61.733671	127
i2	197.318367	0.136508	55.000000	127
i2	197.591156	0.136508	55.000000	127
i2	197.863719	0.136735	55.000000	127
i2	198.000227	0.272789	55.000000	127
i2	198.545578	0.136735	41.2     	127
i2	198.818367	0.136508	41.2     	123
i2	199.091156	0.272789	41.2     	127
i2	199.500227	0.136508	55.000000	127
i2	199.772789	0.136735	55.000000	117
i2	200.045578	0.136735	55.000000	114
i2	200.182086	0.272789	55.000000	123
i2	200.727438	0.136508	61.733671	127
i2	201.000227	0.136508	61.733671	127
i2	201.272789	0.273016	61.733671	127
i2	201.682086	0.136508	55.000000	127
i2	201.954649	0.136735	55.000000	127
i2	202.227438	0.136508	55.000000	127
i2	202.363719	0.273016	55.000000	127
i2	202.909297	0.136508	41.2     	127
i2	203.182086	0.136508	41.2     	123
i2	203.454649	0.273016	41.2     	127
i2	203.863719	0.136735	55.000000	127
i2	204.136508	0.136508	55.000000	117
i2	204.409297	0.136508	55.000000	114
i2	204.545578	0.273016	55.000000	123
i2	205.091156	0.136508	61.733671	127
i2	205.363719	0.136735	61.733671	127
i2	205.636508	0.273016	61.733671	127
i2	206.045578	0.136735	55.000000	127
i2	206.318367	0.136508	55.000000	127
i2	206.591156	0.136508	55.000000	127
i2	206.727438	0.273016	55.000000	127
i2	207.272789	0.545805	41.2     	124
i2	207.818367	0.136508	82.402241	124
i2	207.954649	0.136735	73.414121	87
i2	208.091156	0.136508	82.402241	104
i2	208.227438	0.136508	73.414121	109
i2	208.363719	0.136735	69.291749	89
i2	208.500227	0.136508	73.414121	89
i2	208.636508	0.136508	69.291749	92
i2	208.772789	0.136735	61.733671	87
i2	208.909297	0.136508	55.000000	94
i2	209.045578	0.136735	61.733671	104
i2	209.182086	0.136508	55.000000	98
i2	209.318367	0.136508	51.911623	104
i2	209.454649	0.273016	41.2     	109

; 3:1 starts 10.9 ends 209 ins 3
; ins 3
; short repeating rhythm
; doo doo DO doo DO doo doo

i3	10.9	0.136508	415.292983	127
i3	10.9	0.136508	329.6     	127
i3	10.9	0.136508	246.9     	127
i3	11.2	0.136508	246.9     	116
i3	11.2	0.136508	329.6     	116
i3	11.2	0.136508	415.292983	116
i3	11.45	0.272789	246.9     	127
i3	11.45	0.409297	415.292983	127
i3	11.45	0.409297	329.6     	127
i3	11.7	0.136735	164.804481	113
i3	11.7	0.136735	246.9     	113
i3	11.9	0.136508	329.6     	127
i3	11.9	0.136508	439.999998	127
i3	11.9	0.136508	277.20    	127
i3	12.1	0.136735	329.6     	116
i3	12.11	0.136735	277.20    	116
i3	12.12	0.136735	439.999998	116
i3	12.4	0.136735	329.6     	127
i3	12.4	0.136735	277.20    	127
i3	12.5	0.136508	277.20    	113
i3	12.4	0.341270	439.999998	127
i3	12.5	0.204762	329.6     	113
i3	12.7	0.136508	220.0     	87
i3	12.7	0.136508	277.20    	87
i3	12.8	0.136735	277.20    	125
i3	12.8	0.2     	439.999998	125
i3	12.8	0.2	        329.6     	125
i3	13.0	0.136508	277.20    	87
i3	13.0	0.136508	220.0     	87
i3	13.1	0.136508	311.13  	121
i3	13.11	0.136508	493.869370	122
i3	13.12	0.136508	369.994421	122
i3	13.36	0.136508	311.126982	110
i3	13.37	0.136508	493.869370	110
i3	13.38	0.136508	369.994421	110
i3	13.61	0.273016	311.126982	125
i3	13.62	0.273016	369.994421	125
i3	13.63	0.273016	493.869370	125
i3	13.9	0.136735	246.9     	113
i3	13.91	0.136735	311.126982	113
i3	14.01   	0.137	439.999998	125
i3	14.02   	0.137	277.20    	125
i3	14.03   	0.137	329.6     	125
i3	14.31   	0.139	277.20    	110
i3	14.32   	0.139	329.6     	110
i3	14.33   	0.139	441.0	    109
i3	14.6    	0.14   277.20    	119
i3	14.6    	0.145  	444.4   	119
i3	14.7    	0.14   	329.6     	119
i3	14.72   	0.14	277.20    	101
i3	14.727  	0.20	329.6     	101
i3	14.73   	0.21	440.0     	101
i3	14.863719	0.14	220.0     	95
i3	14.863719	0.14	277.20    	95
i3	15.000000	0.136	277.20    	122
i3	15.000000	0.204762	329.6     	122
i3	15.000000	0.216100	440.0     	122
i3	15.136281	0.091156	220.0     	101
i3	15.136281	0.136735	277.20    	101
i3	15.26   	0.13    	415.292983	127
i3	15.27   	0.14    	329.6     	127
i3	15.273  	0.14    	246.9     	127
i3	15.54   	0.136508	246.9     	116
i3	15.55   	0.136508	329.6     	116
i3	15.56   	0.136508	416.0   	116
i3	15.818141	0.273016	246.9     	127
i3	15.818141	0.409297	414.0   	127
i3	15.818141	0.409297	329.6     	127
i3	16.09   	0.136508	164.8   	111
i3	16.1    	0.136508	246.9     	113
i3	16.227211	0.136735	329.6     	127
i3	16.227211	0.136735	440.0     	127
i3	16.227211	0.136735	277.20    	127
i3	16.500000	0.136508	329.6     	116
i3	16.500000	0.136508	277.20    	116
i3	16.500000	0.136508	440.0     	116
i3	16.772789	0.136508	329.6     	127
i3	16.772789	0.136508	277.20    	127
i3	16.909070	0.136735	277.20    	113
i3	16.772789	0.341043	440.0     	127
i3	16.909070	0.204762	329.6     	113
i3	17.0    	0.14    	220.0     	87
i3	17.1    	0.14    	277.20    	87
i3	17.181859	0.14    	277.20    	125
i3	17.181859	0.204762	440.0     	125
i3	17.181859	0.204762	329.6     	125
i3	17.318141	0.14    	277.20    	87
i3	17.318141	0.14    	220.0     	87
i3	17.454649	0.14    	311.126982	122
i3	17.454649	0.14    	493.869370	122
i3	17.454649	0.14    	369.994421	122
i3	17.727211	0.14    	311.126982	110
i3	17.727211	0.14    	493.869370	110
i3	17.727211	0.14    	369.994421	110
i3	18.000000	0.273016	311.126982	125
i3	18.000000	0.273016	369.994421	125
i3	18.000000	0.273016	493.869370	125
i3	18.272789	0.136508	246.9     	113
i3	18.272789	0.136508	311.126982	113
i3	18.409070	0.136735	440.0     	125
i3	18.409070	0.136735	277.20    	125
i3	18.409070	0.136735	329.6     	125
i3	18.681859	0.136508	277.20    	110
i3	18.681859	0.136508	329.6     	110
i3	18.681859	0.136508	440.0     	110
i3	18.954649	0.136508	277.20    	119
i3	18.954649	0.136508	440.0     	119
i3	18.954649	0.136508	329.6     	119
i3	19.1     	0.136508	277.20    	101
i3	19.11   	0.204762	329.6     	101
i3	19.12   	0.204762	440.0     	101
i3	19.227211	0.136735	220.0     	95
i3	19.227211	0.136735	277.20    	95
i3	19.363719	0.136508	277.20    	122
i3	19.363719	0.204762	329.6     	122
i3	19.363719	0.216100	440.0     	122
i3	19.500000	0.091156	220.0     	101
i3	19.500000	0.136508	277.20    	101
i3	19.636281	0.136735	415.292983	127
i3	19.636281	0.136735	329.6     	127
i3	19.636281	0.136735	246.9     	127
i3	19.909070	0.136735	246.9     	116
i3	19.909070	0.136735	329.6     	116
i3	19.909070	0.136735	415.292983	116
i3	20.181859	0.273016	246.9     	127
i3	20.181859	0.409297	415.292983	127
i3	20.181859	0.409297	329.6     	127
i3	20.454649	0.136508	164.804481	113
i3	20.454649	0.136508	246.9     	113
i3	20.590930	0.136508	329.6     	127
i3	20.590930	0.136508	440.0     	127
i3	20.590930	0.136508	277.20    	127
i3	20.863719	0.136508	329.6     	116
i3	20.863719	0.136508	277.20    	116
i3	20.863719	0.136508	440.0     	116
i3	21.136281	0.136735	329.6     	127
i3	21.136281	0.136735	277.20    	127
i3	21.272789	0.136508	277.20    	113
i3	21.136281	0.341270	440.0     	127
i3	21.272789	0.204762	329.6     	113
i3	21.409070	0.136735	220.0     	87
i3	21.409070	0.136735	277.20    	87
i3	21.545578	0.136508	277.20    	125
i3	21.545578	0.204762	440.0     	125
i3	21.545578	0.204762	329.6     	125
i3	21.681859	0.136508	277.20    	87
i3	21.681859	0.136508	220.0     	87
i3	21.818141	0.136735	311.126982	122
i3	21.818141	0.136735	493.869370	122
i3	21.818141	0.136735	369.994421	122
i3	22.090930	0.136508	311.126982	110
i3	22.090930	0.136508	493.869370	110
i3	22.090930	0.136508	369.994421	110
i3	22.363719	0.272789	311.126982	125
i3	22.363719	0.272789	369.994421	125
i3	22.363719	0.272789	493.869370	125
i3	22.636281	0.136735	246.9     	113
i3	22.636281	0.136735	311.126982	113
i3	22.772789	0.136508	440.0     	125
i3	22.772789	0.136508	277.20    	125
i3	22.772789	0.136508	329.6     	125
i3	23.045578	0.136508	277.20    	110
i3	23.045578	0.136508	329.6     	110
i3	23.045578	0.136508	440.0     	110
i3	23.318141	0.136735	277.20    	119
i3	23.318141	0.136735	440.0     	119
i3	23.318141	0.136735	329.6     	119
i3	23.454649	0.136508	277.20    	101
i3	23.454649	0.204762	329.6     	101
i3	23.454649	0.204762	440.0     	101
i3	23.6    	0.1     	220.0     	95
i3	23.61   	0.2     	277.20    	95
i3	23.727211	0.136735	277.20    	122
i3	23.727211	0.204762	329.6     	122
i3	23.727211	0.216327	440.0     	122
i3	23.863719	0.091156	220.0     	101
i3	23.863719	0.136508	277.20    	101
i3	24.000000	0.136508	415.292983	127
i3	24.000000	0.136508	329.6     	125
i3	24.000000	0.136508	246.9     	125
i3	24.3	0.136508	246.9     	116
i3	24.3	0.136508	329.6     	116
i3	24.3	0.136508	415.292983	116
i3	24.55	0.272789	246.9     	127
i3	24.55	0.409297	415.292983	127
i3	24.55	0.409297	329.6     	127
i3	24.8	0.136735	164.804481	113
i3	24.8	0.136735	246.9     	113
i3	24.95	0.136508	329.6     	127
i3	24.95	0.136508	440.0     	127
i3	24.95	0.136508	277.20    	127
i3	25.227211	0.136735	329.6     	116
i3	25.227211	0.136735	277.20    	116
i3	25.227211	0.136735	440.0     	116
i3	25.500000	0.136508	329.6     	127
i3	25.500000	0.136508	277.20    	127
i3	25.636281	0.136735	277.20    	113
i3	25.500000	0.341270	440.0     	127
i3	25.636281	0.204989	329.6     	113
i3	25.772789	0.136508	220.0     	87
i3	25.772789	0.136508	277.20    	87
i3	25.909070	0.136735	277.20    	125
i3	25.909070	0.204762	440.0     	125
i3	25.909070	0.204762	329.6     	125
i3	26.045578	0.136508	277.20    	87
i3	26.045578	0.136508	220.0     	87
i3	26.181859	0.136508	311.126982	122
i3	26.181859	0.136508	493.869370	122
i3	26.181859	0.136508	369.994421	122
i3	26.454649	0.136508	311.126982	110
i3	26.454649	0.136508	493.869370	110
i3	26.454649	0.136508	369.994421	110
i3	26.727211	0.273016	311.126982	125
i3	26.727211	0.273016	369.994421	125
i3	26.727211	0.273016	493.869370	125
i3	27.000000	0.136508	246.9     	113
i3	27.000000	0.136508	311.126982	113
i3	27.136281	0.136735	440.0     	125
i3	27.136281	0.136735	277.20    	125
i3	27.136281	0.136735	329.6     	125
i3	27.409070	0.136735	277.20    	110
i3	27.409070	0.136735	329.6     	110
i3	27.409070	0.136735	440.0     	110
i3	27.681859	0.136508	277.20    	119
i3	27.681859	0.136508	440.0     	119
i3	27.681859	0.136508	329.6     	119
i3	27.818141	0.136735	277.20    	101
i3	27.818141	0.204762	329.6     	101
i3	27.818141	0.204762	440.0     	101
i3	27.954649	0.136508	220.0     	95
i3	27.954649	0.136508	277.20    	95
i3	28.090930	0.136508	277.20    	122
i3	28.090930	0.204762	329.6     	122
i3	28.090930	0.216100	440.0     	122
i3	28.227211	0.091156	220.0     	101
i3	28.227211	0.136735	277.20    	101
i3	28.363719	0.136508	415.29  	127
i3	28.363719	0.136508	329.6     	127
i3	28.363719	0.136508	246.9     	127
i3	28.636281	0.136735	246.9     	116
i3	28.636281	0.136735	329.6     	116
i3	28.636281	0.136735	415.29  	116
i3	28.909070	0.273016	246.9     	126
i3	28.909070	0.409297	415.29  	126
i3	28.909070	0.409297	329.6     	126
i3	29.181859	0.136508	164.8   	113
i3	29.181859	0.136508	246.9     	113
i3	29.318141	0.136735	329.6     	127
i3	29.318141	0.136735	440.0     	127
i3	29.318141	0.136735	277.20    	127
i3	29.590930	0.136508	329.6     	116
i3	29.590930	0.136508	277.20    	116
i3	29.590930	0.136508	440.0     	116
i3	29.863719	0.136508	329.6     	127
i3	29.863719	0.136508	277.20    	127
i3	30.000000	0.136508	277.20    	113
i3	29.863719	0.341043	440.0     	127
i3	30.000000	0.204762	329.6     	113
i3	30.136281	0.136735	220.0     	87
i3	30.136281	0.136735	277.20    	87
i3	30.272789	0.136508	277.20    	125
i3	30.272789	0.204762	440.0     	125
i3	30.272789	0.204762	329.6     	125
i3	30.409070	0.136735	277.20    	87
i3	30.409070	0.136735	220.0     	87
i3	30.545578	0.136508	311.126982	122
i3	30.545578	0.136508	493.87  	122
i3	30.545578	0.136508	369.994421	122
i3	30.818141	0.136735	311.126982	110
i3	30.818141	0.136735	493.869370	110
i3	30.818141	0.136735	369.994421	110
i3	31.090930	0.273016	311.126982	125
i3	31.090930	0.273016	369.994421	125
i3	31.090930	0.273016	493.869370	125
i3	31.363719	0.136508	246.9     	113
i3	31.363719	0.136508	311.126982	113
i3	31.500000	0.136508	440.0     	125
i3	31.500000	0.136508	277.20    	125
i3	31.500000	0.136508	329.6     	125
i3	31.772789	0.136508	277.20    	110
i3	31.772789	0.136508	329.6     	110
i3	31.772789	0.136508	440.0     	110
i3	32.045578	0.136508	277.20    	119
i3	32.045578	0.136508	440.0     	119
i3	32.045578	0.136508	329.6     	119
i3	32.181859	0.136508	277.20    	101
i3	32.181859	0.204762	329.6     	101
i3	32.181859	0.204762	440.0     	101
i3	32.318141	0.136735	220.0     	95
i3	32.318141	0.136735	277.20    	95
i3	32.454649	0.136508	277.20    	122
i3	32.454649	0.204762	329.6     	122
i3	32.454649	0.216100	440.0     	122
i3	32.590930	0.091156	220.0     	101
i3	32.590930	0.136508	277.20    	101
i3	32.727211	0.136735	415.292983	127
i3	32.727211	0.136735	329.6     	127
i3	32.727211	0.136735	246.9     	127
i3	33.000000	0.136508	246.9     	116
i3	33.000000	0.136508	329.6     	116
i3	33.000000	0.136508	415.292983	116
i3	33.272789	0.273016	246.9     	127
i3	33.272789	0.409297	415.292983	127
i3	33.272789	0.409297	329.6     	127
i3	33.545578	0.136508	164.804481	113
i3	33.545578	0.136508	246.9     	113
i3	33.681859	0.136508	329.6     	127
i3	33.681859	0.136508	440.0     	127
i3	33.681859	0.136508	277.20    	127
i3	33.954649	0.136508	329.6     	116
i3	33.954649	0.136508	277.20    	116
i3	33.954649	0.136508	440.0     	116
i3	34.2	0.136735	329.6     	127
i3	34.2	0.136735	277.20    	127
i3	34.4	0.136508	277.20    	113
i3	34.2	0.341270	440.0     	127
i3	34.4	0.204762	329.6     	113
i3	34.5	0.136508	220.0     	87
i3	34.5	0.136508	277.20    	87
i3	34.6	0.136735	277.20    	125
i3	34.6	0.204989	440.0     	125
i3	34.6	0.204989	329.6     	125
i3	34.8	0.136508	277.20    	87
i3	34.8	0.136508	220.0     	87
i3	34.909070	0.136735	311.126982	122
i3	34.909070	0.136735	493.869370	122
i3	34.909070	0.136735	369.994421	122
i3	35.181859	0.136508	311.126982	110
i3	35.181859	0.136508	493.869370	110
i3	35.181859	0.136508	369.994421	110
i3	35.454649	0.272789	311.126982	125
i3	35.454649	0.272789	369.994421	125
i3	35.454649	0.272789	493.869370	125
i3	35.727211	0.136735	246.9     	113
i3	35.727211	0.136735	311.126982	113
i3	35.863719	0.136508	440.0     	125
i3	35.863719	0.136508	277.20    	125
i3	35.863719	0.136508	329.6     	125
i3	36.136281	0.136735	277.20    	110
i3	36.136281	0.136735	329.6     	110
i3	36.136281	0.136735	440.0     	110
i3	36.409070	0.136735	277.20    	119
i3	36.409070	0.136735	440.0     	119
i3	36.409070	0.136735	329.6     	119
i3	36.545578	0.136508	277.20    	101
i3	36.545578	0.204762	329.6     	101
i3	36.545578	0.204762	440.0     	101
i3	36.681859	0.136508	220.0     	95
i3	36.681859	0.136508	277.20    	95
i3	36.818141	0.136735	277.20    	122
i3	36.818141	0.204762	329.6     	122
i3	36.818141	0.216100	440.0     	122
i3	36.954649	0.091156	220.0     	101
i3	36.954649	0.136508	277.20    	101
i3	37.090930	0.136508	415.292983	127
i3	37.090930	0.136508	329.6     	127
i3	37.090930	0.136508	246.9     	127
i3	37.363719	0.136508	246.9     	116
i3	37.363719	0.136508	329.6     	116
i3	37.363719	0.136508	415.292983	116
i3	37.636508	0.272789	246.9     	127
i3	37.636508	0.409297	415.292983	127
i3	37.636508	0.409297	329.6     	127
i3	37.909070	0.136735	164.804481	113
i3	37.909070	0.136735	246.9     	113
i3	38.045578	0.136508	329.6     	127
i3	38.0    	0.136508	440.0     	125
i3	38.0    	0.136508	277.20    	127
i3	38.318141	0.136735	329.6     	116
i3	38.318141	0.136735	277.20    	116
i3	38.318141	0.136735	440.0     	116
i3	38.590930	0.136508	329.6     	127
i3	38.590930	0.136508	277.20    	127
i3	38.727211	0.136735	277.20    	113
i3	38.590930	0.341043	440.0     	127
i3	38.727211	0.204762	329.6     	113
i3	38.863719	0.136508	220.0     	87
i3	38.863719	0.136508	277.20    	87
i3	39.000000	0.136735	277.20    	125
i3	39.000000	0.204762	440.0     	125
i3	39.000000	0.204762	329.6     	125
i3	39.136508	0.136508	277.20    	87
i3	39.136508	0.136508	220.0     	87
i3	39.272789	0.136508	311.126982	122
i3	39.272789	0.136508	493.869370	122
i3	39.272789	0.136508	369.994421	122
i3	39.545578	0.136508	311.126982	110
i3	39.545578	0.136508	493.869370	110
i3	39.545578	0.136508	369.994421	110
i3	39.818141	0.273016	311.126982	125
i3	39.818141	0.273016	369.994421	125
i3	39.818141	0.273016	493.869370	125
i3	40.090930	0.136508	246.9     	113
i3	40.090930	0.136508	311.126982	113
i3	40.227211	0.136735	440.0     	125
i3	40.227211	0.136735	277.20    	125
i3	40.227211	0.136735	329.6     	125
i3	40.500000	0.136735	277.20    	110
i3	40.500000	0.136735	329.6     	110
i3	40.500000	0.136735	440.0     	110
i3	40.773  	0.137   	277.20    	119
i3	40.77    	0.136508	440.0     	119
i3	40.8    	0.136508	329.6     	119
i3	40.9    	0.136735	277.20    	101
i3	40.909070	0.204762	329.6     	101
i3	40.909070	0.204762	440.0     	101
i3	41.045578	0.136508	220.0     	95
i3	41.045578	0.136508	277.20    	95
i3	41.181859	0.136508	277.20    	122
i3	41.181859	0.204762	329.6     	122
i3	41.181859	0.216100	440.0     	122
i3	41.318141	0.091156	220.0     	101
i3	41.318141	0.136735	277.20    	101
i3	41.454649	0.136508	415.292983	127
i3	41.454649	0.136508	329.6     	127
i3	41.454649	0.136508	246.9     	127
i3	41.727211	0.136735	246.9     	116
i3	41.727211	0.136735	329.6     	116
i3	41.727211	0.136735	415.292983	116
i3	42.000000	0.273016	246.9     	127
i3	42.000000	0.409297	415.292983	127
i3	42.000000	0.409297	329.6     	127
i3	42.272789	0.136508	164.804481	113
i3	42.272789	0.136508	246.9     	113
i3	42.409070	0.136735	329.6     	127
i3	42.409070	0.136735	440.0     	127
i3	42.409070	0.136735	277.20    	127
i3	42.681859	0.136508	329.6     	116
i3	42.681859	0.136508	277.20    	116
i3	42.681859	0.136508	440.0     	116
i3	42.954649	0.136508	329.6     	127
i3	42.954649	0.136508	277.20    	127
i3	43.090930	0.136508	277.20    	113
i3	42.954649	0.341043	440.0     	127
i3	43.090930	0.204762	329.6     	113
i3	43.227211	0.136735	220.0     	87
i3	43.227211	0.136735	277.20    	87
i3	43.363719	0.136508	277.20    	125
i3	43.363719	0.204762	440.0     	125
i3	43.363719	0.204762	329.6     	125
i3	43.500000	0.136735	277.20    	87
i3	43.500000	0.136735	220.0     	87
i3	43.636508	0.136508	311.126982	122
i3	43.636508	0.136508	493.869370	122
i3	43.636508	0.136508	369.994421	122
i3	43.909070	0.136735	311.126982	110
i3	43.909070	0.136735	493.869370	110
i3	43.909070	0.136735	369.994421	110
i3	44.181859	0.273016	311.126982	125
i3	44.181859	0.273016	369.994421	125
i3	44.181859	0.273016	493.869370	125
i3	44.454649	0.136508	246.9     	113
i3	44.454649	0.136508	311.126982	113
i3	44.590930	0.136508	440.0     	125
i3	44.590930	0.136508	277.20    	125
i3	44.590930	0.136508	329.6     	125
i3	44.863719	0.136508	277.20    	110
i3	44.863719	0.136508	329.6     	110
i3	44.863719	0.136508	440.0     	110
i3	45.136508	0.136508	277.20    	119
i3	45.136508	0.136508	440.0     	119
i3	45.136508	0.136508	329.6     	119
i3	45.272789	0.136508	277.20    	101
i3	45.272789	0.204762	329.6     	101
i3	45.272789	0.204762	440.0     	101
i3	45.409070	0.136735	220.0     	95
i3	45.409070	0.136735	277.20    	95
i3	45.545578	0.136508	277.20    	122
i3	45.545578	0.204762	329.6     	122
i3	45.545578	0.216100	440.0     	122
i3	45.681859	0.091156	220.0     	101
i3	45.681859	0.136508	277.20    	101
i3	45.818141	0.136735	415.292983	127
i3	45.818141	0.136735	329.6     	127
i3	45.818141	0.136735	246.9     	127
i3	46.090930	0.136508	246.9     	116
i3	46.090930	0.136508	329.6     	116
i3	46.090930	0.136508	415.292983	116
i3	46.363719	0.273016	246.9     	127
i3	46.363719	0.409297	415.292983	127
i3	46.363719	0.409297	329.6     	127
i3	46.636508	0.136508	164.804481	113
i3	46.636508	0.136508	246.9     	113
i3	46.772789	0.136508	329.6     	127
i3	46.772789	0.136508	440.0     	127
i3	46.772789	0.136508	277.20    	127
i3	47.045578	0.136508	329.6     	116
i3	47.045578	0.136508	277.20    	116
i3	47.045578	0.136508	440.0     	116
i3	47.318141	0.136735	329.6     	127
i3	47.318141	0.136735	277.20    	127
i3	47.454649	0.136508	277.20    	113
i3	47.318141	0.341270	440.0     	127
i3	47.454649	0.204762	329.6     	113
i3	47.590930	0.136508	220.0     	87
i3	47.590930	0.136508	277.20    	87
i3	47.727211	0.136735	277.20    	125
i3	47.727211	0.204762	440.0     	125
i3	47.727211	0.204762	329.6     	125
i3	47.863719	0.136508	277.20    	87
i3	47.863719	0.136508	220.0     	87
i3	48.000000	0.136735	311.126982	122
i3	48.000000	0.136735	493.869370	122
i3	48.000000	0.136735	369.994421	122
i3	48.272789	0.136508	311.126982	110
i3	48.272789	0.136508	493.869370	110
i3	48.272789	0.136508	369.994421	110
i3	48.545578	0.272789	311.126982	125
i3	48.545578	0.272789	369.994421	125
i3	48.545578	0.272789	493.869370	125
i3	48.818141	0.136735	246.9     	113
i3	48.818141	0.136735	311.126982	113
i3	48.954649	0.136508	440.0     	125
i3	48.954649	0.136508	277.20    	125
i3	48.954649	0.136508	329.6     	125
i3	49.227211	0.136735	277.20    	110
i3	49.227211	0.136735	329.6     	110
i3	49.227211	0.136735	440.0     	110
i3	49.500000	0.136735	277.20    	119
i3	49.500000	0.136735	440.0     	119
i3	49.500000	0.136735	329.6     	119
i3	49.636508	0.136508	277.20    	101
i3	49.636508	0.204762	329.6     	101
i3	49.636508	0.204762	440.0     	101
i3	49.772789	0.136508	220.0     	95
i3	49.772789	0.136508	277.20    	95
i3	49.909070	0.136735	277.20    	122
i3	49.909070	0.204762	329.6     	122
i3	49.909070	0.216100	440.0     	122
i3	50.045578	0.091156	220.0     	101
i3	50.045578	0.136508	277.20    	101
i3	50.181859	0.136508	415.292983	127
i3	50.181859	0.136508	329.6     	127
i3	50.181859	0.136508	246.9     	127
i3	50.454649	0.136508	246.9     	116
i3	50.454649	0.136508	329.6     	116
i3	50.454649	0.136508	415.292983	116
i3	50.727211	0.273016	246.9     	127
i3	50.727211	0.409524	415.292983	127
i3	50.727211	0.409524	329.6     	127
i3	51.000000	0.136735	164.804481	113
i3	51.000000	0.136735	246.9     	113
i3	51.136508	0.136508	329.6     	127
i3	51.136508	0.136508	440.0     	127
i3	51.136508	0.136508	277.20    	127
i3	51.409070	0.136735	329.6     	116
i3	51.409070	0.136735	277.20    	116
i3	51.409070	0.136735	440.0     	116
i3	51.681859	0.136508	329.6     	127
i3	51.681859	0.136508	277.20    	127
i3	51.818141	0.136735	277.20    	113
i3	51.681859	0.341043	440.0     	127
i3	51.818141	0.204762	329.6     	113
i3	51.954649	0.136508	220.0     	87
i3	51.954649	0.136508	277.20    	87
i3	52.090930	0.136508	277.20    	125
i3	52.090930	0.204762	440.0     	125
i3	52.090930	0.204762	329.6     	125
i3	52.227211	0.136735	277.20    	87
i3	52.227211	0.136735	220.0     	87
i3	52.363719	0.136508	311.126982	122
i3	52.363719	0.136508	493.869370	122
i3	52.363719	0.136508	369.994421	122
i3	52.636508	0.136508	311.126982	110
i3	52.636508	0.136508	493.869370	110
i3	52.636508	0.136508	369.994421	110
i3	52.9	0.273016	311.126982	125
i3	52.9	0.273016	369.994421	125
i3	52.9	0.273016	493.869370	125
i3	53.2	0.136508	246.9     	113
i3	53.2	0.136508	311.126982	113
i3	53.3	0.136735	440.0     	125
i3	53.3	0.136735	277.20    	125
i3	53.3	0.136735	329.6     	125
i3	53.6	0.136508	277.20    	110
i3	53.6	0.136508	329.6     	110
i3	53.6	0.136508	440.0     	110
i3	53.9	0.136508	277.20    	119
i3	53.9	0.136508	440.0     	119
i3	53.9	0.136508	329.6     	119
i3	54.000000	0.136735	277.20    	101
i3	54.000000	0.204762	329.6     	101
i3	54.000000	0.204762	440.0     	101
i3	54.136508	0.136508	220.0     	95
i3	54.136508	0.136508	277.20    	95
i3	54.272789	0.136508	277.20    	122
i3	54.272789	0.204762	329.6     	122
i3	54.272789	0.216100	440.0     	122
i3	54.409070	0.091156	220.0     	101
i3	54.409070	0.136735	277.20    	101
i3	54.545578	0.136508	246.9     	127
i3	54.545578	0.136508	311.126982	127
i3	54.545578	0.136508	415.292983	127
i3	54.818141	0.136735	246.9     	116
i3	54.818141	0.136735	311.126982	116
i3	54.818141	0.136735	415.292983	116
i3	55.090930	0.273016	246.9     	127
i3	55.090930	0.409297	311.126982	127
i3	55.090930	0.409297	415.292983	127
i3	55.363719	0.136508	246.9     	113
i3	55.363719	0.136508	184.997211	113
i3	55.500000	0.136735	246.9     	127
i3	55.500000	0.136735	311.126982	127
i3	55.500000	0.136735	415.292983	127
i3	55.772789	0.136508	246.9     	116
i3	55.772789	0.136508	311.126982	116
i3	55.772789	0.136508	415.292983	116
i3	56.045578	0.136508	246.9     	127
i3	56.045578	0.136508	311.126982	127
i3	56.181859	0.136508	246.9     	113
i3	56.045578	0.341043	415.292983	127
i3	56.181859	0.204762	311.126982	113
i3	56.318141	0.136735	246.9     	87
i3	56.318141	0.136735	184.997211	87
i3	56.454649	0.136508	246.9     	125
i3	56.454649	0.204762	311.126982	125
i3	56.454649	0.204762	415.292983	125
i3	56.590930	0.136508	184.997211	87
i3	56.590930	0.136508	246.9     	87
i3	56.727211	0.136735	277.20    	122
i3	56.727211	0.136735	329.6     	122
i3	56.727211	0.136735	440.0     	122
i3	57.000000	0.136735	277.20    	110
i3	57.000000	0.136735	329.6     	110
i3	57.000000	0.136735	440.0     	110
i3	57.272789	0.273016	277.20    	125
i3	57.272789	0.273016	329.6     	125
i3	57.272789	0.273016	440.0     	125
i3	57.545578	0.136508	220.0     	113
i3	57.545578	0.136508	277.20    	113
i3	57.681859	0.136508	440.0     	125
i3	57.681859	0.136508	277.20    	125
i3	57.681859	0.136508	329.6     	125
i3	57.954649	0.136508	277.20    	110
i3	57.954649	0.136508	329.6     	110
i3	57.954649	0.136508	440.0     	110
i3	58.227211	0.136735	277.20    	119
i3	58.227211	0.136735	440.0     	119
i3	58.227211	0.136735	329.6     	119
i3	58.363719	0.136508	277.20    	101
i3	58.363719	0.204762	329.6     	101
i3	58.363719	0.204762	440.0     	101
i3	58.500000	0.136735	220.0     	95
i3	58.500000	0.136735	277.20    	95
i3	58.636508	0.136508	277.20    	122
i3	58.636508	0.204762	329.6     	122
i3	58.636508	0.216100	440.0     	122
i3	58.772789	0.091156	220.0     	101
i3	58.772789	0.136508	277.20    	101
i3	58.909070	0.136735	246.9     	127
i3	58.909070	0.136735	311.126982	127
i3	58.909070	0.136735	415.292983	127
i3	59.181859	0.136508	246.9     	116
i3	59.181859	0.136508	311.126982	116
i3	59.181859	0.136508	415.292983	116
i3	59.454649	0.272789	246.9     	127
i3	59.454649	0.409297	311.126982	127
i3	59.454649	0.409297	415.292983	127
i3	59.727211	0.136735	246.9     	113
i3	59.727211	0.136735	184.997211	113
i3	59.863719	0.136508	246.9     	127
i3	59.863719	0.136508	311.126982	127
i3	59.863719	0.136508	415.292983	127
i3	60.136508	0.136508	246.9     	116
i3	60.136508	0.136508	311.126982	116
i3	60.136508	0.136508	415.292983	116
i3	60.409070	0.136735	246.9     	127
i3	60.409070	0.136735	311.126982	127
i3	60.545578	0.136508	246.9     	113
i3	60.409070	0.341270	415.292983	127
i3	60.545578	0.204762	311.126982	113
i3	60.681859	0.136508	246.9     	87
i3	60.681859	0.136508	184.997211	87
i3	60.818141	0.136735	246.9     	125
i3	60.818141	0.204762	311.126982	125
i3	60.818141	0.204762	415.292983	125
i3	60.954649	0.136508	184.997211	87
i3	60.954649	0.136508	246.9     	87
i3	61.090930	0.136508	277.20    	122
i3	61.090930	0.136508	329.6     	122
i3	61.090930	0.136508	440.0     	122
i3	61.363719	0.136508	277.20    	110
i3	61.363719	0.136508	329.6     	110
i3	61.363719	0.136508	440.0     	110
i3	61.636508	0.272789	277.20    	125
i3	61.636508	0.272789	329.6     	125
i3	61.636508	0.272789	440.0     	125
i3	61.909070	0.136735	220.0     	113
i3	61.909070	0.136735	277.20    	113
i3	62.045578	0.136508	440.0     	125
i3	62.045578	0.136508	277.20    	125
i3	62.045578	0.136508	329.6     	125
i3	62.318141	0.136735	277.20    	110
i3	62.318141	0.136735	329.6     	110
i3	62.318141	0.136735	440.0     	110
i3	62.590930	0.136735	277.20    	119
i3	62.590930	0.136735	440.0     	119
i3	62.590930	0.136735	329.6     	119
i3	62.727438	0.136508	277.20    	101
i3	62.727438	0.204762	329.6     	101
i3	62.727438	0.204762	440.0     	101
i3	62.863719	0.136508	220.0     	95
i3	62.863719	0.136508	277.20    	95
i3	63.000000	0.136735	277.20    	122
i3	63.000000	0.204762	329.6     	122
i3	63.000000	0.216100	440.0     	122
i3	63.136508	0.091156	220.0     	101
i3	63.136508	0.136508	277.20    	101
i3	63.272789	0.136508	246.9     	127
i3	63.272789	0.136508	311.126982	127
i3	63.272789	0.136508	415.292983	127
i3	63.545578	0.136508	246.9     	116
i3	63.545578	0.136508	311.126982	116
i3	63.545578	0.136508	415.292983	116
i3	63.818141	0.273016	246.9     	127
i3	63.818141	0.409524	311.126982	127
i3	63.818141	0.409524	415.292983	127
i3	64.090930	0.136735	246.9     	113
i3	64.090930	0.136735	184.997211	113
i3	64.227438	0.136508	246.9     	127
i3	64.227438	0.136508	311.126982	127
i3	64.227438	0.136508	415.292983	127
i3	64.500000	0.136735	246.9     	116
i3	64.500000	0.136735	311.126982	116
i3	64.500000	0.136735	415.292983	116
i3	64.772789	0.136508	246.9     	127
i3	64.772789	0.136508	311.126982	127
i3	64.909070	0.136735	246.9     	113
i3	64.772789	0.341043	415.292983	127
i3	64.909070	0.204762	311.126982	113
i3	65.045578	0.136508	246.9     	87
i3	65.045578	0.136508	184.997211	87
i3	65.181859	0.136508	246.9     	125
i3	65.181859	0.204762	311.126982	125
i3	65.181859	0.204762	415.292983	125
i3	65.318141	0.136735	184.997211	87
i3	65.318141	0.136735	246.9     	87
i3	65.454649	0.136508	277.20    	122
i3	65.454649	0.136508	329.6     	122
i3	65.454649	0.136508	440.0     	122
i3	65.727438	0.136508	277.20    	110
i3	65.727438	0.136508	329.6     	110
i3	65.727438	0.136508	440.0     	110
i3	66.000000	0.273016	277.20    	125
i3	66.000000	0.273016	329.6     	125
i3	66.000000	0.273016	440.0     	125
i3	66.272789	0.136508	220.0     	113
i3	66.272789	0.136508	277.20    	113
i3	66.409070	0.136735	440.0     	125
i3	66.409070	0.136735	277.20    	125
i3	66.409070	0.136735	329.6     	125
i3	66.7	0.136508	277.20    	110
i3	66.7	0.136508	329.6     	110
i3	66.7	0.136508	440.0     	110
i3	66.9	0.136508	277.20    	119
i3	66.9	0.136508	440.0     	119
i3	66.9	0.136508	329.6     	119
i3	67.0	0.136735	277.20    	101
i3	67.0	0.204762	329.6     	101
i3	67.0	0.204762	440.0     	101
i3	67.2	0.136508	220.0     	95
i3	67.2	0.136508	277.20    	95
i3	67.4	0.136508	277.20    	122
i3	67.4	0.204762	329.6     	122
i3	67.4	0.216100	440.0     	122
i3	67.500000	0.091156	220.0     	101
i3	67.500000	0.136735	277.20    	101
i3	67.636508	0.136508	415.292983	127
i3	67.636508	0.136508	329.6     	127
i3	67.636508	0.136508	246.9     	127
i3	67.909070	0.136735	246.9     	116
i3	67.909070	0.136735	329.6     	116
i3	67.909070	0.136735	415.292983	116
i3	68.181859	0.273016	246.9     	127
i3	68.181859	0.409297	415.292983	127
i3	68.181859	0.409297	329.6     	127
i3	68.454649	0.136508	164.804481	113
i3	68.454649	0.136508	246.9     	113
i3	68.590930	0.136735	329.6     	127
i3	68.590930	0.136735	440.0     	127
i3	68.590930	0.136735	277.20    	127
i3	68.863719	0.136508	329.6     	116
i3	68.863719	0.136508	277.20    	116
i3	68.863719	0.136508	440.0     	116
i3	69.136508	0.136508	329.6     	127
i3	69.136508	0.136508	277.20    	127
i3	69.272789	0.136508	277.20    	113
i3	69.136508	0.341043	440.0     	127
i3	69.272789	0.204762	329.6     	113
i3	69.409070	0.136735	220.0     	87
i3	69.409070	0.136735	277.20    	87
i3	69.545578	0.136508	277.20    	125
i3	69.545578	0.204762	440.0     	125
i3	69.545578	0.204762	329.6     	125
i3	69.681859	0.136508	277.20    	87
i3	69.681859	0.136508	220.0     	87
i3	69.818141	0.136735	311.126982	122
i3	69.818141	0.136735	493.869370	122
i3	69.818141	0.136735	369.994421	122
i3	70.090930	0.136735	311.126982	110
i3	70.090930	0.136735	493.869370	110
i3	70.090930	0.136735	369.994421	110
i3	70.363719	0.273016	311.126982	125
i3	70.363719	0.273016	369.994421	125
i3	70.363719	0.273016	493.869370	125
i3	70.636508	0.136508	246.9     	113
i3	70.636508	0.136508	311.126982	113
i3	70.772789	0.136508	440.0     	125
i3	70.772789	0.136508	277.20    	125
i3	70.772789	0.136508	329.6     	125
i3	71.045578	0.136508	277.20    	110
i3	71.045578	0.136508	329.6     	110
i3	71.045578	0.136508	440.0     	110
i3	71.318141	0.136735	277.20    	119
i3	71.318141	0.136735	440.0     	119
i3	71.318141	0.136735	329.6     	119
i3	71.454649	0.136508	277.20    	101
i3	71.454649	0.204762	329.6     	101
i3	71.454649	0.204762	440.0     	101
i3	71.590930	0.136735	220.0     	95
i3	71.590930	0.136735	277.20    	95
i3	71.727438	0.136508	277.20    	122
i3	71.727438	0.204762	329.6     	122
i3	71.727438	0.216100	440.0     	122
i3	71.863719	0.091156	220.0     	101
i3	71.863719	0.136508	277.20    	101
i3	72.000000	0.136735	415.292983	127
i3	72.000000	0.136735	329.6     	127
i3	72.000000	0.136735	246.9     	127
i3	72.272789	0.136508	246.9     	116
i3	72.272789	0.136508	329.6     	116
i3	72.272789	0.136508	415.292983	116
i3	72.545578	0.272789	246.9     	127
i3	72.545578	0.409297	415.292983	127
i3	72.545578	0.409297	329.6     	127
i3	72.818141	0.136735	164.804481	113
i3	72.818141	0.136735	246.9     	113
i3	72.954649	0.136508	329.6     	127
i3	72.954649	0.136508	440.0     	127
i3	72.954649	0.136508	277.20    	127
i3	73.227438	0.136508	329.6     	116
i3	73.227438	0.136508	277.20    	116
i3	73.227438	0.136508	440.0     	116
i3	73.500000	0.136735	329.6     	127
i3	73.500000	0.136735	277.20    	127
i3	73.636508	0.136508	277.20    	113
i3	73.500000	0.341270	440.0     	127
i3	73.636508	0.204762	329.6     	113
i3	73.772789	0.136508	220.0     	87
i3	73.772789	0.136508	277.20    	87
i3	73.909070	0.136735	277.20    	125
i3	73.909070	0.204762	440.0     	125
i3	73.909070	0.204762	329.6     	125
i3	74.045578	0.136508	277.20    	87
i3	74.045578	0.136508	220.0     	87
i3	74.181859	0.136508	311.126982	122
i3	74.181859	0.136508	493.869370	122
i3	74.181859	0.136508	369.994421	122
i3	74.454649	0.136508	311.126982	110
i3	74.454649	0.136508	493.869370	110
i3	74.454649	0.136508	369.994421	110
i3	74.727438	0.272789	311.126982	125
i3	74.727438	0.272789	369.994421	125
i3	74.727438	0.272789	493.869370	125
i3	75.000000	0.136735	246.9     	113
i3	75.000000	0.136735	311.126982	113
i3	75.136508	0.136508	440.0     	125
i3	75.136508	0.136508	277.20    	125
i3	75.136508	0.136508	329.6     	125
i3	75.409070	0.136735	277.20    	110
i3	75.409070	0.136735	329.6     	110
i3	75.409070	0.136735	440.0     	110
i3	75.681859	0.136508	277.20    	119
i3	75.681859	0.136508	440.0     	119
i3	75.681859	0.136508	329.6     	119
i3	75.818141	0.136735	277.20    	101
i3	75.818141	0.204989	329.6     	101
i3	75.818141	0.204989	440.0     	101
i3	75.954649	0.136508	220.0     	95
i3	75.954649	0.136508	277.20    	95
i3	76.090930	0.136735	277.20    	122
i3	76.090930	0.204762	329.6     	122
i3	76.090930	0.216100	440.0     	122
i3	76.227438	0.090930	220.0     	101
i3	76.227438	0.136508	277.20    	101
i3	76.363719	0.136508	415.292983	127
i3	76.363719	0.136508	329.6     	127
i3	76.363719	0.136508	246.9     	127
i3	76.636508	0.136508	246.9     	116
i3	76.636508	0.136508	329.6     	116
i3	76.636508	0.136508	415.292983	116
i3	76.909070	0.273016	246.9     	127
i3	76.909070	0.409297	415.292983	127
i3	76.909070	0.409297	329.6     	127
i3	77.181859	0.136508	164.804481	113
i3	77.181859	0.136508	246.9     	113
i3	77.318141	0.136735	329.6     	127
i3	77.318141	0.136735	440.0     	127
i3	77.318141	0.136735	277.20    	127
i3	77.590930	0.136735	329.6     	116
i3	77.590930	0.136735	277.20    	116
i3	77.590930	0.136735	440.0     	116
i3	77.863719	0.136508	329.6     	127
i3	77.863719	0.136508	277.20    	127
i3	78.000000	0.136735	277.20    	113
i3	77.863719	0.341043	440.0     	127
i3	78.000000	0.204762	329.6     	113
i3	78.136508	0.136508	220.0     	87
i3	78.136508	0.136508	277.20    	87
i3	78.272789	0.136508	277.20    	125
i3	78.272789	0.204762	440.0     	125
i3	78.272789	0.204762	329.6     	125
i3	78.409070	0.136735	277.20    	87
i3	78.409070	0.136735	220.0     	87
i3	78.545578	0.136508	311.126982	122
i3	78.545578	0.136508	493.869370	122
i3	78.545578	0.136508	369.994421	122
i3	78.818141	0.136735	311.126982	110
i3	78.818141	0.136735	493.869370	110
i3	78.818141	0.136735	369.994421	110
i3	79.090930	0.273016	311.126982	125
i3	79.090930	0.273016	369.994421	125
i3	79.090930	0.273016	493.869370	125
i3	79.363719	0.136508	246.9     	113
i3	79.363719	0.136508	311.126982	113
i3	79.500000	0.136735	440.0     	125
i3	79.500000	0.136735	277.20    	125
i3	79.500000	0.136735	329.6     	125
i3	79.772789	0.136508	277.20    	110
i3	79.772789	0.136508	329.6     	110
i3	79.772789	0.136508	440.0     	110
i3	80.045578	0.136508	277.20    	119
i3	80.045578	0.136508	440.0     	119
i3	80.045578	0.136508	329.6     	119
i3	80.181859	0.136508	277.20    	101
i3	80.181859	0.204762	329.6     	101
i3	80.181859	0.204762	440.0     	101
i3	80.318141	0.136735	220.0     	95
i3	80.318141	0.136735	277.20    	95
i3	80.454649	0.136508	277.20    	122
i3	80.454649	0.204762	329.6     	122
i3	80.454649	0.216100	440.0     	122
i3	80.590930	0.091156	220.0     	101
i3	80.590930	0.136735	277.20    	101
i3	80.727438	0.136508	415.292983	127
i3	80.727438	0.136508	329.6     	127
i3	80.727438	0.136508	246.9     	127
i3	81.000000	0.136735	246.9     	116
i3	81.000000	0.136735	329.6     	116
i3	81.000000	0.136735	415.292983	116
i3	81.272789	0.273016	246.9     	127
i3	81.272789	0.409297	415.292983	127
i3	81.272789	0.409297	329.6     	127
i3	81.545578	0.136508	164.804481	113
i3	81.545578	0.136508	246.9     	113
i3	81.681859	0.136508	329.6     	127
i3	81.681859	0.136508	440.0     	127
i3	81.681859	0.136508	277.20    	127
i3	81.954649	0.136508	329.6     	116
i3	81.954649	0.136508	277.20    	116
i3	81.954649	0.136508	440.0     	116
i3	82.227438	0.136508	329.6     	127
i3	82.227438	0.136508	277.20    	127
i3	82.363719	0.136508	277.20    	113
i3	82.227438	0.341043	440.0     	127
i3	82.363719	0.204762	329.6     	113
i3	82.500000	0.136735	220.0     	87
i3	82.500000	0.136735	277.20    	87
i3	82.636508	0.136508	277.20    	125
i3	82.636508	0.204762	440.0     	125
i3	82.636508	0.204762	329.6     	125
i3	82.772789	0.136508	277.20    	87
i3	82.772789	0.136508	220.0     	87
i3	82.909070	0.136735	311.126982	122
i3	82.909070	0.136735	493.869370	122
i3	82.909070	0.136735	369.994421	122
i3	83.181859	0.136508	311.126982	110
i3	83.181859	0.136508	493.869370	110
i3	83.181859	0.136508	369.994421	110
i3	83.454649	0.273016	311.126982	125
i3	83.454649	0.273016	369.994421	125
i3	83.454649	0.273016	493.869370	125
i3	83.727438	0.136508	246.9     	113
i3	83.727438	0.136508	311.126982	113
i3	83.863719	0.136508	440.0     	125
i3	83.863719	0.136508	277.20    	125
i3	83.863719	0.136508	329.6     	125
i3	84.136508	0.136508	277.20    	110
i3	84.136508	0.136508	329.6     	110
i3	84.136508	0.136508	440.0     	110
i3	84.409070	0.136735	277.20    	119
i3	84.409070	0.136735	440.0     	119
i3	84.409070	0.136735	329.6     	119
i3	84.545578	0.136508	277.20    	101
i3	84.545578	0.204762	329.6     	101
i3	84.545578	0.204762	440.0     	101
i3	84.681859	0.136508	220.0     	95
i3	84.681859	0.136508	277.20    	95
i3	84.818141	0.136735	277.20    	122
i3	84.818141	0.204989	329.6     	122
i3	84.818141	0.216327	440.0     	122
i3	84.954649	0.091156	220.0     	101
i3	84.954649	0.136508	277.20    	101
i3	85.090930	0.136735	415.292983	127
i3	85.090930	0.136735	329.6     	127
i3	85.090930	0.136735	246.9     	127
i3	85.363719	0.136508	246.9     	116
i3	85.363719	0.136508	329.6     	116
i3	85.363719	0.136508	415.292983	116
i3	85.636508	0.272789	246.9     	127
i3	85.636508	0.409297	415.292983	127
i3	85.636508	0.409297	329.6     	127
i3	85.909070	0.136735	164.804481	113
i3	85.909070	0.136735	246.9     	113
i3	86.045578	0.136508	329.6     	127
i3	86.045578	0.136508	440.0     	127
i3	86.045578	0.136508	277.20    	127
i3	86.318141	0.136735	329.6     	116
i3	86.318141	0.136735	277.20    	116
i3	86.318141	0.136735	440.0     	116
i3	86.590930	0.136735	329.6     	127
i3	86.590930	0.136735	277.20    	127
i3	86.727438	0.136508	277.20    	113
i3	86.590930	0.341270	440.0     	127
i3	86.727438	0.204762	329.6     	113
i3	86.863719	0.136508	220.0     	87
i3	86.863719	0.136508	277.20    	87
i3	87.000000	0.136735	277.20    	125
i3	87.000000	0.204762	440.0     	125
i3	87.000000	0.204762	329.6     	125
i3	87.136508	0.136508	277.20    	87
i3	87.136508	0.136508	220.0     	87
i3	87.272789	0.136508	311.126982	122
i3	87.272789	0.136508	493.869370	122
i3	87.272789	0.136508	369.994421	122
i3	87.545578	0.136508	311.126982	110
i3	87.545578	0.136508	493.869370	110
i3	87.545578	0.136508	369.994421	110
i3	87.818367	0.272789	311.126982	125
i3	87.818367	0.272789	369.994421	125
i3	87.818367	0.272789	493.869370	125
i3	88.090930	0.136735	246.9     	113
i3	88.090930	0.136735	311.126982	113
i3	88.227438	0.136508	440.0     	125
i3	88.227438	0.136508	277.20    	125
i3	88.227438	0.136508	329.6     	125
i3	88.500000	0.136735	277.20    	110
i3	88.500000	0.136735	329.6     	110
i3	88.500000	0.136735	440.0     	110
i3	88.772789	0.136508	277.20    	119
i3	88.772789	0.136508	440.0     	119
i3	88.772789	0.136508	329.6     	119
i3	88.909070	0.136735	277.20    	101
i3	88.909070	0.204762	329.6     	101
i3	88.909070	0.204762	440.0     	101
i3	89.045578	0.136508	220.0     	95
i3	89.045578	0.136508	277.20    	95
i3	89.181859	0.136735	277.20    	122
i3	89.181859	0.204762	329.6     	122
i3	89.181859	0.216100	440.0     	122
i3	89.318367	0.090930	220.0     	101
i3	89.318367	0.136508	277.20    	101
i3	89.454649	0.136508	415.292983	127
i3	89.454649	0.136508	329.6     	127
i3	89.454649	0.136508	246.9     	127
i3	89.727438	0.136508	246.9     	116
i3	89.727438	0.136508	329.6     	116
i3	89.727438	0.136508	415.292983	116
i3	90.000000	0.273016	246.9     	127
i3	90.000000	0.409297	415.292983	127
i3	90.000000	0.409297	329.6     	127
i3	90.272789	0.136508	164.804481	113
i3	90.272789	0.136508	246.9     	113
i3	90.409070	0.136735	329.6     	127
i3	90.409070	0.136735	440.0     	127
i3	90.409070	0.136735	277.20    	127
i3	90.681859	0.136735	329.6     	116
i3	90.681859	0.136735	277.20    	116
i3	90.681859	0.136735	440.0     	116
i3	90.954649	0.136508	329.6     	127
i3	90.954649	0.136508	277.20    	127
i3	91.090930	0.136735	277.20    	113
i3	90.954649	0.341043	440.0     	127
i3	91.090930	0.204762	329.6     	113
i3	91.227438	0.136508	220.0     	87
i3	91.227438	0.136508	277.20    	87
i3	91.363719	0.136508	277.20    	125
i3	91.363719	0.204762	440.0     	125
i3	91.363719	0.204762	329.6     	125
i3	91.500000	0.136735	277.20    	87
i3	91.500000	0.136735	220.0     	87
i3	91.636508	0.136508	311.126982	122
i3	91.636508	0.136508	493.869370	122
i3	91.636508	0.136508	369.994421	122
i3	91.909070	0.136735	311.126982	110
i3	91.909070	0.136735	493.869370	110
i3	91.909070	0.136735	369.994421	110
i3	92.181859	0.273016	311.126982	125
i3	92.181859	0.273016	369.994421	125
i3	92.181859	0.273016	493.869370	125
i3	92.454649	0.136508	246.9     	113
i3	92.454649	0.136508	311.126982	113
i3	92.590930	0.136735	440.0     	125
i3	92.590930	0.136735	277.20    	125
i3	92.590930	0.136735	329.6     	125
i3	92.863719	0.136508	277.20    	110
i3	92.863719	0.136508	329.6     	110
i3	92.863719	0.136508	440.0     	110
i3	93.136508	0.136508	277.20    	119
i3	93.136508	0.136508	440.0     	119
i3	93.136508	0.136508	329.6     	119
i3	93.272789	0.136508	277.20    	101
i3	93.272789	0.204762	329.6     	101
i3	93.272789	0.204762	440.0     	101
i3	93.409070	0.136735	220.0     	95
i3	93.409070	0.136735	277.20    	95
i3	93.545578	0.136508	277.20    	122
i3	93.545578	0.204762	329.6     	122
i3	93.545578	0.216100	440.0     	122
i3	93.681859	0.091156	220.0     	101
i3	93.681859	0.136735	277.20    	101
i3	93.818367	0.136508	415.292983	127
i3	93.818367	0.136508	329.6     	127
i3	93.818367	0.136508	246.9     	127
i3	94.090930	0.136735	246.9     	116
i3	94.090930	0.136735	329.6     	116
i3	94.090930	0.136735	415.292983	116
i3	94.363719	0.273016	246.9     	127
i3	94.363719	0.409297	415.292983	127
i3	94.363719	0.409297	329.6     	127
i3	94.636508	0.136508	164.804481	113
i3	94.636508	0.136508	246.9     	113
i3	94.772789	0.136508	329.6     	127
i3	94.772789	0.136508	440.0     	127
i3	94.772789	0.136508	277.20    	127
i3	95.045578	0.136508	329.6     	116
i3	95.045578	0.136508	277.20    	116
i3	95.045578	0.136508	440.0     	116
i3	95.318367	0.136508	329.6     	127
i3	95.318367	0.136508	277.20    	127
i3	95.454649	0.136508	277.20    	113
i3	95.318367	0.341043	440.0     	127
i3	95.454649	0.204762	329.6     	113
i3	95.590930	0.136735	220.0     	87
i3	95.590930	0.136735	277.20    	87
i3	95.727438	0.136508	277.20    	125
i3	95.727438	0.204762	440.0     	125
i3	95.727438	0.204762	329.6     	125
i3	95.863719	0.136508	277.20    	87
i3	95.863719	0.136508	220.0     	87
i3	96.000000	0.136735	311.126982	122
i3	96.000000	0.136735	493.869370	122
i3	96.000000	0.136735	369.994421	122
i3	96.272789	0.136508	311.126982	110
i3	96.272789	0.136508	493.869370	110
i3	96.272789	0.136508	369.994421	110
i3	96.545578	0.273016	311.126982	125
i3	96.545578	0.273016	369.994421	125
i3	96.545578	0.273016	493.869370	125
i3	96.818367	0.136508	246.9     	113
i3	96.818367	0.136508	311.126982	113
i3	96.954649	0.136508	440.0     	125
i3	96.954649	0.136508	277.20    	125
i3	96.954649	0.136508	329.6     	125
i3	97.2	0.136508	277.20    	110
i3	97.2	0.136508	329.6     	110
i3	97.2	0.136508	440.0     	110
i3	97.5	0.136735	277.20    	119
i3	97.5	0.136735	440.0     	119
i3	97.5	0.136735	329.6     	119
i3	97.6	0.136508	277.20    	101
i3	97.6	0.204762	329.6     	101
i3	97.6	0.204762	440.0     	101
i3	97.8	0.136508	220.0     	95
i3	97.8	0.136508	277.20    	95
i3	97.9	0.136735	277.20    	122
i3	97.9	0.204762	329.6     	122
i3	97.9	0.216327	440.0     	122
i3	98.045578	0.091156	220.0     	101
i3	98.045578	0.136508	277.20    	101
i3	98.181859	0.136735	415.292983	127
i3	98.181859	0.136735	329.6     	127
i3	98.181859	0.136735	246.9     	127
i3	98.454649	0.136508	246.9     	116
i3	98.454649	0.136508	329.6     	116
i3	98.454649	0.136508	415.292983	116
i3	98.727438	0.272789	246.9     	127
i3	98.727438	0.409297	415.292983	127
i3	98.727438	0.409297	329.6     	127
i3	99.000000	0.136735	164.804481	113
i3	99.000000	0.136735	246.9     	113
i3	99.136508	0.136508	329.6     	127
i3	99.136508	0.136508	440.0     	127
i3	99.136508	0.136508	277.20    	127
i3	99.409070	0.136735	329.6     	116
i3	99.409070	0.136735	277.20    	116
i3	99.409070	0.136735	440.0     	116
i3	99.681859	0.136735	329.6     	127
i3	99.681859	0.136735	277.20    	127
i3	99.818367	0.136508	277.20    	113
i3	99.681859	0.341270	440.0     	127
i3	99.818367	0.204762	329.6     	113
i3	99.954649	0.136508	220.0     	87
i3	99.954649	0.136508	277.20    	87
i3	100.090930	0.136735	277.20    	125
i3	100.090930	0.204762	440.0     	125
i3	100.090930	0.204762	329.6     	125
i3	100.227438	0.136508	277.20    	87
i3	100.227438	0.136508	220.0     	87
i3	100.363719	0.136508	311.126982	122
i3	100.363719	0.136508	493.869370	122
i3	100.363719	0.136508	369.994421	122
i3	100.636508	0.136508	311.126982	110
i3	100.636508	0.136508	493.869370	110
i3	100.636508	0.136508	369.994421	110
i3	100.909070	0.273016	311.126982	125
i3	100.909070	0.273016	369.994421	125
i3	100.909070	0.273016	493.869370	125
i3	101.181859	0.136735	246.9     	113
i3	101.181859	0.136735	311.126982	113
i3	101.318367	0.136508	440.0     	125
i3	101.318367	0.136508	277.20    	125
i3	101.318367	0.136508	329.6     	125
i3	101.590930	0.136735	277.20    	110
i3	101.590930	0.136735	329.6     	110
i3	101.590930	0.136735	440.0     	110
i3	101.863719	0.136508	277.20    	119
i3	101.863719	0.136508	440.0     	119
i3	101.863719	0.136508	329.6     	119
i3	102.000000	0.136735	277.20    	101
i3	102.000000	0.204762	329.6     	101
i3	102.000000	0.204762	440.0     	101
i3	102.136508	0.136508	220.0     	95
i3	102.136508	0.136508	277.20    	95
i3	102.272789	0.136508	277.20    	122
i3	102.272789	0.204762	329.6     	122
i3	102.272789	0.216100	440.0     	122
i3	102.409070	0.091156	220.0     	101
i3	102.409070	0.136735	277.20    	101
i3	102.545578	0.136508	415.292983	127
i3	102.545578	0.136508	329.6     	127
i3	102.545578	0.136508	246.9     	127
i3	102.818367	0.136508	246.9     	116
i3	102.818367	0.136508	329.6     	116
i3	102.818367	0.136508	415.292983	116
i3	103.090930	0.273016	246.9     	127
i3	103.090930	0.409297	415.292983	127
i3	103.090930	0.409297	329.6     	127
i3	103.363719	0.136508	164.804481	113
i3	103.363719	0.136508	246.9     	113
i3	103.500000	0.136735	329.6     	127
i3	103.500000	0.136735	440.0     	127
i3	103.500000	0.136735	277.20    	127
i3	103.772789	0.136508	329.6     	116
i3	103.772789	0.136508	277.20    	116
i3	103.772789	0.136508	440.0     	116
i3	104.045578	0.136508	329.6     	127
i3	104.045578	0.136508	277.20    	127
i3	104.181859	0.136735	277.20    	113
i3	104.045578	0.341043	440.0     	127
i3	104.181859	0.204762	329.6     	113
i3	104.318367	0.136508	220.0     	87
i3	104.318367	0.136508	277.20    	87
i3	104.454649	0.136508	277.20    	125
i3	104.454649	0.204762	440.0     	125
i3	104.454649	0.204762	329.6     	125
i3	104.590930	0.136735	277.20    	87
i3	104.590930	0.136735	220.0     	87
i3	104.727438	0.136508	311.126982	122
i3	104.727438	0.136508	493.869370	122
i3	104.727438	0.136508	369.994421	122
i3	105.000000	0.136735	311.126982	110
i3	105.000000	0.136735	493.869370	110
i3	105.000000	0.136735	369.994421	110
i3	105.272789	0.273016	311.126982	125
i3	105.272789	0.273016	369.994421	125
i3	105.272789	0.273016	493.869370	125
i3	105.545578	0.136508	246.9     	113
i3	105.545578	0.136508	311.126982	113
i3	105.681859	0.136735	440.0     	125
i3	105.681859	0.136735	277.20    	125
i3	105.681859	0.136735	329.6     	125
i3	105.954649	0.136508	277.20    	110
i3	105.954649	0.136508	329.6     	110
i3	105.954649	0.136508	440.0     	110
i3	106.227438	0.136508	277.20    	119
i3	106.227438	0.136508	440.0     	119
i3	106.227438	0.136508	329.6     	119
i3	106.363719	0.136508	277.20    	101
i3	106.363719	0.204762	329.6     	101
i3	106.363719	0.204762	440.0     	101
i3	106.500000	0.136735	220.0     	95
i3	106.500000	0.136735	277.20    	95
i3	106.636508	0.136508	277.20    	122
i3	106.636508	0.204762	329.6     	122
i3	106.636508	0.216100	440.0     	122
i3	106.772789	0.091156	220.0     	101
i3	106.772789	0.136508	277.20    	101
i3	106.909070	0.136735	415.292983	127
i3	106.909070	0.136735	329.6     	127
i3	106.909070	0.136735	246.9     	127
i3	107.181859	0.136735	246.9     	116
i3	107.181859	0.136735	329.6     	116
i3	107.181859	0.136735	415.292983	116
i3	107.454649	0.273016	246.9     	127
i3	107.454649	0.409297	415.292983	127
i3	107.454649	0.409297	329.6     	127
i3	107.727438	0.136508	164.804481	113
i3	107.727438	0.136508	246.9     	113
i3	107.863719	0.136508	329.6     	127
i3	107.863719	0.136508	440.0     	127
i3	107.863719	0.136508	277.20    	127
i3	108.136508	0.136508	329.6     	116
i3	108.136508	0.136508	277.20    	116
i3	108.136508	0.136508	440.0     	116
i3	108.409070	0.136735	329.6     	127
i3	108.409070	0.136735	277.20    	127
i3	108.545578	0.136508	277.20    	113
i3	108.409070	0.341270	440.0     	127
i3	108.545578	0.204762	329.6     	113
i3	108.681859	0.136735	220.0     	87
i3	108.681859	0.136735	277.20    	87
i3	108.818367	0.136508	277.20    	125
i3	108.818367	0.204762	440.0     	125
i3	108.818367	0.204762	329.6     	125
i3	108.954649	0.136508	277.20    	87
i3	108.954649	0.136508	220.0     	87
i3	109.090930	0.136735	311.126982	122
i3	109.090930	0.136735	493.869370	122
i3	109.090930	0.136735	369.994421	122
i3	109.363719	0.136508	311.126982	110
i3	109.363719	0.136508	493.869370	110
i3	109.363719	0.136508	369.994421	110
i3	109.636508	0.272789	311.126982	125
i3	109.636508	0.272789	369.994421	125
i3	109.636508	0.272789	493.869370	125
i3	109.909070	0.136735	246.9     	113
i3	109.909070	0.136735	311.126982	113
i3	110.045578	0.136508	440.0     	125
i3	110.045578	0.136508	277.20    	125
i3	110.045578	0.136508	329.6     	125
i3	110.318367	0.136508	277.20    	110
i3	110.318367	0.136508	329.6     	110
i3	110.318367	0.136508	440.0     	110
i3	110.590930	0.136735	277.20    	119
i3	110.590930	0.136735	440.0     	119
i3	110.590930	0.136735	329.6     	119
i3	110.727438	0.136508	277.20    	101
i3	110.727438	0.204762	329.6     	101
i3	110.727438	0.204762	440.0     	101
i3	110.863719	0.136508	220.0     	95
i3	110.863719	0.136508	277.20    	95
i3	111.000000	0.136735	277.20    	122
i3	111.000000	0.204762	329.6     	122
i3	111.000000	0.216327	440.0     	122
i3	111.136508	0.091156	220.0     	101
i3	111.136508	0.136508	277.20    	101
i3	111.272789	0.136735	246.9     	127
i3	111.272789	0.136735	311.126982	127
i3	111.272789	0.136735	415.292983	127
i3	111.545578	0.136508	246.9     	116
i3	111.545578	0.136508	311.126982	116
i3	111.545578	0.136508	415.292983	116
i3	111.818367	0.272789	246.9     	127
i3	111.818367	0.409297	311.126982	127
i3	111.818367	0.409297	415.292983	127
i3	112.090930	0.136735	246.9     	113
i3	112.090930	0.136735	184.997211	113
i3	112.227438	0.136508	246.9     	127
i3	112.227438	0.136508	311.126982	127
i3	112.227438	0.136508	415.292983	127
i3	112.500000	0.136735	246.9     	116
i3	112.500000	0.136735	311.126982	116
i3	112.500000	0.136735	415.292983	116
i3	112.772789	0.136735	246.9     	127
i3	112.772789	0.136735	311.126982	127
i3	112.909297	0.136508	246.9     	113
i3	112.772789	0.341270	415.292983	127
i3	112.909297	0.204762	311.126982	113
i3	113.045578	0.136508	246.9     	87
i3	113.045578	0.136508	184.997211	87
i3	113.181859	0.136735	246.9     	125
i3	113.181859	0.204762	315.0   	125
i3	113.181859	0.204762	415.292983	125
i3	113.318367	0.136508	184.997211	87
i3	113.318367	0.136508	246.9     	87
i3	113.454649	0.136508	277.20    	122
i3	113.454649	0.136508	329.6     	122
i3	113.454649	0.136508	440.0     	122
i3	113.727438	0.136508	277.20    	110
i3	113.727438	0.136508	329.6     	110
i3	113.727438	0.136508	440.0     	110
i3	114.000000	0.273016	277.20    	125
i3	114.000000	0.273016	329.6     	125
i3	114.000000	0.273016	440.0     	125
i3	114.272789	0.136735	220.0     	113
i3	114.272789	0.136735	277.20    	113
i3	114.409297	0.136508	440.0     	125
i3	114.409297	0.136508	277.20    	125
i3	114.409297	0.136508	329.6     	125
i3	114.681859	0.136735	277.20    	110
i3	114.681859	0.136735	329.6     	110
i3	114.681859	0.136735	440.0     	110
i3	114.954649	0.136508	277.20    	119
i3	114.954649	0.136508	440.0     	119
i3	114.954649	0.136508	329.6     	119
i3	115.090930	0.136735	277.20    	101
i3	115.090930	0.204762	329.6     	101
i3	115.090930	0.204762	440.0     	101
i3	115.227438	0.136508	220.0     	95
i3	115.227438	0.136508	277.20    	95
i3	115.363719	0.136508	277.20    	122
i3	115.363719	0.204762	329.6     	122
i3	115.363719	0.216100	440.0     	122
i3	115.500000	0.091156	220.0     	101
i3	115.500000	0.136735	277.20    	101
i3	115.636508	0.136508	246.9     	127
i3	115.636508	0.136508	311.126982	127
i3	115.636508	0.136508	415.292983	127
i3	115.909297	0.136508	246.9     	116
i3	115.909297	0.136508	311.126982	116
i3	115.909297	0.136508	415.292983	116
i3	116.181859	0.273016	246.9     	127
i3	116.181859	0.409297	311.126982	127
i3	116.181859	0.409297	415.292983	127
i3	116.454649	0.136508	246.9     	113
i3	116.454649	0.136508	184.997211	113
i3	116.590930	0.136735	246.9     	127
i3	116.590930	0.136735	311.126982	127
i3	116.590930	0.136735	415.292983	127
i3	116.863719	0.136508	246.9     	116
i3	116.863719	0.136508	311.126982	116
i3	116.863719	0.136508	415.292983	116
i3	117.136508	0.136508	246.9     	127
i3	117.136508	0.136508	311.126982	127
i3	117.272789	0.136735	246.9     	113
i3	117.136508	0.341043	415.292983	127
i3	117.272789	0.204762	311.126982	113
i3	117.409297	0.136508	246.9     	87
i3	117.409297	0.136508	184.997211	87
i3	117.545578	0.136508	246.9     	125
i3	117.545578	0.204762	311.126982	125
i3	117.545578	0.204762	415.292983	125
i3	117.681859	0.136735	184.997211	87
i3	117.681859	0.136735	246.9     	87
i3	117.818367	0.136508	277.20    	122
i3	117.818367	0.136508	329.6     	122
i3	117.818367	0.136508	440.0     	122
i3	118.090930	0.136735	277.20    	110
i3	118.090930	0.136735	329.6     	110
i3	118.090930	0.136735	440.0     	110
i3	118.363719	0.273016	277.20    	125
i3	118.363719	0.273016	329.6     	125
i3	118.363719	0.273016	440.0     	125
i3	118.636508	0.136508	220.0     	113
i3	118.636508	0.136508	277.20    	113
i3	118.772789	0.136735	440.0     	125
i3	118.772789	0.136735	277.20    	125
i3	118.772789	0.136735	329.6     	125
i3	119.045578	0.136508	277.20    	110
i3	119.045578	0.136508	329.6     	110
i3	119.045578	0.136508	440.0     	110
i3	119.318367	0.136508	277.20    	119
i3	119.318367	0.136508	440.0     	119
i3	119.318367	0.136508	329.6     	119
i3	119.454649	0.136508	277.20    	101
i3	119.454649	0.204762	329.6     	101
i3	119.454649	0.204762	440.0     	101
i3	119.590930	0.136735	220.0     	95
i3	119.590930	0.136735	277.20    	95
i3	119.727438	0.136508	277.20    	122
i3	119.727438	0.204762	329.6     	122
i3	119.727438	0.216100	440.0     	122
i3	119.863719	0.091156	220.0     	101
i3	119.863719	0.136508	277.20    	101
i3	120.000000	0.136735	246.9     	127
i3	120.000000	0.136735	311.126982	127
i3	120.000000	0.136735	415.292983	127
i3	120.272789	0.136735	246.9     	116
i3	120.272789	0.136735	311.126982	116
i3	120.272789	0.136735	415.292983	116
i3	120.545578	0.273016	246.9     	127
i3	120.545578	0.409297	311.126982	127
i3	120.545578	0.409297	415.292983	127
i3	120.818367	0.136508	246.9     	113
i3	120.818367	0.136508	184.997211	113
i3	120.954649	0.136508	246.9     	127
i3	120.954649	0.136508	311.126982	127
i3	120.954649	0.136508	415.292983	127
i3	121.227438	0.136508	246.9     	116
i3	121.227438	0.136508	311.126982	116
i3	121.227438	0.136508	415.292983	116
i3	121.500000	0.136735	246.9     	127
i3	121.500000	0.136735	311.126982	127
i3	121.636508	0.136508	246.9     	113
i3	121.500000	0.341270	415.292983	127
i3	121.636508	0.204762	311.126982	113
i3	121.772789	0.136735	246.9     	87
i3	121.772789	0.136735	184.997211	87
i3	121.909297	0.136508	246.9     	125
i3	121.909297	0.204762	311.126982	125
i3	121.909297	0.204762	415.292983	125
i3	122.045578	0.136508	184.997211	87
i3	122.045578	0.136508	246.9     	87
i3	122.181859	0.136735	277.20    	122
i3	122.181859	0.136735	329.6     	122
i3	122.181859	0.136735	440.0     	122
i3	122.454649	0.136508	277.20    	110
i3	122.454649	0.136508	329.6     	110
i3	122.454649	0.136508	440.0     	110
i3	122.727438	0.272789	277.20    	125
i3	122.727438	0.272789	329.6     	125
i3	122.727438	0.272789	440.0     	125
i3	123.000000	0.136735	220.0     	113
i3	123.000000	0.136735	277.20    	113
i3	123.136508	0.136508	440.0     	125
i3	123.136508	0.136508	277.20    	125
i3	123.136508	0.136508	329.6     	125
i3	123.409297	0.136508	277.20    	110
i3	123.409297	0.136508	329.6     	110
i3	123.409297	0.136508	440.0     	110
i3	123.681859	0.136735	277.20    	119
i3	123.681859	0.136735	440.0     	119
i3	123.681859	0.136735	329.6     	119
i3	123.818367	0.136508	277.20    	101
i3	123.818367	0.204762	329.6     	101
i3	123.818367	0.204762	440.0     	101
i3	123.954649	0.136508	220.0     	95
i3	123.954649	0.136508	277.20    	95
i3	124.090930	0.136735	277.20    	122
i3	124.090930	0.204762	329.6     	122
i3	124.090930	0.216327	440.0     	122
i3	124.227438	0.091156	220.0     	101
i3	124.227438	0.136508	277.20    	101
i3	124.363719	0.136508	415.292983	127
i3	124.363719	0.136508	329.6     	127
i3	124.363719	0.136508	246.9     	127
i3	124.636508	0.136508	246.9     	116
i3	124.636508	0.136508	329.6     	116
i3	124.636508	0.136508	415.292983	116
i3	124.909297	0.272789	246.9     	127
i3	124.909297	0.409297	415.292983	127
i3	124.909297	0.409297	329.6     	127
i3	125.181859	0.136735	164.804481	113
i3	125.181859	0.136735	246.9     	113
i3	125.318367	0.136508	329.6     	127
i3	125.318367	0.136508	440.0     	127
i3	125.318367	0.136508	277.20    	127
i3	125.590930	0.136735	329.6     	116
i3	125.590930	0.136735	277.20    	116
i3	125.590930	0.136735	440.0     	116
i3	125.863719	0.136508	329.6     	127
i3	125.863719	0.136508	277.20    	127
i3	126.000000	0.136735	277.20    	113
i3	125.863719	0.341270	440.0     	127
i3	126.000000	0.204989	329.6     	113
i3	126.136508	0.136508	220.0     	87
i3	126.136508	0.136508	277.20    	87
i3	126.272789	0.136735	277.20    	125
i3	126.272789	0.204762	440.0     	125
i3	126.272789	0.204762	329.6     	125
i3	126.409297	0.136508	277.20    	87
i3	126.409297	0.136508	220.0     	87
i3	126.545578	0.136508	311.126982	122
i3	126.545578	0.136508	493.869370	122
i3	126.545578	0.136508	369.994421	122
i3	126.818367	0.136508	311.126982	110
i3	126.818367	0.136508	493.869370	110
i3	126.818367	0.136508	369.994421	110
i3	127.090930	0.273016	311.126982	125
i3	127.090930	0.273016	369.994421	125
i3	127.090930	0.273016	493.869370	125
i3	127.363719	0.136508	246.9     	113
i3	127.363719	0.136508	311.126982	113
i3	127.500000	0.136735	440.0     	125
i3	127.500000	0.136735	277.20    	125
i3	127.500000	0.136735	329.6     	125
i3	127.772789	0.136735	277.20    	110
i3	127.772789	0.136735	329.6     	110
i3	127.772789	0.136735	440.0     	110
i3	128.045578	0.136508	277.20    	119
i3	128.045578	0.136508	440.0     	119
i3	128.045578	0.136508	329.6     	119
i3	128.181859	0.136735	277.20    	101
i3	128.181859	0.204762	329.6     	101
i3	128.181859	0.204762	440.0     	101
i3	128.318367	0.136508	220.0     	95
i3	128.318367	0.136508	277.20    	95
i3	128.454649	0.136508	277.20    	122
i3	128.454649	0.204762	329.6     	122
i3	128.454649	0.216100	440.0     	122
i3	128.590930	0.091156	220.0     	101
i3	128.590930	0.136735	277.20    	101
i3	128.727438	0.136508	415.292983	127
i3	128.727438	0.136508	329.6     	127
i3	128.727438	0.136508	246.9     	127
i3	129.000000	0.136735	246.9     	116
i3	129.000000	0.136735	329.6     	116
i3	129.000000	0.136735	415.292983	116
i3	129.272789	0.273016	246.9     	127
i3	129.272789	0.409297	415.292983	127
i3	129.272789	0.409297	329.6     	127
i3	129.545578	0.136508	164.804481	113
i3	129.545578	0.136508	246.9     	113
i3	129.69  	0.136735	329.6     	126
i3	129.681859	0.136735	440.0     	127
i3	129.681859	0.136735	277.20    	127
i3	129.954649	0.136508	329.6     	116
i3	129.954649	0.136508	277.20    	116
i3	129.954649	0.136508	440.0     	116
i3	130.227438	0.136508	329.6     	127
i3	130.227438	0.136508	277.20    	127
i3	130.363719	0.136508	277.20    	113
i3	130.227438	0.341043	440.0     	127
i3	130.363719	0.204762	329.6     	113
i3	130.500000	0.136735	220.0     	87
i3	130.500000	0.136735	277.20    	87
i3	130.636508	0.136508	277.20    	125
i3	130.636508	0.204762	440.0     	125
i3	130.636508	0.204762	329.6     	125
i3	130.772789	0.136735	277.20    	87
i3	130.772789	0.136735	220.0     	87
i3	130.909297	0.136508	311.126982	122
i3	130.909297	0.136508	493.869370	122
i3	130.909297	0.136508	369.994421	122
i3	131.181859	0.136735	311.126982	110
i3	131.181859	0.136735	493.869370	110
i3	131.181859	0.136735	369.994421	110
i3	131.454649	0.273016	311.126982	125
i3	131.454649	0.273016	369.994421	125
i3	131.454649	0.273016	493.869370	125
i3	131.727438	0.136508	246.9     	113
i3	131.727438	0.136508	311.126982	113
i3	131.863719	0.136508	440.0     	125
i3	131.863719	0.136508	277.20    	125
i3	131.863719	0.136508	329.6     	125
i3	132.136508	0.136508	277.20    	110
i3	132.136508	0.136508	329.6     	110
i3	132.136508	0.136508	440.0     	110
i3	132.409297	0.136508	277.20    	119
i3	132.409297	0.136508	440.0     	119
i3	132.409297	0.136508	329.6     	119
i3	132.545578	0.136508	277.20    	101
i3	132.545578	0.204762	329.6     	101
i3	132.545578	0.204762	440.0     	101
i3	132.681859	0.136735	220.0     	95
i3	132.681859	0.136735	277.20    	95
i3	132.818367	0.136508	277.20    	122
i3	132.818367	0.204762	329.6     	122
i3	132.818367	0.216100	440.0     	122
i3	132.954649	0.091156	220.0     	101
i3	132.954649	0.136508	277.20    	101
i3	133.090930	0.136735	415.292983	127
i3	133.090930	0.136735	329.6     	127
i3	133.090930	0.136735	246.9     	127
i3	133.363719	0.136508	246.9     	116
i3	133.363719	0.136508	329.6     	116
i3	133.363719	0.136508	415.292983	116
i3	133.636508	0.273016	246.9     	127
i3	133.636508	0.409297	415.292983	127
i3	133.636508	0.409297	329.6     	127
i3	133.909297	0.136508	164.804481	113
i3	133.909297	0.136508	246.9     	113
i3	134.045578	0.136508	329.6     	127
i3	134.045578	0.136508	440.0     	127
i3	134.045578	0.136508	277.20    	127
i3	134.318367	0.136508	329.6     	116
i3	134.318367	0.136508	277.20    	116
i3	134.318367	0.136508	440.0     	116
i3	134.590930	0.136735	329.6     	127
i3	134.590930	0.136735	277.20    	127
i3	134.727438	0.136508	277.20    	113
i3	134.590930	0.341270	440.0     	127
i3	134.727438	0.204762	329.6     	113
i3	134.863719	0.136508	220.0     	87
i3	134.863719	0.136508	277.20    	87
i3	135.000000	0.136735	277.20    	125
i3	135.000000	0.204989	440.0     	125
i3	135.000000	0.204989	329.6     	125
i3	135.136508	0.136508	277.20    	87
i3	135.136508	0.136508	220.0     	87
i3	135.272789	0.136735	311.126982	122
i3	135.272789	0.136735	493.869370	122
i3	135.272789	0.136735	369.994421	122
i3	135.545578	0.136508	311.126982	110
i3	135.545578	0.136508	493.869370	110
i3	135.545578	0.136508	369.994421	110
i3	135.818367	0.272789	311.126982	125
i3	135.818367	0.272789	369.994421	125
i3	135.818367	0.272789	493.869370	125
i3	136.090930	0.136735	246.9     	113
i3	136.090930	0.136735	311.126982	113
i3	136.227438	0.136508	440.0     	125
i3	136.227438	0.136508	277.20    	125
i3	136.227438	0.136508	329.6     	125
i3	136.500227	0.136508	277.20    	110
i3	136.500227	0.136508	329.6     	110
i3	136.500227	0.136508	440.0     	110
i3	136.772789	0.136735	277.20    	119
i3	136.772789	0.136735	440.0     	119
i3	136.772789	0.136735	329.6     	119
i3	136.909297	0.136508	277.20    	101
i3	136.909297	0.204762	329.6     	101
i3	136.909297	0.204762	440.0     	101
i3	137.045578	0.136508	220.0     	95
i3	137.045578	0.136508	277.20    	95
i3	137.181859	0.136735	277.20    	122
i3	137.181859	0.204762	329.6     	122
i3	137.181859	0.216100	440.0     	122
i3	137.318367	0.091156	220.0     	101
i3	137.318367	0.136508	277.20    	101
i3	137.454649	0.136508	415.292983	127
i3	137.454649	0.136508	329.6     	127
i3	137.454649	0.136508	246.9     	127
i3	137.727438	0.136508	246.9     	116
i3	137.727438	0.136508	329.6     	116
i3	137.727438	0.136508	415.292983	116
i3	138.000227	0.272789	246.9     	127
i3	138.000227	0.409297	415.292983	127
i3	138.000227	0.409297	329.6     	127
i3	138.272789	0.136735	164.804481	113
i3	138.272789	0.136735	246.9     	113
i3	138.409297	0.136508	329.6     	127
i3	138.409297	0.136508	440.0     	127
i3	138.409297	0.136508	277.20    	127
i3	138.681859	0.136735	329.6     	116
i3	138.681859	0.136735	277.20    	116
i3	138.681859	0.136735	440.0     	116
i3	138.954649	0.136508	329.6     	127
i3	138.954649	0.136508	277.20    	127
i3	139.090930	0.136735	277.20    	113
i3	138.954649	0.341043	440.0     	127
i3	139.090930	0.204762	329.6     	113
i3	139.227438	0.136508	220.0     	87
i3	139.227438	0.136508	277.20    	87
i3	139.363719	0.136735	277.20    	125
i3	139.363719	0.204762	440.0     	125
i3	139.363719	0.204762	329.6     	125
i3	139.500227	0.136508	277.20    	87
i3	139.500227	0.136508	220.0     	87
i3	139.636508	0.136508	311.126982	122
i3	139.636508	0.136508	493.869370	122
i3	139.636508	0.136508	369.994421	122
i3	139.909297	0.136508	311.126982	110
i3	139.909297	0.136508	493.869370	110
i3	139.909297	0.136508	369.994421	110
i3	140.181859	0.273016	311.126982	125
i3	140.181859	0.273016	369.994421	125
i3	140.181859	0.273016	493.869370	125
i3	140.454649	0.136508	246.9     	113
i3	140.454649	0.136508	311.126982	113
i3	140.590930	0.136735	440.0     	125
i3	140.590930	0.136735	277.20    	125
i3	140.590930	0.136735	329.6     	125
i3	140.863719	0.136735	277.20    	110
i3	140.863719	0.136735	329.6     	110
i3	140.863719	0.136735	440.0     	110
i3	141.136508	0.136508	277.20    	119
i3	141.136508	0.136508	440.0     	119
i3	141.136508	0.136508	329.6     	119
i3	141.272789	0.136735	277.20    	101
i3	141.272789	0.204762	329.6     	101
i3	141.272789	0.204762	440.0     	101
i3	141.409297	0.136508	220.0     	95
i3	141.409297	0.136508	277.20    	95
i3	141.545578	0.136508	277.20    	122
i3	141.545578	0.204762	329.6     	122
i3	141.545578	0.216100	440.0     	122
i3	141.681859	0.091156	220.0     	101
i3	141.681859	0.136735	277.20    	101
i3	141.818367	0.136508	415.292983	127
i3	141.818367	0.136508	329.6     	127
i3	141.818367	0.136508	246.9     	127
i3	142.090930	0.136735	246.9     	116
i3	142.090930	0.136735	329.6     	116
i3	142.090930	0.136735	415.292983	116
i3	142.363719	0.273016	246.9     	127
i3	142.363719	0.409297	415.292983	127
i3	142.363719	0.409297	329.6     	127
i3	142.636508	0.136508	164.804481	113
i3	142.636508	0.136508	246.9     	113
i3	142.772789	0.136735	329.6     	127
i3	142.772789	0.136735	440.0     	127
i3	142.772789	0.136735	277.20    	127
i3	143.045578	0.136508	329.6     	116
i3	143.045578	0.136508	277.20    	116
i3	143.045578	0.136508	440.0     	116
i3	143.318367	0.136508	329.6     	127
i3	143.318367	0.136508	277.20    	127
i3	143.454649	0.136508	277.20    	113
i3	143.318367	0.341043	440.0     	127
i3	143.454649	0.204762	329.6     	113
i3	143.6	0.14	220.0     	87
i3	143.6	0.14	277.20    	87
i3	143.7	0.14	277.20    	125
i3	143.7	0.20	440.0     	125
i3	143.7	0.20	329.6     	125
i3	143.9	0.14	277.20    	87
i3	143.9	0.14	220.0     	87
i3	144.0	0.14	311.126982	122
i3	144.0	0.14	493.869370	122
i3	144.0	0.14	369.994421	122
i3	144.272789	0.136735	311.126982	110
i3	144.272789	0.136735	493.869370	110
i3	144.272789	0.136735	369.994421	110
i3	144.545578	0.273016	311.126982	125
i3	144.545578	0.273016	369.994421	125
i3	144.545578	0.273016	493.869370	125
i3	144.818367	0.136508	246.9     	113
i3	144.818367	0.136508	311.126982	113
i3	144.954649	0.136508	440.0     	125
i3	144.954649	0.136508	277.20    	125
i3	144.954649	0.136508	329.6     	125
i3	145.227438	0.136508	277.20    	110
i3	145.227438	0.136508	329.6     	110
i3	145.227438	0.136508	440.0     	110
i3	145.500227	0.136508	277.20    	119
i3	145.500227	0.136508	440.0     	119
i3	145.500227	0.136508	329.6     	119
i3	145.636508	0.136508	277.20    	101
i3	145.636508	0.204762	329.6     	101
i3	145.636508	0.204762	440.0     	101
i3	145.772789	0.136735	220.0     	95
i3	145.772789	0.136735	277.20    	95
i3	145.909297	0.136508	277.20    	122
i3	145.909297	0.204762	329.6     	122
i3	145.909297	0.216100	440.0     	122
i3	146.045578	0.091156	220.0     	101
i3	146.045578	0.136508	277.20    	101
i3	146.181859	0.136735	415.292983	127
i3	146.181859	0.136735	329.6     	127
i3	146.181859	0.136735	246.9     	127
i3	146.454649	0.136508	246.9     	116
i3	146.454649	0.136508	329.6     	116
i3	146.454649	0.136508	415.292983	116
i3	146.727438	0.273016	246.9     	127
i3	146.727438	0.409297	415.292983	127
i3	146.727438	0.409297	329.6     	127
i3	147.000227	0.136508	164.804481	113
i3	147.000227	0.136508	246.9     	113
i3	147.136508	0.136508	329.6     	127
i3	147.136508	0.136508	440.0     	127
i3	147.136508	0.136508	277.20    	127
i3	147.409297	0.136508	329.6     	116
i3	147.409297	0.136508	277.20    	116
i3	147.409297	0.136508	440.0     	116
i3	147.681859	0.136735	329.6     	127
i3	147.681859	0.136735	277.20    	127
i3	147.818367	0.136508	277.20    	113
i3	147.681859	0.341270	440.0     	127
i3	147.818367	0.204762	329.6     	113
i3	147.954649	0.136508	220.0     	87
i3	147.954649	0.136508	277.20    	87
i3	148.090930	0.136735	277.20    	125
i3	148.090930	0.204762	440.0     	125
i3	148.090930	0.204762	329.6     	125
i3	148.227438	0.136508	277.20    	87
i3	148.227438	0.136508	220.0     	87
i3	148.363719	0.136735	311.126982	122
i3	148.363719	0.136735	493.869370	122
i3	148.363719	0.136735	369.994421	122
i3	148.636508	0.136508	311.126982	110
i3	148.636508	0.136508	493.869370	110
i3	148.636508	0.136508	369.994421	110
i3	148.909297	0.272789	311.126982	125
i3	148.909297	0.272789	369.994421	125
i3	148.909297	0.272789	493.869370	125
i3	149.181859	0.136735	246.9     	113
i3	149.181859	0.136735	311.126982	113
i3	149.318367	0.136508	440.0     	125
i3	149.318367	0.136508	277.20    	125
i3	149.318367	0.136508	329.6     	125
i3	149.590930	0.136735	277.20    	110
i3	149.590930	0.136735	329.6     	110
i3	149.590930	0.136735	440.0     	110
i3	149.863719	0.136735	277.20    	119
i3	149.863719	0.136735	440.0     	119
i3	149.863719	0.136735	329.6     	119
i3	150.000227	0.136508	277.20    	101
i3	150.000227	0.204762	329.6     	101
i3	150.000227	0.204762	440.0     	101
i3	150.136508	0.136508	220.0     	95
i3	150.136508	0.136508	277.20    	95
i3	150.272789	0.136735	277.20    	122
i3	150.272789	0.204762	329.6     	122
i3	150.272789	0.216100	440.0     	122
i3	150.409297	0.091156	220.0     	101
i3	150.409297	0.136508	277.20    	101
i3	150.545578	0.136508	415.292983	127
i3	150.545578	0.136508	329.6     	127
i3	150.545578	0.136508	246.9     	127
i3	150.818367	0.136508	246.9     	116
i3	150.818367	0.136508	329.6     	116
i3	150.818367	0.136508	415.292983	116
i3	151.090930	0.273016	246.9     	127
i3	151.090930	0.409524	415.292983	127
i3	151.090930	0.409524	329.6     	127
i3	151.363719	0.136735	164.804481	113
i3	151.363719	0.136735	246.9     	113
i3	151.500227	0.136508	329.6     	127
i3	151.500227	0.136508	440.0     	127
i3	151.500227	0.136508	277.20    	127
i3	151.772789	0.136735	329.6     	116
i3	151.772789	0.136735	277.20    	116
i3	151.772789	0.136735	440.0     	116
i3	152.045578	0.136508	329.6     	127
i3	152.045578	0.136508	277.20    	127
i3	152.181859	0.136735	277.20    	113
i3	152.045578	0.341043	440.0     	127
i3	152.181859	0.204762	329.6     	113
i3	152.318367	0.136508	220.0     	87
i3	152.318367	0.136508	277.20    	87
i3	152.454649	0.136508	277.20    	125
i3	152.454649	0.204762	440.0     	125
i3	152.454649	0.204762	329.6     	125
i3	152.590930	0.136735	277.20    	87
i3	152.590930	0.136735	220.0     	87
i3	152.727438	0.136508	311.126982	122
i3	152.727438	0.136508	493.869370	122
i3	152.727438	0.136508	369.994421	122
i3	153.000227	0.136508	311.126982	110
i3	153.000227	0.136508	493.869370	110
i3	153.000227	0.136508	369.994421	110
i3	153.272789	0.273016	311.126982	125
i3	153.272789	0.273016	369.994421	125
i3	153.272789	0.273016	493.869370	125
i3	153.545578	0.136508	246.9     	113
i3	153.545578	0.136508	311.126982	113
i3	153.681859	0.136735	440.0     	125
i3	153.681859	0.136735	277.20    	125
i3	153.681859	0.136735	329.6     	125
i3	153.954649	0.136508	277.20    	110
i3	153.954649	0.136508	329.6     	110
i3	153.954649	0.136508	440.0     	110
i3	154.227438	0.136508	277.20    	119
i3	154.227438	0.136508	440.0     	119
i3	154.227438	0.136508	329.6     	119
i3	154.363719	0.136735	277.20    	101
i3	154.363719	0.204762	329.6     	101
i3	154.363719	0.204762	440.0     	101
i3	154.500227	0.136508	220.0     	95
i3	154.500227	0.136508	277.20    	95
i3	154.636508	0.136508	277.20    	122
i3	154.636508	0.204762	329.6     	122
i3	154.636508	0.216100	440.0     	122
i3	154.772789	0.091156	220.0     	101
i3	154.772789	0.136735	277.20    	101
i3	154.909297	0.136508	415.292983	127
i3	154.909297	0.136508	329.6     	127
i3	154.909297	0.136508	246.9     	127
i3	155.181859	0.136735	246.9     	116
i3	155.181859	0.136735	329.6     	116
i3	155.181859	0.136735	415.292983	116
i3	155.454649	0.273016	246.9     	127
i3	155.454649	0.409297	415.292983	127
i3	155.454649	0.409297	329.6     	127
i3	155.727438	0.136508	164.804481	113
i3	155.727438	0.136508	246.9     	113
i3	155.863719	0.136735	329.6     	127
i3	155.863719	0.136735	440.0     	127
i3	155.863719	0.136735	277.20    	127
i3	156.136508	0.136508	329.6     	116
i3	156.136508	0.136508	277.20    	116
i3	156.136508	0.136508	440.0     	116
i3	156.409297	0.136508	329.6     	127
i3	156.409297	0.136508	277.20    	127
i3	156.545578	0.136508	277.20    	113
i3	156.409297	0.341043	440.0     	127
i3	156.545578	0.204762	329.6     	113
i3	156.681859	0.136735	220.0     	87
i3	156.681859	0.136735	277.20    	87
i3	156.818367	0.136508	277.20    	125
i3	156.818367	0.204762	440.0     	125
i3	156.818367	0.204762	329.6     	125
i3	156.954649	0.136508	277.20    	87
i3	156.954649	0.136508	220.0     	87
i3	157.090930	0.136735	311.126982	122
i3	157.090930	0.136735	493.869370	122
i3	157.090930	0.136735	369.994421	122
i3	157.363719	0.136735	311.126982	110
i3	157.363719	0.136735	493.869370	110
i3	157.363719	0.136735	369.994421	110
i3	157.636508	0.273016	311.126982	125
i3	157.636508	0.273016	369.994421	125
i3	157.636508	0.273016	493.869370	125
i3	157.909297	0.136508	246.9     	113
i3	157.909297	0.136508	311.126982	113
i3	158.045578	0.136508	440.0     	125
i3	158.045578	0.136508	277.20    	125
i3	158.045578	0.136508	329.6     	125
i3	158.318367	0.136508	277.20    	110
i3	158.318367	0.136508	329.6     	110
i3	158.318367	0.136508	440.0     	110
i3	158.590930	0.136735	277.20    	119
i3	158.590930	0.136735	440.0     	119
i3	158.590930	0.136735	329.6     	119
i3	158.727438	0.136508	277.20    	101
i3	158.727438	0.204762	329.6     	101
i3	158.727438	0.204762	440.0     	101
i3	158.863719	0.136735	220.0     	95
i3	158.863719	0.136735	277.20    	95
i3	159.000227	0.136508	277.20    	122
i3	159.000227	0.204762	329.6     	122
i3	159.000227	0.216100	440.0     	122
i3	159.136508	0.091156	220.0     	101
i3	159.136508	0.136508	277.20    	101
i3	159.272789	0.136735	415.292983	127
i3	159.272789	0.136735	329.6     	127
i3	159.272789	0.136735	246.9     	127
i3	159.545578	0.136508	246.9     	116
i3	159.545578	0.136508	329.6     	116
i3	159.545578	0.136508	415.292983	116
i3	159.818367	0.272789	246.9     	127
i3	159.818367	0.409297	415.292983	127
i3	159.818367	0.409297	329.6     	127
i3	160.090930	0.136735	164.804481	113
i3	160.090930	0.136735	246.9     	113
i3	160.227438	0.136508	329.6     	127
i3	160.227438	0.136508	440.0     	127
i3	160.227438	0.136508	277.20    	127
i3	160.500227	0.136508	329.6     	116
i3	160.500227	0.136508	277.20    	116
i3	160.500227	0.136508	440.0     	116
i3	160.772789	0.136735	329.6     	127
i3	160.772789	0.136735	277.20    	127
i3	160.909297	0.136508	277.20    	113
i3	160.772789	0.341270	440.0     	127
i3	160.909297	0.204762	329.6     	113
i3	161.045578	0.136508	220.0     	87
i3	161.045578	0.136508	277.20    	87
i3	161.181859	0.136735	277.20    	125
i3	161.181859	0.204762	440.0     	125
i3	161.181859	0.204762	329.6     	125
i3	161.318367	0.136508	277.20    	87
i3	161.318367	0.136508	220.0     	87
i3	161.454649	0.136735	311.126982	122
i3	161.454649	0.136735	493.869370	122
i3	161.454649	0.136735	369.994421	122
i3	161.727438	0.136508	311.126982	110
i3	161.727438	0.136508	493.869370	110
i3	161.727438	0.136508	369.994421	110
i3	162.000227	0.272789	311.126982	125
i3	162.000227	0.272789	369.994421	125
i3	162.000227	0.272789	493.869370	125
i3	162.272789	0.136735	246.9     	113
i3	162.272789	0.136735	311.126982	113
i3	162.409297	0.136508	440.0     	125
i3	162.409297	0.136508	277.20    	125
i3	162.409297	0.136508	329.6     	125
i3	162.681859	0.136735	277.20    	110
i3	162.681859	0.136735	329.6     	110
i3	162.681859	0.136735	440.0     	110
i3	162.954649	0.136735	277.20    	119
i3	162.954649	0.136735	440.0     	119
i3	162.954649	0.136735	329.6     	119
i3	163.091156	0.136508	277.20    	101
i3	163.091156	0.204762	329.6     	101
i3	163.091156	0.204762	440.0     	101
i3	163.227438	0.136508	220.0     	95
i3	163.227438	0.136508	277.20    	95
i3	163.363719	0.136735	277.20    	122
i3	163.363719	0.204762	329.6     	122
i3	163.363719	0.216100	440.0     	122
i3	163.500227	0.091156	220.0     	101
i3	163.500227	0.136508	277.20    	101
i3	163.636508	0.136508	415.292983	127
i3	163.636508	0.136508	329.6     	127
i3	163.636508	0.136508	246.9     	127
i3	163.909297	0.136508	246.9     	116
i3	163.909297	0.136508	329.6     	116
i3	163.909297	0.136508	415.292983	116
i3	164.2       0.3	        246.9     	127
i3	164.2	    0.4     	415.292983	127
i3	164.2	    0.4	        329.6     	127
i3	164.454649	0.14	164.804481	113
i3	164.454649	0.14	246.9     	113
i3	164.6	    0.14	329.6     	127
i3	164.6	    0.14	440.0     	127
i3	164.6	    0.14	277.20    	127
i3	164.9	    0.14	329.6     	116
i3	164.9	    0.14	277.20    	116
i3	164.9	    0.14	440.0     	116
i3	165.1	    0.14	329.6     	127
i3	165.1	    0.14	277.20    	127
i3	165.272789	0.136735	277.20    	113
i3	165.136508	0.341043	440.0     	127
i3	165.272789	0.204762	329.6     	113
i3	165.409297	0.136508	220.0     	87
i3	165.409297	0.136508	277.20    	87
i3	165.545578	0.136508	277.20    	125
i3	165.545578	0.204762	440.0     	125
i3	165.545578	0.204762	329.6     	125
i3	165.681859	0.136735	277.20    	87
i3	165.681859	0.136735	220.0     	87
i3	165.818367	0.136508	311.126982	122
i3	165.818367	0.136508	493.869370	122
i3	165.818367	0.136508	369.994421	122
i3	166.091156	0.136508	311.126982	110
i3	166.091156	0.136508	493.869370	110
i3	166.091156	0.136508	369.994421	110
i3	166.363719	0.273016	311.126982	125
i3	166.363719	0.273016	369.994421	125
i3	166.363719	0.273016	493.869370	125
i3	166.636508	0.136508	246.9     	113
i3	166.636508	0.136508	311.126982	113
i3	166.772789	0.136735	440.0     	125
i3	166.772789	0.136735	277.20    	125
i3	166.772789	0.136735	329.6     	125
i3	167.045578	0.136508	277.20    	110
i3	167.045578	0.136508	329.6     	110
i3	167.045578	0.136508	440.0     	110
i3	167.318367	0.136508	277.20    	119
i3	167.318367	0.136508	440.0     	119
i3	167.318367	0.136508	329.6     	119
i3	167.454649	0.136735	277.20    	101
i3	167.454649	0.204762	329.6     	101
i3	167.454649	0.204762	440.0     	101
i3	167.591156	0.136508	220.0     	95
i3	167.591156	0.136508	277.20    	95
i3	167.727438	0.136508	277.20    	122
i3	167.727438	0.204762	329.6     	122
i3	167.727438	0.216100	440.0     	122
i3	167.863719	0.091156	220.0     	101
i3	167.863719	0.136735	277.20    	101
i3	168.000227	0.136508	246.9     	127
i3	168.000227	0.136508	311.126982	127
i3	168.000227	0.136508	415.292983	127
i3	168.272789	0.136735	246.9     	116
i3	168.272789	0.136735	311.126982	116
i3	168.272789	0.136735	415.292983	116
i3	168.545578	0.273016	246.9     	127
i3	168.545578	0.409297	311.126982	127
i3	168.545578	0.409297	415.292983	127
i3	168.818367	0.136508	246.9     	113
i3	168.818367	0.136508	184.997211	113
i3	168.954649	0.136735	246.9     	127
i3	168.954649	0.136735	311.126982	127
i3	168.954649	0.136735	415.292983	127
i3	169.227438	0.136508	246.9     	116
i3	169.227438	0.136508	311.126982	116
i3	169.227438	0.136508	415.292983	116
i3	169.500227	0.136508	246.9     	127
i3	169.500227	0.136508	311.126982	127
i3	169.636508	0.136508	246.9     	113
i3	169.500227	0.341043	415.292983	127
i3	169.636508	0.204762	311.126982	113
i3	169.772789	0.136735	246.9     	87
i3	169.772789	0.136735	184.997211	87
i3	169.909297	0.136508	246.9     	125
i3	169.909297	0.204762	311.126982	125
i3	169.909297	0.204762	415.292983	125
i3	170.045578	0.136508	184.997211	87
i3	170.045578	0.136508	246.9     	87
i3	170.181859	0.136735	277.20    	122
i3	170.181859	0.136735	329.6     	122
i3	170.181859	0.136735	440.0     	122
i3	170.454649	0.136735	277.20    	110
i3	170.454649	0.136735	329.6     	110
i3	170.454649	0.136735	440.0     	110
i3	170.727438	0.273016	277.20    	125
i3	170.727438	0.273016	329.6     	125
i3	170.727438	0.273016	440.0     	125
i3	171.000227	0.136508	220.0     	113
i3	171.000227	0.136508	277.20    	113
i3	171.136508	0.136508	440.0     	125
i3	171.136508	0.136508	277.20    	125
i3	171.136508	0.136508	329.6     	125
i3	171.409297	0.136508	277.20    	110
i3	171.409297	0.136508	329.6     	110
i3	171.409297	0.136508	440.0     	110
i3	171.681859	0.136735	277.20    	119
i3	171.681859	0.136735	440.0     	119
i3	171.681859	0.136735	329.6     	119
i3	171.818367	0.136508	277.20    	101
i3	171.818367	0.204762	329.6     	101
i3	171.818367	0.204762	440.0     	101
i3	171.954649	0.136735	220.0     	95
i3	171.954649	0.136735	277.20    	95
i3	172.091156	0.136508	277.20    	122
i3	172.091156	0.204762	329.6     	122
i3	172.091156	0.216100	440.0     	122
i3	172.227438	0.091156	220.0     	101
i3	172.227438	0.136508	277.20    	101
i3	172.363719	0.136735	246.9     	127
i3	172.363719	0.136735	311.126982	127
i3	172.363719	0.136735	415.292983	127
i3	172.636508	0.136508	246.9     	116
i3	172.636508	0.136508	311.126982	116
i3	172.636508	0.136508	415.292983	116
i3	172.909297	0.272789	246.9     	127
i3	172.909297	0.409297	311.126982	127
i3	172.909297	0.409297	415.292983	127
i3	173.181859	0.136735	246.9     	113
i3	173.181859	0.136735	184.997211	113
i3	173.318367	0.136508	246.9     	127
i3	173.318367	0.136508	311.126982	127
i3	173.318367	0.136508	415.292983	127
i3	173.591156	0.136508	246.9     	116
i3	173.591156	0.136508	311.126982	116
i3	173.591156	0.136508	415.292983	116
i3	173.863719	0.136735	246.9     	127
i3	173.863719	0.136735	311.126982	127
i3	174.000227	0.136508	246.9     	113
i3	173.863719	0.341270	415.292983	127
i3	174.000227	0.204762	311.126982	113
i3	174.136508	0.136508	246.9     	87
i3	174.136508	0.136508	184.997211	87
i3	174.272789	0.136735	246.9     	125
i3	174.272789	0.204762	311.126982	125
i3	174.272789	0.204762	415.292983	125
i3	174.409297	0.136508	184.997211	87
i3	174.409297	0.136508	246.9     	87
i3	174.545578	0.136508	277.20    	122
i3	174.545578	0.136508	329.6     	122
i3	174.545578	0.136508	440.0     	122
i3	174.818367	0.136508	277.20    	110
i3	174.818367	0.136508	329.6     	110
i3	174.818367	0.136508	440.0     	110
i3	175.091156	0.272789	277.20    	125
i3	175.091156	0.272789	329.6     	125
i3	175.091156	0.272789	440.0     	125
i3	175.363719	0.136735	220.0     	113
i3	175.363719	0.136735	277.20    	113
i3	175.500227	0.136508	440.0     	125
i3	175.500227	0.136508	277.20    	125
i3	175.500227	0.136508	329.6     	125
i3	175.772789	0.136735	277.20    	110
i3	175.772789	0.136735	329.6     	110
i3	175.772789	0.136735	440.0     	110
i3	176.045578	0.136508	277.20    	119
i3	176.045578	0.136508	440.0     	119
i3	176.045578	0.136508	329.6     	119
i3	176.181859	0.136735	277.20    	101
i3	176.181859	0.204989	329.6     	101
i3	176.181859	0.204989	440.0     	101
i3	176.318367	0.136508	220.0     	95
i3	176.318367	0.136508	277.20    	95
i3	176.454649	0.136735	277.20    	122
i3	176.454649	0.204762	329.6     	122
i3	176.454649	0.216100	440.0     	122
i3	176.591156	0.090930	220.0     	101
i3	176.591156	0.136508	277.20    	101
i3	176.727438	0.136508	246.9     	127
i3	176.727438	0.136508	311.126982	127
i3	176.727438	0.136508	415.292983	127
i3	177.000227	0.136508	246.9     	116
i3	177.000227	0.136508	311.126982	116
i3	177.000227	0.136508	415.292983	116
i3	177.272789	0.273016	246.9     	127
i3	177.272789	0.409297	311.126982	127
i3	177.272789	0.409297	415.292983	127
i3	177.545578	0.136508	246.9     	113
i3	177.545578	0.136508	184.997211	113
i3	177.681859	0.136735	246.9     	127
i3	177.681859	0.136735	311.126982	127
i3	177.681859	0.136735	415.292983	127
i3	177.954649	0.136735	246.9     	116
i3	177.954649	0.136735	311.126982	116
i3	177.954649	0.136735	415.292983	116
i3	178.227438	0.136508	246.9     	127
i3	178.227438	0.136508	311.126982	127
i3	178.363719	0.136735	246.9     	113
i3	178.227438	0.341043	415.292983	127
i3	178.363719	0.204762	311.126982	113
i3	178.500227	0.136508	246.9     	87
i3	178.500227	0.136508	184.997211	87
i3	178.636508	0.136508	246.9     	125
i3	178.636508	0.204762	311.126982	125
i3	178.636508	0.204762	415.292983	125
i3	178.772789	0.136735	184.997211	87
i3	178.772789	0.136735	246.9     	87
i3	178.909297	0.136508	277.20    	122
i3	178.909297	0.136508	329.6     	122
i3	178.909297	0.136508	440.0     	122
i3	179.181859	0.136735	277.20    	110
i3	179.181859	0.136735	329.6     	110
i3	179.181859	0.136735	440.0     	110
i3	179.454649	0.273016	277.20    	125
i3	179.454649	0.273016	329.6     	125
i3	179.454649	0.273016	440.0     	125
i3	179.727438	0.136508	220.0     	113
i3	179.727438	0.136508	277.20    	113
i3	179.863719	0.136735	440.0     	125
i3	179.863719	0.136735	277.20    	125
i3	179.863719	0.136735	329.6     	125
i3	180.136508	0.136508	277.20    	110
i3	180.136508	0.136508	329.6     	110
i3	180.136508	0.136508	440.0     	110
i3	180.409297	0.136508	277.20    	119
i3	180.409297	0.136508	440.0     	119
i3	180.409297	0.136508	329.6     	119
i3	180.545578	0.136508	277.20    	101
i3	180.545578	0.204762	329.6     	101
i3	180.545578	0.204762	440.0     	101
i3	180.681859	0.136735	220.0     	95
i3	180.681859	0.136735	277.20    	95
i3	180.818367	0.136508	277.20    	122
i3	180.818367	0.204762	329.6     	122
i3	180.818367	0.216100	440.0     	122
i3	180.954649	0.091156	220.0     	101
i3	180.954649	0.136735	277.20    	101
i3	181.091156	0.136508	415.292983	127
i3	181.091156	0.136508	329.6     	127
i3	181.091156	0.136508	246.9     	127
i3	181.363719	0.136735	246.9     	116
i3	181.363719	0.136735	329.6     	116
i3	181.363719	0.136735	415.292983	116
i3	181.636508	0.273016	246.9     	127
i3	181.636508	0.409297	415.292983	127
i3	181.636508	0.409297	329.6     	127
i3	181.909297	0.136508	164.804481	113
i3	181.909297	0.136508	246.9     	113
i3	182.045578	0.136508	329.6     	127
i3	182.045578	0.136508	440.0     	127
i3	182.045578	0.136508	277.20    	127
i3	182.318367	0.136508	329.6     	116
i3	182.318367	0.136508	277.20    	116
i3	182.318367	0.136508	440.0     	116
i3	182.591156	0.136508	329.6     	127
i3	182.591156	0.136508	277.20    	127
i3	182.727438	0.136508	277.20    	113
i3	182.591156	0.341043	440.0     	127
i3	182.727438	0.204762	329.6     	113
i3	182.863719	0.136735	220.0     	87
i3	182.863719	0.136735	277.20    	87
i3	183.000227	0.136508	277.20    	125
i3	183.000227	0.204762	440.0     	125
i3	183.000227	0.204762	329.6     	125
i3	183.136508	0.136508	277.20    	87
i3	183.136508	0.136508	220.0     	87
i3	183.272789	0.136735	311.126982	122
i3	183.272789	0.136735	493.869370	122
i3	183.3   	0.14    	369.994421	123
i3	183.545578	0.136508	311.126982	110
i3	183.545578	0.136508	493.869370	110
i3	183.545578	0.136508	369.994421	110
i3	183.818367	0.273016	311.126982	125
i3	183.818367	0.273016	369.994421	125
i3	183.818367	0.273016	493.869370	125
i3	184.091156	0.136508	246.9     	113
i3	184.091156	0.136508	311.126982	113
i3	184.227438	0.136508	440.0     	125
i3	184.227438	0.136508	277.20    	125
i3	184.227438	0.136508	329.6     	125
i3	184.500227	0.136508	277.20    	110
i3	184.500227	0.136508	329.6     	110
i3	184.500227	0.136508	440.0     	110
i3	184.772789	0.136735	277.20    	119
i3	184.772789	0.136735	440.0     	119
i3	184.772789	0.136735	329.6     	119
i3	184.909297	0.136508	277.20    	101
i3	184.909297	0.204762	329.6     	101
i3	184.909297	0.204762	440.0     	101
i3	185.045578	0.136508	220.0     	95
i3	185.045578	0.136508	277.20    	95
i3	185.181859	0.136735	277.20    	122
i3	185.181859	0.204989	329.6     	122
i3	185.181859	0.216327	440.0     	122
i3	185.318367	0.091156	220.0     	101
i3	185.318367	0.136508	277.20    	101
i3	185.454649	0.136735	415.292983	127
i3	185.454649	0.136735	329.6     	127
i3	185.454649	0.136735	246.9     	127
i3	185.727438	0.136508	246.9     	116
i3	185.727438	0.136508	329.6     	116
i3	185.727438	0.136508	415.292983	116
i3	186.000227	0.272789	246.9     	127
i3	186.000227	0.409297	415.292983	127
i3	186.000227	0.409297	329.6     	127
i3	186.272789	0.136735	164.804481	113
i3	186.272789	0.136735	246.9     	113
i3	186.409297	0.136508	329.6     	127
i3	186.409297	0.136508	440.0     	127
i3	186.409297	0.136508	277.20    	127
i3	186.682086	0.136508	329.6     	116
i3	186.682086	0.136508	277.20    	116
i3	186.682086	0.136508	440.0     	116
i3	186.954649	0.136735	329.6     	127
i3	186.954649	0.136735	277.20    	127
i3	187.091156	0.136508	277.20    	113
i3	186.954649	0.341270	440.0     	127
i3	187.091156	0.204762	329.6     	113
i3	187.227438	0.136508	220.0     	87
i3	187.227438	0.136508	277.20    	87
i3	187.363719	0.136735	277.20    	125
i3	187.363719	0.204762	440.0     	125
i3	187.363719	0.204762	329.6     	125
i3	187.500227	0.136508	277.20    	87
i3	187.500227	0.136508	220.0     	87
i3	187.636508	0.136508	311.126982	122
i3	187.636508	0.136508	493.869370	122
i3	187.636508	0.136508	369.994421	122
i3	187.909297	0.136508	311.126982	110
i3	187.909297	0.136508	493.869370	110
i3	187.909297	0.136508	369.994421	110
i3	188.182086	0.272789	311.126982	125
i3	188.182086	0.272789	369.994421	125
i3	188.182086	0.272789	493.869370	125
i3	188.454649	0.136735	246.9     	113
i3	188.454649	0.136735	311.126982	113
i3	188.6	    0.14	440.0     	125
i3	188.6	    0.14	277.20    	125
i3	188.6	    0.14	329.6     	125
i3	188.9	    0.14	277.20    	110
i3	188.9	    0.14	329.6     	110
i3	188.9	    0.14	440.0     	110
i3	189.1	    0.14	277.20    	119
i3	189.1	    0.14	440.0     	119
i3	189.1	    0.14	329.6     	119
i3	189.272789	0.14	277.20    	101
i3	189.272789	0.2 	329.6     	101
i3	189.272789	0.2	    440.0     	101
i3	189.409297	0.136508	220.0     	95
i3	189.409297	0.136508	277.20    	95
i3	189.545578	0.136735	277.20    	122
i3	189.545578	0.204762	329.6     	122
i3	189.545578	0.216100	440.0     	122
i3	189.682086	0.090930	220.0     	101
i3	189.682086	0.136508	277.20    	101
i3	189.818367	0.136508	415.292983	127
i3	189.818367	0.136508	329.6     	127
i3	189.818367	0.136508	246.9     	127
i3	190.091156	0.136508	246.9     	116
i3	190.091156	0.136508	329.6     	116
i3	190.091156	0.136508	415.292983	116
i3	190.363719	0.273016	246.9     	127
i3	190.363719	0.409297	415.292983	127
i3	190.363719	0.409297	329.6     	127
i3	190.636508	0.136508	164.804481	113
i3	190.636508	0.136508	246.9     	113
i3	190.772789	0.136735	329.6     	127
i3	190.772789	0.136735	440.0     	127
i3	190.772789	0.136735	277.20    	127
i3	191.045578	0.136735	329.6     	116
i3	191.045578	0.136735	277.20    	116
i3	191.045578	0.136735	440.0     	116
i3	191.318367	0.136508	329.6     	127
i3	191.318367	0.136508	277.20    	127
i3	191.454649	0.136735	277.20    	113
i3	191.318367	0.341043	440.0     	127
i3	191.454649	0.204762	329.6     	113
i3	191.591156	0.136508	220.0     	87
i3	191.591156	0.136508	277.20    	87
i3	191.727438	0.136508	277.20    	125
i3	191.727438	0.204762	440.0     	125
i3	191.727438	0.204762	329.6     	125
i3	191.863719	0.136735	277.20    	87
i3	191.863719	0.136735	220.0     	87
i3	192.000227	0.136508	311.126982	122
i3	192.000227	0.136508	493.869370	122
i3	192.000227	0.136508	369.994421	122
i3	192.272789	0.136735	311.126982	110
i3	192.272789	0.136735	493.869370	110
i3	192.272789	0.136735	369.994421	110
i3	192.545578	0.273016	311.126982	125
i3	192.545578	0.273016	369.994421	125
i3	192.545578	0.273016	493.869370	125
i3	192.818367	0.136508	246.9     	113
i3	192.818367	0.136508	311.126982	113
i3	192.954649	0.136735	440.0     	125
i3	192.954649	0.136735	277.20    	125
i3	192.954649	0.136735	329.6     	125
i3	193.227438	0.136508	277.20    	110
i3	193.227438	0.136508	329.6     	110
i3	193.227438	0.136508	440.0     	110
i3	193.500227	0.136508	277.20    	119
i3	193.500227	0.136508	440.0     	119
i3	193.500227	0.136508	329.6     	119
i3	193.636508	0.136508	277.20    	101
i3	193.636508	0.204762	329.6     	101
i3	193.636508	0.204762	440.0     	101
i3	193.772789	0.136735	220.0     	95
i3	193.772789	0.136735	277.20    	95
i3	193.909297	0.136508	277.20    	122
i3	193.909297	0.204762	329.6     	122
i3	193.909297	0.216100	440.0     	122
i3	194.045578	0.091156	220.0     	101
i3	194.045578	0.136735	277.20    	101
i3	194.182086	0.136508	415.292983	127
i3	194.182086	0.136508	329.6     	127
i3	194.182086	0.136508	246.9     	127
i3	194.454649	0.136735	246.9     	116
i3	194.454649	0.136735	329.6     	116
i3	194.454649	0.136735	415.292983	116
i3	194.727438	0.273016	246.9     	127
i3	194.727438	0.409297	415.292983	127
i3	194.727438	0.409297	329.6     	127
i3	195.000227	0.136508	164.804481	113
i3	195.000227	0.136508	246.9     	113
i3	195.136508	0.136508	329.6     	127
i3	195.136508	0.136508	440.0     	127
i3	195.136508	0.136508	277.20    	127
i3	195.409297	0.136508	329.6     	116
i3	195.409297	0.136508	277.20    	116
i3	195.409297	0.136508	440.0     	116
i3	195.682086	0.136508	329.6     	127
i3	195.682086	0.136508	277.20    	127
i3	195.818367	0.136508	277.20    	113
i3	195.682086	0.341043	440.0     	127
i3	195.818367	0.204762	329.6     	113
i3	195.0	0.136735	220.0     	87
i3	195.0	0.136735	277.20    	87
i3	196.1	0.136508	277.20    	125
i3	196.1	0.204762	440.0     	125
i3	196.1	0.204762	329.6     	125
i3	196.227438	0.136508	277.20    	87
i3	196.227438	0.136508	220.0     	87
i3	196.363719	0.136735	311.126982	122
i3	196.363719	0.136735	493.869370	122
i3	196.363719	0.136735	369.994421	122
i3	196.636508	0.136508	311.126982	110
i3	196.636508	0.136508	493.869370	110
i3	196.636508	0.136508	369.994421	110
i3	196.909297	0.273016	311.126982	125
i3	196.909297	0.273016	369.994421	125
i3	196.909297	0.273016	493.869370	125
i3	197.182086	0.136508	246.9     	113
i3	197.182086	0.136508	311.126982	113
i3	197.318367	0.136508	440.0     	125
i3	197.318367	0.136508	277.20    	125
i3	197.318367	0.136508	329.6     	125
i3	197.591156	0.136508	277.20    	110
i3	197.591156	0.136508	329.6     	110
i3	197.591156	0.136508	440.0     	110
i3	197.863719	0.136735	277.20    	119
i3	197.863719	0.136735	440.0     	119
i3	197.863719	0.136735	329.6     	119
i3	198.000227	0.136508	277.20    	101
i3	198.000227	0.204762	329.6     	101
i3	198.000227	0.204762	440.0     	101
i3	198.136508	0.136508	220.0     	95
i3	198.136508	0.136508	277.20    	95
i3	198.272789	0.136735	277.20    	122
i3	198.272789	0.204989	329.6     	122
i3	198.272789	0.216327	440.0     	122
i3	198.409297	0.091156	220.0     	101
i3	198.409297	0.136508	277.20    	101
i3	198.545578	0.136735	415.292983	127
i3	198.545578	0.136735	329.6     	127
i3	198.545578	0.136735	246.9     	127
i3	198.818367	0.136508	246.9     	116
i3	198.818367	0.136508	329.6     	116
i3	198.818367	0.136508	415.292983	116
i3	199.091156	0.272789	246.9     	127
i3	199.091156	0.409297	415.292983	127
i3	199.091156	0.409297	329.6     	127
i3	199.363719	0.136735	164.804481	113
i3	199.363719	0.136735	246.9     	113
i3	199.500227	0.136508	329.6     	127
i3	199.500227	0.136508	440.0     	127
i3	199.500227	0.136508	277.20    	127
i3	199.772789	0.136735	329.6     	116
i3	199.772789	0.136735	277.20    	116
i3	199.772789	0.136735	440.0     	116
i3	200.045578	0.136735	329.6     	127
i3	200.045578	0.136735	277.20    	127
i3	200.182086	0.136508	277.20    	113
i3	200.045578	0.341270	440.0     	127
i3	200.182086	0.204762	329.6     	113
i3	200.318367	0.136508	220.0     	87
i3	200.318367	0.136508	277.20    	87
i3	200.454649	0.136735	277.20    	125
i3	200.454649	0.204762	440.0     	125
i3	200.454649	0.204762	329.6     	125
i3	200.591156	0.136508	277.20    	87
i3	200.591156	0.136508	220.0     	87
i3	200.727438	0.136508	311.126982	122
i3	200.727438	0.136508	493.869370	122
i3	200.727438	0.136508	369.994421	122
i3	201.000227	0.136508	311.126982	110
i3	201.000227	0.136508	493.869370	110
i3	201.000227	0.136508	369.994421	110
i3	201.272789	0.273016	311.126982	125
i3	201.272789	0.273016	369.994421	125
i3	201.272789	0.273016	493.869370	125
i3	201.545578	0.136735	246.9     	113
i3	201.545578	0.136735	311.126982	113
i3	201.682086	0.136508	440.0     	125
i3	201.682086	0.136508	277.20    	125
i3	201.682086	0.136508	329.6     	125
i3	201.954649	0.136735	277.20    	110
i3	201.954649	0.136735	329.6     	110
i3	201.954649	0.136735	440.0     	110
i3	202.227438	0.136508	277.20    	119
i3	202.227438	0.136508	440.0     	119
i3	202.227438	0.136508	329.6     	119
i3	202.363719	0.136735	277.20    	101
i3	202.363719	0.204762	329.6     	101
i3	202.363719	0.204762	440.0     	101
i3	202.500227	0.136508	220.0     	95
i3	202.500227	0.136508	277.20    	95
i3	202.636508	0.136508	277.20    	122
i3	202.636508	0.204762	329.6     	122
i3	202.636508	0.216100	440.0     	122
i3	202.772789	0.091156	220.0     	101
i3	202.772789	0.136735	277.20    	101
i3	202.909297	0.136508	415.292983	127
i3	202.909297	0.136508	329.6     	127
i3	202.909297	0.136508	246.9     	127
i3	203.2	0.136508	246.9     	116
i3	203.2	0.136508	329.6     	116
i3	203.2	0.136508	415.292983	116
i3	203.5	0.273016	246.9     	127
i3	203.5	0.409297	415.292983	127
i3	203.5	0.409297	329.6     	127
i3	203.727438	0.136508	164.804481	113
i3	203.727438	0.136508	246.9     	113
i3	203.81  	0.136735	329.6     	121
i3	203.87  	0.136735	440.0     	127
i3	203.88  	0.136735	277.20    	127
i3	204.136508	0.136508	329.6     	116
i3	204.136508	0.136508	277.20    	116
i3	204.136508	0.136508	440.0     	116
i3	204.409297	0.136508	329.6     	127
i3	204.409297	0.136508	277.20    	127
i3	204.545578	0.136735	277.20    	113
i3	204.409297	0.341043	440.0     	127
i3	204.545578	0.204762	329.6     	113
i3	204.682086	0.136508	220.0     	87
i3	204.682086	0.136508	277.20    	87
i3	204.818367	0.136508	277.20    	125
i3	204.818367	0.204762	440.0     	125
i3	204.818367	0.204762	329.6     	125
i3	204.954649	0.136735	277.20    	87
i3	204.954649	0.136735	220.0     	87
i3	205.091156	0.136508	311.126982	122
i3	205.091156	0.136508	493.869370	122
i3	205.091156	0.136508	369.994421	122
i3	205.3   	0.14    	311.1   	90
i3	205.31  	0.15    	493.9   	100
i3	205.4   	0.136   	367.5   	85
i3	205.6   	0.28    	310.00  	105
i3	205.61  	0.27    	370.00  	115
i3	205.69  	0.275   	494.00  	125
i3	205.909297	0.136508	246.9     	113
i3	205.909297	0.136508	311.126982	113
i3	206.1   	0.14    	440.0     	125
i3	206.2   	0.14    	277.20    	125
i3	206.21  	0.11    	329.6     	125
i3	206.321 	0.136508	277.20    	110
i3	206.322 	0.136508	329.6     	110
i3	206.324 	0.136508	440.0     	110
i3	206.591  	0.136508	277.20    	119
i3	206.592  	0.136508	440.0     	119
i3	206.593  	0.136508	329.6     	119
i3	206.7   	0.136508	277.20    	101
i3	206.75  	0.204762	329.6     	101
i3	206.76  	0.204762	440.0     	101
i3	206.863719	0.136735	220.0     	95
i3	206.863719	0.136735	277.20    	95
i3	207.000227	0.136508	277.20    	122
i3	207.000227	0.204762	329.6     	122
i3	207.000227	0.216100	440.0     	122
i3	207.136508	0.091156	220.0     	101
i3	207.136508	0.136508	277.20    	101
i3	207.272789	0.545805	329.6     	127
i3	207.272789	0.545805	246.9     	127
i3	207.272789	0.545805	415.292983	127
i3	207.818367	0.136508	246.9     	122
i3	207.818367	0.136508	415.292983	122
i3	207.818367	0.136508	329.6     	127
i3	207.954649	0.136735	329.6     	90
i3	207.954649	0.136735	415.29  	92
i3	207.954649	0.136735	246.9     	90
i3	208.091156	0.136508	246.9     	122
i3	208.091156	0.136508	415.292983	122
i3	208.091156	0.136508	329.6     	127
i3	208.227438	0.136508	415.292983	90
i3	208.227438	0.136508	246.9     	90
i3	208.227438	0.136508	329.6     	90
i3	208.363719	0.136735	246.9     	122
i3	208.363719	0.136735	415.292983	122
i3	208.363719	0.136735	329.6     	127
i3	208.500227	0.136508	246.9     	93
i3	208.500227	0.136508	415.292983	93
i3	208.500227	0.136508	329.6     	93
i3	208.636508	0.136508	246.9     	122
i3	208.636508	0.136508	415.292983	122
i3	208.636508	0.136508	329.6     	127
i3	208.772789	0.136735	329.6     	87
i3	208.772789	0.136735	246.9     	87
i3	208.772789	0.136735	415.292983	87
i3	208.909297	0.136508	246.9     	127
i3	208.909297	0.136508	415.292983	127
i3	208.909297	0.136508	329.6     	127
i3	209.045578	0.136735	415.292983	93
i3	209.045578	0.136735	246.9     	93
i3	209.045578	0.136735	329.6     	99
i3	209.175 	0.14    	246.9     	127
i3	209.18  	0.137   	415.29  	127
i3	209.182  	0.1365  	329.6   	127
i3	209.3   	0.14    	415.3    	91
i3	209.32   	0.15    	329.6   	93
i3	209.32   	0.14    	246.9   	103
i3	209.39   	0.3     	246.9      127
i3	209.45  	0.273   	415.3   	125
i3	209.45   	0.26    	329.6    	127

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
i4	39.000000	0.545805	493.869370	127
i4	39.545578	0.136508	554.333990	127
i4	40.090930	0.136508	415.292983	127
i4	40.227211	0.136735	493.869370	121
i4	40.363719	0.136508	554.333990	127
i4	40.636508	0.136508	493.869370	114
i4	40.909070	0.136735	415.292983	127
i4	41.181859	0.545578	554.333990	103
i4	41.727211	1.091156	493.869370	109
i4	45.545578	0.136508	493.869370	127
i4	45.818141	0.273016	659.217924	127
i4	46.090930	0.136508	554.333990	127
i4	46.363719	0.136508	493.869370	127
i4	46.636508	0.409297	554.333990	127
i4	47.181859	0.273016	493.869370	127
i4	47.454649	0.136508	415.292983	127
i4	47.727211	0.545805	493.869370	127
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
i4	124.363719	0.409297	659.217924	127
i4	125.318367	0.409297	659.217924	127
i4	126.272789	0.273016	659.217924	127
i4	126.545578	0.136508	554.333990	127
i4	126.818367	0.545578	493.869370	127
i4	127.500000	0.136735	493.869370	127
i4	127.636508	0.273016	659.217924	127
i4	127.909297	0.136508	554.333990	127
i4	128.181859	0.409297	659.217924	127
i4	128.727438	0.409297	659.217924	127
i4	129.681859	0.409297	659.217924	127
i4	130.636508	0.136508	659.217924	127
i4	130.909297	0.136508	554.333990	127
i4	131.181859	0.682086	493.869370	127
i4	132.000000	0.136735	554.333990	127
i4	132.136508	0.273016	659.217924	127
i4	132.409297	0.136508	554.333990	127
i4	132.545578	0.409297	659.217924	127
i4	133.090930	0.409297	659.217924	127
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
i4	196.636508	0.443537	493.869370	122
i4	197.182086	0.272789	554.333990	127
i4	197.454649	0.273016	659.217924	127
i4	197.727438	0.273016	554.333990	119
i4	198.000227	0.545578	659.217924	127
i4	198.545578	0.273016	659.217924	127
i4	201.545578	0.273016	554.333990	127
i4	201.818367	0.273016	659.217924	127
i4	202.091156	0.272789	554.333990	127
i4	202.363719	0.41      	659.217924	127
i4	202.909297	0.42      	659.217924	127
i4	205.909297	0.273016	554.333990	127
i4	206.182086	0.272789	659.217924	127
i4	206.454649	0.273016	554.333990	119
i4	206.727438	0.545578	659.217924	127

; 5:1 start 10.9 ends 209
; ins 5
; note the chords

i5	10.9	0.136508	246.934685	127
i5	10.9	0.136508	164.804481	127
i5	10.9	0.136508	207.646491	127
i5	11.181859	0.136508	246.934685	116
i5	11.181859	0.136508	207.646491	116
i5	11.181859	0.136508	164.804481	116
i5	11.44   	0.272789	246.934685	127
i5	11.45   	0.409297	164.804481	127
i5	11.455  	0.409297	207.646491	127
i5	11.727211	0.136735	246.934685	113
i5	11.863719	0.136508	184.997211	127
i5	11.863719	0.136508	219.999999	127
i5	11.863719	0.136508	277.166995	127
i5	12.136281	0.14    	184.997211	116
i5	12.136281	0.14    	219.999999	116
i5	12.136281	0.14    	277.166995	116
i5	12.409070	0.15    	184.997211	126
i5	12.409070	0.14    	219.999999	127
i5	12.545578	0.14    	184.997211	113
i5	12.409070	0.341270	277.166995	127
i5	12.545578	0.204762	219.999999	113
i5	12.681859	0.136508	184.997211	87
i5	12.818141	0.136735	184.997211	125
i5	12.818141	0.204762	219.999999	125
i5	12.818141	0.204762	277.166995	125
i5	12.954649	0.136508	184.997211	87
i5	13.090930	0.136508	207.646491	122
i5	13.090930	0.136508	246.934685	122
i5	13.090930	0.136508	311.126982	122
i5	13.363719	0.136508	207.646491	110
i5	13.363719	0.136508	246.934685	110
i5	13.363719	0.136508	311.126982	110
i5	13.636281	0.273016	207.646491	125
i5	13.636281	0.273016	246.934685	125
i5	13.636281	0.273016	311.126982	125
i5	14.045578	0.136508	184.997211	125
i5	14.045578	0.136508	219.999999	125
i5	14.045578	0.136508	277.166995	125
i5	14.318141	0.136735	184.997211	110
i5	14.318141	0.136735	219.999999	110
i5	14.318141	0.136735	277.166995	110
i5	14.590930	0.136508	184.997211	119
i5	14.590930	0.136508	219.999999	119
i5	14.590930	0.136508	277.166995	119
i5	14.727211	0.136735	184.997211	101
i5	14.727211	0.204762	219.999999	101
i5	14.727211	0.204762	277.166995	101
i5	14.863719	0.136508	184.997211	95
i5	15.000000	0.136508	184.997211	122
i5	15.000000	0.204762	219.999999	122
i5	15.000000	0.216100	277.166995	122
i5	15.136281	0.136735	184.997211	101
i5	15.272789	0.136508	246.934685	127
i5	15.272789	0.136508	164.804481	127
i5	15.272789	0.136508	207.646491	127
i5	15.545578	0.136508	246.934685	116
i5	15.545578	0.136508	207.646491	116
i5	15.545578	0.136508	164.804481	116
i5	15.818141	0.273016	246.934685	127
i5	15.818141	0.409297	164.804481	127
i5	15.818141	0.409297	207.646491	127
i5	16.090930	0.136508	246.934685	113
i5	16.227211	0.136735	184.997211	127
i5	16.227211	0.136735	219.999999	127
i5	16.227211	0.136735	277.166995	127
i5	16.500000	0.136508	184.997211	116
i5	16.500000	0.136508	219.999999	116
i5	16.500000	0.136508	277.166995	116
i5	16.772789	0.136508	184.997211	127
i5	16.772789	0.136508	219.999999	127
i5	16.909070	0.136735	184.997211	113
i5	16.772789	0.341043	277.166995	127
i5	16.909070	0.204762	219.999999	113
i5	17.045578	0.136508	184.997211	87
i5	17.181859	0.136508	184.997211	125
i5	17.181859	0.204762	219.999999	125
i5	17.181859	0.204762	277.166995	125
i5	17.318141	0.136735	184.997211	87
i5	17.454649	0.136508	207.646491	122
i5	17.454649	0.136508	246.934685	122
i5	17.454649	0.136508	311.126982	122
i5	17.727211	0.136735	207.646491	110
i5	17.727211	0.136735	246.934685	110
i5	17.727211	0.136735	311.126982	110
i5	18.000000	0.273016	207.646491	125
i5	18.000000	0.273016	246.934685	125
i5	18.000000	0.273016	311.126982	125
i5	18.409070	0.136735	184.997211	125
i5	18.409070	0.136735	219.999999	125
i5	18.409070	0.136735	277.166995	125
i5	18.681859	0.136508	184.997211	110
i5	18.681859	0.136508	219.999999	110
i5	18.681859	0.136508	277.166995	110
i5	18.954649	0.136508	184.997211	119
i5	18.954649	0.136508	219.999999	119
i5	18.954649	0.136508	277.166995	119
i5	19.090930	0.136508	184.997211	101
i5	19.090930	0.204762	219.999999	101
i5	19.090930	0.204762	277.166995	101
i5	19.227211	0.136735	184.997211	95
i5	19.363719	0.136508	184.997211	122
i5	19.363719	0.204762	219.999999	122
i5	19.363719	0.216100	277.166995	122
i5	19.500000	0.136508	184.997211	101
i5	19.636281	0.136735	246.934685	127
i5	19.636281	0.136735	164.804481	127
i5	19.636281	0.136735	207.646491	127
i5	19.909070	0.136735	246.934685	116
i5	19.909070	0.136735	207.646491	116
i5	19.909070	0.136735	164.804481	116
i5	20.181859	0.273016	246.934685	127
i5	20.181859	0.409297	164.804481	127
i5	20.181859	0.409297	207.646491	127
i5	20.454649	0.136508	246.934685	113
i5	20.590930	0.136508	164.804481	127
i5	20.590930	0.136508	219.999999	127
i5	20.590930	0.136508	277.166995	127
i5	20.863719	0.136508	164.804481	116
i5	20.863719	0.136508	219.999999	116
i5	20.863719	0.136508	277.166995	116
i5	21.136281	0.136735	164.804481	127
i5	21.136281	0.136735	219.999999	127
i5	21.272789	0.136508	164.804481	113
i5	21.136281	0.341270	277.166995	127
i5	21.272789	0.204762	219.999999	113
i5	21.409070	0.136735	164.804481	87
i5	21.545578	0.136508	164.804481	125
i5	21.545578	0.204762	219.999999	125
i5	21.545578	0.204762	277.166995	125
i5	21.681859	0.136508	164.804481	87
i5	21.818141	0.136735	184.997211	122
i5	21.818141	0.136735	246.934685	122
i5	21.818141	0.136735	311.126982	122
i5	22.090930	0.136508	184.997211	110
i5	22.090930	0.136508	246.934685	110
i5	22.090930	0.136508	311.126982	110
i5	22.363719	0.272789	184.997211	125
i5	22.363719	0.272789	246.934685	125
i5	22.363719	0.272789	311.126982	125
i5	22.772789	0.136508	164.804481	125
i5	22.772789	0.136508	219.999999	125
i5	22.772789	0.136508	277.166995	125
i5	23.045578	0.136508	164.804481	110
i5	23.045578	0.136508	219.999999	110
i5	23.045578	0.136508	277.166995	110
i5	23.318141	0.136735	164.804481	119
i5	23.318141	0.136735	219.999999	119
i5	23.318141	0.136735	277.166995	119
i5	23.454649	0.136508	164.804481	101
i5	23.454649	0.204762	219.999999	101
i5	23.454649	0.204762	277.166995	101
i5	23.590930	0.136508	164.804481	95
i5	23.727211	0.136735	164.804481	122
i5	23.727211	0.204762	219.999999	122
i5	23.727211	0.216327	277.166995	122
i5	23.863719	0.136508	164.804481	101
i5	24.000000	0.136508	246.934685	127
i5	24.000000	0.136508	164.804481	127
i5	24.000000	0.136508	207.646491	127
i5	24.272789	0.136508	246.934685	116
i5	24.272789	0.136508	207.646491	116
i5	24.272789	0.136508	164.804481	116
i5	24.545578	0.272789	246.934685	127
i5	24.545578	0.409297	164.804481	127
i5	24.545578	0.409297	207.646491	127
i5	24.818141	0.136735	246.934685	113
i5	24.954649	0.136508	164.804481	127
i5	24.954649	0.136508	219.999999	127
i5	24.954649	0.136508	277.166995	127
i5	25.227211	0.136735	164.804481	116
i5	25.227211	0.136735	219.999999	116
i5	25.227211	0.136735	277.166995	116
i5	25.500000	0.136508	164.804481	127
i5	25.500000	0.136508	219.999999	127
i5	25.636281	0.136735	164.804481	113
i5	25.500000	0.341270	277.166995	127
i5	25.636281	0.204989	219.999999	113
i5	25.772789	0.136508	164.804481	87
i5	25.909070	0.136735	164.804481	125
i5	25.909070	0.2     	220     	125
i5	25.909070	0.204762	277.166995	125
i5	26.045578	0.136508	164.804481	87
i5	26.181859	0.136508	184.997211	122
i5	26.181859	0.136508	246.934685	122
i5	26.181859	0.136508	311.126982	122
i5	26.454649	0.136508	184.997211	110
i5	26.454649	0.136508	246.934685	110
i5	26.454649	0.136508	311.126982	110
i5	26.727211	0.273016	184.997211	125
i5	26.727211	0.273016	246.934685	125
i5	26.727211	0.273016	311.126982	125
i5	27.136281	0.136735	164.804481	125
i5	27.136281	0.136735	219.999999	125
i5	27.136281	0.136735	277.166995	125
i5	27.409070	0.136735	164.804481	110
i5	27.409070	0.136735	219.999999	110
i5	27.409070	0.136735	277.166995	110
i5	27.681859	0.136508	164.804481	119
i5	27.681859	0.136508	219.999999	119
i5	27.681859	0.136508	277.166995	119
i5	27.818141	0.136735	164.804481	101
i5	27.818141	0.204762	219.999999	101
i5	27.818141	0.204762	277.166995	101
i5	27.954649	0.136508	164.804481	95
i5	28.090930	0.136508	164.804481	122
i5	28.090930	0.204762	219.999999	122
i5	28.090930	0.216100	277.166995	122
i5	28.227211	0.136735	164.804481	101
i5	28.363719	0.136508	246.934685	127
i5	28.363719	0.136508	164.804481	127
i5	28.363719	0.136508	207.646491	127
i5	28.636281	0.136735	246.934685	116
i5	28.636281	0.136735	207.646491	116
i5	28.636281	0.136735	164.804481	116
i5	28.909070	0.273016	246.934685	127
i5	28.909070	0.409297	164.804481	127
i5	28.909070	0.409297	207.646491	127
i5	29.181859	0.136508	246.934685	113
i5	29.318141	0.136735	164.804481	127
i5	29.318141	0.136735	219.999999	127
i5	29.318141	0.136735	277.166995	127
i5	29.590930	0.136508	164.804481	116
i5	29.590930	0.136508	219.999999	116
i5	29.590930	0.136508	277.166995	116
i5	29.863719	0.136508	164.804481	127
i5	29.863719	0.136508	219.999999	127
i5	30.000000	0.136508	164.804481	113
i5	29.863719	0.341043	277.166995	127
i5	30.000000	0.204762	219.999999	113
i5	30.136281	0.136735	164.804481	87
i5	30.272789	0.136508	164.804481	125
i5	30.272789	0.204762	219.999999	125
i5	30.272789	0.204762	277.166995	125
i5	30.409070	0.136735	164.804481	87
i5	30.545578	0.136508	184.997211	122
i5	30.545578	0.136508	246.934685	122
i5	30.545578	0.136508	311.126982	122
i5	30.818141	0.136735	184.997211	110
i5	30.818141	0.136735	246.934685	110
i5	30.818141	0.136735	311.126982	110
i5	31.090930	0.273016	184.997211	125
i5	31.090930	0.273016	246.934685	125
i5	31.090930	0.273016	311.126982	125
i5	31.500000	0.136508	164.804481	125
i5	31.500000	0.136508	219.999999	125
i5	31.500000	0.136508	277.166995	125
i5	31.772789	0.136508	164.804481	110
i5	31.772789	0.136508	219.999999	110
i5	31.772789	0.136508	277.166995	110
i5	32.045578	0.136508	164.804481	119
i5	32.045578	0.136508	219.999999	119
i5	32.045578	0.136508	277.166995	119
i5	32.181859	0.136508	164.804481	101
i5	32.181859	0.204762	219.999999	101
i5	32.181859	0.204762	277.166995	101
i5	32.318141	0.136735	164.804481	95
i5	32.454649	0.136508	164.804481	122
i5	32.454649	0.204762	219.999999	122
i5	32.454649	0.216100	277.166995	122
i5	32.590930	0.136508	164.804481	101
i5	32.727211	0.136735	246.934685	127
i5	32.727211	0.136735	164.804481	127
i5	32.727211	0.136735	207.646491	127
i5	33.000000	0.136508	246.934685	116
i5	33.000000	0.136508	207.646491	116
i5	33.000000	0.136508	164.804481	116
i5	33.272789	0.273016	246.934685	127
i5	33.272789	0.409297	164.804481	127
i5	33.272789	0.409297	207.646491	127
i5	33.545578	0.136508	246.934685	113
i5	33.681859	0.136508	164.804481	127
i5	33.681859	0.136508	219.999999	127
i5	33.681859	0.136508	277.166995	127
i5	33.954649	0.136508	164.804481	116
i5	33.954649	0.136508	219.999999	116
i5	33.954649	0.136508	277.166995	116
i5	34.227211	0.136735	164.804481	127
i5	34.227211	0.136735	219.999999	127
i5	34.363719	0.136508	164.804481	113
i5	34.227211	0.341270	277.166995	127
i5	34.363719	0.204762	219.999999	113
i5	34.500000	0.136508	164.804481	87
i5	34.636281	0.136735	164.804481	125
i5	34.636281	0.204989	219.999999	125
i5	34.636281	0.204989	277.166995	125
i5	34.772789	0.136508	164.804481	87
i5	34.909070	0.136735	184.997211	122
i5	34.909070	0.136735	246.934685	122
i5	34.909070	0.136735	311.126982	122
i5	35.181859	0.136508	184.997211	110
i5	35.181859	0.136508	246.934685	110
i5	35.181859	0.136508	311.126982	110
i5	35.454649	0.272789	184.997211	125
i5	35.454649	0.272789	246.934685	125
i5	35.454649	0.272789	311.126982	125
i5	35.863719	0.136508	164.804481	125
i5	35.863719	0.136508	219.999999	125
i5	35.863719	0.136508	277.166995	125
i5	36.136281	0.136735	164.804481	110
i5	36.136281	0.136735	219.999999	110
i5	36.136281	0.136735	277.166995	110
i5	36.409070	0.136735	164.804481	119
i5	36.409070	0.136735	219.999999	119
i5	36.409070	0.136735	277.166995	119
i5	36.545578	0.136508	164.804481	101
i5	36.545578	0.204762	219.999999	101
i5	36.545578	0.204762	277.166995	101
i5	36.681859	0.136508	164.804481	95
i5	36.818141	0.136735	164.804481	122
i5	36.818141	0.204762	219.999999	122
i5	36.818141	0.216100	277.166995	122
i5	36.954649	0.136508	164.804481	101
i5	37.090930	0.136508	246.934685	127
i5	37.090930	0.136508	164.804481	127
i5	37.090930	0.136508	207.646491	127
i5	37.363719	0.136508	246.934685	116
i5	37.363719	0.136508	207.646491	116
i5	37.363719	0.136508	164.804481	116
i5	37.636508	0.272789	246.934685	127
i5	37.636508	0.409297	164.804481	127
i5	37.636508	0.409297	207.646491	127
i5	37.909070	0.136735	246.934685	113
i5	38.045578	0.136508	164.804481	127
i5	38.045578	0.136508	219.999999	127
i5	38.045578	0.136508	277.166995	127
i5	38.318141	0.136735	164.804481	116
i5	38.318141	0.136735	219.999999	116
i5	38.318141	0.136735	277.166995	116
i5	38.590930	0.136508	164.804481	127
i5	38.590930	0.136508	219.999999	127
i5	38.727211	0.136735	164.804481	113
i5	38.590930	0.341043	277.166995	127
i5	38.727211	0.204762	219.999999	113
i5	38.863719	0.136508	164.804481	87
i5	39.000000	0.136735	164.804481	125
i5	39.000000	0.204762	219.999999	125
i5	39.000000	0.204762	277.166995	125
i5	39.136508	0.136508	164.804481	87
i5	39.272789	0.136508	184.997211	122
i5	39.272789	0.136508	246.934685	122
i5	39.272789	0.136508	311.126982	122
i5	39.545578	0.136508	184.997211	110
i5	39.545578	0.136508	246.934685	110
i5	39.545578	0.136508	311.126982	110
i5	39.818141	0.273016	184.997211	125
i5	39.818141	0.273016	246.934685	125
i5	39.818141	0.273016	311.126982	125
i5	40.227211	0.136735	164.804481	125
i5	40.227211	0.136735	219.999999	125
i5	40.227211	0.136735	277.166995	125
i5	40.500000	0.136735	164.804481	110
i5	40.500000	0.136735	219.999999	110
i5	40.500000	0.136735	277.166995	110
i5	40.772789	0.136508	164.804481	119
i5	40.772789	0.136508	219.999999	119
i5	40.772789	0.136508	277.166995	119
i5	40.909070	0.136735	164.804481	101
i5	40.909070	0.204762	219.999999	101
i5	40.909070	0.204762	277.166995	101
i5	41.045578	0.136508	164.804481	95
i5	41.181859	0.136508	164.804481	122
i5	41.181859	0.204762	219.999999	122
i5	41.181859	0.216100	277.166995	122
i5	41.318141	0.136735	164.804481	101
i5	41.454649	0.136508	246.934685	127
i5	41.454649	0.136508	164.804481	127
i5	41.454649	0.136508	207.646491	127
i5	41.727211	0.136735	246.934685	116
i5	41.727211	0.136735	207.646491	116
i5	41.727211	0.136735	164.804481	116
i5	42.000000	0.273016	246.934685	127
i5	42.000000	0.409297	164.804481	127
i5	42.000000	0.409297	207.646491	127
i5	42.272789	0.136508	246.934685	113
i5	42.409070	0.136735	164.804481	127
i5	42.409070	0.136735	219.999999	127
i5	42.409070	0.136735	277.166995	127
i5	42.681859	0.136508	164.804481	116
i5	42.681859	0.136508	219.999999	116
i5	42.681859	0.136508	277.166995	116
i5	42.954649	0.136508	164.804481	127
i5	42.954649	0.136508	219.999999	127
i5	43.090930	0.136508	164.804481	113
i5	42.954649	0.341043	277.166995	127
i5	43.090930	0.204762	219.999999	113
i5	43.227211	0.136735	164.804481	87
i5	43.363719	0.136508	164.804481	125
i5	43.363719	0.204762	219.999999	125
i5	43.363719	0.204762	277.166995	125
i5	43.500000	0.136735	164.804481	87
i5	43.636508	0.136508	184.997211	122
i5	43.636508	0.136508	246.934685	122
i5	43.636508	0.136508	311.126982	122
i5	43.909070	0.136735	184.997211	110
i5	43.909070	0.136735	246.934685	110
i5	43.909070	0.136735	311.126982	110
i5	44.181859	0.273016	184.997211	125
i5	44.181859	0.273016	246.934685	125
i5	44.181859	0.273016	311.126982	125
i5	44.590930	0.136508	164.804481	125
i5	44.590930	0.136508	219.999999	125
i5	44.590930	0.136508	277.166995	125
i5	44.863719	0.136508	164.804481	110
i5	44.863719	0.136508	219.999999	110
i5	44.863719	0.136508	277.166995	110
i5	45.136508	0.136508	164.804481	119
i5	45.136508	0.136508	219.999999	119
i5	45.136508	0.136508	277.166995	119
i5	45.272789	0.136508	164.804481	101
i5	45.272789	0.204762	219.999999	101
i5	45.272789	0.204762	277.166995	101
i5	45.409070	0.136735	164.804481	95
i5	45.545578	0.136508	164.804481	122
i5	45.545578	0.204762	219.999999	122
i5	45.545578	0.216100	277.166995	122
i5	45.681859	0.136508	164.804481	101
i5	45.818141	0.136735	246.934685	127
i5	45.818141	0.136735	164.804481	127
i5	45.818141	0.136735	207.646491	127
i5	46.090930	0.136508	246.934685	116
i5	46.090930	0.136508	207.646491	116
i5	46.090930	0.136508	164.804481	116
i5	46.363719	0.273016	246.934685	127
i5	46.363719	0.409297	164.804481	127
i5	46.363719	0.409297	207.646491	127
i5	46.636508	0.136508	246.934685	113
i5	46.772789	0.136508	164.804481	127
i5	46.772789	0.136508	219.999999	127
i5	46.772789	0.136508	277.166995	127
i5	47.045578	0.136508	164.804481	116
i5	47.045578	0.136508	219.999999	116
i5	47.045578	0.136508	277.166995	116
i5	47.318141	0.136735	164.804481	127
i5	47.318141	0.136735	219.999999	127
i5	47.454649	0.136508	164.804481	113
i5	47.318141	0.341270	277.166995	127
i5	47.454649	0.204762	219.999999	113
i5	47.590930	0.136508	164.804481	87
i5	47.727211	0.136735	164.804481	125
i5	47.727211	0.204762	219.999999	125
i5	47.727211	0.204762	277.166995	125
i5	47.863719	0.136508	164.804481	87
i5	48.000000	0.136735	184.997211	122
i5	48.000000	0.136735	246.934685	122
i5	48.000000	0.136735	311.126982	122
i5	48.272789	0.136508	184.997211	110
i5	48.272789	0.136508	246.934685	110
i5	48.272789	0.136508	311.126982	110
i5	48.545578	0.272789	184.997211	125
i5	48.545578	0.272789	246.934685	125
i5	48.545578	0.272789	311.126982	125
i5	48.954649	0.136508	164.804481	125
i5	48.954649	0.136508	219.999999	125
i5	48.954649	0.136508	277.166995	125
i5	49.227211	0.136735	164.804481	110
i5	49.227211	0.136735	219.999999	110
i5	49.227211	0.136735	277.166995	110
i5	49.500000	0.136735	164.804481	119
i5	49.500000	0.136735	219.999999	119
i5	49.500000	0.136735	277.166995	119
i5	49.636508	0.136508	164.804481	101
i5	49.636508	0.204762	219.999999	101
i5	49.636508	0.204762	277.166995	101
i5	49.772789	0.136508	164.804481	95
i5	49.909070	0.136735	164.804481	122
i5	49.909070	0.204762	219.999999	122
i5	49.909070	0.216100	277.166995	122
i5	50.045578	0.136508	164.804481	101
i5	50.181859	0.136508	246.934685	127
i5	50.181859	0.136508	164.804481	127
i5	50.181859	0.136508	207.646491	127
i5	50.454649	0.136508	246.934685	116
i5	50.454649	0.136508	207.646491	116
i5	50.454649	0.136508	164.804481	116
i5	50.727211	0.273016	246.934685	127
i5	50.727211	0.409524	164.804481	127
i5	50.727211	0.409524	207.646491	127
i5	51.000000	0.136735	246.934685	113
i5	51.136508	0.136508	164.804481	127
i5	51.136508	0.136508	219.999999	127
i5	51.136508	0.136508	277.166995	127
i5	51.409070	0.136735	164.804481	116
i5	51.409070	0.136735	219.999999	116
i5	51.409070	0.136735	277.166995	116
i5	51.681859	0.136508	164.804481	127
i5	51.681859	0.136508	219.999999	127
i5	51.818141	0.136735	164.804481	113
i5	51.681859	0.341043	277.166995	127
i5	51.818141	0.204762	219.999999	113
i5	51.954649	0.136508	164.804481	87
i5	52.090930	0.136508	164.804481	125
i5	52.090930	0.204762	219.999999	125
i5	52.090930	0.204762	277.166995	125
i5	52.227211	0.136735	164.804481	87
i5	52.363719	0.136508	184.997211	122
i5	52.363719	0.136508	246.934685	122
i5	52.363719	0.136508	311.126982	122
i5	52.636508	0.136508	184.997211	110
i5	52.636508	0.136508	246.934685	110
i5	52.636508	0.136508	311.126982	110
i5	52.909070	0.273016	184.997211	125
i5	52.909070	0.273016	246.934685	125
i5	52.909070	0.273016	311.126982	125
i5	53.318141	0.136735	164.804481	125
i5	53.318141	0.136735	219.999999	125
i5	53.318141	0.136735	277.166995	125
i5	53.590930	0.136508	164.804481	110
i5	53.590930	0.136508	219.999999	110
i5	53.590930	0.136508	277.166995	110
i5	53.863719	0.136508	164.804481	119
i5	53.863719	0.136508	219.999999	119
i5	53.863719	0.136508	277.166995	119
i5	54.000000	0.136735	164.804481	101
i5	54.000000	0.204762	219.999999	101
i5	54.000000	0.204762	277.166995	101
i5	54.136508	0.136508	164.804481	95
i5	54.272789	0.136508	164.804481	122
i5	54.272789	0.204762	219.999999	122
i5	54.272789	0.216100	277.166995	122
i5	54.409070	0.136735	164.804481	101
i5	54.545578	0.136508	207.646491	127
i5	54.545578	0.136508	155.563491	127
i5	54.545578	0.136508	246.934685	127
i5	54.818141	0.136735	207.646491	116
i5	54.818141	0.136735	155.563491	116
i5	54.818141	0.136735	246.934685	116
i5	55.090930	0.273016	246.934685	127
i5	55.090930	0.409297	207.646491	127
i5	55.090930	0.409297	155.563491	127
i5	55.363719	0.136508	246.934685	113
i5	55.500000	0.136735	207.646491	127
i5	55.500000	0.136735	246.934685	127
i5	55.500000	0.136735	155.563491	127
i5	55.772789	0.136508	207.646491	116
i5	55.772789	0.136508	155.563491	116
i5	55.772789	0.136508	246.934685	116
i5	56.045578	0.136508	207.646491	127
i5	56.045578	0.136508	155.563491	127
i5	56.181859	0.136508	155.563491	113
i5	56.045578	0.341043	246.934685	127
i5	56.181859	0.204762	207.646491	113
i5	56.318141	0.136735	155.563491	87
i5	56.454649	0.136508	155.563491	125
i5	56.454649	0.204762	207.646491	125
i5	56.454649	0.204762	246.934685	125
i5	56.590930	0.136508	155.563491	87
i5	56.727211	0.136735	164.804481	122
i5	56.727211	0.136735	219.999999	122
i5	56.727211	0.136735	277.166995	122
i5	57.000000	0.136735	164.804481	110
i5	57.000000	0.136735	219.999999	110
i5	57.000000	0.136735	277.166995	110
i5	57.272789	0.273016	164.804481	125
i5	57.272789	0.273016	219.999999	125
i5	57.272789	0.273016	277.166995	125
i5	57.681859	0.136508	164.804481	125
i5	57.681859	0.136508	219.999999	125
i5	57.681859	0.136508	277.166995	125
i5	57.954649	0.136508	164.804481	110
i5	57.954649	0.136508	219.999999	110
i5	57.954649	0.136508	277.166995	110
i5	58.227211	0.136735	164.804481	119
i5	58.227211	0.136735	219.999999	119
i5	58.227211	0.136735	277.166995	119
i5	58.363719	0.136508	164.804481	101
i5	58.363719	0.204762	219.999999	101
i5	58.363719	0.204762	277.166995	101
i5	58.500000	0.136735	164.804481	95
i5	58.636508	0.136508	164.804481	122
i5	58.636508	0.204762	219.999999	122
i5	58.636508	0.216100	277.166995	122
i5	58.772789	0.136508	164.804481	101
i5	58.909070	0.136735	207.646491	127
i5	58.909070	0.136735	155.563491	127
i5	58.909070	0.136735	246.934685	127
i5	59.181859	0.136508	207.646491	116
i5	59.181859	0.136508	155.563491	116
i5	59.181859	0.136508	246.934685	116
i5	59.454649	0.272789	246.934685	127
i5	59.454649	0.409297	207.646491	127
i5	59.454649	0.409297	155.563491	127
i5	59.727211	0.136735	246.934685	113
i5	59.863719	0.136508	207.646491	127
i5	59.863719	0.136508	246.934685	127
i5	59.863719	0.136508	155.563491	127
i5	60.136508	0.136508	207.646491	116
i5	60.136508	0.136508	155.563491	116
i5	60.136508	0.136508	246.934685	116
i5	60.409070	0.136735	207.646491	127
i5	60.409070	0.136735	155.563491	127
i5	60.545578	0.136508	155.563491	113
i5	60.409070	0.341270	246.934685	127
i5	60.545578	0.204762	207.646491	113
i5	60.681859	0.136508	155.563491	87
i5	60.818141	0.136735	155.563491	125
i5	60.818141	0.204762	207.646491	125
i5	60.818141	0.204762	246.934685	125
i5	60.954649	0.136508	155.563491	87
i5	61.090930	0.136508	164.804481	122
i5	61.090930	0.136508	219.999999	122
i5	61.090930	0.136508	277.166995	122
i5	61.363719	0.136508	164.804481	110
i5	61.363719	0.136508	219.999999	110
i5	61.363719	0.136508	277.166995	110
i5	61.636508	0.272789	164.804481	125
i5	61.636508	0.272789	219.999999	125
i5	61.636508	0.272789	277.166995	125
i5	62.045578	0.136508	164.804481	125
i5	62.045578	0.136508	219.999999	125
i5	62.045578	0.136508	277.166995	125
i5	62.318141	0.136735	164.804481	110
i5	62.318141	0.136735	219.999999	110
i5	62.318141	0.136735	277.166995	110
i5	62.590930	0.136735	164.804481	119
i5	62.590930	0.136735	219.999999	119
i5	62.590930	0.136735	277.166995	119
i5	62.727438	0.136508	164.804481	101
i5	62.727438	0.204762	219.999999	101
i5	62.727438	0.204762	277.166995	101
i5	62.863719	0.136508	164.804481	95
i5	63.000000	0.136735	164.804481	122
i5	63.000000	0.204762	219.999999	122
i5	63.000000	0.216100	277.166995	122
i5	63.136508	0.136508	164.804481	101
i5	63.272789	0.136508	207.646491	127
i5	63.272789	0.136508	155.563491	127
i5	63.272789	0.136508	246.934685	127
i5	63.545578	0.136508	207.646491	116
i5	63.545578	0.136508	155.563491	116
i5	63.545578	0.136508	246.934685	116
i5	63.818141	0.273016	246.934685	127
i5	63.818141	0.4     	207.646491	127
i5	63.818141	0.4     	155.563491	127
i5	64.090930	0.13    	246.934685	113
i5	64.227438	0.13    	207.646491	127
i5	64.227438	0.13    	246.934685	127
i5	64.227438	0.13    	155.563491	127
i5	64.500000	0.13    	207.646491	116
i5	64.500000	0.13    	155.563491	116
i5	64.500000	0.13    	246.9   	116
i5	64.772789	0.13    	207.6   	127
i5	64.772789	0.13    	155.6   	125
i5	64.9    	0.13    	155.6   	113
i5	64.772789	0.341043	246.934685	127
i5	64.9    	0.204762	207.6   	113
i5	65.0    	0.14    	155.563491	87
i5	65.181859	0.14    	155.563491	125
i5	65.181859	0.2     	207.6   	125
i5	65.181859	0.2     	246.934685	125
i5	65.318141	0.136735	155.563491	87
i5	65.454649	0.136508	164.804481	122
i5	65.454649	0.136508	219.999999	122
i5	65.454649	0.136508	277.166995	122
i5	65.727438	0.136508	164.804481	110
i5	65.727438	0.136508	219.999999	110
i5	65.727438	0.136508	277.166995	110
i5	66.000000	0.273016	164.804481	125
i5	66.000000	0.273016	219.999999	125
i5	66.000000	0.273016	277.166995	125
i5	66.409070	0.136735	164.804481	125
i5	66.409070	0.136735	219.999999	125
i5	66.409070	0.136735	277.166995	125
i5	66.681859	0.136508	164.804481	110
i5	66.681859	0.136508	219.999999	110
i5	66.681859	0.136508	277.166995	110
i5	66.954649	0.136508	164.804481	119
i5	66.954649	0.136508	219.999999	119
i5	66.954649	0.136508	277.166995	119
i5	67.090930	0.136735	164.804481	101
i5	67.090930	0.204762	219.999999	101
i5	67.090930	0.204762	277.166995	101
i5	67.227438	0.136508	164.804481	95
i5	67.363719	0.136508	164.804481	122
i5	67.363719	0.2     	220.0   	122
i5	67.363719	0.216100	277.166995	122
i5	67.500000	0.14    	164.804481	101
i5	67.636508	0.14	    246.934685	127
i5	67.636508	0.14    	164.804481	127
i5	67.636508	0.14	    207.646491	127
i5	67.909070	0.14    	246.934685	116
i5	67.909070	0.14    	207.646491	116
i5	67.909070	0.14    	164.804481	116
i5	68.181859	0.273016	246.934685	127
i5	68.181859	0.41    	164.804481	127
i5	68.181859	0.41    	207.646491	127
i5	68.454649	0.136508	246.934685	113
i5	68.590930	0.136735	184.997211	127
i5	68.590930	0.136735	219.999999	127
i5	68.590930	0.136735	277.166995	127
i5	68.863719	0.136508	184.997211	116
i5	68.863719	0.136508	219.999999	116
i5	68.863719	0.136508	277.166995	116
i5	69.136508	0.136508	184.997211	127
i5	69.136508	0.136508	219.999999	127
i5	69.272789	0.136508	184.997211	113
i5	69.136508	0.341043	277.166995	127
i5	69.272789	0.204762	219.999999	113
i5	69.409070	0.136735	184.997211	87
i5	69.545578	0.136508	184.997211	125
i5	69.545578	0.204762	219.999999	125
i5	69.545578	0.204762	277.166995	125
i5	69.681859	0.136508	184.997211	87
i5	69.818141	0.136735	207.646491	122
i5	69.818141	0.136735	246.934685	122
i5	69.818141	0.136735	311.126982	122
i5	70.090930	0.136735	207.646491	110
i5	70.090930	0.136735	246.934685	110
i5	70.090930	0.136735	311.126982	110
i5	70.363719	0.273016	207.646491	125
i5	70.363719	0.273016	246.934685	125
i5	70.363719	0.273016	311.126982	125
i5	70.772789	0.136508	184.997211	125
i5	70.772789	0.136508	219.999999	125
i5	70.772789	0.136508	277.166995	125
i5	71.045578	0.136508	184.997211	110
i5	71.045578	0.136508	219.999999	110
i5	71.045578	0.136508	277.166995	110
i5	71.318141	0.136735	184.997211	119
i5	71.318141	0.136735	219.999999	119
i5	71.318141	0.136735	277.166995	119
i5	71.454649	0.136508	184.997211	101
i5	71.454649	0.204762	219.999999	101
i5	71.454649	0.204762	277.166995	101
i5	71.590930	0.136735	184.997211	95
i5	71.727438	0.136508	184.997211	122
i5	71.727438	0.204762	219.999999	122
i5	71.727438	0.216100	277.166995	122
i5	71.863719	0.136508	184.997211	101
i5	72.000000	0.136735	246.934685	127
i5	72.000000	0.136735	164.804481	127
i5	72.000000	0.136735	207.646491	127
i5	72.272789	0.136508	246.934685	116
i5	72.272789	0.136508	207.646491	116
i5	72.272789	0.136508	164.804481	116
i5	72.545578	0.272789	246.934685	127
i5	72.545578	0.409297	164.804481	127
i5	72.545578	0.409297	207.646491	127
i5	72.818141	0.136735	246.934685	113
i5	72.954649	0.136508	184.997211	127
i5	72.954649	0.136508	219.999999	127
i5	72.954649	0.136508	277.166995	127
i5	73.227438	0.136508	184.997211	116
i5	73.227438	0.136508	219.999999	116
i5	73.227438	0.136508	277.166995	116
i5	73.500000	0.136735	184.997211	127
i5	73.500000	0.136735	219.999999	127
i5	73.636508	0.136508	184.997211	113
i5	73.500000	0.341270	277.166995	127
i5	73.636508	0.204762	219.999999	113
i5	73.772789	0.136508	184.997211	87
i5	73.909070	0.136735	184.997211	125
i5	73.909070	0.204762	219.999999	125
i5	73.909070	0.204762	277.166995	125
i5	74.045578	0.136508	184.997211	87
i5	74.181859	0.136508	207.646491	122
i5	74.181859	0.136508	246.934685	122
i5	74.181859	0.136508	311.126982	122
i5	74.454649	0.136508	207.646491	110
i5	74.454649	0.136508	246.934685	110
i5	74.454649	0.136508	311.126982	110
i5	74.727438	0.272789	207.646491	125
i5	74.727438	0.272789	246.934685	125
i5	74.727438	0.272789	311.126982	125
i5	75.136508	0.136508	184.997211	125
i5	75.136508	0.136508	219.999999	125
i5	75.136508	0.136508	277.166995	125
i5	75.409070	0.136735	184.997211	110
i5	75.409070	0.136735	219.999999	110
i5	75.409070	0.136735	277.166995	110
i5	75.681859	0.136508	184.997211	119
i5	75.681859	0.136508	219.999999	119
i5	75.681859	0.136508	277.166995	119
i5	75.818141	0.136735	184.997211	101
i5	75.818141	0.204989	219.999999	101
i5	75.818141	0.204989	277.166995	101
i5	75.954649	0.136508	184.997211	95
i5	76.090930	0.136735	184.997211	122
i5	76.090930	0.204762	219.999999	122
i5	76.090930	0.216100	277.166995	122
i5	76.227438	0.136508	184.997211	101
i5	76.363719	0.136508	246.934685	127
i5	76.363719	0.136508	164.804481	127
i5	76.363719	0.136508	207.646491	127
i5	76.636508	0.136508	246.934685	116
i5	76.636508	0.136508	207.646491	116
i5	76.636508	0.136508	164.804481	116
i5	76.909070	0.273016	246.934685	127
i5	76.909070	0.409297	164.804481	127
i5	76.909070	0.409297	207.646491	127
i5	77.181859	0.136508	246.934685	113
i5	77.318141	0.136735	164.804481	127
i5	77.318141	0.136735	219.999999	127
i5	77.318141	0.136735	277.166995	127
i5	77.590930	0.136735	164.804481	116
i5	77.590930	0.136735	219.999999	116
i5	77.590930	0.136735	277.166995	116
i5	77.863719	0.136508	164.804481	127
i5	77.863719	0.136508	219.999999	127
i5	78.000000	0.136735	164.804481	113
i5	77.863719	0.341043	277.166995	127
i5	78.000000	0.204762	219.999999	113
i5	78.136508	0.136508	164.804481	87
i5	78.272789	0.136508	164.804481	125
i5	78.272789	0.2     	220.0    	125
i5	78.272789	0.2     	277.166995	125
i5	78.409070	0.136735	164.804481	87
i5	78.545578	0.136508	184.997211	122
i5	78.545578	0.136508	246.934685	122
i5	78.545578	0.136508	311.126982	122
i5	78.818141	0.136735	184.997211	110
i5	78.818141	0.136735	246.934685	110
i5	78.818141	0.136735	311.126982	110
i5	79.090930	0.273016	184.997211	125
i5	79.090930	0.273016	246.934685	125
i5	79.090930	0.273016	311.126982	125
i5	79.500000	0.136735	164.804481	125
i5	79.500000	0.136735	219.999999	125
i5	79.500000	0.136735	277.166995	125
i5	79.772789	0.136508	164.804481	110
i5	79.772789	0.136508	219.999999	110
i5	79.772789	0.136508	277.166995	110
i5	80.045578	0.136508	164.804481	119
i5	80.045578	0.136508	219.999999	119
i5	80.045578	0.136508	277.166995	119
i5	80.181859	0.136508	164.804481	101
i5	80.181859	0.204762	219.999999	101
i5	80.181859	0.204762	277.166995	101
i5	80.318141	0.136735	164.804481	95
i5	80.454649	0.136508	164.804481	122
i5	80.454649	0.204762	219.999999	122
i5	80.454649	0.216100	277.166995	122
i5	80.590930	0.136735	164.804481	101
i5	80.727438	0.136508	246.934685	127
i5	80.727438	0.136508	164.804481	127
i5	80.727438	0.136508	207.646491	127
i5	81.000000	0.136735	246.934685	116
i5	81.000000	0.136735	207.646491	116
i5	81.000000	0.136735	164.804481	116
i5	81.272789	0.273016	246.934685	127
i5	81.272789	0.409297	164.804481	127
i5	81.272789	0.409297	207.646491	127
i5	81.545578	0.136508	246.934685	113
i5	81.681859	0.136508	164.804481	127
i5	81.681859	0.136508	219.999999	127
i5	81.681859	0.136508	277.166995	127
i5	81.954649	0.136508	164.804481	116
i5	81.954649	0.136508	219.999999	116
i5	81.954649	0.136508	277.166995	116
i5	82.227438	0.136508	164.804481	127
i5	82.227438	0.136508	219.999999	127
i5	82.363719	0.136508	164.804481	113
i5	82.227438	0.341043	277.166995	127
i5	82.363719	0.204762	219.999999	113
i5	82.500000	0.136735	164.804481	87
i5	82.636508	0.136508	164.804481	125
i5	82.636508	0.204762	219.999999	125
i5	82.636508	0.204762	277.166995	125
i5	82.772789	0.136508	164.804481	87
i5	82.909070	0.136735	184.997211	122
i5	82.909070	0.136735	246.934685	122
i5	82.909070	0.136735	311.126982	122
i5	83.181859	0.136508	184.997211	110
i5	83.181859	0.136508	246.934685	110
i5	83.181859	0.136508	311.126982	110
i5	83.454649	0.273016	184.997211	125
i5	83.454649	0.273016	246.934685	125
i5	83.454649	0.273016	311.126982	125
i5	83.863719	0.136508	164.804481	125
i5	83.863719	0.136508	219.999999	125
i5	83.863719	0.136508	277.166995	125
i5	84.136508	0.136508	164.804481	110
i5	84.136508	0.136508	219.999999	110
i5	84.136508	0.136508	277.166995	110
i5	84.409070	0.136735	164.804481	119
i5	84.409070	0.136735	219.999999	119
i5	84.409070	0.136735	277.166995	119
i5	84.545578	0.136508	164.804481	101
i5	84.545578	0.204762	219.999999	101
i5	84.545578	0.204762	277.166995	101
i5	84.681859	0.136508	164.804481	95
i5	84.818141	0.136735	164.804481	122
i5	84.818141	0.204989	219.999999	122
i5	84.818141	0.216327	277.166995	122
i5	84.954649	0.136508	164.804481	101
i5	85.090930	0.136735	246.934685	127
i5	85.090930	0.136735	164.804481	127
i5	85.090930	0.136735	207.646491	127
i5	85.363719	0.136508	246.934685	116
i5	85.363719	0.136508	207.646491	116
i5	85.363719	0.136508	164.804481	116
i5	85.636508	0.272789	246.934685	127
i5	85.636508	0.409297	164.804481	127
i5	85.636508	0.409297	207.646491	127
i5	85.909070	0.136735	246.934685	113
i5	86.045578	0.136508	164.804481	127
i5	86.045578	0.136508	219.999999	127
i5	86.045578	0.136508	277.166995	127
i5	86.318141	0.136735	164.804481	116
i5	86.318141	0.136735	219.999999	116
i5	86.318141	0.136735	277.166995	116
i5	86.590930	0.136735	164.804481	127
i5	86.590930	0.136735	219.999999	127
i5	86.727438	0.136508	164.804481	113
i5	86.590930	0.341270	277.166995	127
i5	86.727438	0.204762	219.999999	113
i5	86.863719	0.136508	164.804481	87
i5	87.000000	0.136735	164.804481	125
i5	87.000000	0.204762	219.999999	125
i5	87.000000	0.204762	277.166995	125
i5	87.136508	0.136508	164.804481	87
i5	87.272789	0.136508	184.997211	122
i5	87.272789	0.136508	246.934685	122
i5	87.272789	0.136508	311.126982	122
i5	87.545578	0.136508	184.997211	110
i5	87.545578	0.136508	246.934685	110
i5	87.545578	0.136508	311.126982	110
i5	87.818367	0.272789	184.997211	125
i5	87.818367	0.272789	246.934685	125
i5	87.818367	0.272789	311.126982	125
i5	88.227438	0.136508	164.804481	125
i5	88.227438	0.136508	219.999999	125
i5	88.227438	0.136508	277.166995	125
i5	88.500000	0.136735	164.804481	110
i5	88.500000	0.136735	219.999999	110
i5	88.500000	0.136735	277.166995	110
i5	88.772789	0.136508	164.804481	119
i5	88.772789	0.136508	219.999999	119
i5	88.772789	0.136508	277.166995	119
i5	88.909070	0.136735	164.804481	101
i5	88.909070	0.204762	219.999999	101
i5	88.909070	0.204762	277.166995	101
i5	89.045578	0.136508	164.804481	95
i5	89.181859	0.136735	164.804481	122
i5	89.181859	0.204762	219.999999	122
i5	89.181859	0.216100	277.166995	122
i5	89.318367	0.136508	164.804481	101
i5	89.454649	0.136508	246.934685	127
i5	89.454649	0.136508	164.804481	127
i5	89.454649	0.136508	207.646491	127
i5	89.727438	0.136508	246.934685	116
i5	89.727438	0.136508	207.646491	116
i5	89.727438	0.136508	164.804481	116
i5	90.000000	0.273016	246.934685	127
i5	90.000000	0.409297	164.804481	127
i5	90.000000	0.409297	207.646491	127
i5	90.272789	0.136508	246.934685	113
i5	90.409070	0.136735	164.804481	127
i5	90.409070	0.136735	219.999999	127
i5	90.409070	0.136735	277.166995	127
i5	90.681859	0.136735	164.804481	116
i5	90.681859	0.136735	219.999999	116
i5	90.681859	0.136735	277.166995	116
i5	90.954649	0.136508	164.804481	127
i5	90.954649	0.136508	219.999999	127
i5	91.090930	0.136735	164.804481	113
i5	90.954649	0.341043	277.166995	127
i5	91.090930	0.204762	219.999999	113
i5	91.227438	0.136508	164.804481	87
i5	91.363719	0.136508	164.804481	125
i5	91.363719	0.204762	219.999999	125
i5	91.363719	0.204762	277.166995	125
i5	91.500000	0.136735	164.804481	87
i5	91.636508	0.136508	184.997211	122
i5	91.636508	0.136508	246.934685	122
i5	91.636508	0.136508	311.126982	122
i5	91.909070	0.136735	184.997211	110
i5	91.909070	0.136735	246.934685	110
i5	91.909070	0.136735	311.126982	110
i5	92.181859	0.273016	184.997211	125
i5	92.181859	0.273016	246.934685	125
i5	92.181859	0.273016	311.126982	125
i5	92.590930	0.136735	164.804481	125
i5	92.590930	0.136735	219.999999	125
i5	92.590930	0.136735	277.166995	125
i5	92.863719	0.136508	164.804481	110
i5	92.863719	0.136508	219.999999	110
i5	92.863719	0.136508	277.166995	110
i5	93.136508	0.136508	164.804481	119
i5	93.136508	0.136508	219.999999	119
i5	93.136508	0.136508	277.166995	119
i5	93.272789	0.136508	164.804481	101
i5	93.272789	0.204762	219.999999	101
i5	93.272789	0.204762	277.166995	101
i5	93.409070	0.136735	164.804481	95
i5	93.545578	0.136508	164.804481	122
i5	93.545578	0.204762	219.999999	122
i5	93.545578	0.216100	277.166995	122
i5	93.681859	0.136735	164.804481	101
i5	93.818367	0.136508	246.934685	127
i5	93.818367	0.136508	164.804481	127
i5	93.818367	0.136508	207.646491	127
i5	94.090930	0.136735	246.934685	116
i5	94.090930	0.136735	207.646491	116
i5	94.090930	0.136735	164.804481	116
i5	94.363719	0.273016	246.934685	127
i5	94.363719	0.409297	164.804481	127
i5	94.363719	0.409297	207.646491	127
i5	94.636508	0.136508	246.934685	113
i5	94.772789	0.136508	164.804481	127
i5	94.772789	0.136508	219.999999	127
i5	94.772789	0.136508	277.166995	127
i5	95.045578	0.136508	164.804481	116
i5	95.045578	0.136508	219.999999	116
i5	95.045578	0.136508	277.166995	116
i5	95.318367	0.136508	164.804481	127
i5	95.318367	0.136508	219.999999	127
i5	95.454649	0.136508	164.804481	113
i5	95.318367	0.341043	277.166995	127
i5	95.454649	0.204762	219.999999	113
i5	95.590930	0.136735	164.804481	87
i5	95.727438	0.136508	164.804481	125
i5	95.727438	0.204762	219.999999	125
i5	95.727438	0.204762	277.166995	125
i5	95.863719	0.136508	164.804481	87
i5	96.000000	0.136735	184.997211	122
i5	96.000000	0.136735	246.934685	122
i5	96.000000	0.136735	311.126982	122
i5	96.272789	0.136508	184.997211	110
i5	96.272789	0.136508	246.934685	110
i5	96.272789	0.136508	311.126982	110
i5	96.545578	0.273016	184.997211	125
i5	96.545578	0.273016	246.934685	125
i5	96.545578	0.273016	311.126982	125
i5	96.954649	0.136508	164.804481	125
i5	96.954649	0.136508	219.999999	125
i5	96.954649	0.136508	277.166995	125
i5	97.227438	0.136508	164.804481	110
i5	97.227438	0.136508	219.999999	110
i5	97.227438	0.136508	277.166995	110
i5	97.500000	0.136735	164.804481	119
i5	97.500000	0.136735	219.999999	119
i5	97.500000	0.136735	277.166995	119
i5	97.636508	0.136508	164.804481	101
i5	97.636508	0.204762	219.999999	101
i5	97.636508	0.204762	277.166995	101
i5	97.772789	0.136508	164.804481	95
i5	97.909070	0.136735	164.804481	122
i5	97.909070	0.204762	219.999999	122
i5	97.909070	0.216327	277.166995	122
i5	98.045578	0.136508	164.804481	101
i5	98.181859	0.136735	246.934685	127
i5	98.181859	0.136735	164.804481	127
i5	98.181859	0.136735	207.646491	127
i5	98.454649	0.136508	246.934685	116
i5	98.454649	0.136508	207.646491	116
i5	98.454649	0.136508	164.804481	116
i5	98.727438	0.272789	246.934685	127
i5	98.727438	0.409297	164.804481	127
i5	98.727438	0.409297	207.646491	127
i5	99.000000	0.136735	246.934685	113
i5	99.136508	0.136508	164.804481	127
i5	99.136508	0.136508	219.999999	127
i5	99.136508	0.136508	277.166995	127
i5	99.409070	0.136735	164.804481	116
i5	99.409070	0.136735	219.999999	116
i5	99.409070	0.136735	277.166995	116
i5	99.681859	0.136735	164.804481	127
i5	99.681859	0.136735	219.999999	127
i5	99.818367	0.136508	164.804481	113
i5	99.681859	0.341270	277.166995	127
i5	99.818367	0.204762	219.999999	113
i5	99.954649	0.136508	164.804481	87
i5	100.090930	0.136735	164.804481	125
i5	100.090930	0.204762	219.999999	125
i5	100.090930	0.204762	277.166995	125
i5	100.227438	0.136508	164.804481	87
i5	100.363719	0.136508	184.997211	122
i5	100.363719	0.136508	246.934685	122
i5	100.363719	0.136508	311.126982	122
i5	100.636508	0.136508	184.997211	110
i5	100.636508	0.136508	246.934685	110
i5	100.636508	0.136508	311.126982	110
i5	100.909070	0.273016	184.997211	125
i5	100.909070	0.273016	246.934685	125
i5	100.909070	0.273016	311.126982	125
i5	101.318367	0.136508	164.804481	125
i5	101.318367	0.136508	219.999999	125
i5	101.318367	0.136508	277.166995	125
i5	101.590930	0.136735	164.804481	110
i5	101.590930	0.136735	219.999999	110
i5	101.590930	0.136735	277.166995	110
i5	101.863719	0.136508	164.804481	119
i5	101.863719	0.136508	219.999999	119
i5	101.863719	0.136508	277.166995	119
i5	102.000000	0.136735	164.804481	101
i5	102.000000	0.204762	219.999999	101
i5	102.000000	0.204762	277.166995	101
i5	102.136508	0.136508	164.804481	95
i5	102.272789	0.136508	164.804481	122
i5	102.272789	0.204762	219.999999	122
i5	102.272789	0.216100	277.166995	122
i5	102.409070	0.136735	164.804481	101
i5	102.545578	0.136508	246.934685	127
i5	102.545578	0.136508	164.804481	127
i5	102.545578	0.136508	207.646491	127
i5	102.818367	0.136508	246.934685	116
i5	102.818367	0.136508	207.646491	116
i5	102.818367	0.136508	164.804481	116
i5	103.090930	0.273016	246.934685	127
i5	103.090930	0.409297	164.804481	127
i5	103.090930	0.409297	207.646491	127
i5	103.363719	0.136508	246.934685	113
i5	103.500000	0.136735	164.804481	127
i5	103.500000	0.136735	219.999999	127
i5	103.500000	0.136735	277.166995	127
i5	103.772789	0.136508	164.804481	116
i5	103.772789	0.136508	219.999999	116
i5	103.772789	0.136508	277.166995	116
i5	104.045578	0.136508	164.804481	127
i5	104.045578	0.136508	219.999999	127
i5	104.181859	0.136735	164.804481	113
i5	104.045578	0.341043	277.166995	127
i5	104.181859	0.204762	219.999999	113
i5	104.318367	0.136508	164.804481	87
i5	104.454649	0.136508	164.804481	125
i5	104.454649	0.204762	219.999999	125
i5	104.454649	0.204762	277.166995	125
i5	104.590930	0.136735	164.804481	87
i5	104.727438	0.136508	184.997211	122
i5	104.727438	0.136508	246.934685	122
i5	104.727438	0.136508	311.126982	122
i5	105.000000	0.136735	184.997211	110
i5	105.000000	0.136735	246.934685	110
i5	105.000000	0.136735	311.126982	110
i5	105.272789	0.273016	184.997211	125
i5	105.272789	0.273016	246.934685	125
i5	105.272789	0.273016	311.126982	125
i5	105.681859	0.136735	164.804481	125
i5	105.681859	0.136735	219.999999	125
i5	105.681859	0.136735	277.166995	125
i5	105.954649	0.136508	164.804481	110
i5	105.954649	0.136508	219.999999	110
i5	105.954649	0.136508	277.166995	110
i5	106.227438	0.136508	164.804481	119
i5	106.227438	0.136508	219.999999	119
i5	106.227438	0.136508	277.166995	119
i5	106.363719	0.136508	164.804481	101
i5	106.363719	0.204762	219.999999	101
i5	106.363719	0.204762	277.166995	101
i5	106.500000	0.136735	164.804481	95
i5	106.636508	0.136508	164.804481	122
i5	106.636508	0.204762	219.999999	122
i5	106.636508	0.216100	277.166995	122
i5	106.772789	0.136508	164.804481	101
i5	106.909070	0.136735	246.934685	127
i5	106.909070	0.136735	164.804481	127
i5	106.909070	0.136735	207.646491	127
i5	107.181859	0.136735	246.934685	116
i5	107.181859	0.136735	207.646491	116
i5	107.181859	0.136735	164.804481	116
i5	107.454649	0.273016	246.934685	127
i5	107.454649	0.409297	164.804481	127
i5	107.454649	0.409297	207.646491	127
i5	107.727438	0.136508	246.934685	113
i5	107.863719	0.136508	164.804481	127
i5	107.863719	0.136508	219.999999	127
i5	107.863719	0.136508	277.166995	127
i5	108.136508	0.136508	164.804481	116
i5	108.136508	0.136508	219.999999	116
i5	108.136508	0.136508	277.166995	116
i5	108.409070	0.136735	164.804481	127
i5	108.409070	0.136735	219.999999	127
i5	108.545578	0.136508	164.804481	113
i5	108.409070	0.341270	277.166995	127
i5	108.545578	0.204762	219.999999	113
i5	108.681859	0.136735	164.804481	87
i5	108.818367	0.136508	164.804481	125
i5	108.818367	0.204762	219.999999	125
i5	108.818367	0.204762	277.166995	125
i5	108.954649	0.136508	164.804481	87
i5	109.090930	0.136735	184.997211	122
i5	109.090930	0.136735	246.934685	122
i5	109.090930	0.136735	311.126982	122
i5	109.363719	0.136508	184.997211	110
i5	109.363719	0.136508	246.934685	110
i5	109.363719	0.136508	311.126982	110
i5	109.636508	0.272789	184.997211	125
i5	109.636508	0.272789	246.934685	125
i5	109.636508	0.272789	311.126982	125
i5	110.045578	0.136508	164.804481	125
i5	110.045578	0.136508	219.999999	125
i5	110.045578	0.136508	277.166995	125
i5	110.318367	0.136508	164.804481	110
i5	110.318367	0.136508	219.999999	110
i5	110.318367	0.136508	277.166995	110
i5	110.590930	0.136735	164.804481	119
i5	110.590930	0.136735	219.999999	119
i5	110.590930	0.136735	277.166995	119
i5	110.727438	0.136508	164.804481	101
i5	110.727438	0.204762	219.999999	101
i5	110.727438	0.204762	277.166995	101
i5	110.863719	0.136508	164.804481	95
i5	111.000000	0.136735	164.804481	122
i5	111.000000	0.204762	219.999999	122
i5	111.000000	0.216327	277.166995	122
i5	111.136508	0.136508	164.804481	101
i5	111.272789	0.136735	207.646491	127
i5	111.272789	0.136735	155.563491	127
i5	111.272789	0.136735	246.934685	127
i5	111.545578	0.136508	207.646491	116
i5	111.545578	0.136508	155.563491	116
i5	111.545578	0.136508	246.934685	116
i5	111.818367	0.272789	246.934685	127
i5	111.818367	0.409297	207.646491	127
i5	111.818367	0.409297	155.563491	127
i5	112.090930	0.136735	246.934685	113
i5	112.227438	0.136508	207.646491	127
i5	112.227438	0.136508	246.934685	127
i5	112.227438	0.136508	155.563491	127
i5	112.500000	0.136735	207.646491	116
i5	112.500000	0.136735	155.563491	116
i5	112.500000	0.136735	246.934685	116
i5	112.772789	0.136735	207.646491	127
i5	112.772789	0.136735	155.563491	127
i5	112.909297	0.136508	155.563491	113
i5	112.772789	0.341270	246.934685	127
i5	112.909297	0.204762	207.646491	113
i5	113.045578	0.136508	155.563491	87
i5	113.181859	0.136735	155.563491	125
i5	113.181859	0.204762	207.646491	125
i5	113.181859	0.204762	246.934685	125
i5	113.318367	0.136508	155.563491	87
i5	113.454649	0.136508	164.804481	122
i5	113.454649	0.136508	219.999999	122
i5	113.454649	0.136508	277.166995	122
i5	113.727438	0.136508	164.804481	110
i5	113.727438	0.136508	219.999999	110
i5	113.727438	0.136508	277.166995	110
i5	114.000000	0.273016	164.804481	125
i5	114.000000	0.273016	219.999999	125
i5	114.000000	0.273016	277.166995	125
i5	114.409297	0.136508	164.804481	125
i5	114.409297	0.136508	219.999999	125
i5	114.409297	0.136508	277.166995	125
i5	114.681859	0.136735	164.804481	110
i5	114.681859	0.136735	219.999999	110
i5	114.681859	0.136735	277.166995	110
i5	114.954649	0.136508	164.804481	119
i5	114.954649	0.136508	219.999999	119
i5	114.954649	0.136508	277.166995	119
i5	115.090930	0.136735	164.804481	101
i5	115.090930	0.204762	219.999999	101
i5	115.090930	0.204762	277.166995	101
i5	115.227438	0.136508	164.804481	95
i5	115.363719	0.136508	164.804481	122
i5	115.363719	0.204762	219.999999	122
i5	115.363719	0.216100	277.166995	122
i5	115.500000	0.136735	164.804481	101
i5	115.636508	0.136508	207.646491	127
i5	115.636508	0.136508	155.563491	127
i5	115.636508	0.136508	246.934685	127
i5	115.909297	0.136508	207.646491	116
i5	115.909297	0.136508	155.563491	116
i5	115.909297	0.136508	246.934685	116
i5	116.181859	0.273016	246.934685	127
i5	116.181859	0.409297	207.646491	127
i5	116.181859	0.409297	155.563491	127
i5	116.454649	0.136508	246.934685	113
i5	116.590930	0.136735	207.646491	127
i5	116.590930	0.136735	246.934685	127
i5	116.590930	0.136735	155.563491	127
i5	116.863719	0.136508	207.646491	116
i5	116.863719	0.136508	155.563491	116
i5	116.863719	0.136508	246.934685	116
i5	117.136508	0.136508	207.646491	127
i5	117.136508	0.136508	155.563491	127
i5	117.272789	0.136735	155.563491	113
i5	117.136508	0.341043	246.934685	127
i5	117.272789	0.204762	207.646491	113
i5	117.409297	0.136508	155.563491	87
i5	117.545578	0.136508	155.563491	125
i5	117.545578	0.204762	207.646491	125
i5	117.545578	0.204762	246.934685	125
i5	117.681859	0.136735	155.563491	87
i5	117.818367	0.136508	164.804481	122
i5	117.818367	0.136508	219.999999	122
i5	117.818367	0.136508	277.166995	122
i5	118.090930	0.136735	164.804481	110
i5	118.090930	0.136735	219.999999	110
i5	118.090930	0.136735	277.166995	110
i5	118.363719	0.273016	164.804481	125
i5	118.363719	0.273016	219.999999	125
i5	118.363719	0.273016	277.166995	125
i5	118.772789	0.136735	164.804481	125
i5	118.772789	0.136735	219.999999	125
i5	118.772789	0.136735	277.166995	125
i5	119.045578	0.136508	164.804481	110
i5	119.045578	0.136508	219.999999	110
i5	119.045578	0.136508	277.166995	110
i5	119.318367	0.136508	164.804481	119
i5	119.318367	0.136508	219.999999	119
i5	119.318367	0.136508	277.166995	119
i5	119.454649	0.136508	164.804481	101
i5	119.454649	0.204762	219.999999	101
i5	119.454649	0.204762	277.166995	101
i5	119.590930	0.136735	164.804481	95
i5	119.727438	0.136508	164.804481	122
i5	119.727438	0.204762	219.999999	122
i5	119.727438	0.216100	277.166995	122
i5	119.863719	0.136508	164.804481	101
i5	120.000000	0.136735	207.646491	127
i5	120.000000	0.136735	155.563491	127
i5	120.000000	0.136735	246.934685	127
i5	120.272789	0.136735	207.646491	116
i5	120.272789	0.136735	155.563491	116
i5	120.272789	0.136735	246.934685	116
i5	120.545578	0.273016	246.934685	127
i5	120.545578	0.409297	207.646491	127
i5	120.545578	0.409297	155.563491	127
i5	120.818367	0.136508	246.934685	113
i5	120.954649	0.136508	207.646491	127
i5	120.954649	0.136508	246.934685	127
i5	120.954649	0.136508	155.563491	127
i5	121.227438	0.136508	207.646491	116
i5	121.227438	0.136508	155.563491	116
i5	121.227438	0.136508	246.934685	116
i5	121.500000	0.136735	207.646491	127
i5	121.500000	0.136735	155.563491	127
i5	121.636508	0.136508	155.563491	113
i5	121.500000	0.341270	246.934685	127
i5	121.636508	0.204762	207.646491	113
i5	121.772789	0.136735	155.563491	87
i5	121.909297	0.136508	155.563491	125
i5	121.909297	0.204762	207.646491	125
i5	121.909297	0.204762	246.934685	125
i5	122.045578	0.136508	155.563491	87
i5	122.181859	0.136735	164.804481	122
i5	122.181859	0.136735	219.999999	122
i5	122.181859	0.136735	277.166995	122
i5	122.454649	0.136508	164.804481	110
i5	122.454649	0.136508	219.999999	110
i5	122.454649	0.136508	277.166995	110
i5	122.727438	0.272789	164.804481	125
i5	122.727438	0.272789	219.999999	125
i5	122.727438	0.272789	277.166995	125
i5	123.136508	0.136508	164.804481	125
i5	123.136508	0.136508	219.999999	125
i5	123.136508	0.136508	277.166995	125
i5	123.409297	0.136508	164.804481	110
i5	123.409297	0.136508	219.999999	110
i5	123.409297	0.136508	277.166995	110
i5	123.681859	0.136735	164.804481	119
i5	123.681859	0.136735	219.999999	119
i5	123.681859	0.136735	277.166995	119
i5	123.818367	0.136508	164.804481	101
i5	123.818367	0.204762	219.999999	101
i5	123.818367	0.204762	277.166995	101
i5	123.954649	0.136508	164.804481	95
i5	124.090930	0.136735	164.804481	122
i5	124.090930	0.204762	219.999999	122
i5	124.090930	0.216327	277.166995	122
i5	124.227438	0.136508	164.804481	101
i5	124.363719	0.136508	246.934685	127
i5	124.363719	0.136508	164.804481	127
i5	124.363719	0.136508	207.646491	127
i5	124.636508	0.136508	246.934685	116
i5	124.636508	0.136508	207.646491	116
i5	124.636508	0.136508	164.804481	116
i5	124.909297	0.272789	246.934685	127
i5	124.909297	0.409297	164.804481	127
i5	124.909297	0.409297	207.646491	127
i5	125.181859	0.136735	246.934685	113
i5	125.318367	0.136508	184.997211	127
i5	125.318367	0.136508	219.999999	127
i5	125.318367	0.136508	277.166995	127
i5	125.590930	0.136735	184.997211	116
i5	125.590930	0.136735	219.999999	116
i5	125.590930	0.136735	277.166995	116
i5	125.863719	0.136508	184.997211	127
i5	125.863719	0.136508	219.999999	127
i5	126.000000	0.136735	184.997211	113
i5	125.863719	0.341270	277.166995	127
i5	126.000000	0.204989	219.999999	113
i5	126.136508	0.136508	184.997211	87
i5	126.272789	0.136735	184.997211	125
i5	126.272789	0.204762	219.999999	125
i5	126.272789	0.204762	277.166995	125
i5	126.409297	0.136508	184.997211	87
i5	126.545578	0.136508	207.646491	122
i5	126.545578	0.136508	246.934685	122
i5	126.545578	0.136508	311.126982	122
i5	126.818367	0.136508	207.646491	110
i5	126.818367	0.136508	246.934685	110
i5	126.818367	0.136508	311.126982	110
i5	127.090930	0.273016	207.646491	125
i5	127.090930	0.273016	246.934685	125
i5	127.090930	0.273016	311.126982	125
i5	127.500000	0.136735	184.997211	125
i5	127.500000	0.136735	219.999999	125
i5	127.500000	0.136735	277.166995	125
i5	127.772789	0.136735	184.997211	110
i5	127.772789	0.136735	219.999999	110
i5	127.772789	0.136735	277.166995	110
i5	128.045578	0.136508	184.997211	119
i5	128.045578	0.136508	219.999999	119
i5	128.045578	0.136508	277.166995	119
i5	128.181859	0.136735	184.997211	101
i5	128.181859	0.204762	219.999999	101
i5	128.181859	0.204762	277.166995	101
i5	128.318367	0.136508	184.997211	95
i5	128.454649	0.136508	184.997211	122
i5	128.454649	0.204762	219.999999	122
i5	128.454649	0.216100	277.166995	122
i5	128.590930	0.136735	184.997211	101
i5	128.727438	0.136508	246.934685	127
i5	128.727438	0.136508	164.804481	127
i5	128.727438	0.136508	207.646491	127
i5	129.000000	0.136735	246.934685	116
i5	129.000000	0.136735	207.646491	116
i5	129.000000	0.136735	164.804481	116
i5	129.272789	0.273016	246.934685	127
i5	129.272789	0.409297	164.804481	127
i5	129.272789	0.409297	207.646491	127
i5	129.545578	0.136508	246.934685	113
i5	129.681859	0.136735	184.997211	127
i5	129.681859	0.136735	219.999999	127
i5	129.681859	0.136735	277.166995	127
i5	129.954649	0.136508	184.997211	116
i5	129.954649	0.136508	219.999999	116
i5	129.954649	0.136508	277.166995	116
i5	130.227438	0.136508	184.997211	127
i5	130.227438	0.136508	219.999999	127
i5	130.363719	0.136508	184.997211	113
i5	130.227438	0.341043	277.166995	127
i5	130.363719	0.204762	219.999999	113
i5	130.500000	0.136735	184.997211	87
i5	130.636508	0.136508	184.997211	125
i5	130.636508	0.204762	219.999999	125
i5	130.636508	0.204762	277.166995	125
i5	130.772789	0.136735	184.997211	87
i5	130.909297	0.136508	207.646491	122
i5	130.909297	0.136508	246.934685	122
i5	130.909297	0.136508	311.126982	122
i5	131.181859	0.136735	207.646491	110
i5	131.181859	0.136735	246.934685	110
i5	131.181859	0.136735	311.126982	110
i5	131.454649	0.273016	207.646491	125
i5	131.454649	0.273016	246.934685	125
i5	131.454649	0.273016	311.126982	125
i5	131.863719	0.136508	184.997211	125
i5	131.863719	0.136508	219.999999	125
i5	131.863719	0.136508	277.166995	125
i5	132.136508	0.136508	184.997211	110
i5	132.136508	0.136508	219.999999	110
i5	132.136508	0.136508	277.166995	110
i5	132.409297	0.136508	184.997211	119
i5	132.409297	0.136508	219.999999	119
i5	132.409297	0.136508	277.166995	119
i5	132.545578	0.136508	184.997211	101
i5	132.545578	0.204762	219.999999	101
i5	132.545578	0.204762	277.166995	101
i5	132.681859	0.136735	184.997211	95
i5	132.818367	0.136508	184.997211	122
i5	132.818367	0.204762	219.999999	122
i5	132.818367	0.216100	277.166995	122
i5	132.954649	0.136508	184.997211	101
i5	133.090930	0.136735	246.934685	127
i5	133.090930	0.136735	164.804481	127
i5	133.090930	0.136735	207.646491	127
i5	133.363719	0.136508	246.934685	116
i5	133.363719	0.136508	207.646491	116
i5	133.363719	0.136508	164.804481	116
i5	133.636508	0.273016	246.934685	127
i5	133.636508	0.409297	164.804481	127
i5	133.636508	0.409297	207.646491	127
i5	133.909297	0.136508	246.934685	113
i5	134.045578	0.136508	164.804481	127
i5	134.045578	0.136508	219.999999	127
i5	134.045578	0.136508	277.166995	127
i5	134.318367	0.136508	164.804481	116
i5	134.318367	0.136508	219.999999	116
i5	134.318367	0.136508	277.166995	116
i5	134.590930	0.136735	164.804481	127
i5	134.590930	0.136735	219.999999	127
i5	134.727438	0.136508	164.804481	113
i5	134.590930	0.341270	277.166995	127
i5	134.727438	0.204762	219.999999	113
i5	134.863719	0.136508	164.804481	87
i5	135.000000	0.136735	164.804481	125
i5	135.000000	0.204989	219.999999	125
i5	135.000000	0.204989	277.166995	125
i5	135.136508	0.136508	164.804481	87
i5	135.272789	0.136735	184.997211	122
i5	135.272789	0.136735	246.934685	122
i5	135.272789	0.136735	311.126982	122
i5	135.545578	0.136508	184.997211	110
i5	135.545578	0.136508	246.934685	110
i5	135.545578	0.136508	311.126982	110
i5	135.818367	0.272789	184.997211	125
i5	135.818367	0.272789	246.934685	125
i5	135.818367	0.272789	311.126982	125
i5	136.227438	0.136508	164.804481	125
i5	136.227438	0.136508	219.999999	125
i5	136.227438	0.136508	277.166995	125
i5	136.500227	0.136508	164.804481	110
i5	136.500227	0.136508	219.999999	110
i5	136.500227	0.136508	277.166995	110
i5	136.772789	0.136735	164.804481	119
i5	136.772789	0.136735	219.999999	119
i5	136.772789	0.136735	277.166995	119
i5	136.909297	0.136508	164.804481	101
i5	136.909297	0.204762	219.999999	101
i5	136.909297	0.204762	277.166995	101
i5	137.045578	0.136508	164.804481	95
i5	137.181859	0.136735	164.804481	122
i5	137.181859	0.204762	219.999999	122
i5	137.181859	0.216100	277.166995	122
i5	137.318367	0.136508	164.804481	101
i5	137.454649	0.136508	246.934685	127
i5	137.454649	0.136508	164.804481	127
i5	137.454649	0.136508	207.646491	127
i5	137.727438	0.136508	246.934685	116
i5	137.727438	0.136508	207.646491	116
i5	137.727438	0.136508	164.804481	116
i5	138.000227	0.272789	246.934685	127
i5	138.000227	0.409297	164.804481	127
i5	138.000227	0.409297	207.646491	127
i5	138.272789	0.136735	246.934685	113
i5	138.409297	0.136508	164.804481	127
i5	138.409297	0.136508	219.999999	127
i5	138.409297	0.136508	277.166995	127
i5	138.681859	0.136735	164.804481	116
i5	138.681859	0.136735	219.999999	116
i5	138.681859	0.136735	277.166995	116
i5	138.954649	0.136508	164.804481	127
i5	138.954649	0.136508	219.999999	127
i5	139.090930	0.136735	164.804481	113
i5	138.954649	0.341043	277.166995	127
i5	139.090930	0.204762	219.999999	113
i5	139.227438	0.136508	164.804481	87
i5	139.363719	0.136735	164.804481	125
i5	139.363719	0.204762	219.999999	125
i5	139.363719	0.204762	277.166995	125
i5	139.500227	0.136508	164.804481	87
i5	139.636508	0.136508	184.997211	122
i5	139.636508	0.136508	246.934685	122
i5	139.636508	0.136508	311.126982	122
i5	139.909297	0.136508	184.997211	110
i5	139.909297	0.136508	246.934685	110
i5	139.909297	0.136508	311.126982	110
i5	140.181859	0.273016	184.997211	125
i5	140.181859	0.273016	246.934685	125
i5	140.181859	0.273016	311.126982	125
i5	140.590930	0.136735	164.804481	125
i5	140.590930	0.136735	219.999999	125
i5	140.590930	0.136735	277.166995	125
i5	140.863719	0.136735	164.804481	110
i5	140.863719	0.136735	219.999999	110
i5	140.863719	0.136735	277.166995	110
i5	141.136508	0.136508	164.804481	119
i5	141.136508	0.136508	219.999999	119
i5	141.136508	0.136508	277.166995	119
i5	141.272789	0.136735	164.804481	101
i5	141.272789	0.204762	219.999999	101
i5	141.272789	0.204762	277.166995	101
i5	141.409297	0.136508	164.804481	95
i5	141.545578	0.136508	164.804481	122
i5	141.545578	0.204762	219.999999	122
i5	141.545578	0.216100	277.166995	122
i5	141.681859	0.136735	164.804481	101
i5	141.818367	0.136508	246.934685	127
i5	141.818367	0.136508	164.804481	127
i5	141.818367	0.136508	207.646491	127
i5	142.090930	0.136735	246.934685	116
i5	142.090930	0.136735	207.646491	116
i5	142.090930	0.136735	164.804481	116
i5	142.363719	0.273016	246.934685	127
i5	142.363719	0.409297	164.804481	127
i5	142.363719	0.409297	207.646491	127
i5	142.636508	0.136508	246.934685	113
i5	142.772789	0.136735	164.804481	127
i5	142.772789	0.136735	219.999999	127
i5	142.772789	0.136735	277.166995	127
i5	143.045578	0.136508	164.804481	116
i5	143.045578	0.136508	219.999999	116
i5	143.045578	0.136508	277.166995	116
i5	143.318367	0.136508	164.804481	127
i5	143.318367	0.136508	219.999999	127
i5	143.454649	0.136508	164.804481	113
i5	143.318367	0.341043	277.166995	127
i5	143.454649	0.204762	219.999999	113
i5	143.590930	0.136735	164.804481	87
i5	143.727438	0.136508	164.804481	125
i5	143.727438	0.204762	219.999999	125
i5	143.727438	0.204762	277.166995	125
i5	143.863719	0.136735	164.804481	87
i5	144.000227	0.136508	184.997211	122
i5	144.000227	0.136508	246.934685	122
i5	144.000227	0.136508	311.126982	122
i5	144.272789	0.136735	184.997211	110
i5	144.272789	0.136735	246.934685	110
i5	144.272789	0.136735	311.126982	110
i5	144.545578	0.273016	184.997211	125
i5	144.545578	0.273016	246.934685	125
i5	144.545578	0.273016	311.126982	125
i5	144.954649	0.136508	164.804481	125
i5	144.954649	0.136508	219.999999	125
i5	144.954649	0.136508	277.166995	125
i5	145.227438	0.136508	164.804481	110
i5	145.227438	0.136508	219.999999	110
i5	145.227438	0.136508	277.166995	110
i5	145.500227	0.136508	164.804481	119
i5	145.500227	0.136508	219.999999	119
i5	145.500227	0.136508	277.166995	119
i5	145.636508	0.136508	164.804481	101
i5	145.636508	0.204762	219.999999	101
i5	145.636508	0.204762	277.166995	101
i5	145.772789	0.136735	164.804481	95
i5	145.909297	0.136508	164.804481	122
i5	145.909297	0.204762	219.999999	122
i5	145.909297	0.216100	277.166995	122
i5	146.045578	0.136508	164.804481	101
i5	146.181859	0.136735	246.934685	127
i5	146.181859	0.136735	164.804481	127
i5	146.181859	0.136735	207.646491	127
i5	146.454649	0.136508	246.934685	116
i5	146.454649	0.136508	207.646491	116
i5	146.454649	0.136508	164.804481	116
i5	146.727438	0.273016	246.934685	127
i5	146.727438	0.409297	164.804481	127
i5	146.727438	0.409297	207.646491	127
i5	147.000227	0.136508	246.934685	113
i5	147.136508	0.136508	164.804481	127
i5	147.136508	0.136508	219.999999	127
i5	147.136508	0.136508	277.166995	127
i5	147.409297	0.136508	164.804481	116
i5	147.409297	0.136508	219.999999	116
i5	147.409297	0.136508	277.166995	116
i5	147.681859	0.136735	164.804481	127
i5	147.681859	0.136735	219.999999	127
i5	147.818367	0.136508	164.804481	113
i5	147.681859	0.341270	277.166995	127
i5	147.818367	0.204762	219.999999	113
i5	147.954649	0.136508	164.804481	87
i5	148.090930	0.136735	164.804481	125
i5	148.090930	0.204762	219.999999	125
i5	148.090930	0.204762	277.166995	125
i5	148.227438	0.136508	164.804481	87
i5	148.363719	0.136735	184.997211	122
i5	148.363719	0.136735	246.934685	122
i5	148.363719	0.136735	311.126982	122
i5	148.636508	0.136508	184.997211	110
i5	148.636508	0.136508	246.934685	110
i5	148.636508	0.136508	311.126982	110
i5	148.909297	0.272789	184.997211	125
i5	148.909297	0.272789	246.934685	125
i5	148.909297	0.272789	311.126982	125
i5	149.318367	0.136508	164.804481	125
i5	149.318367	0.136508	219.999999	125
i5	149.318367	0.136508	277.166995	125
i5	149.590930	0.136735	164.804481	110
i5	149.590930	0.136735	219.999999	110
i5	149.590930	0.136735	277.166995	110
i5	149.863719	0.136735	164.804481	119
i5	149.863719	0.136735	219.999999	119
i5	149.863719	0.136735	277.166995	119
i5	150.000227	0.136508	164.804481	101
i5	150.000227	0.204762	219.999999	101
i5	150.000227	0.204762	277.166995	101
i5	150.136508	0.136508	164.804481	95
i5	150.272789	0.136735	164.804481	122
i5	150.272789	0.204762	219.999999	122
i5	150.272789	0.216100	277.166995	122
i5	150.409297	0.136508	164.804481	101
i5	150.545578	0.136508	246.934685	127
i5	150.545578	0.136508	164.804481	127
i5	150.545578	0.136508	207.646491	127
i5	150.818367	0.136508	246.934685	116
i5	150.818367	0.136508	207.646491	116
i5	150.818367	0.136508	164.804481	116
i5	151.090930	0.273016	246.934685	127
i5	151.090930	0.409524	164.804481	127
i5	151.090930	0.409524	207.646491	127
i5	151.363719	0.136735	246.934685	113
i5	151.500227	0.136508	164.804481	127
i5	151.500227	0.136508	219.999999	127
i5	151.500227	0.136508	277.166995	127
i5	151.772789	0.136735	164.804481	116
i5	151.772789	0.136735	219.999999	116
i5	151.772789	0.136735	277.166995	116
i5	152.045578	0.136508	164.804481	127
i5	152.045578	0.136508	219.999999	127
i5	152.181859	0.136735	164.804481	113
i5	152.045578	0.341043	277.166995	127
i5	152.181859	0.204762	219.999999	113
i5	152.318367	0.136508	164.804481	87
i5	152.454649	0.136508	164.804481	125
i5	152.454649	0.204762	219.999999	125
i5	152.454649	0.204762	277.166995	125
i5	152.590930	0.136735	164.804481	87
i5	152.727438	0.136508	184.997211	122
i5	152.727438	0.136508	246.934685	122
i5	152.727438	0.136508	311.126982	122
i5	153.000227	0.136508	184.997211	110
i5	153.000227	0.136508	246.934685	110
i5	153.000227	0.136508	311.126982	110
i5	153.272789	0.273016	184.997211	125
i5	153.272789	0.273016	246.934685	125
i5	153.272789	0.273016	311.126982	125
i5	153.681859	0.136735	164.804481	125
i5	153.681859	0.136735	219.999999	125
i5	153.681859	0.136735	277.166995	125
i5	153.954649	0.136508	164.804481	110
i5	153.954649	0.136508	219.999999	110
i5	153.954649	0.136508	277.166995	110
i5	154.227438	0.136508	164.804481	119
i5	154.227438	0.136508	219.999999	119
i5	154.227438	0.136508	277.166995	119
i5	154.363719	0.136735	164.804481	101
i5	154.363719	0.204762	219.999999	101
i5	154.363719	0.204762	277.166995	101
i5	154.500227	0.136508	164.804481	95
i5	154.636508	0.136508	164.804481	122
i5	154.636508	0.204762	219.999999	122
i5	154.636508	0.216100	277.166995	122
i5	154.772789	0.136735	164.804481	101
i5	154.909297	0.136508	246.934685	127
i5	154.909297	0.136508	164.804481	127
i5	154.909297	0.136508	207.646491	127
i5	155.181859	0.136735	246.934685	116
i5	155.181859	0.136735	207.646491	116
i5	155.181859	0.136735	164.804481	116
i5	155.454649	0.273016	246.934685	127
i5	155.454649	0.409297	164.804481	127
i5	155.454649	0.409297	207.646491	127
i5	155.727438	0.136508	246.934685	113
i5	155.863719	0.136735	164.804481	127
i5	155.863719	0.136735	219.999999	127
i5	155.863719	0.136735	277.166995	127
i5	156.136508	0.136508	164.804481	116
i5	156.136508	0.136508	219.999999	116
i5	156.136508	0.136508	277.166995	116
i5	156.409297	0.136508	164.804481	127
i5	156.409297	0.136508	219.999999	127
i5	156.545578	0.136508	164.804481	113
i5	156.409297	0.341043	277.166995	127
i5	156.545578	0.204762	219.999999	113
i5	156.681859	0.136735	164.804481	87
i5	156.818367	0.136508	164.804481	125
i5	156.818367	0.204762	219.999999	125
i5	156.818367	0.204762	277.166995	125
i5	156.954649	0.136508	164.804481	87
i5	157.090930	0.136735	184.997211	122
i5	157.090930	0.136735	246.934685	122
i5	157.090930	0.136735	311.126982	122
i5	157.363719	0.136735	184.997211	110
i5	157.363719	0.136735	246.934685	110
i5	157.363719	0.136735	311.126982	110
i5	157.636508	0.273016	184.997211	125
i5	157.636508	0.273016	246.934685	125
i5	157.636508	0.273016	311.126982	125
i5	158.045578	0.136508	164.804481	125
i5	158.045578	0.136508	219.999999	125
i5	158.045578	0.136508	277.166995	125
i5	158.318367	0.136508	164.804481	110
i5	158.318367	0.136508	219.999999	110
i5	158.318367	0.136508	277.166995	110
i5	158.590930	0.136735	164.804481	119
i5	158.590930	0.136735	219.999999	119
i5	158.590930	0.136735	277.166995	119
i5	158.727438	0.136508	164.804481	101
i5	158.727438	0.204762	219.999999	101
i5	158.727438	0.204762	277.166995	101
i5	158.863719	0.136735	164.804481	95
i5	159.000227	0.136508	164.804481	122
i5	159.000227	0.204762	219.999999	122
i5	159.000227	0.216100	277.166995	122
i5	159.136508	0.136508	164.804481	101
i5	159.272789	0.136735	246.934685	127
i5	159.272789	0.136735	164.804481	127
i5	159.272789	0.136735	207.646491	127
i5	159.545578	0.136508	246.934685	116
i5	159.545578	0.136508	207.646491	116
i5	159.545578	0.136508	164.804481	116
i5	159.818367	0.272789	246.934685	127
i5	159.818367	0.409297	164.804481	127
i5	159.818367	0.409297	207.646491	127
i5	160.090930	0.136735	246.934685	113
i5	160.227438	0.136508	164.804481	127
i5	160.227438	0.136508	219.999999	127
i5	160.227438	0.136508	277.166995	127
i5	160.500227	0.136508	164.804481	116
i5	160.500227	0.136508	219.999999	116
i5	160.500227	0.136508	277.166995	116
i5	160.772789	0.136735	164.804481	127
i5	160.772789	0.136735	219.999999	127
i5	160.909297	0.136508	164.804481	113
i5	160.772789	0.341270	277.166995	127
i5	160.909297	0.204762	219.999999	113
i5	161.045578	0.136508	164.804481	87
i5	161.181859	0.136735	164.804481	125
i5	161.181859	0.204762	219.999999	125
i5	161.181859	0.204762	277.166995	125
i5	161.318367	0.136508	164.804481	87
i5	161.454649	0.136735	184.997211	122
i5	161.454649	0.136735	246.934685	122
i5	161.454649	0.136735	311.126982	122
i5	161.727438	0.136508	184.997211	110
i5	161.727438	0.136508	246.934685	110
i5	161.727438	0.136508	311.126982	110
i5	162.000227	0.272789	184.997211	125
i5	162.000227	0.272789	246.934685	125
i5	162.000227	0.272789	311.126982	125
i5	162.409297	0.136508	164.804481	125
i5	162.409297	0.136508	219.999999	125
i5	162.409297	0.136508	277.166995	125
i5	162.681859	0.136735	164.804481	110
i5	162.681859	0.136735	219.999999	110
i5	162.681859	0.136735	277.166995	110
i5	162.954649	0.136735	164.804481	119
i5	162.954649	0.136735	219.999999	119
i5	162.954649	0.136735	277.166995	119
i5	163.091156	0.136508	164.804481	101
i5	163.091156	0.204762	219.999999	101
i5	163.091156	0.204762	277.166995	101
i5	163.227438	0.136508	164.804481	95
i5	163.363719	0.136735	164.804481	122
i5	163.363719	0.204762	219.999999	122
i5	163.363719	0.216100	277.166995	122
i5	163.500227	0.136508	164.804481	101
i5	163.636508	0.136508	246.934685	127
i5	163.636508	0.136508	164.804481	127
i5	163.636508	0.136508	207.646491	127
i5	163.909297	0.136508	246.934685	116
i5	163.909297	0.136508	207.646491	116
i5	163.909297	0.136508	164.804481	116
i5	164.181859	0.273016	246.934685	127
i5	164.181859	0.409524	164.804481	127
i5	164.181859	0.409524	207.646491	127
i5	164.454649	0.136735	246.934685	113
i5	164.591156	0.136508	164.804481	127
i5	164.591156	0.136508	219.999999	127
i5	164.591156	0.136508	277.166995	127
i5	164.863719	0.136735	164.804481	116
i5	164.863719	0.136735	219.999999	116
i5	164.863719	0.136735	277.166995	116
i5	165.136508	0.136508	164.804481	127
i5	165.136508	0.136508	219.999999	127
i5	165.272789	0.136735	164.804481	113
i5	165.136508	0.341043	277.166995	127
i5	165.272789	0.204762	219.999999	113
i5	165.409297	0.136508	164.804481	87
i5	165.545578	0.136508	164.804481	125
i5	165.545578	0.204762	219.999999	125
i5	165.545578	0.204762	277.166995	125
i5	165.681859	0.136735	164.804481	87
i5	165.818367	0.136508	184.997211	122
i5	165.818367	0.136508	246.934685	122
i5	165.818367	0.136508	311.126982	122
i5	166.091156	0.136508	184.997211	110
i5	166.091156	0.136508	246.934685	110
i5	166.091156	0.136508	311.126982	110
i5	166.363719	0.273016	184.997211	125
i5	166.363719	0.273016	246.934685	125
i5	166.363719	0.273016	311.126982	125
i5	166.772789	0.136735	164.804481	125
i5	166.772789	0.136735	219.999999	125
i5	166.772789	0.136735	277.166995	125
i5	167.045578	0.136508	164.804481	110
i5	167.045578	0.136508	219.999999	110
i5	167.045578	0.136508	277.166995	110
i5	167.318367	0.136508	164.804481	119
i5	167.318367	0.136508	219.999999	119
i5	167.318367	0.136508	277.166995	119
i5	167.454649	0.136735	164.804481	101
i5	167.454649	0.204762	219.999999	101
i5	167.454649	0.204762	277.166995	101
i5	167.591156	0.136508	164.804481	95
i5	167.727438	0.136508	164.804481	122
i5	167.727438	0.204762	219.999999	122
i5	167.727438	0.216100	277.166995	122
i5	167.863719	0.136735	164.804481	101
i5	168.000227	0.136508	207.646491	127
i5	168.000227	0.136508	155.563491	127
i5	168.000227	0.136508	246.934685	127
i5	168.272789	0.136735	207.646491	116
i5	168.272789	0.136735	155.563491	116
i5	168.272789	0.136735	246.934685	116
i5	168.545578	0.273016	246.934685	127
i5	168.545578	0.409297	207.646491	127
i5	168.545578	0.409297	155.563491	127
i5	168.818367	0.136508	246.934685	113
i5	168.954649	0.136735	207.646491	127
i5	168.954649	0.136735	246.934685	127
i5	168.954649	0.136735	155.563491	127
i5	169.227438	0.136508	207.646491	116
i5	169.227438	0.136508	155.563491	116
i5	169.227438	0.136508	246.934685	116
i5	169.500227	0.136508	207.646491	127
i5	169.500227	0.136508	155.563491	127
i5	169.636508	0.136508	155.563491	113
i5	169.500227	0.341043	246.934685	127
i5	169.636508	0.204762	207.646491	113
i5	169.772789	0.136735	155.563491	87
i5	169.909297	0.136508	155.563491	125
i5	169.909297	0.204762	207.646491	125
i5	169.909297	0.204762	246.934685	125
i5	170.045578	0.136508	155.563491	87
i5	170.181859	0.136735	164.804481	122
i5	170.181859	0.136735	219.999999	122
i5	170.181859	0.136735	277.166995	122
i5	170.454649	0.136735	164.804481	110
i5	170.454649	0.136735	219.999999	110
i5	170.454649	0.136735	277.166995	110
i5	170.727438	0.273016	164.804481	125
i5	170.727438	0.273016	219.999999	125
i5	170.727438	0.273016	277.166995	125
i5	171.136508	0.136508	164.804481	125
i5	171.136508	0.136508	219.999999	125
i5	171.136508	0.136508	277.166995	125
i5	171.409297	0.136508	164.804481	110
i5	171.409297	0.136508	219.999999	110
i5	171.409297	0.136508	277.166995	110
i5	171.681859	0.136735	164.804481	119
i5	171.681859	0.136735	219.999999	119
i5	171.681859	0.136735	277.166995	119
i5	171.818367	0.136508	164.804481	101
i5	171.818367	0.204762	219.999999	101
i5	171.818367	0.204762	277.166995	101
i5	171.954649	0.136735	164.804481	95
i5	172.091156	0.136508	164.804481	122
i5	172.091156	0.204762	219.999999	122
i5	172.091156	0.216100	277.166995	122
i5	172.227438	0.136508	164.804481	101
i5	172.363719	0.136735	207.646491	127
i5	172.363719	0.136735	155.563491	127
i5	172.363719	0.136735	246.934685	127
i5	172.636508	0.136508	207.646491	116
i5	172.636508	0.136508	155.563491	116
i5	172.636508	0.136508	246.934685	116
i5	172.909297	0.272789	246.934685	127
i5	172.909297	0.409297	207.646491	127
i5	172.909297	0.409297	155.563491	127
i5	173.181859	0.136735	246.934685	113
i5	173.318367	0.136508	207.646491	127
i5	173.318367	0.136508	246.934685	127
i5	173.318367	0.136508	155.563491	127
i5	173.591156	0.136508	207.646491	116
i5	173.591156	0.136508	155.563491	116
i5	173.591156	0.136508	246.934685	116
i5	173.863719	0.136735	207.646491	127
i5	173.863719	0.136735	155.563491	127
i5	174.000227	0.136508	155.563491	113
i5	173.863719	0.341270	246.934685	127
i5	174.000227	0.204762	207.646491	113
i5	174.136508	0.136508	155.563491	87
i5	174.272789	0.136735	155.563491	125
i5	174.272789	0.204762	207.646491	125
i5	174.272789	0.204762	246.934685	125
i5	174.409297	0.136508	155.563491	87
i5	174.545578	0.136508	164.804481	122
i5	174.545578	0.136508	219.999999	122
i5	174.545578	0.136508	277.166995	122
i5	174.818367	0.136508	164.804481	110
i5	174.818367	0.136508	219.999999	110
i5	174.818367	0.136508	277.166995	110
i5	175.091156	0.272789	164.804481	125
i5	175.091156	0.272789	219.999999	125
i5	175.091156	0.272789	277.166995	125
i5	175.500227	0.136508	164.804481	125
i5	175.500227	0.136508	219.999999	125
i5	175.500227	0.136508	277.166995	125
i5	175.772789	0.136735	164.804481	110
i5	175.772789	0.136735	219.999999	110
i5	175.772789	0.136735	277.166995	110
i5	176.045578	0.136508	164.804481	119
i5	176.045578	0.136508	219.999999	119
i5	176.045578	0.136508	277.166995	119
i5	176.181859	0.136735	164.804481	101
i5	176.181859	0.204989	219.999999	101
i5	176.181859	0.204989	277.166995	101
i5	176.318367	0.136508	164.804481	95
i5	176.454649	0.136735	164.804481	122
i5	176.454649	0.204762	219.999999	122
i5	176.454649	0.216100	277.166995	122
i5	176.591156	0.136508	164.804481	101
i5	176.727438	0.136508	207.646491	127
i5	176.727438	0.136508	155.563491	127
i5	176.727438	0.136508	246.934685	127
i5	177.000227	0.136508	207.646491	116
i5	177.000227	0.136508	155.563491	116
i5	177.000227	0.136508	246.934685	116
i5	177.272789	0.273016	246.934685	127
i5	177.272789	0.409297	207.646491	127
i5	177.272789	0.409297	155.563491	127
i5	177.545578	0.136508	246.934685	113
i5	177.681859	0.136735	207.646491	127
i5	177.681859	0.136735	246.934685	127
i5	177.681859	0.136735	155.563491	127
i5	177.954649	0.136735	207.646491	116
i5	177.954649	0.136735	155.563491	116
i5	177.954649	0.136735	246.934685	116
i5	178.227438	0.136508	207.646491	127
i5	178.227438	0.136508	155.563491	127
i5	178.363719	0.136735	155.563491	113
i5	178.227438	0.341043	246.934685	127
i5	178.363719	0.204762	207.646491	113
i5	178.500227	0.136508	155.563491	87
i5	178.636508	0.136508	155.563491	125
i5	178.636508	0.204762	207.646491	125
i5	178.636508	0.204762	246.934685	125
i5	178.772789	0.136735	155.563491	87
i5	178.909297	0.136508	164.804481	122
i5	178.909297	0.136508	219.999999	122
i5	178.909297	0.136508	277.166995	122
i5	179.181859	0.136735	164.804481	110
i5	179.181859	0.136735	219.999999	110
i5	179.181859	0.136735	277.166995	110
i5	179.454649	0.273016	164.804481	125
i5	179.454649	0.273016	219.999999	125
i5	179.454649	0.273016	277.166995	125
i5	179.863719	0.136735	164.804481	125
i5	179.863719	0.136735	219.999999	125
i5	179.863719	0.136735	277.166995	125
i5	180.136508	0.136508	164.804481	110
i5	180.136508	0.136508	219.999999	110
i5	180.136508	0.136508	277.166995	110
i5	180.409297	0.136508	164.804481	119
i5	180.409297	0.136508	219.999999	119
i5	180.409297	0.136508	277.166995	119
i5	180.545578	0.136508	164.804481	101
i5	180.545578	0.204762	219.999999	101
i5	180.545578	0.204762	277.166995	101
i5	180.681859	0.136735	164.804481	95
i5	180.818367	0.136508	164.804481	122
i5	180.818367	0.204762	219.999999	122
i5	180.818367	0.216100	277.166995	122
i5	180.954649	0.136735	164.804481	101
i5	181.091156	0.136508	246.934685	127
i5	181.091156	0.136508	164.804481	127
i5	181.091156	0.136508	207.646491	127
i5	181.363719	0.136735	246.934685	116
i5	181.363719	0.136735	207.646491	116
i5	181.363719	0.136735	164.804481	116
i5	181.636508	0.273016	246.934685	127
i5	181.636508	0.409297	164.804481	127
i5	181.636508	0.409297	207.646491	127
i5	181.909297	0.136508	246.934685	113
i5	182.045578	0.136508	184.997211	127
i5	182.045578	0.136508	219.999999	127
i5	182.045578	0.136508	277.166995	127
i5	182.318367	0.136508	184.997211	116
i5	182.318367	0.136508	219.999999	116
i5	182.318367	0.136508	277.166995	116
i5	182.591156	0.136508	184.997211	127
i5	182.591156	0.136508	219.999999	127
i5	182.727438	0.136508	184.997211	113
i5	182.591156	0.341043	277.166995	127
i5	182.727438	0.204762	219.999999	113
i5	182.863719	0.136735	184.997211	87
i5	183.000227	0.136508	184.997211	125
i5	183.000227	0.204762	219.999999	125
i5	183.000227	0.204762	277.166995	125
i5	183.136508	0.136508	184.997211	87
i5	183.272789	0.136735	207.646491	122
i5	183.272789	0.136735	246.934685	122
i5	183.272789	0.136735	311.126982	122
i5	183.545578	0.136508	207.646491	110
i5	183.545578	0.136508	246.934685	110
i5	183.545578	0.136508	311.126982	110
i5	183.818367	0.273016	207.646491	125
i5	183.818367	0.273016	246.934685	125
i5	183.818367	0.273016	311.126982	125
i5	184.227438	0.136508	184.997211	125
i5	184.227438	0.136508	219.999999	125
i5	184.227438	0.136508	277.166995	125
i5	184.500227	0.136508	184.997211	110
i5	184.500227	0.136508	219.999999	110
i5	184.500227	0.136508	277.166995	110
i5	184.772789	0.136735	184.997211	119
i5	184.772789	0.136735	219.999999	119
i5	184.772789	0.136735	277.166995	119
i5	184.909297	0.136508	184.997211	101
i5	184.909297	0.204762	219.999999	101
i5	184.909297	0.204762	277.166995	101
i5	185.0   	0.1     	185.0   	95
i5	185.2   	0.1     	185.0	    122
i5	185.2	    0.2	       219.999999	122
i5	185.2      0.2 	    277.166995	122
i5	185.3    	0.14	184.997211	101
i5	185.454649	0.14	246.934685	127
i5	185.454649	0.14	164.804481	127
i5	185.454649	0.14	207.646491	127
i5	185.727438	0.14	246.934685	116
i5	185.727438	0.14	207.646491	116
i5	185.727438	0.14	164.804481	116
i5	186.000227	0.272789	246.934685	127
i5	186.000227	0.409297	164.804481	127
i5	186.000227	0.409297	207.646491	127
i5	186.272789	0.136735	246.934685	113
i5	186.409297	0.136508	184.997211	127
i5	186.409297	0.136508	219.999999	127
i5	186.409297	0.136508	277.166995	127
i5	186.682086	0.136508	184.997211	116
i5	186.682086	0.136508	219.999999	116
i5	186.682086	0.136508	277.166995	116
i5	186.954649	0.136735	184.997211	127
i5	186.954649	0.136735	219.999999	127
i5	187.091156	0.136508	184.997211	113
i5	186.954649	0.341270	277.166995	127
i5	187.091156	0.204762	219.999999	113
i5	187.227438	0.136508	184.997211	87
i5	187.363719	0.136735	184.997211	125
i5	187.363719	0.204762	219.999999	125
i5	187.363719	0.204762	277.166995	125
i5	187.500227	0.136508	184.997211	87
i5	187.636508	0.136508	207.646491	122
i5	187.636508	0.136508	246.934685	122
i5	187.636508	0.136508	311.126982	122
i5	187.909297	0.136508	207.646491	110
i5	187.909297	0.136508	246.934685	110
i5	187.909297	0.136508	311.126982	110
i5	188.182086	0.272789	207.646491	125
i5	188.182086	0.272789	246.934685	125
i5	188.182086	0.272789	311.126982	125
i5	188.591156	0.136508	184.997211	125
i5	188.591156	0.136508	219.999999	125
i5	188.591156	0.136508	277.166995	125
i5	188.863719	0.136735	184.997211	110
i5	188.863719	0.136735	219.999999	110
i5	188.863719	0.136735	277.166995	110
i5	189.136508	0.136508	184.997211	119
i5	189.136508	0.136508	219.999999	119
i5	189.136508	0.136508	277.166995	119
i5	189.272789	0.136735	184.997211	101
i5	189.272789	0.204762	219.999999	101
i5	189.272789	0.204762	277.166995	101
i5	189.409297	0.136508	184.997211	95
i5	189.545578	0.136735	184.997211	122
i5	189.545578	0.204762	219.999999	122
i5	189.545578	0.216100	277.166995	122
i5	189.682086	0.136508	184.997211	101
i5	189.818367	0.136508	246.934685	127
i5	189.818367	0.136508	164.804481	127
i5	189.818367	0.136508	207.646491	127
i5	190.091156	0.136508	246.934685	116
i5	190.091156	0.136508	207.646491	116
i5	190.091156	0.136508	164.804481	116
i5	190.363719	0.273016	246.934685	127
i5	190.363719	0.409297	164.804481	127
i5	190.363719	0.409297	207.646491	127
i5	190.636508	0.136508	246.934685	113
i5	190.772789	0.136735	184.997211	127
i5	190.772789	0.136735	219.999999	127
i5	190.772789	0.136735	277.166995	127
i5	191.045578	0.136735	184.997211	116
i5	191.045578	0.136735	219.999999	116
i5	191.045578	0.136735	277.166995	116
i5	191.318367	0.136508	184.997211	127
i5	191.318367	0.136508	219.999999	127
i5	191.454649	0.136735	184.997211	113
i5	191.318367	0.341043	277.166995	127
i5	191.454649	0.204762	219.999999	113
i5	191.591156	0.136508	184.997211	87
i5	191.727438	0.136508	184.997211	125
i5	191.727438	0.204762	219.999999	125
i5	191.727438	0.204762	277.166995	125
i5	191.863719	0.136735	184.997211	87
i5	192.000227	0.136508	207.646491	122
i5	192.000227	0.136508	246.934685	122
i5	192.000227	0.136508	311.126982	122
i5	192.272789	0.136735	207.646491	110
i5	192.272789	0.136735	246.934685	110
i5	192.272789	0.136735	311.126982	110
i5	192.545578	0.273016	207.646491	125
i5	192.545578	0.273016	246.934685	125
i5	192.545578	0.273016	311.126982	125
i5	192.954649	0.136735	184.997211	125
i5	192.954649	0.136735	219.999999	125
i5	192.954649	0.136735	277.166995	125
i5	193.227438	0.136508	184.997211	110
i5	193.227438	0.136508	219.999999	110
i5	193.227438	0.136508	277.166995	110
i5	193.500227	0.136508	184.997211	119
i5	193.500227	0.136508	219.999999	119
i5	193.500227	0.136508	277.166995	119
i5	193.636508	0.136508	184.997211	101
i5	193.636508	0.204762	219.999999	101
i5	193.636508	0.204762	277.166995	101
i5	193.772789	0.136735	184.997211	95
i5	193.909297	0.136508	184.997211	122
i5	193.909297	0.204762	219.999999	122
i5	193.909297	0.216100	277.166995	122
i5	194.045578	0.136735	184.997211	101
i5	194.182086	0.136508	246.934685	127
i5	194.182086	0.136508	164.804481	127
i5	194.182086	0.136508	207.646491	127
i5	194.454649	0.136735	246.934685	116
i5	194.454649	0.136735	207.646491	116
i5	194.454649	0.136735	164.804481	116
i5	194.727438	0.273016	246.934685	127
i5	194.727438	0.409297	164.804481	127
i5	194.727438	0.409297	207.646491	127
i5	195.000227	0.136508	246.934685	113
i5	195.136508	0.136508	184.997211	127
i5	195.136508	0.136508	219.999999	127
i5	195.136508	0.136508	277.166995	127
i5	195.409297	0.136508	184.997211	116
i5	195.409297	0.136508	219.999999	116
i5	195.409297	0.136508	277.166995	116
i5	195.682086	0.136508	184.997211	127
i5	195.682086	0.136508	219.999999	127
i5	195.818367	0.136508	184.997211	113
i5	195.682086	0.341043	277.166995	127
i5	195.818367	0.204762	219.999999	113
i5	195.954649	0.136735	184.997211	87
i5	196.091156	0.136508	184.997211	125
i5	196.091156	0.204762	219.999999	125
i5	196.091156	0.204762	277.166995	125
i5	196.227438	0.136508	184.997211	87
i5	196.363719	0.136735	207.646491	122
i5	196.363719	0.136735	246.934685	122
i5	196.363719	0.136735	311.126982	122
i5	196.636508	0.136508	207.646491	110
i5	196.636508	0.136508	246.934685	110
i5	196.636508	0.136508	311.126982	110
i5	196.909297	0.273016	207.646491	125
i5	196.909297	0.273016	246.934685	125
i5	196.909297	0.273016	311.126982	125
i5	197.318367	0.136508	184.997211	125
i5	197.318367	0.136508	219.999999	125
i5	197.318367	0.136508	277.166995	125
i5	197.591156	0.136508	184.997211	110
i5	197.591156	0.136508	219.999999	110
i5	197.591156	0.136508	277.166995	110
i5	197.863719	0.136735	184.997211	119
i5	197.863719	0.136735	219.999999	119
i5	197.863719	0.136735	277.166995	119
i5	198.000227	0.136508	184.997211	101
i5	198.000227	0.204762	219.999999	101
i5	198.000227	0.204762	277.166995	101
i5	198.136508	0.136508	184.997211	95
i5	198.272789	0.136735	184.997211	122
i5	198.272789	0.204989	219.999999	122
i5	198.272789	0.216327	277.166995	122
i5	198.409297	0.136508	184.997211	101
i5	198.545578	0.136735	246.934685	127
i5	198.545578	0.136735	164.804481	127
i5	198.545578	0.136735	207.646491	127
i5	198.818367	0.136508	246.934685	116
i5	198.818367	0.136508	207.646491	116
i5	198.818367	0.136508	164.804481	116
i5	199.091156	0.272789	246.934685	127
i5	199.091156	0.409297	164.804481	127
i5	199.091156	0.409297	207.646491	127
i5	199.363719	0.136735	246.934685	113
i5	199.500227	0.136508	184.997211	127
i5	199.500227	0.136508	219.999999	127
i5	199.500227	0.136508	277.166995	127
i5	199.772789	0.136735	184.997211	116
i5	199.772789	0.136735	219.999999	116
i5	199.772789	0.136735	277.166995	116
i5	200.045578	0.136735	184.997211	127
i5	200.045578	0.136735	219.999999	127
i5	200.182086	0.136508	184.997211	113
i5	200.045578	0.341270	277.166995	127
i5	200.182086	0.204762	219.999999	113
i5	200.318367	0.136508	184.997211	87
i5	200.454649	0.136735	184.997211	125
i5	200.454649	0.204762	219.999999	125
i5	200.454649	0.204762	277.166995	125
i5	200.591156	0.136508	184.997211	87
i5	200.727438	0.136508	207.646491	122
i5	200.727438	0.136508	246.934685	122
i5	200.727438	0.136508	311.126982	122
i5	201.000227	0.136508	207.646491	110
i5	201.000227	0.136508	246.934685	110
i5	201.000227	0.136508	311.126982	110
i5	201.272789	0.273016	207.646491	125
i5	201.272789	0.273016	246.934685	125
i5	201.272789	0.273016	311.126982	125
i5	201.682086	0.136508	184.997211	125
i5	201.682086	0.136508	219.999999	125
i5	201.682086	0.136508	277.166995	125
i5	201.954649	0.136735	184.997211	110
i5	201.954649	0.136735	219.999999	110
i5	201.954649	0.136735	277.166995	110
i5	202.227438	0.136508	184.997211	119
i5	202.227438	0.136508	219.999999	119
i5	202.227438	0.136508	277.166995	119
i5	202.363719	0.136735	184.997211	101
i5	202.363719	0.204762	219.00    	101
i5	202.363719	0.204762	277.166995	101
i5	202.500227	0.136508	184.997211	95
i5	202.636508	0.136508	184.997211	122
i5	202.636508	0.204762	219.00    	122
i5	202.636508	0.216100	277.166995	122
i5	202.772789	0.136735	184.997211	101
i5	202.909297	0.136508	246.934685	127
i5	202.909297	0.136508	164.804481	127
i5	202.909297	0.136508	207.646491	127
i5	203.182086	0.136508	246.934685	116
i5	203.182086	0.136508	207.646491	116
i5	203.182086	0.136508	164.804481	116
i5	203.454649	0.273016	246.934685	127
i5	203.454649	0.409297	164.804481	127
i5	203.454649	0.409297	207.646491	127
i5	203.727438	0.136508	246.934685	113
i5	203.863719	0.136735	184.997211	127
i5	203.863719	0.136735	219.00    	127
i5	203.863719	0.136735	277.166995	127
i5	204.136508	0.136508	184.997211	116
i5	204.136508	0.136508	219.00    	116
i5	204.136508	0.136508	277.166995	116
i5	204.409297	0.136508	184.997211	127
i5	204.409297	0.136508	219.00    	127
i5	204.545578	0.136735	184.997211	113
i5	204.409297	0.341043	277.166995	127
i5	204.545578	0.204762	219.00    	113
i5	204.682086	0.136508	184.997211	87
i5	204.818367	0.136508	184.997211	125
i5	204.818367	0.204762	219.00    	125
i5	204.818367	0.204762	277.166995	125
i5	204.954649	0.136735	184.997211	87
i5	205.091156	0.136508	207.646491	122
i5	205.091156	0.136508	246.934685	122
i5	205.091156	0.136508	311.126982	122
i5	205.363719	0.136735	207.646491	110
i5	205.363719	0.136735	246.934685	110
i5	205.363719	0.136735	311.126982	110
i5	205.636508	0.273016	207.646491	125
i5	205.636508	0.273016	246.934685	125
i5	205.636508	0.273016	311.126982	125
i5	206.045578	0.136735	184.997211	125
i5	206.045578	0.136735	219.00    	125
i5	206.045578	0.136735	277.166995	125
i5	206.318367	0.136508	184.997211	110
i5	206.318367	0.136508	219.00    	110
i5	206.318367	0.136508	277.166995	110
i5	206.591156	0.136508	184.997211	119
i5	206.591156	0.136508	219.00    	119
i5	206.591156	0.136508	277.166995	119
i5	206.727438	0.136508	184.997211	101
i5	206.727438	0.204762	219.00    	101
i5	206.727438	0.204762	277.166995	101
i5	206.863719	0.136735	184.997211	95
i5	207.000227	0.136508	184.997211	122
i5	207.000227	0.204762	219.00    	122
i5	207.000227	0.216100	277.166995	122
i5	207.136508	0.136508	184.997211	101
i5	207.272789	0.545805	207.646491	127
i5	207.272789	0.545805	123.467342	127
i5	207.272789	0.545805	164.804481	127
i5	207.818367	0.136508	164.804481	127
i5	207.818367	0.136508	207.646491	127
i5	207.818367	0.136508	123.467342	127
i5	207.954649	0.136735	123.467342	90
i5	207.954649	0.136735	207.646491	90
i5	207.954649	0.136735	164.804481	90
i5	208.091156	0.136508	164.804481	127
i5	208.091156	0.136508	207.646491	127
i5	208.091156	0.136508	123.467342	127
i5	208.227438	0.136508	164.804481	90
i5	208.227438	0.136508	123.467342	90
i5	208.227438	0.136508	207.646491	90
i5	208.363719	0.136735	164.804481	127
i5	208.363719	0.136735	207.646491	127
i5	208.363719	0.136735	123.467342	127
i5	208.500227	0.136508	164.804481	93
i5	208.500227	0.136508	207.646491	93
i5	208.500227	0.136508	123.467342	93
i5	208.636508	0.136508	164.804481	127
i5	208.636508	0.136508	207.646491	124
i5	208.636508	0.136508	123.467342	127
i5	208.772789	0.136735	207.646491	87
i5	208.772789	0.136735	123.467342	87
i5	208.772789	0.136735	164.804481	87
i5	208.909297	0.136508	164.804481	123
i5	208.909297	0.136508	207.646491	123
i5	208.909297	0.136508	123.467342	127
i5	209.045578	0.136735	164.804481	93
i5	209.045578	0.136735	123.467342	93
i5	209.045578	0.136735	207.646491	93
i5	209.182086	0.136508	164.804481	127
i5	209.182086	0.136508	207.646491	127
i5	209.182086	0.136508	123.467342	127
i5	209.318367	0.136508	123.467342	93
i5	209.318367	0.136508	164.804481	93
i5	209.318367	0.136508	207.646491	93
i5	209.5   	0.273016	164.804481	127
i5	209.52  	0.273016	207.646491	127
i5	209.54  	0.273016	123.467342	127

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
i6	150.545578	0.136508	415.292983	127
i6	150.545578	0.136508	329.608962	127
i6	150.545578	0.136508	246.934685	127
i6	150.818367	0.136508	415.292983	95
i6	150.818367	0.136508	329.608962	95
i6	150.818367	0.136508	246.934685	95
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
i7	47.727211	0.136735	246.934685	127
i7	47.727211	0.273016	164.804481	122
i7	47.727211	0.273016	219.00    	122
i7	47.863719	0.136508	246.934685	94
i7	48.000000	0.136735	246.934685	127
i7	48.000000	0.136735	184.997211	125
i7	48.000000	0.136735	246.934685	127
i7	48.272789	0.136508	246.934685	122
i7	48.272789	0.136508	184.997211	107
i7	48.272789	0.136508	246.934685	127
i7	48.545578	0.272789	246.934685	127
i7	48.545578	0.409297	184.997211	119
i7	48.545578	0.409297	246.934685	127
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
i7	86.045578	0.136508	219.00    	122
i7	86.045578	0.136508	164.804481	122
i7	86.045578	0.136508	246.934685	127
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
i7	100.909070	0.273016	246.934685	127
i7	100.909070	0.409524	184.997211	119
i7	100.909070	0.409524	246.934685	127
i7	101.318367	0.136508	164.804481	116
i7	101.318367	0.136508	219.00    	116
i7	101.318367	0.136508	246.934685	127
i7	101.590930	0.136735	164.804481	107
i7	101.590930	0.136735	219.00    	107
i7	101.590930	0.136735	246.934685	127
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
i7	201.000227	0.136508	246.934685	122
i7	201.000227	0.136508	184.997211	107
i7	201.000227	0.136508	246.934685	127
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
; 8:3
i8	181.636508	0.273016	1318.435849	127
i8	181.636508	0.278   	659.217924 	127
i8	181.909297	0.034240	1244.507929	125
i8	181.943311	0.034240	1108.667979	108
i8	181.977324	0.034467	987.738739	92
i8	182.011565	0.034240	932.274929	76
i8	182.045578	0.034240	830.585965	59
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
i8	183.681859	0.102721	622.253965	127
i8	183.681859	0.102721	1244.507929	127
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
i9	100.636508	0.15    	311.126982	113
i9	100.909070	0.42	311.126982	119
i9	100.909070	0.43	369.994421	119
i9	101.318367	0.15    	277.166995	127
i9	101.318367	0.15    	329.608962	127
i9	101.590930	0.14      	277.17  	126
i9	101.590930	0.14      	329.608962	116
i9	101.318367	0.56	219.999999	127
i9	101.863719	0.28       	329.608962	127
i9	101.863719	0.28       	277.166995	127
i9	101.863719	0.42               	207.646491	127
i9	102.136508	0.15    	329.608962	84
i9	102.136508	0.15    	277.166995	84
i9	102.272789	0.15    	277.166995	127
i9	102.272789	0.15    	329.608962	127
i9	102.272789	0.28       	184.997211	127
i9	102.545578	0.29       	329.608962	127
i9	102.545578	0.29       	246.934685	127
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
; rapid drums?
; 10:1 starts 2.1 ends 3:30 ins 10

i10	2.181859	0.034240	65.406391	127
i10	2.181859	0.034240	92.498605	110
i10	2.181859	0.113832	138.583497	107
i10	2.318141	0.034467	103.823246	40
i10	2.454649	0.034240	82.402241	127
i10	2.454649	0.034240	184.997211	127
i10	2.454649	0.034240	92.498605	107
i10	2.590930	0.034240	103.823246	43
i10	2.727211	0.034467	65.406391	127
i10	2.727211	0.034467	92.498605	110
i10	2.863719	0.034240	103.823246	43
i10	3.000000	0.034240	82.402241	127
i10	3.000000	0.034240	184.997211	127
i10	3.000000	0.034240	92.498605	110
i10	3.136281	0.034467	65.406391	98
i10	3.136281	0.034467	103.823246	46
i10	3.272789	0.034240	92.498605	107
i10	3.409070	0.034240	65.406391	127
i10	3.409070	0.034240	103.823246	43
i10	3.545351	0.034467	82.402241	127
i10	3.545351	0.034467	184.997211	127
i10	3.545351	0.034467	92.498605	104
i10	3.681859	0.034240	65.406391	95
i10	3.681859	0.034240	103.823246	40
i10	3.818141	0.034467	65.406391	127
i10	3.818141	0.034467	92.498605	101
i10	3.954649	0.034240	103.823246	40
i10	4.090930	0.034240	82.402241	127
i10	4.090930	0.034240	184.997211	127
i10	4.090930	0.034240	92.498605	107
i10	4.227211	0.034467	103.823246	40
i10	4.363719	0.034240	65.406391	127
i10	4.363719	0.034240	92.498605	110
i10	4.500000	0.034240	103.823246	40
i10	4.636281	0.034467	82.402241	127
i10	4.636281	0.034467	184.997211	127
i10	4.636281	0.034467	92.498605	107
i10	4.772789	0.034240	103.823246	43
i10	4.909070	0.034240	65.406391	127
i10	4.909070	0.034240	92.498605	110
i10	5.045351	0.034467	103.823246	43
i10	5.181859	0.034240	82.402241	127
i10	5.181859	0.034240	184.997211	127
i10	5.181859	0.034240	92.498605	110
i10	5.318141	0.034467	65.406391	98
i10	5.318141	0.034467	103.823246	46
i10	5.454649	0.034240	92.498605	107
i10	5.590930	0.034240	65.406391	127
i10	5.590930	0.034240	103.823246	43
i10	5.727211	0.034467	82.402241	127
i10	5.727211	0.034467	184.997211	127
i10	5.727211	0.034467	92.498605	104
i10	5.863719	0.034240	65.406391	95
i10	5.863719	0.034240	103.823246	40
i10	6.000000	0.034240	65.406391	127
i10	6.000000	0.034240	92.498605	101
i10	6.136281	0.034467	103.823246	40
i10	6.272789	0.034240	82.402241	127
i10	6.272789	0.034240	184.997211	127
i10	6.272789	0.034240	92.498605	107
i10	6.409070	0.034240	103.823246	40
i10	6.545351	0.034467	65.406391	127
i10	6.545351	0.034467	92.498605	110
i10	6.681859	0.034240	103.823246	40
i10	6.818141	0.034467	82.402241	127
i10	6.818141	0.034467	184.997211	127
i10	6.818141	0.034467	92.498605	107
i10	6.954649	0.034240	103.823246	43
i10	7.090930	0.034240	65.406391	127
i10	7.090930	0.034240	92.498605	110
i10	7.227211	0.034467	103.823246	43
i10	7.363719	0.034240	82.402241	127
i10	7.363719	0.034240	184.997211	127
i10	7.363719	0.034240	92.498605	110
i10	7.500000	0.034240	65.406391	98
i10	7.500000	0.034240	103.823246	46
i10	7.636281	0.034467	92.498605	107
i10	7.772789	0.034240	65.406391	127
i10	7.772789	0.034240	103.823246	43
i10	7.909070	0.034240	82.402241	127
i10	7.909070	0.034240	184.997211	127
i10	7.909070	0.034240	92.498605	104
i10	8.045351	0.034467	65.406391	95
i10	8.045351	0.034467	103.823246	40
i10	8.181859	0.034240	65.406391	127
i10	8.181859	0.034240	92.498605	101
i10	8.318141	0.034467	103.823246	40
i10	8.454649	0.034240	82.402241	127
i10	8.454649	0.034240	184.997211	127
i10	8.454649	0.034240	92.498605	107
i10	8.590930	0.034240	103.823246	40
i10	8.727211	0.034467	65.406391	127
i10	8.727211	0.034467	92.498605	110
i10	8.863719	0.034240	103.823246	40
i10	9.000000	0.034240	82.402241	127
i10	9.000000	0.034240	184.997211	127
i10	9.000000	0.034240	92.498605	107
i10	9.136281	0.034467	103.823246	43
i10	9.272789	0.034240	65.406391	127
i10	9.272789	0.034240	92.498605	110
i10	9.409070	0.034240	103.823246	43
i10	9.545351	0.034467	82.402241	127
i10	9.545351	0.034467	184.997211	127
i10	9.545351	0.034467	92.498605	110
i10	9.681859	0.034240	65.406391	98
i10	9.681859	0.034240	103.823246	46
i10	9.818141	0.034467	92.498605	107
i10	9.954649	0.034240	65.406391	127
i10	9.954649	0.034240	103.823246	43
i10	10.090930	0.034240	82.402241	127
i10	10.090930	0.034240	184.997211	127
i10	10.090930	0.034240	92.498605	104
i10	10.227211	0.034467	65.406391	95
i10	10.227211	0.034467	103.823246	40
i10	10.363719	0.034240	65.406391	127
i10	10.363719	0.034240	92.498605	101
i10	10.500000	0.034240	82.402241	127
i10	10.500000	0.034240	103.823246	40
i10	10.636281	0.034467	82.402241	107
i10	10.636281	0.034467	184.997211	127
i10	10.636281	0.034467	92.498605	107
i10	10.772789	0.034240	103.823246	40
i10	10.909070	0.034240	65.406391	127
i10	10.909070	0.034240	92.498605	110
i10	10.909070	0.113832	138.583497	107
i10	11.045351	0.034467	103.823246	40
i10	11.181859	0.034240	82.402241	127
i10	11.181859	0.034240	184.997211	127
i10	11.181859	0.034240	92.498605	107
i10	11.318141	0.034467	103.823246	43
i10	11.454649	0.034240	65.406391	127
i10	11.454649	0.034240	92.498605	110
i10	11.590930	0.034240	103.823246	43
i10	11.727211	0.034467	82.402241	127
i10	11.727211	0.034467	184.997211	127
i10	11.727211	0.034467	92.498605	110
i10	11.863719	0.034240	65.406391	98
i10	11.863719	0.034240	103.823246	46
i10	12.000000	0.034240	92.498605	107
i10	12.136281	0.034467	65.406391	127
i10	12.136281	0.034467	103.823246	43
i10	12.272789	0.034240	82.402241	127
i10	12.272789	0.034240	184.997211	127
i10	12.272789	0.034240	92.498605	104
i10	12.409070	0.034240	65.406391	95
i10	12.409070	0.034240	103.823246	40
i10	12.545578	0.034240	65.406391	127
i10	12.545578	0.034240	92.498605	101
i10	12.681859	0.034240	103.823246	40
i10	12.818141	0.034467	82.402241	127
i10	12.818141	0.034467	184.997211	127
i10	12.818141	0.034467	92.498605	107
i10	12.954649	0.034240	103.823246	40
i10	13.090930	0.034240	65.406391	127
i10	13.090930	0.034240	92.498605	110
i10	13.227211	0.034467	103.823246	40
i10	13.363719	0.034240	82.402241	127
i10	13.363719	0.034240	184.997211	127
i10	13.363719	0.034240	92.498605	107
i10	13.500000	0.034240	103.823246	43
i10	13.636281	0.034467	65.406391	127
i10	13.636281	0.034467	92.498605	110
i10	13.772789	0.034240	103.823246	43
i10	13.909070	0.034240	82.402241	127
i10	13.909070	0.034240	184.997211	127
i10	13.909070	0.034240	92.498605	110
i10	14.045578	0.034240	65.406391	98
i10	14.045578	0.034240	103.823246	46
i10	14.181859	0.034240	92.498605	107
i10	14.318141	0.034467	65.406391	127
i10	14.318141	0.034467	103.823246	43
i10	14.454649	0.034240	82.402241	127
i10	14.454649	0.034240	184.997211	127
i10	14.454649	0.034240	92.498605	104
i10	14.590930	0.034240	65.406391	95
i10	14.590930	0.034240	103.823246	40
i10	14.727211	0.034467	65.406391	127
i10	14.727211	0.034467	92.498605	101
i10	14.863719	0.034240	103.823246	40
i10	15.000000	0.034240	82.402241	127
i10	15.000000	0.034240	184.997211	127
i10	15.000000	0.034240	92.498605	107
i10	15.136281	0.034467	103.823246	40
i10	15.272789	0.034240	65.406391	127
i10	15.272789	0.034240	92.498605	110
i10	15.409070	0.034240	103.823246	40
i10	15.545578	0.034240	82.402241	127
i10	15.545578	0.034240	184.997211	127
i10	15.545578	0.034240	92.498605	107
i10	15.681859	0.034240	103.823246	43
i10	15.818141	0.034467	65.406391	127
i10	15.818141	0.034467	92.498605	110
i10	15.954649	0.034240	103.823246	43
i10	16.090930	0.034240	82.402241	127
i10	16.090930	0.034240	184.997211	127
i10	16.090930	0.034240	92.498605	110
i10	16.227211	0.034467	65.406391	98
i10	16.227211	0.034467	103.823246	46
i10	16.363719	0.034240	92.498605	107
i10	16.500000	0.034240	65.406391	127
i10	16.500000	0.034240	103.823246	43
i10	16.636281	0.034467	82.402241	127
i10	16.636281	0.034467	184.997211	127
i10	16.636281	0.034467	92.498605	104
i10	16.772789	0.034240	65.406391	95
i10	16.772789	0.034240	103.823246	40
i10	16.909070	0.034240	65.406391	127
i10	16.909070	0.034240	92.498605	101
i10	17.045578	0.034240	103.823246	40
i10	17.181859	0.034240	82.402241	127
i10	17.181859	0.034240	184.997211	127
i10	17.181859	0.034240	92.498605	107
i10	17.318141	0.034467	103.823246	40
i10	17.454649	0.034240	65.406391	127
i10	17.454649	0.034240	92.498605	110
i10	17.590930	0.034240	103.823246	40
i10	17.727211	0.034467	82.402241	127
i10	17.727211	0.034467	184.997211	127
i10	17.727211	0.034467	92.498605	107
i10	17.863719	0.034240	103.823246	43
i10	18.000000	0.034240	65.406391	127
i10	18.000000	0.034240	92.498605	110
i10	18.136281	0.034467	103.823246	43
i10	18.272789	0.034240	82.402241	127
i10	18.272789	0.034240	184.997211	127
i10	18.272789	0.034240	92.498605	110
i10	18.409070	0.034240	65.406391	98
i10	18.409070	0.034240	103.823246	46
i10	18.545578	0.034240	92.498605	107
i10	18.681859	0.034240	65.406391	127
i10	18.681859	0.034240	103.823246	43
i10	18.818141	0.034467	82.402241	127
i10	18.818141	0.034467	184.997211	127
i10	18.818141	0.034467	92.498605	104
i10	18.954649	0.034240	65.406391	95
i10	18.954649	0.034240	103.823246	40
i10	19.090930	0.034240	65.406391	127
i10	19.090930	0.034240	92.498605	101
i10	19.227211	0.034467	82.402241	127
i10	19.227211	0.034467	103.823246	40
i10	19.363719	0.034240	82.402241	107
i10	19.363719	0.034240	184.997211	127
i10	19.363719	0.034240	92.498605	107
i10	19.500000	0.034240	103.823246	40
i10	19.636281	0.034467	65.406391	127
i10	19.636281	0.034467	92.498605	110
i10	19.772789	0.034240	103.823246	40
i10	19.909070	0.034467	82.402241	127
i10	19.909070	0.034467	184.997211	127
i10	19.909070	0.034467	92.498605	107
i10	20.045578	0.034240	103.823246	43
i10	20.181859	0.034240	65.406391	127
i10	20.181859	0.034240	92.498605	110
i10	20.318141	0.034467	103.823246	43
i10	20.454649	0.034240	82.402241	127
i10	20.454649	0.034240	184.997211	127
i10	20.454649	0.034240	92.498605	110
i10	20.590930	0.034240	65.406391	98
i10	20.590930	0.034240	103.823246	46
i10	20.727211	0.034467	92.498605	107
i10	20.863719	0.034240	65.406391	127
i10	20.863719	0.034240	103.823246	43
i10	21.000000	0.034240	82.402241	127
i10	21.000000	0.034240	184.997211	127
i10	21.000000	0.034240	92.498605	104
i10	21.136281	0.034467	65.406391	95
i10	21.136281	0.034467	103.823246	40
i10	21.272789	0.034240	65.406391	127
i10	21.272789	0.034240	92.498605	101
i10	21.409070	0.034467	103.823246	40
i10	21.545578	0.034240	82.402241	127
i10	21.545578	0.034240	184.997211	127
i10	21.545578	0.034240	92.498605	107
i10	21.681859	0.034240	103.823246	40
i10	21.818141	0.034467	65.406391	127
i10	21.818141	0.034467	92.498605	110
i10	21.954649	0.034240	103.823246	40
i10	22.090930	0.034240	82.402241	127
i10	22.090930	0.034240	184.997211	127
i10	22.090930	0.034240	92.498605	107
i10	22.227211	0.034467	103.823246	43
i10	22.363719	0.034240	65.406391	127
i10	22.363719	0.034240	92.498605	110
i10	22.500000	0.034240	103.823246	43
i10	22.636281	0.034467	82.402241	127
i10	22.636281	0.034467	184.997211	127
i10	22.636281	0.034467	92.498605	110
i10	22.772789	0.034240	65.406391	98
i10	22.772789	0.034240	103.823246	46
i10	22.909070	0.034467	92.498605	107
i10	23.045578	0.034240	65.406391	127
i10	23.045578	0.034240	103.823246	43
i10	23.181859	0.034240	82.402241	127
i10	23.181859	0.034240	184.997211	127
i10	23.181859	0.034240	92.498605	104
i10	23.318141	0.034467	65.406391	95
i10	23.318141	0.034467	103.823246	40
i10	23.454649	0.034240	65.406391	127
i10	23.454649	0.034240	92.498605	101
i10	23.590930	0.034240	103.823246	40
i10	23.727211	0.034467	82.402241	127
i10	23.727211	0.034467	184.997211	127
i10	23.727211	0.034467	92.498605	107
i10	23.863719	0.034240	103.823246	40
i10	24.000000	0.034240	65.406391	127
i10	24.000000	0.034240	92.498605	110
i10	24.136281	0.034467	103.823246	40
i10	24.272789	0.034240	82.402241	127
i10	24.272789	0.034240	184.997211	127
i10	24.272789	0.034240	92.498605	107
i10	24.409070	0.034467	103.823246	43
i10	24.545578	0.034240	65.406391	127
i10	24.545578	0.034240	92.498605	110
i10	24.681859	0.034240	103.823246	43
i10	24.818141	0.034467	82.402241	127
i10	24.818141	0.034467	184.997211	127
i10	24.818141	0.034467	92.498605	110
i10	24.954649	0.034240	65.406391	98
i10	24.954649	0.034240	103.823246	46
i10	25.090930	0.034240	92.498605	107
i10	25.227211	0.034467	65.406391	127
i10	25.227211	0.034467	103.823246	43
i10	25.363719	0.034240	82.402241	127
i10	25.363719	0.034240	184.997211	127
i10	25.363719	0.034240	92.498605	104
i10	25.500000	0.034240	65.406391	95
i10	25.500000	0.034240	103.823246	40
i10	25.636281	0.034467	65.406391	127
i10	25.636281	0.034467	92.498605	101
i10	25.772789	0.034240	103.823246	40
i10	25.909070	0.034467	82.402241	127
i10	25.909070	0.034467	184.997211	127
i10	25.909070	0.034467	92.498605	107
i10	26.045578	0.034240	103.823246	40
i10	26.181859	0.034240	65.406391	127
i10	26.181859	0.034240	92.498605	110
i10	26.318141	0.034467	103.823246	40
i10	26.454649	0.034240	82.402241	127
i10	26.454649	0.034240	184.997211	127
i10	26.454649	0.034240	92.498605	107
i10	26.590930	0.034240	103.823246	43
i10	26.727211	0.034467	65.406391	127
i10	26.727211	0.034467	92.498605	110
i10	26.863719	0.034240	103.823246	43
i10	27.000000	0.034240	82.402241	127
i10	27.000000	0.034240	184.997211	127
i10	27.000000	0.034240	92.498605	110
i10	27.136281	0.034467	65.406391	98
i10	27.136281	0.034467	103.823246	46
i10	27.272789	0.034240	92.498605	107
i10	27.409070	0.034467	65.406391	127
i10	27.409070	0.034467	103.823246	43
i10	27.545578	0.034240	82.402241	127
i10	27.545578	0.034240	184.997211	127
i10	27.545578	0.034240	92.498605	104
i10	27.681859	0.034240	65.406391	95
i10	27.681859	0.034240	103.823246	40
i10	27.818141	0.034467	65.406391	127
i10	27.818141	0.034467	92.498605	101
i10	27.954649	0.034240	103.823246	40
i10	28.090930	0.034240	82.402241	127
i10	28.090930	0.034240	184.997211	127
i10	28.090930	0.034240	116.534366	113
i10	28.363719	0.034240	65.406391	127
i10	28.363719	0.034240	92.498605	110
i10	28.500000	0.034240	103.823246	40
i10	28.636281	0.034467	82.402241	127
i10	28.636281	0.034467	184.997211	127
i10	28.636281	0.034467	92.498605	107
i10	28.772789	0.034240	103.823246	43
i10	28.909070	0.034467	65.406391	127
i10	28.909070	0.034467	92.498605	110
i10	29.045578	0.034240	103.823246	43
i10	29.181859	0.034240	82.402241	127
i10	29.181859	0.034240	184.997211	127
i10	29.181859	0.034240	92.498605	110
i10	29.318141	0.034467	65.406391	98
i10	29.318141	0.034467	103.823246	46
i10	29.454649	0.034240	92.498605	107
i10	29.590930	0.034240	65.406391	127
i10	29.590930	0.034240	103.823246	43
i10	29.727211	0.034467	82.402241	127
i10	29.727211	0.034467	184.997211	127
i10	29.727211	0.034467	92.498605	104
i10	29.863719	0.034240	65.406391	95
i10	29.863719	0.034240	103.823246	40
i10	30.000000	0.034240	65.406391	127
i10	30.000000	0.034240	92.498605	101
i10	30.136281	0.034467	103.823246	40
i10	30.272789	0.034240	82.402241	127
i10	30.272789	0.034240	184.997211	127
i10	30.272789	0.034240	92.498605	107
i10	30.409070	0.034467	103.823246	40
i10	30.545578	0.034240	65.406391	127
i10	30.545578	0.034240	92.498605	110
i10	30.681859	0.034240	103.823246	40
i10	30.818141	0.034467	82.402241	127
i10	30.818141	0.034467	184.997211	127
i10	30.818141	0.034467	92.498605	107
i10	30.954649	0.034240	103.823246	43
i10	31.090930	0.034240	65.406391	127
i10	31.090930	0.034240	92.498605	110
i10	31.227211	0.034467	103.823246	43
i10	31.363719	0.034240	82.402241	127
i10	31.363719	0.034240	184.997211	127
i10	31.363719	0.034240	92.498605	110
i10	31.500000	0.034240	65.406391	98
i10	31.500000	0.034240	103.823246	46
i10	31.636281	0.034467	92.498605	107
i10	31.772789	0.034240	65.406391	127
i10	31.772789	0.034240	103.823246	43
i10	31.909070	0.034467	82.402241	127
i10	31.909070	0.034467	184.997211	127
i10	31.909070	0.034467	92.498605	104
i10	32.045578	0.034240	65.406391	95
i10	32.045578	0.034240	103.823246	40
i10	32.181859	0.034240	65.406391	127
i10	32.181859	0.034240	92.498605	101
i10	32.318141	0.034467	103.823246	40
i10	32.454649	0.034240	82.402241	127
i10	32.454649	0.034240	184.997211	127
i10	32.454649	0.034240	92.498605	107
i10	32.590930	0.034240	103.823246	40
i10	32.727211	0.034467	65.406391	127
i10	32.727211	0.034467	92.498605	110
i10	32.863719	0.034240	103.823246	40
i10	33.000000	0.034240	82.402241	127
i10	33.000000	0.034240	184.997211	127
i10	33.000000	0.034240	92.498605	107
i10	33.136281	0.034467	103.823246	43
i10	33.272789	0.034240	65.406391	127
i10	33.272789	0.034240	92.498605	110
i10	33.409070	0.034467	103.823246	43
i10	33.545578	0.034240	82.402241	127
i10	33.545578	0.034240	184.997211	127
i10	33.545578	0.034240	92.498605	110
i10	33.681859	0.034240	65.406391	98
i10	33.681859	0.034240	103.823246	46
i10	33.818141	0.034467	92.498605	107
i10	33.954649	0.034240	65.406391	127
i10	33.954649	0.034240	103.823246	43
i10	34.090930	0.034240	82.402241	127
i10	34.090930	0.034240	184.997211	127
i10	34.090930	0.034240	92.498605	104
i10	34.227211	0.034467	65.406391	95
i10	34.227211	0.034467	103.823246	40
i10	34.363719	0.034240	65.406391	127
i10	34.363719	0.034240	92.498605	101
i10	34.500000	0.034240	103.823246	40
i10	34.636281	0.034467	82.402241	127
i10	34.636281	0.034467	184.997211	127
i10	34.636281	0.034467	92.498605	107
i10	34.772789	0.034240	103.823246	40
i10	34.909070	0.034467	65.406391	127
i10	34.909070	0.034467	92.498605	110
i10	35.045578	0.034240	103.823246	40
i10	35.181859	0.034240	82.402241	127
i10	35.181859	0.034240	184.997211	127
i10	35.181859	0.034240	92.498605	107
i10	35.318141	0.034467	103.823246	43
i10	35.454649	0.034240	65.406391	127
i10	35.454649	0.034240	92.498605	110
i10	35.590930	0.034240	103.823246	43
i10	35.727211	0.034467	82.402241	127
i10	35.727211	0.034467	184.997211	127
i10	35.727211	0.034467	92.498605	110
i10	35.863719	0.034240	65.406391	98
i10	35.863719	0.034240	103.823246	46
i10	36.000000	0.034240	92.498605	107
i10	36.136281	0.034467	82.402241	101
i10	36.136281	0.034467	65.406391	127
i10	36.136281	0.034467	103.823246	43
i10	36.272789	0.034240	82.402241	127
i10	36.272789	0.034240	184.997211	127
i10	36.272789	0.034240	92.498605	104
i10	36.409070	0.034467	65.406391	95
i10	36.409070	0.034467	103.823246	40
i10	36.545578	0.034240	65.406391	127
i10	36.545578	0.034240	92.498605	101
i10	36.681859	0.034240	103.823246	40
i10	36.818141	0.034467	82.402241	127
i10	36.818141	0.034467	184.997211	127
i10	36.818141	0.034467	116.534366	113
i10	37.090930	0.034240	65.406391	127
i10	37.090930	0.034240	92.498605	110
i10	37.227211	0.034467	103.823246	40
i10	37.363719	0.034240	82.402241	127
i10	37.363719	0.034240	184.997211	127
i10	37.363719	0.034240	92.498605	107
i10	37.500000	0.034240	103.823246	43
i10	37.636508	0.034240	65.406391	127
i10	37.636508	0.034240	92.498605	110
i10	37.772789	0.034240	103.823246	43
i10	37.909070	0.034467	82.402241	127
i10	37.909070	0.034467	184.997211	127
i10	37.909070	0.034467	92.498605	110
i10	38.045578	0.034240	65.406391	98
i10	38.045578	0.034240	103.823246	46
i10	38.181859	0.034240	92.498605	107
i10	38.318141	0.034467	65.406391	127
i10	38.318141	0.034467	103.823246	43
i10	38.454649	0.034240	82.402241	127
i10	38.454649	0.034240	184.997211	127
i10	38.454649	0.034240	92.498605	104
i10	38.590930	0.034240	65.406391	95
i10	38.590930	0.034240	103.823246	40
i10	38.727211	0.034467	65.406391	127
i10	38.727211	0.034467	92.498605	101
i10	38.863719	0.034240	103.823246	40
i10	39.000000	0.034240	82.402241	127
i10	39.000000	0.034240	184.997211	127
i10	39.000000	0.034240	92.498605	107
i10	39.136508	0.034240	103.823246	40
i10	39.272789	0.034240	65.406391	127
i10	39.272789	0.034240	92.498605	110
i10	39.409070	0.034467	103.823246	40
i10	39.545578	0.034240	82.402241	127
i10	39.545578	0.034240	184.997211	127
i10	39.545578	0.034240	92.498605	107
i10	39.681859	0.034240	103.823246	43
i10	39.818141	0.034467	65.406391	127
i10	39.818141	0.034467	92.498605	110
i10	39.954649	0.034240	103.823246	43
i10	40.090930	0.034240	82.402241	127
i10	40.090930	0.034240	184.997211	127
i10	40.090930	0.034240	92.498605	110
i10	40.227211	0.034467	65.406391	98
i10	40.227211	0.034467	103.823246	46
i10	40.363719	0.034240	92.498605	107
i10	40.500000	0.034240	65.406391	127
i10	40.500000	0.034240	103.823246	43
i10	40.636508	0.034240	82.402241	127
i10	40.636508	0.034240	184.997211	127
i10	40.636508	0.034240	92.498605	104
i10	40.772789	0.034240	65.406391	95
i10	40.772789	0.034240	103.823246	40
i10	40.909070	0.034467	65.406391	127
i10	40.909070	0.034467	92.498605	101
i10	41.045578	0.034240	103.823246	40
i10	41.181859	0.034240	82.402241	127
i10	41.181859	0.034240	184.997211	127
i10	41.181859	0.034240	92.498605	107
i10	41.318141	0.034467	103.823246	40
i10	41.454649	0.034240	65.406391	127
i10	41.454649	0.034240	92.498605	110
i10	41.590930	0.034240	103.823246	40
i10	41.727211	0.034467	82.402241	127
i10	41.727211	0.034467	184.997211	127
i10	41.727211	0.034467	92.498605	107
i10	41.863719	0.034240	103.823246	43
i10	42.000000	0.034240	65.406391	127
i10	42.000000	0.034240	92.498605	110
i10	42.136508	0.034240	103.823246	43
i10	42.272789	0.034240	82.402241	127
i10	42.272789	0.034240	184.997211	127
i10	42.272789	0.034240	92.498605	110
i10	42.409070	0.034467	65.406391	98
i10	42.409070	0.034467	103.823246	46
i10	42.545578	0.034240	92.498605	107
i10	42.681859	0.034240	65.406391	127
i10	42.681859	0.034240	103.823246	43
i10	42.818141	0.034467	82.402241	127
i10	42.818141	0.034467	184.997211	127
i10	42.818141	0.034467	92.498605	104
i10	42.954649	0.034240	65.406391	95
i10	42.954649	0.034240	103.823246	40
i10	43.090930	0.034240	65.406391	127
i10	43.090930	0.034240	92.498605	101
i10	43.227211	0.034467	103.823246	40
i10	43.363719	0.034240	82.402241	127
i10	43.363719	0.034240	184.997211	127
i10	43.363719	0.034240	92.498605	107
i10	43.500000	0.034467	103.823246	40
i10	43.636508	0.034240	65.406391	127
i10	43.636508	0.034240	92.498605	110
i10	43.772789	0.034240	103.823246	40
i10	43.909070	0.034467	82.402241	127
i10	43.909070	0.034467	184.997211	127
i10	43.909070	0.034467	92.498605	107
i10	44.045578	0.034240	103.823246	43
i10	44.181859	0.034240	65.406391	127
i10	44.181859	0.034240	92.498605	110
i10	44.318141	0.034467	103.823246	43
i10	44.454649	0.034240	82.402241	127
i10	44.454649	0.034240	184.997211	127
i10	44.454649	0.034240	92.498605	110
i10	44.590930	0.034240	65.406391	98
i10	44.590930	0.034240	103.823246	46
i10	44.727211	0.034467	92.498605	107
i10	44.863719	0.034240	82.402241	81
i10	44.863719	0.034240	65.406391	81
i10	44.863719	0.034240	103.823246	43
i10	45.000000	0.034467	82.402241	127
i10	45.000000	0.034467	184.997211	127
i10	45.000000	0.034467	92.498605	104
i10	45.136508	0.034240	65.406391	95
i10	45.136508	0.034240	103.823246	40
i10	45.272789	0.034240	65.406391	127
i10	45.272789	0.034240	92.498605	101
i10	45.409070	0.034467	82.402241	87
i10	45.409070	0.034467	103.823246	40
i10	45.545578	0.034240	82.402241	127
i10	45.545578	0.034240	184.997211	127
i10	45.545578	0.034240	116.534366	113
i10	45.818141	0.034467	65.406391	127
i10	45.818141	0.034467	92.498605	110
i10	45.954649	0.034240	103.823246	40
i10	46.090930	0.034240	82.402241	127
i10	46.090930	0.034240	184.997211	127
i10	46.090930	0.034240	92.498605	107
i10	46.227211	0.034467	103.823246	43
i10	46.363719	0.034240	65.406391	127
i10	46.363719	0.034240	92.498605	110
i10	46.500000	0.034467	103.823246	43
i10	46.636508	0.034240	82.402241	127
i10	46.636508	0.034240	184.997211	127
i10	46.636508	0.034240	92.498605	110
i10	46.772789	0.034240	65.406391	98
i10	46.772789	0.034240	103.823246	46
i10	46.909070	0.034467	92.498605	107
i10	47.045578	0.034240	65.406391	127
i10	47.045578	0.034240	103.823246	43
i10	47.181859	0.034240	82.402241	127
i10	47.181859	0.034240	184.997211	127
i10	47.181859	0.034240	92.498605	104
i10	47.318141	0.034467	65.406391	95
i10	47.318141	0.034467	103.823246	40
i10	47.454649	0.034240	65.406391	127
i10	47.454649	0.034240	92.498605	101
i10	47.590930	0.034240	103.823246	40
i10	47.727211	0.034467	82.402241	127
i10	47.727211	0.034467	184.997211	127
i10	47.727211	0.034467	92.498605	107
i10	47.863719	0.034240	103.823246	40
i10	48.000000	0.034467	65.406391	127
i10	48.000000	0.034467	92.498605	110
i10	48.136508	0.034240	103.823246	40
i10	48.272789	0.034240	82.402241	127
i10	48.272789	0.034240	184.997211	127
i10	48.272789	0.034240	92.498605	107
i10	48.409070	0.034467	103.823246	43
i10	48.545578	0.034240	65.406391	127
i10	48.545578	0.034240	92.498605	110
i10	48.681859	0.034240	103.823246	43
i10	48.8    	0.034467	82.402241	127
i10	48.82   	0.034467	184.997211	127
i10	48.818141	0.034467	92.498605	110
i10	48.954649	0.034240	65.406391	98
i10	48.954649	0.034240	103.823246	46
i10	49.090930	0.034240	92.498605	107
i10	49.227211	0.034467	65.406391	127
i10	49.227211	0.034467	103.823246	43
i10	49.36   	0.034240	82.402241	127
i10	49.3    	0.034240	184.997211	127
i10	49.364   	0.034240	92.498605	104
i10	49.500000	0.034467	65.406391	95
i10	49.500000	0.034467	103.823246	40
i10	49.636508	0.034240	65.406391	127
i10	49.636508	0.034240	92.498605	101
i10	49.772789	0.034240	103.823246	40
i10	49.909070	0.034467	82.402241	127
i10	49.909070	0.034467	184.997211	127
i10	49.909070	0.034467	92.498605	107
i10	50.045578	0.034240	103.823246	40
i10	50.181859	0.034240	65.406391	127
i10	50.181859	0.034240	92.498605	110
i10	50.318141	0.034467	103.823246	40
i10	50.454649	0.034240	82.402241	127
i10	50.454649	0.034240	184.997211	127
i10	50.454649	0.034240	92.498605	107
i10	50.590930	0.034240	103.823246	43
i10	50.727211	0.034467	65.406391	127
i10	50.727211	0.034467	92.498605	110
i10	50.863719	0.034240	103.823246	43
i10	51.000000	0.034467	82.402241	127
i10	51.000000	0.034467	184.997211	127
i10	51.000000	0.034467	92.498605	110
i10	51.136508	0.034240	65.406391	98
i10	51.136508	0.034240	103.823246	46
i10	51.272789	0.034240	92.498605	107
i10	51.409070	0.034467	65.406391	127
i10	51.409070	0.034467	103.823246	43
i10	51.545578	0.034240	82.402241	127
i10	51.545578	0.034240	184.997211	127
i10	51.545578	0.034240	92.498605	104
i10	51.681859	0.034240	65.406391	95
i10	51.681859	0.034240	103.823246	40
i10	51.818141	0.034467	65.406391	127
i10	51.818141	0.034467	92.498605	101
i10	51.954649	0.034240	103.823246	40
i10	52.090930	0.034240	82.402241	127
i10	52.090930	0.034240	184.997211	127
i10	52.090930	0.034240	92.498605	107
i10	52.227211	0.034467	103.823246	40
i10	52.363719	0.034240	65.406391	127
i10	52.363719	0.034240	92.498605	110
i10	52.500000	0.034467	103.823246	40
i10	52.636508	0.034240	82.402241	127
i10	52.636508	0.034240	184.997211	127
i10	52.636508	0.034240	92.498605	107
i10	52.772789	0.034240	103.823246	43
i10	52.909070	0.034467	65.406391	127
i10	52.909070	0.034467	92.498605	110
i10	53.045578	0.034240	103.823246	43
i10	53.181859	0.034240	82.402241	127
i10	53.181859	0.034240	184.997211	127
i10	53.181859	0.034240	92.498605	110
i10	53.318141	0.034467	65.406391	98
i10	53.318141	0.034467	103.823246	46
i10	53.454649	0.034240	92.498605	107
i10	53.590930	0.034240	65.406391	81
i10	53.590930	0.034240	103.823246	43
i10	53.727211	0.034467	82.402241	127
i10	53.727211	0.034467	184.997211	127
i10	53.727211	0.034467	92.498605	104
i10	53.863719	0.034240	65.406391	95
i10	53.863719	0.034240	116.534366	93
i10	54.000000	0.034467	65.406391	127
i10	54.000000	0.034467	82.4     	115
i10	54.136508	0.034240	82.4    	102
i10	54.136508	0.034240	82.4    	107
i10	54.136508	0.034240	103.823246	40
i10	54.272789	0.034240	82.402241	127
i10	54.272789	0.034240	184.997211	127
i10	54.272789	0.034240	92.498605	113
i10	54.409070	0.034467	82.402241	107
i10	54.409070	0.034467	103.823246	40
i10	54.5    	0.034240	65.4     	127
i10	54.55    	0.034240	138.6    	99
i10	54.546   	0.034240	92.5    	110
i10	54.681859	0.034240	103.823246	40
i10	54.818141	0.034467	82.402241	127
i10	54.818141	0.034467	92.498605	107
i10	54.954649	0.034240	103.823246	43
i10	55.090930	0.034240	65.406391	127
i10	55.090930	0.034240	92.498605	110
i10	55.227211	0.034467	103.823246	43
i10	55.363719	0.034240	82.402241	127
i10	55.363719	0.034240	92.498605	110
i10	55.500000	0.034467	65.406391	98
i10	55.500000	0.034467	103.823246	46
i10	55.636508	0.034240	92.498605	107
i10	55.772789	0.034240	65.406391	127
i10	55.772789	0.034240	103.823246	43
i10	55.909070	0.034467	82.402241	127
i10	55.909070	0.034467	92.498605	104
i10	56.045578	0.034240	65.406391	95
i10	56.045578	0.034240	103.823246	40
i10	56.181859	0.034240	65.406391	127
i10	56.181859	0.034240	92.498605	101
i10	56.318141	0.034467	103.823246	40
i10	56.454649	0.034240	82.402241	127
i10	56.454649	0.034240	92.498605	107
i10	56.590930	0.034240	103.823246	40
i10	56.727211	0.034467	65.406391	127
i10	56.727211	0.034467	219.999999	107
i10	56.727211	0.034467	92.498605	110
i10	56.863719	0.034240	103.823246	40
i10	57.000000	0.034467	82.402241	127
i10	57.000000	0.034467	92.498605	107
i10	57.136508	0.034240	103.823246	43
i10	57.272789	0.034240	65.406391	127
i10	57.272789	0.034240	92.498605	110
i10	57.409070	0.034467	103.823246	43
i10	57.545578	0.034240	82.402241	127
i10	57.545578	0.034240	92.498605	110
i10	57.681859	0.034240	65.406391	98
i10	57.681859	0.034240	103.823246	46
i10	57.818141	0.034467	92.498605	107
i10	57.954649	0.034240	65.406391	127
i10	57.954649	0.034240	103.823246	43
i10	58.090930	0.034240	82.402241	127
i10	58.090930	0.034240	92.498605	104
i10	58.227211	0.034467	65.406391	95
i10	58.227211	0.034467	103.823246	40
i10	58.363719	0.034240	65.406391	127
i10	58.363719	0.034240	92.498605	101
i10	58.500000	0.034467	103.823246	40
i10	58.636508	0.034240	82.402241	127
i10	58.636508	0.034240	92.498605	107
i10	58.772789	0.034240	103.823246	40
i10	58.909070	0.034467	65.406391	127
i10	58.909070	0.034467	138.583497	91
i10	58.909070	0.034467	92.498605	110
i10	59.045578	0.034240	103.823246	40
i10	59.181859	0.034240	82.402241	127
i10	59.181859	0.034240	92.498605	107
i10	59.318141	0.034467	103.823246	43
i10	59.454649	0.034240	65.406391	127
i10	59.454649	0.034240	92.498605	110
i10	59.590930	0.034240	103.823246	43
i10	59.727211	0.034467	82.402241	127
i10	59.727211	0.034467	92.498605	110
i10	59.863719	0.034240	65.406391	98
i10	59.863719	0.034240	103.823246	46
i10	60.000000	0.034467	92.498605	107
i10	60.136508	0.034240	65.406391	127
i10	60.136508	0.034240	103.823246	43
i10	60.272789	0.034240	82.402241	127
i10	60.272789	0.034240	92.498605	104
i10	60.409070	0.034467	65.406391	95
i10	60.409070	0.034467	103.823246	40
i10	60.545578	0.034240	65.406391	127
i10	60.545578	0.034240	92.498605	101
i10	60.681859	0.034240	103.823246	40
i10	60.818141	0.034467	82.402241	127
i10	60.818141	0.034467	92.498605	107
i10	60.954649	0.034240	103.823246	40
i10	61.090930	0.034240	65.406391	127
i10	61.090930	0.034240	219.999999	99
i10	61.090930	0.034240	92.498605	110
i10	61.227211	0.034467	103.823246	40
i10	61.363719	0.034240	82.402241	127
i10	61.363719	0.034240	92.498605	107
i10	61.500000	0.034467	103.823246	43
i10	61.636508	0.034240	65.406391	127
i10	61.636508	0.034240	92.498605	110
i10	61.772789	0.034240	103.823246	43
i10	61.909070	0.034467	82.402241	127
i10	61.909070	0.034467	92.498605	110
i10	62.045578	0.034240	65.406391	98
i10	62.045578	0.034240	103.823246	46
i10	62.181859	0.034240	92.498605	107
i10	62.318141	0.034467	65.406391	81
i10	62.318141	0.034467	103.823246	43
i10	62.454649	0.034240	82.402241	127
i10	62.454649	0.034240	92.498605	104
i10	62.590930	0.034240	65.406391	95
i10	62.590930	0.034240	103.823246	40
i10	62.727438	0.034240	65.406391	127
i10	62.727438	0.034240	92.498605	101
i10	62.863719	0.034240	103.823246	40
i10	63.000000	0.034467	82.402241	127
i10	63.000000	0.034467	116.534366	113
i10	63.272789	0.034240	65.406391	127
i10	63.272789	0.034240	138.583497	79
i10	63.272789	0.034240	92.498605	110
i10	63.409070	0.034467	103.823246	40
i10	63.545578	0.034240	82.402241	127
i10	63.545578	0.034240	92.498605	107
i10	63.681859	0.034240	103.823246	43
i10	63.818141	0.034467	65.406391	127
i10	63.818141	0.034467	92.498605	110
i10	63.954649	0.034240	103.823246	43
i10	64.090930	0.034240	82.402241	127
i10	64.090930	0.034240	92.498605	110
i10	64.227438	0.034240	65.406391	98
i10	64.227438	0.034240	103.823246	46
i10	64.363719	0.034240	92.498605	107
i10	64.500000	0.034467	65.406391	127
i10	64.500000	0.034467	103.823246	43
i10	64.636508	0.034240	82.402241	127
i10	64.636508	0.034240	92.498605	104
i10	64.772789	0.034240	65.406391	95
i10	64.772789	0.034240	103.823246	40
i10	64.909070	0.034467	65.406391	127
i10	64.909070	0.034467	92.498605	101
i10	65.045578	0.034240	103.823246	40
i10	65.181859	0.034240	82.402241	127
i10	65.181859	0.034240	92.498605	107
i10	65.318141	0.034467	103.823246	40
i10	65.454649	0.034240	65.406391	127
i10	65.454649	0.034240	219.999999	99
i10	65.454649	0.034240	92.498605	110
i10	65.590930	0.034240	103.823246	40
i10	65.727438	0.034240	82.402241	127
i10	65.727438	0.034240	92.498605	107
i10	65.863719	0.034240	103.823246	43
i10	66.000000	0.034467	65.406391	127
i10	66.000000	0.034467	92.498605	110
i10	66.136508	0.034240	103.823246	43
i10	66.272789	0.034240	82.402241	127
i10	66.272789	0.034240	92.498605	110
i10	66.409070	0.034467	65.406391	98
i10	66.409070	0.034467	103.823246	46
i10	66.545578	0.034240	92.498605	107
i10	66.681859	0.034240	65.406391	81
i10	66.681859	0.034240	103.823246	43
i10	66.818141	0.034467	82.402241	127
i10	66.818141	0.034467	92.498605	104
i10	66.954649	0.034240	65.406391	95
i10	66.954649	0.034240	116.534366	93
i10	67.090930	0.034240	65.406391	127
i10	67.090930	0.034240	82.402241	115
i10	67.227438	0.034240	82.402241	107
i10	67.227438	0.034240	82.402241	107
i10	67.227438	0.034240	103.823246	40
i10	67.363719	0.034240	82.402241	127
i10	67.363719	0.034240	92.498605	113
i10	67.500000	0.034467	82.402241	107
i10	67.500000	0.034467	103.823246	40
i10	67.636508	0.034240	65.406391	127
i10	67.636508	0.034240	138.583497	107
i10	67.636508	0.034240	92.498605	110
i10	67.772789	0.034240	103.823246	40
i10	67.909070	0.034467	82.402241	127
i10	67.909070	0.034467	184.997211	127
i10	67.909070	0.034467	92.498605	107
i10	68.045578	0.034240	103.823246	43
i10	68.181859	0.034240	65.406391	127
i10	68.181859	0.034240	92.498605	110
i10	68.318141	0.034467	103.823246	43
i10	68.454649	0.034240	82.402241	127
i10	68.454649	0.034240	184.997211	127
i10	68.454649	0.034240	92.498605	110
i10	68.590930	0.034467	65.406391	98
i10	68.590930	0.034467	103.823246	46
i10	68.727438	0.034240	92.498605	107
i10	68.863719	0.034240	65.406391	127
i10	68.863719	0.034240	103.823246	43
i10	69.000000	0.034467	82.402241	127
i10	69.000000	0.034467	184.997211	127
i10	69.000000	0.034467	92.498605	104
i10	69.136508	0.034240	65.406391	95
i10	69.136508	0.034240	103.823246	40
i10	69.272789	0.034240	65.406391	127
i10	69.272789	0.034240	92.498605	101
i10	69.409070	0.034467	103.823246	40
i10	69.545578	0.034240	82.402241	127
i10	69.545578	0.034240	184.997211	127
i10	69.545578	0.034240	92.498605	107
i10	69.681859	0.034240	103.823246	40
i10	69.818141	0.034467	65.406391	127
i10	69.818141	0.034467	92.498605	110
i10	69.954649	0.034240	103.823246	40
i10	70.090930	0.034467	82.402241	127
i10	70.090930	0.034467	184.997211	127
i10	70.090930	0.034467	92.498605	107
i10	70.227438	0.034240	103.823246	43
i10	70.363719	0.034240	65.406391	127
i10	70.363719	0.034240	92.498605	110
i10	70.500000	0.034467	103.823246	43
i10	70.636508	0.034240	82.402241	127
i10	70.636508	0.034240	184.997211	127
i10	70.636508	0.034240	92.498605	110
i10	70.772789	0.034240	65.406391	98
i10	70.772789	0.034240	103.823246	46
i10	70.909070	0.034467	92.498605	107
i10	71.045578	0.034240	65.406391	127
i10	71.045578	0.034240	103.823246	43
i10	71.181859	0.034240	82.402241	127
i10	71.181859	0.034240	184.997211	127
i10	71.181859	0.034240	92.498605	104
i10	71.318141	0.034467	65.406391	95
i10	71.318141	0.034467	103.823246	40
i10	71.454649	0.034240	65.406391	127
i10	71.454649	0.034240	92.498605	101
i10	71.590930	0.034467	103.823246	40
i10	71.727438	0.034240	82.402241	127
i10	71.727438	0.034240	184.997211	127
i10	71.727438	0.034240	92.498605	107
i10	71.863719	0.034240	103.823246	40
i10	72.000000	0.034467	65.406391	127
i10	72.000000	0.034467	92.498605	110
i10	72.136508	0.034240	103.823246	40
i10	72.272789	0.034240	82.402241	127
i10	72.272789	0.034240	184.997211	127
i10	72.272789	0.034240	92.498605	107
i10	72.409070	0.034467	103.823246	43
i10	72.545578	0.034240	65.406391	127
i10	72.545578	0.034240	92.498605	110
i10	72.681859	0.034240	103.823246	43
i10	72.818141	0.034467	82.402241	127
i10	72.818141	0.034467	184.997211	127
i10	72.818141	0.034467	92.498605	110
i10	72.954649	0.034240	65.406391	98
i10	72.954649	0.034240	103.823246	46
i10	73.090930	0.034467	92.498605	107
i10	73.227438	0.034240	65.406391	127
i10	73.227438	0.034240	103.823246	43
i10	73.363719	0.034240	82.402241	127
i10	73.363719	0.034240	184.997211	127
i10	73.363719	0.034240	92.498605	104
i10	73.500000	0.034467	65.406391	95
i10	73.500000	0.034467	103.823246	40
i10	73.636508	0.034240	65.406391	127
i10	73.636508	0.034240	92.498605	101
i10	73.772789	0.034240	103.823246	40
i10	73.909070	0.034467	82.402241	127
i10	73.909070	0.034467	184.997211	127
i10	73.909070	0.034467	92.498605	107
i10	74.045578	0.034240	103.823246	40
i10	74.181859	0.034240	65.406391	127
i10	74.181859	0.034240	92.498605	110
i10	74.318141	0.034467	103.823246	40
i10	74.454649	0.034240	82.402241	127
i10	74.454649	0.034240	184.997211	127
i10	74.454649	0.034240	92.498605	107
i10	74.590930	0.034467	103.823246	43
i10	74.727438	0.034240	65.406391	127
i10	74.727438	0.034240	92.498605	110
i10	74.863719	0.034240	103.823246	43
i10	75.000000	0.034467	82.402241	127
i10	75.000000	0.034467	184.997211	127
i10	75.000000	0.034467	92.498605	110
i10	75.136508	0.034240	65.406391	98
i10	75.136508	0.034240	103.823246	46
i10	75.272789	0.034240	92.498605	107
i10	75.409070	0.034467	65.406391	81
i10	75.409070	0.034467	103.823246	43
i10	75.545578	0.034240	82.402241	127
i10	75.545578	0.034240	184.997211	127
i10	75.545578	0.034240	92.498605	104
i10	75.681859	0.034240	65.406391	95
i10	75.681859	0.034240	103.823246	40
i10	75.818141	0.034467	65.406391	127
i10	75.818141	0.034467	82.402241	115
i10	75.818141	0.034467	92.498605	101
i10	75.954649	0.034240	82.402241	107
i10	75.954649	0.034240	82.402241	107
i10	75.954649	0.034240	103.823246	40
i10	76.090930	0.034467	82.402241	127
i10	76.090930	0.034467	184.997211	127
i10	76.090930	0.034467	92.498605	107
i10	76.227438	0.034240	82.402241	107
i10	76.227438	0.034240	103.823246	40
i10	76.363719	0.034240	65.406391	127
i10	76.363719	0.034240	92.498605	110
i10	76.363719	0.136508	138.583497	84
i10	76.500000	0.034467	103.823246	40
i10	76.636508	0.034240	82.402241	127
i10	76.636508	0.034240	184.997211	127
i10	76.636508	0.034240	92.498605	107
i10	76.772789	0.034240	103.823246	43
i10	76.909070	0.034467	65.406391	127
i10	76.909070	0.034467	92.498605	110
i10	77.045578	0.034240	103.823246	43
i10	77.181859	0.034240	82.402241	127
i10	77.181859	0.034240	184.997211	127
i10	77.181859	0.034240	92.498605	110
i10	77.318141	0.034467	65.406391	98
i10	77.318141	0.034467	103.823246	46
i10	77.454649	0.034240	92.498605	107
i10	77.590930	0.034467	65.406391	127
i10	77.590930	0.034467	103.823246	43
i10	77.727438	0.034240	82.402241	127
i10	77.727438	0.034240	184.997211	127
i10	77.727438	0.034240	92.498605	104
i10	77.863719	0.034240	65.406391	95
i10	77.863719	0.034240	103.823246	40
i10	78.000000	0.034467	65.406391	127
i10	78.000000	0.034467	92.498605	101
i10	78.136508	0.034240	103.823246	40
i10	78.272789	0.034240	82.402241	127
i10	78.272789	0.034240	184.997211	127
i10	78.272789	0.034240	92.498605	107
i10	78.409070	0.034467	103.823246	40
i10	78.545578	0.034240	65.406391	127
i10	78.545578	0.034240	92.498605	110
i10	78.681859	0.034240	103.823246	40
i10	78.818141	0.034467	82.402241	127
i10	78.818141	0.034467	184.997211	127
i10	78.818141	0.034467	92.498605	107
i10	78.954649	0.034240	103.823246	43
i10	79.090930	0.034467	65.406391	127
i10	79.090930	0.034467	92.498605	110
i10	79.227438	0.034240	103.823246	43
i10	79.363719	0.034240	82.402241	127
i10	79.363719	0.034240	184.997211	127
i10	79.363719	0.034240	92.498605	110
i10	79.500000	0.034467	65.406391	98
i10	79.500000	0.034467	103.823246	46
i10	79.636508	0.034240	92.498605	107
i10	79.772789	0.034240	65.406391	127
i10	79.772789	0.034240	103.823246	43
i10	79.909070	0.034467	82.402241	127
i10	79.909070	0.034467	184.997211	127
i10	79.909070	0.034467	92.498605	104
i10	80.045578	0.034240	65.406391	95
i10	80.045578	0.034240	103.823246	40
i10	80.181859	0.034240	65.406391	127
i10	80.181859	0.034240	92.498605	101
i10	80.318141	0.034467	103.823246	40
i10	80.454649	0.034240	82.402241	127
i10	80.454649	0.034240	184.997211	127
i10	80.454649	0.034240	92.498605	107
i10	80.590930	0.034467	103.823246	40
i10	80.727438	0.034240	65.406391	127
i10	80.727438	0.034240	92.498605	110
i10	80.863719	0.034240	103.823246	40
i10	81.000000	0.034467	82.402241	127
i10	81.000000	0.034467	184.997211	127
i10	81.000000	0.034467	92.498605	107
i10	81.136508	0.034240	103.823246	43
i10	81.272789	0.034240	65.406391	127
i10	81.272789	0.034240	92.498605	110
i10	81.409070	0.034467	103.823246	43
i10	81.545578	0.034240	82.402241	127
i10	81.545578	0.034240	184.997211	127
i10	81.545578	0.034240	92.498605	110
i10	81.681859	0.034240	65.406391	98
i10	81.681859	0.034240	103.823246	46
i10	81.818141	0.034467	92.498605	107
i10	81.954649	0.034240	65.406391	127
i10	81.954649	0.034240	103.823246	43
i10	82.090930	0.034467	82.402241	127
i10	82.090930	0.034467	184.997211	127
i10	82.090930	0.034467	92.498605	104
i10	82.227438	0.034240	65.406391	95
i10	82.227438	0.034240	103.823246	40
i10	82.363719	0.034240	65.406391	127
i10	82.363719	0.034240	92.498605	101
i10	82.500000	0.034467	103.823246	40
i10	82.636508	0.034240	82.402241	127
i10	82.636508	0.034240	184.997211	127
i10	82.636508	0.034240	92.498605	107
i10	82.772789	0.034240	103.823246	40
i10	82.909070	0.034467	65.406391	127
i10	82.909070	0.034467	92.498605	110
i10	83.045578	0.034240	103.823246	40
i10	83.181859	0.034240	82.402241	127
i10	83.181859	0.034240	184.997211	127
i10	83.181859	0.034240	92.498605	107
i10	83.318141	0.034467	103.823246	43
i10	83.454649	0.034240	65.406391	127
i10	83.454649	0.034240	92.498605	110
i10	83.590930	0.034467	103.823246	43
i10	83.727438	0.034240	82.402241	127
i10	83.727438	0.034240	184.997211	127
i10	83.727438	0.034240	92.498605	110
i10	83.863719	0.034240	65.406391	98
i10	83.863719	0.034240	103.823246	46
i10	84.000000	0.034467	92.498605	107
i10	84.136508	0.034240	82.402241	101
i10	84.136508	0.034240	65.406391	127
i10	84.136508	0.034240	103.823246	43
i10	84.272789	0.034240	82.402241	127
i10	84.272789	0.034240	184.997211	127
i10	84.272789	0.034240	92.498605	104
i10	84.409070	0.034467	65.406391	95
i10	84.409070	0.034467	103.823246	40
i10	84.545578	0.034240	65.406391	127
i10	84.545578	0.034240	92.498605	101
i10	84.681859	0.034240	103.823246	40
i10	84.818141	0.034467	82.402241	127
i10	84.818141	0.034467	138.583497	99
i10	84.818141	0.034467	184.997211	127
i10	84.818141	0.034467	116.534366	113
i10	85.090930	0.034467	65.406391	127
i10	85.090930	0.034467	92.498605	110
i10	85.227438	0.034240	103.823246	40
i10	85.363719	0.034240	82.402241	127
i10	85.363719	0.034240	184.997211	127
i10	85.363719	0.034240	92.498605	107
i10	85.500000	0.034467	103.823246	43
i10	85.636508	0.034240	65.406391	127
i10	85.636508	0.034240	92.498605	110
i10	85.772789	0.034240	103.823246	43
i10	85.909070	0.034467	82.402241	127
i10	85.909070	0.034467	184.997211	127
i10	85.909070	0.034467	92.498605	110
i10	86.045578	0.034240	65.406391	98
i10	86.045578	0.034240	103.823246	46
i10	86.181859	0.034240	92.498605	107
i10	86.318141	0.034467	65.406391	127
i10	86.318141	0.034467	103.823246	43
i10	86.454649	0.034240	82.402241	127
i10	86.454649	0.034240	184.997211	127
i10	86.454649	0.034240	92.498605	104
i10	86.590930	0.034467	65.406391	95
i10	86.590930	0.034467	103.823246	40
i10	86.727438	0.034240	65.406391	127
i10	86.727438	0.034240	92.498605	101
i10	86.863719	0.034240	103.823246	40
i10	87.000000	0.034467	82.402241	127
i10	87.000000	0.034467	184.997211	127
i10	87.000000	0.034467	92.498605	107
i10	87.136508	0.034240	103.823246	40
i10	87.272789	0.034240	65.406391	127
i10	87.272789	0.034240	92.498605	110
i10	87.409070	0.034467	103.823246	40
i10	87.545578	0.034240	82.402241	127
i10	87.545578	0.034240	184.997211	127
i10	87.545578	0.034240	92.498605	107
i10	87.681859	0.034240	103.823246	43
i10	87.818367	0.034240	65.406391	127
i10	87.818367	0.034240	92.498605	110
i10	87.954649	0.034240	103.823246	43
i10	88.090930	0.034467	82.402241	127
i10	88.090930	0.034467	184.997211	127
i10	88.090930	0.034467	92.498605	110
i10	88.227438	0.034240	65.406391	98
i10	88.227438	0.034240	103.823246	46
i10	88.363719	0.034240	92.498605	107
i10	88.500000	0.034467	65.406391	127
i10	88.500000	0.034467	103.823246	43
i10	88.636508	0.034240	82.402241	127
i10	88.636508	0.034240	184.997211	127
i10	88.636508	0.034240	92.498605	104
i10	88.772789	0.034240	65.406391	95
i10	88.772789	0.034240	103.823246	40
i10	88.909070	0.034467	65.406391	127
i10	88.909070	0.034467	92.498605	101
i10	89.045578	0.034240	103.823246	40
i10	89.181859	0.034240	82.402241	127
i10	89.181859	0.034240	219.999999	99
i10	89.181859	0.034240	184.997211	127
i10	89.181859	0.034240	92.498605	107
i10	89.318367	0.034240	103.823246	40
i10	89.454649	0.034240	65.406391	127
i10	89.454649	0.034240	138.583497	99
i10	89.454649	0.034240	92.498605	110
i10	89.590930	0.034467	103.823246	40
i10	89.727438	0.034240	82.402241	127
i10	89.727438	0.034240	184.997211	127
i10	89.727438	0.034240	92.498605	107
i10	89.863719	0.034240	103.823246	43
i10	90.000000	0.034467	65.406391	127
i10	90.000000	0.034467	92.498605	110
i10	90.136508	0.034240	103.823246	43
i10	90.272789	0.034240	82.402241	127
i10	90.272789	0.034240	184.997211	127
i10	90.272789	0.034240	92.498605	110
i10	90.409070	0.034467	65.406391	98
i10	90.409070	0.034467	103.823246	46
i10	90.545578	0.034240	92.498605	107
i10	90.681859	0.034240	65.406391	127
i10	90.681859	0.034240	103.823246	43
i10	90.818367	0.034240	82.402241	127
i10	90.818367	0.034240	184.997211	127
i10	90.818367	0.034240	92.498605	104
i10	90.954649	0.034240	65.406391	95
i10	90.954649	0.034240	103.823246	40
i10	91.090930	0.034467	65.406391	127
i10	91.090930	0.034467	92.498605	101
i10	91.227438	0.034240	103.823246	40
i10	91.363719	0.034240	82.402241	127
i10	91.363719	0.034240	184.997211	127
i10	91.363719	0.034240	92.498605	107
i10	91.500000	0.034467	103.823246	40
i10	91.636508	0.034240	65.406391	127
i10	91.636508	0.034240	92.498605	110
i10	91.772789	0.034240	103.823246	40
i10	91.909070	0.034467	82.402241	127
i10	91.909070	0.034467	184.997211	127
i10	91.909070	0.034467	92.498605	107
i10	92.045578	0.034240	103.823246	43
i10	92.181859	0.034240	65.406391	127
i10	92.181859	0.034240	92.498605	110
i10	92.318367	0.034240	103.823246	43
i10	92.454649	0.034240	82.402241	127
i10	92.454649	0.034240	184.997211	127
i10	92.454649	0.034240	92.498605	110
i10	92.590930	0.034467	65.406391	98
i10	92.590930	0.034467	103.823246	46
i10	92.727438	0.034240	92.498605	107
i10	92.863719	0.034240	82.402241	81
i10	92.863719	0.034240	65.406391	81
i10	92.863719	0.034240	103.823246	43
i10	93.000000	0.034467	82.402241	127
i10	93.000000	0.034467	184.997211	127
i10	93.000000	0.034467	92.498605	104
i10	93.136508	0.034240	65.406391	95
i10	93.136508	0.034240	103.823246	40
i10	93.272789	0.034240	65.406391	127
i10	93.272789	0.034240	92.498605	101
i10	93.409070	0.034467	82.402241	87
i10	93.409070	0.034467	103.823246	40
i10	93.545578	0.034240	82.402241	127
i10	93.545578	0.034240	219.999999	99
i10	93.545578	0.034240	184.997211	127
i10	93.545578	0.034240	116.534366	113
i10	93.818367	0.034240	65.406391	127
i10	93.818367	0.034240	138.583497	99
i10	93.818367	0.034240	92.498605	110
i10	93.954649	0.034240	103.823246	40
i10	94.090930	0.034467	82.402241	127
i10	94.090930	0.034467	184.997211	127
i10	94.090930	0.034467	92.498605	107
i10	94.227438	0.034240	103.823246	43
i10	94.363719	0.034240	65.406391	127
i10	94.363719	0.034240	92.498605	110
i10	94.500000	0.034467	103.823246	43
i10	94.636508	0.034240	82.402241	127
i10	94.636508	0.034240	184.997211	127
i10	94.636508	0.034240	92.498605	110
i10	94.772789	0.034240	65.406391	98
i10	94.772789	0.034240	103.823246	46
i10	94.909070	0.034467	92.498605	107
i10	95.045578	0.034240	65.406391	127
i10	95.045578	0.034240	103.823246	43
i10	95.181859	0.034467	82.402241	127
i10	95.181859	0.034467	184.997211	127
i10	95.181859	0.034467	92.498605	104
i10	95.318367	0.034240	65.406391	95
i10	95.318367	0.034240	103.823246	40
i10	95.454649	0.034240	65.406391	127
i10	95.454649	0.034240	92.498605	101
i10	95.590930	0.034467	103.823246	40
i10	95.727438	0.034240	82.402241	127
i10	95.727438	0.034240	184.997211	127
i10	95.727438	0.034240	92.498605	107
i10	95.863719	0.034240	103.823246	40
i10	96.000000	0.034467	65.406391	127
i10	96.000000	0.034467	92.498605	110
i10	96.136508	0.034240	103.823246	40
i10	96.272789	0.034240	82.402241	127
i10	96.272789	0.034240	184.997211	127
i10	96.272789	0.034240	92.498605	107
i10	96.409070	0.034467	103.823246	43
i10	96.545578	0.034240	65.406391	127
i10	96.545578	0.034240	92.498605	110
i10	96.681859	0.034467	103.823246	43
i10	96.818367	0.034240	82.402241	127
i10	96.818367	0.034240	184.997211	127
i10	96.818367	0.034240	92.498605	110
i10	96.954649	0.034240	65.406391	98
i10	96.954649	0.034240	103.823246	46
i10	97.090930	0.034467	92.498605	107
i10	97.227438	0.034240	65.406391	127
i10	97.227438	0.034240	103.823246	43
i10	97.363719	0.034240	82.402241	127
i10	97.363719	0.034240	184.997211	127
i10	97.363719	0.034240	92.498605	104
i10	97.500000	0.034467	65.406391	95
i10	97.500000	0.034467	103.823246	40
i10	97.636508	0.034240	65.406391	127
i10	97.636508	0.034240	92.498605	101
i10	97.772789	0.034240	103.823246	40
i10	97.909070	0.034467	82.402241	127
i10	97.909070	0.034467	184.997211	127
i10	97.909070	0.034467	92.498605	107
i10	98.045578	0.034240	103.823246	40
i10	98.181859	0.034467	65.406391	127
i10	98.181859	0.034467	92.498605	110
i10	98.318367	0.034240	103.823246	40
i10	98.454649	0.034240	82.402241	127
i10	98.454649	0.034240	184.997211	127
i10	98.454649	0.034240	92.498605	107
i10	98.590930	0.034467	103.823246	43
i10	98.727438	0.034240	65.406391	127
i10	98.727438	0.034240	92.498605	110
i10	98.863719	0.034240	103.823246	43
i10	99.000000	0.034467	82.402241	127
i10	99.000000	0.034467	184.997211	127
i10	99.000000	0.034467	92.498605	110
i10	99.136508	0.034240	65.406391	98
i10	99.136508	0.034240	103.823246	46
i10	99.272789	0.034240	92.498605	107
i10	99.409070	0.034467	65.406391	127
i10	99.409070	0.034467	103.823246	43
i10	99.545578	0.034240	82.402241	127
i10	99.545578	0.034240	184.997211	127
i10	99.545578	0.034240	92.498605	104
i10	99.681859	0.034467	65.406391	95
i10	99.681859	0.034467	103.823246	40
i10	99.818367	0.034240	65.406391	127
i10	99.818367	0.034240	92.498605	101
i10	99.954649	0.034240	103.823246	40
i10	100.090930	0.034467	82.402241	127
i10	100.090930	0.034467	184.997211	127
i10	100.090930	0.034467	92.498605	107
i10	100.227438	0.034240	103.823246	40
i10	100.363719	0.034240	65.406391	127
i10	100.363719	0.034240	92.498605	110
i10	100.500000	0.034467	103.823246	40
i10	100.636508	0.034240	82.402241	127
i10	100.636508	0.034240	184.997211	127
i10	100.636508	0.034240	92.498605	107
i10	100.772789	0.034240	103.823246	43
i10	100.909070	0.034467	65.406391	127
i10	100.909070	0.034467	92.498605	110
i10	101.045578	0.034240	103.823246	43
i10	101.181859	0.034467	82.402241	127
i10	101.181859	0.034467	184.997211	127
i10	101.181859	0.034467	92.498605	110
i10	101.318367	0.034240	65.406391	98
i10	101.318367	0.034240	103.823246	46
i10	101.454649	0.034240	92.498605	107
i10	101.590930	0.034467	65.406391	81
i10	101.590930	0.034467	103.823246	43
i10	101.727438	0.034240	82.402241	127
i10	101.727438	0.034240	184.997211	127
i10	101.727438	0.034240	92.498605	104
i10	101.863719	0.034240	65.406391	95
i10	101.863719	0.034240	116.534366	93
i10	102.000000	0.034467	65.406391	127
i10	102.000000	0.034467	82.402241	115
i10	102.136508	0.034240	82.402241	107
i10	102.136508	0.034240	82.402241	107
i10	102.136508	0.034240	103.823246	40
i10	102.272789	0.034240	82.402241	127
i10	102.272789	0.034240	138.583497	91
i10	102.272789	0.034240	184.997211	127
i10	102.272789	0.034240	92.498605	113
i10	102.409070	0.034467	82.402241	107
i10	102.409070	0.034467	103.823246	40
i10	102.545578	0.034240	65.406391	127
i10	102.545578	0.034240	219.999999	79
i10	102.545578	0.034240	92.498605	110
i10	102.681859	0.034467	103.823246	40
i10	102.818367	0.034240	82.402241	127
i10	102.818367	0.034240	184.997211	127
i10	102.818367	0.034240	92.498605	107
i10	102.954649	0.034240	103.823246	43
i10	103.090930	0.034467	65.406391	127
i10	103.090930	0.034467	92.498605	110
i10	103.227438	0.034240	103.823246	43
i10	103.363719	0.034240	82.402241	127
i10	103.363719	0.034240	184.997211	127
i10	103.363719	0.034240	92.498605	110
i10	103.500000	0.034467	65.406391	98
i10	103.500000	0.034467	103.823246	46
i10	103.636508	0.034240	92.498605	107
i10	103.772789	0.034240	65.406391	127
i10	103.772789	0.034240	103.823246	43
i10	103.909070	0.034467	82.402241	127
i10	103.909070	0.034467	184.997211	127
i10	103.909070	0.034467	92.498605	104
i10	104.045578	0.034240	65.406391	95
i10	104.045578	0.034240	103.823246	40
i10	104.181859	0.034467	65.406391	127
i10	104.181859	0.034467	92.498605	101
i10	104.318367	0.034240	103.823246	40
i10	104.454649	0.034240	82.402241	127
i10	104.454649	0.034240	184.997211	127
i10	104.454649	0.034240	92.498605	107
i10	104.590930	0.034467	103.823246	40
i10	104.727438	0.034240	65.406391	127
i10	104.727438	0.034240	92.498605	110
i10	104.863719	0.034240	103.823246	40
i10	105.000000	0.034467	82.402241	127
i10	105.000000	0.034467	184.997211	127
i10	105.000000	0.034467	92.498605	107
i10	105.136508	0.034240	103.823246	43
i10	105.272789	0.034240	65.406391	127
i10	105.272789	0.034240	92.498605	110
i10	105.409070	0.034467	103.823246	43
i10	105.545578	0.034240	82.402241	127
i10	105.545578	0.034240	184.997211	127
i10	105.545578	0.034240	92.498605	110
i10	105.681859	0.034467	65.406391	98
i10	105.681859	0.034467	103.823246	46
i10	105.818367	0.034240	92.498605	107
i10	105.954649	0.034240	65.406391	127
i10	105.954649	0.034240	103.823246	43
i10	106.090930	0.034467	82.402241	127
i10	106.090930	0.034467	184.997211	127
i10	106.090930	0.034467	92.498605	104
i10	106.227438	0.034240	65.406391	95
i10	106.227438	0.034240	103.823246	40
i10	106.363719	0.034240	65.406391	127
i10	106.363719	0.034240	92.498605	101
i10	106.500000	0.034467	103.823246	40
i10	106.636508	0.034240	82.402241	127
i10	106.636508	0.034240	184.997211	127
i10	106.636508	0.034240	92.498605	107
i10	106.772789	0.034240	103.823246	40
i10	106.909070	0.034467	65.406391	127
i10	106.909070	0.034467	92.498605	110
i10	107.045578	0.034240	103.823246	40
i10	107.181859	0.034467	82.402241	127
i10	107.181859	0.034467	184.997211	127
i10	107.181859	0.034467	92.498605	107
i10	107.318367	0.034240	103.823246	43
i10	107.454649	0.034240	65.406391	127
i10	107.454649	0.034240	92.498605	110
i10	107.590930	0.034467	103.823246	43
i10	107.727438	0.034240	82.402241	127
i10	107.727438	0.034240	184.997211	127
i10	107.727438	0.034240	92.498605	110
i10	107.863719	0.034240	65.406391	98
i10	107.863719	0.034240	103.823246	46
i10	108.000000	0.034467	92.498605	107
i10	108.136508	0.034240	65.406391	127
i10	108.136508	0.034240	103.823246	43
i10	108.272789	0.034240	82.402241	127
i10	108.272789	0.034240	184.997211	127
i10	108.272789	0.034240	92.498605	104
i10	108.409070	0.034467	65.406391	95
i10	108.409070	0.034467	103.823246	40
i10	108.545578	0.034240	65.406391	127
i10	108.545578	0.034240	92.498605	101
i10	108.681859	0.034467	103.823246	40
i10	108.818367	0.034240	82.402241	127
i10	108.818367	0.034240	184.997211	127
i10	108.818367	0.034240	92.498605	107
i10	108.954649	0.034240	103.823246	40
i10	109.090930	0.034467	65.406391	127
i10	109.090930	0.034467	92.498605	110
i10	109.227438	0.034240	103.823246	40
i10	109.363719	0.034240	82.402241	127
i10	109.363719	0.034240	184.997211	127
i10	109.363719	0.034240	92.498605	107
i10	109.500000	0.034467	103.823246	43
i10	109.636508	0.034240	65.406391	127
i10	109.636508	0.034240	92.498605	110
i10	109.772789	0.034240	103.823246	43
i10	109.909070	0.034467	82.402241	127
i10	109.909070	0.034467	184.997211	127
i10	109.909070	0.034467	92.498605	110
i10	110.045578	0.034240	65.406391	98
i10	110.045578	0.034240	103.823246	46
i10	110.181859	0.034467	92.498605	107
i10	110.318367	0.034240	65.406391	81
i10	110.318367	0.034240	103.823246	43
i10	110.454649	0.034240	82.402241	127
i10	110.454649	0.034240	184.997211	127
i10	110.454649	0.034240	92.498605	104
i10	110.590930	0.034467	65.406391	95
i10	110.590930	0.034467	116.534366	93
i10	110.727438	0.034240	65.406391	127
i10	110.727438	0.034240	82.402241	115
i10	110.863719	0.034240	82.402241	107
i10	110.863719	0.034240	82.402241	107
i10	110.863719	0.034240	103.823246	40
i10	111.000000	0.034467	82.402241	127
i10	111.000000	0.034467	184.997211	127
i10	111.000000	0.034467	92.498605	113
i10	111.136508	0.034240	82.402241	107
i10	111.136508	0.034240	103.823246	40
i10	111.272789	0.034240	65.406391	127
i10	111.272789	0.034240	138.583497	99
i10	111.272789	0.034240	92.498605	110
i10	111.409297	0.034240	103.823246	40
i10	111.545578	0.034240	82.402241	127
i10	111.545578	0.034240	92.498605	107
i10	111.681859	0.034467	103.823246	43
i10	111.818367	0.034240	65.406391	127
i10	111.818367	0.034240	92.498605	110
i10	111.954649	0.034240	103.823246	43
i10	112.090930	0.034467	82.402241	127
i10	112.090930	0.034467	92.498605	110
i10	112.227438	0.034240	65.406391	98
i10	112.227438	0.034240	103.823246	46
i10	112.363719	0.034240	92.498605	107
i10	112.500000	0.034467	65.406391	127
i10	112.500000	0.034467	103.823246	43
i10	112.636508	0.034240	82.402241	127
i10	112.636508	0.034240	92.498605	104
i10	112.772789	0.034240	65.406391	95
i10	112.772789	0.034240	103.823246	40
i10	112.909297	0.034240	65.406391	127
i10	112.909297	0.034240	92.498605	101
i10	113.045578	0.034240	103.823246	40
i10	113.181859	0.034467	82.402241	127
i10	113.181859	0.034467	92.498605	107
i10	113.318367	0.034240	103.823246	40
i10	113.454649	0.034240	65.406391	127
i10	113.454649	0.034240	219.999999	107
i10	113.454649	0.034240	92.498605	110
i10	113.590930	0.034467	103.823246	40
i10	113.727438	0.034240	82.402241	127
i10	113.727438	0.034240	92.498605	107
i10	113.863719	0.034240	103.823246	43
i10	114.000000	0.034467	65.406391	127
i10	114.000000	0.034467	92.498605	110
i10	114.136508	0.034240	103.823246	43
i10	114.272789	0.034240	82.402241	127
i10	114.272789	0.034240	92.498605	110
i10	114.409297	0.034240	65.406391	98
i10	114.409297	0.034240	103.823246	46
i10	114.545578	0.034240	92.498605	107
i10	114.681859	0.034467	65.406391	127
i10	114.681859	0.034467	103.823246	43
i10	114.818367	0.034240	82.402241	127
i10	114.818367	0.034240	92.498605	104
i10	114.954649	0.034240	65.406391	95
i10	114.954649	0.034240	103.823246	40
i10	115.090930	0.034467	65.406391	127
i10	115.090930	0.034467	92.498605	101
i10	115.227438	0.034240	103.823246	40
i10	115.363719	0.034240	82.402241	127
i10	115.363719	0.034240	92.498605	107
i10	115.500000	0.034467	103.823246	40
i10	115.636508	0.034240	65.406391	127
i10	115.636508	0.034240	138.583497	91
i10	115.636508	0.034240	92.498605	110
i10	115.772789	0.034240	103.823246	40
i10	115.909297	0.034240	82.402241	127
i10	115.909297	0.034240	92.498605	107
i10	116.045578	0.034240	103.823246	43
i10	116.181859	0.034467	65.406391	127
i10	116.181859	0.034467	92.498605	110
i10	116.318367	0.034240	103.823246	43
i10	116.454649	0.034240	82.402241	127
i10	116.454649	0.034240	92.498605	110
i10	116.590930	0.034467	65.406391	98
i10	116.590930	0.034467	103.823246	46
i10	116.727438	0.034240	92.498605	107
i10	116.863719	0.034240	65.406391	127
i10	116.863719	0.034240	103.823246	43
i10	117.000000	0.034467	82.402241	127
i10	117.000000	0.034467	92.498605	104
i10	117.136508	0.034240	65.406391	95
i10	117.136508	0.034240	103.823246	40
i10	117.272789	0.034240	65.406391	127
i10	117.272789	0.034240	92.498605	101
i10	117.409297	0.034240	103.823246	40
i10	117.545578	0.034240	82.402241	127
i10	117.545578	0.034240	92.498605	107
i10	117.681859	0.034467	103.823246	40
i10	117.818367	0.034240	65.406391	127
i10	117.818367	0.034240	219.999999	99
i10	117.818367	0.034240	92.498605	110
i10	117.954649	0.034240	103.823246	40
i10	118.090930	0.034467	82.402241	127
i10	118.090930	0.034467	92.498605	107
i10	118.227438	0.034240	103.823246	43
i10	118.363719	0.034240	65.406391	127
i10	118.363719	0.034240	92.498605	110
i10	118.500000	0.034467	103.823246	43
i10	118.636508	0.034240	82.402241	127
i10	118.636508	0.034240	92.498605	110
i10	118.772789	0.034467	65.406391	98
i10	118.772789	0.034467	103.823246	46
i10	118.909297	0.034240	92.498605	107
i10	119.045578	0.034240	65.406391	81
i10	119.045578	0.034240	103.823246	43
i10	119.181859	0.034467	82.402241	127
i10	119.181859	0.034467	92.498605	104
i10	119.318367	0.034240	65.406391	95
i10	119.318367	0.034240	103.823246	40
i10	119.454649	0.034240	65.406391	127
i10	119.454649	0.034240	92.498605	101
i10	119.590930	0.034467	103.823246	40
i10	119.727438	0.034240	82.402241	127
i10	119.727438	0.034240	116.534366	113
i10	120.000000	0.034467	65.406391	127
i10	120.000000	0.034467	138.583497	79
i10	120.000000	0.034467	92.498605	110
i10	120.136508	0.034240	103.823246	40
i10	120.272789	0.034467	82.402241	127
i10	120.272789	0.034467	92.498605	107
i10	120.409297	0.034240	103.823246	43
i10	120.545578	0.034240	65.406391	127
i10	120.545578	0.034240	92.498605	110
i10	120.681859	0.034467	103.823246	43
i10	120.818367	0.034240	82.402241	127
i10	120.818367	0.034240	92.498605	110
i10	120.954649	0.034240	65.406391	98
i10	120.954649	0.034240	103.823246	46
i10	121.090930	0.034467	92.498605	107
i10	121.227438	0.034240	65.406391	127
i10	121.227438	0.034240	103.823246	43
i10	121.363719	0.034240	82.402241	127
i10	121.363719	0.034240	92.498605	104
i10	121.500000	0.034467	65.406391	95
i10	121.500000	0.034467	103.823246	40
i10	121.636508	0.034240	65.406391	127
i10	121.636508	0.034240	92.498605	101
i10	121.772789	0.034467	103.823246	40
i10	121.909297	0.034240	82.402241	127
i10	121.909297	0.034240	92.498605	107
i10	122.045578	0.034240	103.823246	40
i10	122.181859	0.034467	65.406391	127
i10	122.181859	0.034467	219.999999	99
i10	122.181859	0.034467	92.498605	110
i10	122.318367	0.034240	103.823246	40
i10	122.454649	0.034240	82.402241	127
i10	122.454649	0.034240	92.498605	107
i10	122.590930	0.034467	103.823246	43
i10	122.727438	0.034240	65.406391	127
i10	122.727438	0.034240	92.498605	110
i10	122.863719	0.034240	103.823246	43
i10	123.000000	0.034467	82.402241	127
i10	123.000000	0.034467	92.498605	110
i10	123.136508	0.034240	65.406391	98
i10	123.136508	0.034240	103.823246	46
i10	123.272789	0.034467	92.498605	107
i10	123.409297	0.034240	65.406391	81
i10	123.409297	0.034240	103.823246	43
i10	123.545578	0.034240	82.402241	127
i10	123.545578	0.034240	92.498605	104
i10	123.681859	0.034467	65.406391	95
i10	123.681859	0.034467	116.534366	93
i10	123.818367	0.034240	65.406391	127
i10	123.818367	0.034240	82.402241	115
i10	123.954649	0.034240	82.402241	107
i10	123.954649	0.034240	82.402241	107
i10	123.954649	0.034240	103.823246	40
i10	124.090930	0.034467	82.402241	127
i10	124.090930	0.034467	92.498605	113
i10	124.227438	0.034240	82.402241	107
i10	124.227438	0.034240	103.823246	40
i10	124.363719	0.034240	65.406391	127
i10	124.363719	0.034240	92.498605	110
i10	124.363719	0.113832	138.583497	107
i10	124.500000	0.034467	103.823246	40
i10	124.636508	0.034240	82.402241	127
i10	124.636508	0.034240	184.997211	127
i10	124.636508	0.034240	92.498605	107
i10	124.772789	0.034467	103.823246	43
i10	124.909297	0.034240	65.406391	127
i10	124.909297	0.034240	92.498605	110
i10	125.045578	0.034240	103.823246	43
i10	125.181859	0.034467	82.402241	127
i10	125.181859	0.034467	184.997211	127
i10	125.181859	0.034467	92.498605	110
i10	125.318367	0.034240	65.406391	98
i10	125.318367	0.034240	103.823246	46
i10	125.454649	0.034240	92.498605	107
i10	125.590930	0.034467	65.406391	127
i10	125.590930	0.034467	103.823246	43
i10	125.727438	0.034240	82.402241	127
i10	125.727438	0.034240	184.997211	127
i10	125.727438	0.034240	92.498605	104
i10	125.863719	0.034240	65.406391	95
i10	125.863719	0.034240	103.823246	40
i10	126.000000	0.034467	65.406391	127
i10	126.000000	0.034467	92.498605	101
i10	126.136508	0.034240	103.823246	40
i10	126.272789	0.034467	82.402241	127
i10	126.272789	0.034467	184.997211	127
i10	126.272789	0.034467	92.498605	107
i10	126.409297	0.034240	103.823246	40
i10	126.545578	0.034240	65.406391	127
i10	126.545578	0.034240	92.498605	110
i10	126.681859	0.034467	103.823246	40
i10	126.818367	0.034240	82.402241	127
i10	126.818367	0.034240	184.997211	127
i10	126.818367	0.034240	92.498605	107
i10	126.954649	0.034240	103.823246	43
i10	127.090930	0.034467	65.406391	127
i10	127.090930	0.034467	92.498605	110
i10	127.227438	0.034240	103.823246	43
i10	127.363719	0.034240	82.402241	127
i10	127.363719	0.034240	184.997211	127
i10	127.363719	0.034240	92.498605	110
i10	127.500000	0.034467	65.406391	98
i10	127.500000	0.034467	103.823246	46
i10	127.636508	0.034240	92.498605	107
i10	127.772789	0.034467	65.406391	127
i10	127.772789	0.034467	103.823246	43
i10	127.909297	0.034240	82.402241	127
i10	127.909297	0.034240	184.997211	127
i10	127.909297	0.034240	92.498605	104
i10	128.045578	0.034240	65.406391	95
i10	128.045578	0.034240	103.823246	40
i10	128.181859	0.034467	65.406391	127
i10	128.181859	0.034467	92.498605	101
i10	128.318367	0.034240	103.823246	40
i10	128.454649	0.034240	82.402241	127
i10	128.454649	0.034240	184.997211	127
i10	128.454649	0.034240	92.498605	107
i10	128.590930	0.034467	103.823246	40
i10	128.727438	0.034240	65.406391	127
i10	128.727438	0.034240	92.498605	110
i10	128.863719	0.034240	103.823246	40
i10	129.000000	0.034467	82.402241	127
i10	129.000000	0.034467	184.997211	127
i10	129.000000	0.034467	92.498605	107
i10	129.136508	0.034240	103.823246	43
i10	129.272789	0.034467	65.406391	127
i10	129.272789	0.034467	92.498605	110
i10	129.409297	0.034240	103.823246	43
i10	129.545578	0.034240	82.402241	127
i10	129.545578	0.034240	184.997211	127
i10	129.545578	0.034240	92.498605	110
i10	129.681859	0.034467	65.406391	98
i10	129.681859	0.034467	103.823246	46
i10	129.818367	0.034240	92.498605	107
i10	129.954649	0.034240	65.406391	127
i10	129.954649	0.034240	103.823246	43
i10	130.090930	0.034467	82.402241	127
i10	130.090930	0.034467	184.997211	127
i10	130.090930	0.034467	92.498605	104
i10	130.227438	0.034240	65.406391	95
i10	130.227438	0.034240	103.823246	40
i10	130.363719	0.034240	65.406391	127
i10	130.363719	0.034240	92.498605	101
i10	130.500000	0.034467	103.823246	40
i10	130.636508	0.034240	82.402241	127
i10	130.636508	0.034240	184.997211	127
i10	130.636508	0.034240	92.498605	107
i10	130.772789	0.034467	103.823246	40
i10	130.909297	0.034240	65.406391	127
i10	130.909297	0.034240	92.498605	110
i10	131.045578	0.034240	103.823246	40
i10	131.181859	0.034467	82.402241	127
i10	131.181859	0.034467	184.997211	127
i10	131.181859	0.034467	92.498605	107
i10	131.318367	0.034240	103.823246	43
i10	131.454649	0.034240	65.406391	127
i10	131.454649	0.034240	92.498605	110
i10	131.590930	0.034467	103.823246	43
i10	131.727438	0.034240	82.402241	127
i10	131.727438	0.034240	184.997211	127
i10	131.727438	0.034240	92.498605	110
i10	131.863719	0.034240	65.406391	98
i10	131.863719	0.034240	103.823246	46
i10	132.000000	0.034467	92.498605	107
i10	132.136508	0.034240	65.406391	127
i10	132.136508	0.034240	103.823246	43
i10	132.272789	0.034467	82.402241	127
i10	132.272789	0.034467	184.997211	127
i10	132.272789	0.034467	92.498605	104
i10	132.409297	0.034240	65.406391	95
i10	132.409297	0.034240	103.823246	40
i10	132.545578	0.034240	65.406391	127
i10	132.545578	0.034240	92.498605	101
i10	132.681859	0.034467	82.402241	127
i10	132.681859	0.034467	103.823246	40
i10	132.818367	0.034240	82.402241	107
i10	132.818367	0.034240	184.997211	127
i10	132.818367	0.034240	92.498605	107
i10	132.954649	0.034240	103.823246	40
i10	133.090930	0.034467	65.406391	127
i10	133.090930	0.034467	92.498605	110
i10	133.090930	0.114059	138.583497	107
i10	133.227438	0.034240	103.823246	40
i10	133.363719	0.034240	82.402241	127
i10	133.363719	0.034240	184.997211	127
i10	133.363719	0.034240	92.498605	107
i10	133.363719	0.034240	77.781746	127
i10	133.500000	0.034467	103.823246	43
i10	133.636508	0.034240	65.406391	127
i10	133.636508	0.034240	92.498605	110
i10	133.772789	0.034467	103.823246	43
i10	133.909297	0.034240	82.402241	127
i10	133.909297	0.034240	184.997211	127
i10	133.909297	0.034240	92.498605	110
i10	133.909297	0.034240	77.781746	127
i10	134.045578	0.034240	65.406391	98
i10	134.045578	0.034240	103.823246	46
i10	134.181859	0.034467	92.498605	107
i10	134.318367	0.034240	65.406391	127
i10	134.318367	0.034240	103.823246	43
i10	134.454649	0.034240	82.402241	127
i10	134.454649	0.034240	184.997211	127
i10	134.454649	0.034240	92.498605	104
i10	134.454649	0.034240	77.781746	127
i10	134.590930	0.034467	65.406391	95
i10	134.590930	0.034467	103.823246	40
i10	134.727438	0.034240	65.406391	127
i10	134.727438	0.034240	92.498605	101
i10	134.863719	0.034240	103.823246	40
i10	135.000000	0.034467	82.402241	127
i10	135.000000	0.034467	184.997211	127
i10	135.000000	0.034467	92.498605	107
i10	135.000000	0.034467	77.781746	127
i10	135.136508	0.034240	103.823246	40
i10	135.272789	0.034467	65.406391	127
i10	135.272789	0.034467	92.498605	110
i10	135.409297	0.034240	103.823246	40
i10	135.545578	0.034240	82.402241	127
i10	135.545578	0.034240	184.997211	127
i10	135.545578	0.034240	92.498605	107
i10	135.545578	0.034240	77.781746	127
i10	135.681859	0.034467	103.823246	43
i10	135.818367	0.034240	65.406391	127
i10	135.818367	0.034240	92.498605	110
i10	135.954649	0.034240	103.823246	43
i10	136.090930	0.034467	82.402241	127
i10	136.090930	0.034467	184.997211	127
i10	136.090930	0.034467	92.498605	110
i10	136.090930	0.034467	77.781746	127
i10	136.227438	0.034240	65.406391	98
i10	136.227438	0.034240	103.823246	46
i10	136.363719	0.034240	92.498605	107
i10	136.500227	0.034240	65.406391	127
i10	136.500227	0.034240	103.823246	43
i10	136.636508	0.034240	82.402241	127
i10	136.636508	0.034240	184.997211	127
i10	136.636508	0.034240	92.498605	104
i10	136.636508	0.034240	77.781746	127
i10	136.772789	0.034467	65.406391	95
i10	136.772789	0.034467	103.823246	40
i10	136.909297	0.034240	65.406391	127
i10	136.909297	0.034240	92.498605	101
i10	137.045578	0.034240	103.823246	40
i10	137.181859	0.034467	82.402241	127
i10	137.181859	0.034467	184.997211	127
i10	137.181859	0.034467	92.498605	107
i10	137.181859	0.034467	77.781746	127
i10	137.318367	0.034240	103.823246	40
i10	137.454649	0.034240	65.406391	127
i10	137.454649	0.034240	92.498605	110
i10	137.590930	0.034467	103.823246	40
i10	137.727438	0.034240	82.402241	127
i10	137.727438	0.034240	184.997211	127
i10	137.727438	0.034240	92.498605	107
i10	137.727438	0.034240	77.781746	127
i10	137.863719	0.034240	103.823246	43
i10	138.000227	0.034240	65.406391	127
i10	138.000227	0.034240	92.498605	110
i10	138.136508	0.034240	103.823246	43
i10	138.272789	0.034467	82.402241	127
i10	138.272789	0.034467	184.997211	127
i10	138.272789	0.034467	92.498605	110
i10	138.272789	0.034467	77.781746	127
i10	138.409297	0.034240	65.406391	98
i10	138.409297	0.034240	103.823246	46
i10	138.545578	0.034240	92.498605	107
i10	138.681859	0.034467	65.406391	127
i10	138.681859	0.034467	103.823246	43
i10	138.818367	0.034240	82.402241	127
i10	138.818367	0.034240	184.997211	127
i10	138.818367	0.034240	92.498605	104
i10	138.818367	0.034240	77.781746	127
i10	138.954649	0.034240	65.406391	95
i10	138.954649	0.034240	103.823246	40
i10	139.090930	0.034467	65.406391	127
i10	139.090930	0.034467	92.498605	101
i10	139.227438	0.034240	103.823246	40
i10	139.363719	0.034240	82.402241	127
i10	139.363719	0.034240	184.997211	127
i10	139.363719	0.034240	92.498605	107
i10	139.363719	0.034240	77.781746	127
i10	139.500227	0.034240	103.823246	40
i10	139.636508	0.034240	65.406391	127
i10	139.636508	0.034240	92.498605	110
i10	139.772789	0.034467	103.823246	40
i10	139.909297	0.034240	82.402241	127
i10	139.909297	0.034240	184.997211	127
i10	139.909297	0.034240	92.498605	107
i10	139.909297	0.034240	77.781746	127
i10	140.045578	0.034240	103.823246	43
i10	140.181859	0.034467	65.406391	127
i10	140.181859	0.034467	92.498605	110
i10	140.318367	0.034240	103.823246	43
i10	140.454649	0.034240	82.402241	127
i10	140.454649	0.034240	184.997211	127
i10	140.454649	0.034240	92.498605	110
i10	140.454649	0.034240	77.781746	127
i10	140.590930	0.034467	65.406391	98
i10	140.590930	0.034467	103.823246	46
i10	140.727438	0.034240	92.498605	107
i10	140.863719	0.034240	82.402241	81
i10	140.863719	0.034240	65.406391	81
i10	140.863719	0.034240	103.823246	43
i10	141.000227	0.034240	82.402241	127
i10	141.000227	0.034240	184.997211	127
i10	141.000227	0.034240	92.498605	104
i10	141.000227	0.034240	77.781746	127
i10	141.136508	0.034240	65.406391	95
i10	141.136508	0.034240	103.823246	40
i10	141.272789	0.034467	65.406391	127
i10	141.272789	0.034467	92.498605	101
i10	141.409297	0.034240	82.402241	87
i10	141.409297	0.034240	103.823246	40
i10	141.545578	0.034240	82.402241	127
i10	141.545578	0.034240	184.997211	127
i10	141.545578	0.034240	116.534366	113
i10	141.545578	0.034240	77.781746	127
i10	141.818367	0.034240	65.406391	127
i10	141.818367	0.034240	92.498605	110
i10	141.954649	0.034240	103.823246	40
i10	142.090930	0.034467	82.402241	127
i10	142.090930	0.034467	184.997211	127
i10	142.090930	0.034467	92.498605	107
i10	142.090930	0.034467	77.781746	127
i10	142.227438	0.034240	103.823246	43
i10	142.363719	0.034467	65.406391	127
i10	142.363719	0.034467	92.498605	110
i10	142.500227	0.034240	103.823246	43
i10	142.636508	0.034240	82.402241	127
i10	142.636508	0.034240	184.997211	127
i10	142.636508	0.034240	92.498605	110
i10	142.636508	0.034240	77.781746	127
i10	142.772789	0.034467	65.406391	98
i10	142.772789	0.034467	103.823246	46
i10	142.909297	0.034240	92.498605	107
i10	143.045578	0.034240	65.406391	127
i10	143.045578	0.034240	103.823246	43
i10	143.181859	0.034467	82.402241	127
i10	143.181859	0.034467	184.997211	127
i10	143.181859	0.034467	92.498605	104
i10	143.181859	0.034467	77.781746	127
i10	143.318367	0.034240	65.406391	95
i10	143.318367	0.034240	103.823246	40
i10	143.454649	0.034240	65.406391	127
i10	143.454649	0.034240	92.498605	101
i10	143.590930	0.034467	103.823246	40
i10	143.727438	0.034240	82.402241	127
i10	143.727438	0.034240	184.997211	127
i10	143.727438	0.034240	92.498605	107
i10	143.727438	0.034240	77.781746	127
i10	143.863719	0.034467	103.823246	40
i10	144.000227	0.034240	65.406391	127
i10	144.000227	0.034240	92.498605	110
i10	144.136508	0.034240	103.823246	40
i10	144.272789	0.034467	82.402241	127
i10	144.272789	0.034467	184.997211	127
i10	144.272789	0.034467	92.498605	107
i10	144.272789	0.034467	77.781746	127
i10	144.409297	0.034240	103.823246	43
i10	144.545578	0.034240	65.406391	127
i10	144.545578	0.034240	92.498605	110
i10	144.681859	0.034467	103.823246	43
i10	144.818367	0.034240	82.402241	127
i10	144.818367	0.034240	184.997211	127
i10	144.818367	0.034240	92.498605	110
i10	144.818367	0.034240	77.781746	127
i10	144.954649	0.034240	65.406391	98
i10	144.954649	0.034240	103.823246	46
i10	145.090930	0.034467	92.498605	107
i10	145.227438	0.034240	65.406391	127
i10	145.227438	0.034240	103.823246	43
i10	145.363719	0.034467	82.402241	127
i10	145.363719	0.034467	184.997211	127
i10	145.363719	0.034467	92.498605	104
i10	145.363719	0.034467	77.781746	127
i10	145.500227	0.034240	65.406391	95
i10	145.500227	0.034240	103.823246	40
i10	145.636508	0.034240	65.406391	127
i10	145.636508	0.034240	92.498605	101
i10	145.772789	0.034467	103.823246	40
i10	145.909297	0.034240	82.402241	127
i10	145.909297	0.034240	184.997211	127
i10	145.909297	0.034240	92.498605	107
i10	145.909297	0.034240	77.781746	127
i10	146.045578	0.034240	103.823246	40
i10	146.181859	0.034467	65.406391	127
i10	146.181859	0.034467	92.498605	110
i10	146.318367	0.034240	103.823246	40
i10	146.454649	0.034240	82.402241	127
i10	146.454649	0.034240	184.997211	127
i10	146.454649	0.034240	92.498605	107
i10	146.454649	0.034240	77.781746	127
i10	146.590930	0.034467	103.823246	43
i10	146.727438	0.034240	65.406391	127
i10	146.727438	0.034240	92.498605	110
i10	146.863719	0.034467	103.823246	43
i10	147.000227	0.034240	82.402241	127
i10	147.000227	0.034240	184.997211	127
i10	147.000227	0.034240	92.498605	110
i10	147.000227	0.034240	77.781746	127
i10	147.136508	0.034240	65.406391	98
i10	147.136508	0.034240	103.823246	46
i10	147.272789	0.034467	92.498605	107
i10	147.409297	0.034240	65.406391	127
i10	147.409297	0.034240	103.823246	43
i10	147.545578	0.034240	82.402241	127
i10	147.545578	0.034240	184.997211	127
i10	147.545578	0.034240	92.498605	104
i10	147.545578	0.034240	77.781746	127
i10	147.681859	0.034467	65.406391	95
i10	147.681859	0.034467	103.823246	40
i10	147.818367	0.034240	65.406391	127
i10	147.818367	0.034240	92.498605	101
i10	147.954649	0.034240	103.823246	40
i10	148.090930	0.034467	82.402241	127
i10	148.090930	0.034467	184.997211	127
i10	148.090930	0.034467	92.498605	107
i10	148.090930	0.034467	77.781746	127
i10	148.227438	0.034240	103.823246	40
i10	148.363719	0.034467	65.406391	127
i10	148.363719	0.034467	92.498605	110
i10	148.500227	0.034240	103.823246	40
i10	148.636508	0.034240	82.402241	127
i10	148.636508	0.034240	184.997211	127
i10	148.636508	0.034240	92.498605	107
i10	148.636508	0.034240	77.781746	127
i10	148.772789	0.034467	103.823246	43
i10	148.909297	0.034240	65.406391	127
i10	148.909297	0.034240	92.498605	110
i10	149.045578	0.034240	103.823246	43
i10	149.181859	0.034467	82.402241	127
i10	149.181859	0.034467	184.997211	127
i10	149.181859	0.034467	92.498605	110
i10	149.181859	0.034467	77.781746	127
i10	149.318367	0.034240	65.406391	98
i10	149.318367	0.034240	103.823246	46
i10	149.454649	0.034240	92.498605	107
i10	149.590930	0.034467	65.406391	81
i10	149.590930	0.034467	103.823246	43
i10	149.727438	0.034240	82.402241	127
i10	149.727438	0.034240	184.997211	127
i10	149.727438	0.034240	92.498605	104
i10	149.727438	0.034240	77.781746	127
i10	149.863719	0.034467	65.406391	95
i10	149.863719	0.034467	103.823246	40
i10	150.000227	0.034240	65.406391	127
i10	150.000227	0.034240	82.402241	115
i10	150.000227	0.034240	92.498605	101
i10	150.136508	0.034240	82.402241	107
i10	150.136508	0.034240	82.402241	107
i10	150.136508	0.034240	103.823246	40
i10	150.272789	0.034467	82.402241	127
i10	150.272789	0.034467	184.997211	127
i10	150.272789	0.034467	116.534366	113
i10	150.272789	0.034467	77.781746	127
i10	150.409297	0.034240	82.402241	107
i10	150.545578	0.034240	65.406391	127
i10	150.545578	0.034240	92.498605	110
i10	150.545578	0.113832	138.583497	107
i10	150.681859	0.034467	103.823246	40
i10	150.818367	0.034240	82.402241	127
i10	150.818367	0.034240	184.997211	127
i10	150.818367	0.034240	92.498605	107
i10	150.818367	0.034240	77.781746	127
i10	150.954649	0.034240	103.823246	43
i10	151.090930	0.034467	65.406391	127
i10	151.090930	0.034467	92.498605	110
i10	151.227438	0.034240	103.823246	43
i10	151.363719	0.034467	82.402241	127
i10	151.363719	0.034467	184.997211	127
i10	151.363719	0.034467	92.498605	110
i10	151.363719	0.034467	77.781746	127
i10	151.500227	0.034240	65.406391	98
i10	151.500227	0.034240	103.823246	46
i10	151.636508	0.034240	92.498605	107
i10	151.772789	0.034467	65.406391	127
i10	151.772789	0.034467	103.823246	43
i10	151.909297	0.034240	82.402241	127
i10	151.909297	0.034240	184.997211	127
i10	151.909297	0.034240	92.498605	104
i10	151.909297	0.034240	77.781746	127
i10	152.045578	0.034240	65.406391	95
i10	152.045578	0.034240	103.823246	40
i10	152.181859	0.034467	65.406391	127
i10	152.181859	0.034467	92.498605	101
i10	152.318367	0.034240	103.823246	40
i10	152.454649	0.034240	82.402241	127
i10	152.454649	0.034240	184.997211	127
i10	152.454649	0.034240	92.498605	107
i10	152.454649	0.034240	77.781746	127
i10	152.590930	0.034467	103.823246	40
i10	152.727438	0.034240	65.406391	127
i10	152.727438	0.034240	92.498605	110
i10	152.863719	0.034467	103.823246	40
i10	153.000227	0.034240	82.402241	127
i10	153.000227	0.034240	184.997211	127
i10	153.000227	0.034240	92.498605	107
i10	153.000227	0.034240	77.781746	127
i10	153.136508	0.034240	103.823246	43
i10	153.272789	0.034467	65.406391	127
i10	153.272789	0.034467	92.498605	110
i10	153.409297	0.034240	103.823246	43
i10	153.545578	0.034240	82.402241	127
i10	153.545578	0.034240	184.997211	127
i10	153.545578	0.034240	92.498605	110
i10	153.545578	0.034240	77.781746	127
i10	153.681859	0.034467	65.406391	98
i10	153.681859	0.034467	103.823246	46
i10	153.818367	0.034240	92.498605	107
i10	153.954649	0.034240	65.406391	127
i10	153.954649	0.034240	103.823246	43
i10	154.090930	0.034467	82.402241	127
i10	154.090930	0.034467	184.997211	127
i10	154.090930	0.034467	92.498605	104
i10	154.090930	0.034467	77.781746	127
i10	154.227438	0.034240	65.406391	95
i10	154.227438	0.034240	103.823246	40
i10	154.363719	0.034467	65.406391	127
i10	154.363719	0.034467	92.498605	101
i10	154.500227	0.034240	103.823246	40
i10	154.636508	0.034240	82.402241	127
i10	154.636508	0.034240	184.997211	127
i10	154.636508	0.034240	92.498605	107
i10	154.636508	0.034240	77.781746	127
i10	154.772789	0.034467	103.823246	40
i10	154.909297	0.034240	65.406391	127
i10	154.909297	0.034240	92.498605	110
i10	155.045578	0.034240	103.823246	40
i10	155.181859	0.034467	82.402241	127
i10	155.181859	0.034467	184.997211	127
i10	155.181859	0.034467	92.498605	107
i10	155.181859	0.034467	77.781746	127
i10	155.318367	0.034240	103.823246	43
i10	155.454649	0.034240	65.406391	127
i10	155.454649	0.034240	92.498605	110
i10	155.590930	0.034467	103.823246	43
i10	155.727438	0.034240	82.402241	127
i10	155.727438	0.034240	184.997211	127
i10	155.727438	0.034240	92.498605	110
i10	155.727438	0.034240	77.781746	127
i10	155.863719	0.034467	65.406391	98
i10	155.863719	0.034467	103.823246	46
i10	156.000227	0.034240	92.498605	107
i10	156.136508	0.034240	65.406391	127
i10	156.136508	0.034240	103.823246	43
i10	156.272789	0.034467	82.402241	127
i10	156.272789	0.034467	184.997211	127
i10	156.272789	0.034467	92.498605	104
i10	156.272789	0.034467	77.781746	127
i10	156.409297	0.034240	65.406391	95
i10	156.409297	0.034240	103.823246	40
i10	156.545578	0.034240	65.406391	127
i10	156.545578	0.034240	92.498605	101
i10	156.681859	0.034467	103.823246	40
i10	156.818367	0.034240	82.402241	127
i10	156.818367	0.034240	184.997211	127
i10	156.818367	0.034240	92.498605	107
i10	156.818367	0.034240	77.781746	127
i10	156.954649	0.034240	103.823246	40
i10	157.090930	0.034467	65.406391	127
i10	157.090930	0.034467	92.498605	110
i10	157.227438	0.034240	103.823246	40
i10	157.363719	0.034467	82.402241	127
i10	157.363719	0.034467	184.997211	127
i10	157.363719	0.034467	92.498605	107
i10	157.363719	0.034467	77.781746	127
i10	157.500227	0.034240	103.823246	43
i10	157.636508	0.034240	65.406391	127
i10	157.636508	0.034240	92.498605	110
i10	157.772789	0.034467	103.823246	43
i10	157.909297	0.034240	82.402241	127
i10	157.909297	0.034240	184.997211	127
i10	157.909297	0.034240	92.498605	110
i10	157.909297	0.034240	77.781746	127
i10	158.045578	0.034240	65.406391	98
i10	158.045578	0.034240	103.823246	46
i10	158.181859	0.034467	92.498605	107
i10	158.318367	0.034240	82.402241	81
i10	158.318367	0.034240	65.406391	81
i10	158.318367	0.034240	103.823246	43
i10	158.454649	0.034240	82.402241	127
i10	158.454649	0.034240	184.997211	127
i10	158.454649	0.034240	92.498605	104
i10	158.454649	0.034240	77.781746	127
i10	158.590930	0.034467	65.406391	95
i10	158.590930	0.034467	103.823246	40
i10	158.727438	0.034240	65.406391	127
i10	158.727438	0.034240	92.498605	101
i10	158.863719	0.034467	82.402241	87
i10	158.863719	0.034467	103.823246	40
i10	159.000227	0.034240	82.402241	127
i10	159.000227	0.034240	184.997211	127
i10	159.000227	0.034240	116.534366	113
i10	159.000227	0.034240	77.781746	127
i10	159.272789	0.034467	65.406391	127
i10	159.272789	0.034467	92.498605	110
i10	159.409297	0.034240	103.823246	40
i10	159.545578	0.034240	82.402241	127
i10	159.545578	0.034240	184.997211	127
i10	159.545578	0.034240	92.498605	107
i10	159.545578	0.034240	77.781746	127
i10	159.681859	0.034467	103.823246	43
i10	159.818367	0.034240	65.406391	127
i10	159.818367	0.034240	92.498605	110
i10	159.954649	0.034240	103.823246	43
i10	160.090930	0.034467	82.402241	127
i10	160.090930	0.034467	184.997211	127
i10	160.090930	0.034467	92.498605	110
i10	160.090930	0.034467	77.781746	127
i10	160.227438	0.034240	65.406391	98
i10	160.227438	0.034240	103.823246	46
i10	160.363719	0.034467	92.498605	107
i10	160.500227	0.034240	65.406391	127
i10	160.500227	0.034240	103.823246	43
i10	160.636508	0.034240	82.402241	127
i10	160.636508	0.034240	184.997211	127
i10	160.636508	0.034240	92.498605	104
i10	160.636508	0.034240	77.781746	127
i10	160.772789	0.034467	65.406391	95
i10	160.772789	0.034467	103.823246	40
i10	160.909297	0.034240	65.406391	127
i10	160.909297	0.034240	92.498605	101
i10	161.045578	0.034240	103.823246	40
i10	161.181859	0.034467	82.402241	127
i10	161.181859	0.034467	184.997211	127
i10	161.181859	0.034467	92.498605	107
i10	161.181859	0.034467	77.781746	127
i10	161.318367	0.034240	103.823246	40
i10	161.454649	0.034240	65.406391	127
i10	161.454649	0.034240	92.498605	110
i10	161.591156	0.034240	103.823246	40
i10	161.727438	0.034240	82.402241	127
i10	161.727438	0.034240	184.997211	127
i10	161.727438	0.034240	92.498605	107
i10	161.727438	0.034240	77.781746	127
i10	161.863719	0.034467	103.823246	43
i10	162.000227	0.034240	65.406391	127
i10	162.000227	0.034240	92.498605	110
i10	162.136508	0.034240	103.823246	43
i10	162.272789	0.034467	82.402241	127
i10	162.272789	0.034467	184.997211	127
i10	162.272789	0.034467	92.498605	110
i10	162.272789	0.034467	77.781746	127
i10	162.409297	0.034240	65.406391	98
i10	162.409297	0.034240	103.823246	46
i10	162.545578	0.034240	92.498605	107
i10	162.681859	0.034467	65.406391	127
i10	162.681859	0.034467	103.823246	43
i10	162.818367	0.034240	82.402241	127
i10	162.818367	0.034240	184.997211	127
i10	162.818367	0.034240	92.498605	104
i10	162.818367	0.034240	77.781746	127
i10	162.954649	0.034240	65.406391	95
i10	162.954649	0.034240	103.823246	40
i10	163.091156	0.034240	65.406391	127
i10	163.091156	0.034240	92.498605	101
i10	163.227438	0.034240	103.823246	40
i10	163.363719	0.034467	82.402241	127
i10	163.363719	0.034467	184.997211	127
i10	163.363719	0.034467	92.498605	107
i10	163.363719	0.034467	77.781746	127
i10	163.500227	0.034240	103.823246	40
i10	163.636508	0.034240	65.406391	127
i10	163.636508	0.034240	92.498605	110
i10	163.772789	0.034467	103.823246	40
i10	163.909297	0.034240	82.402241	127
i10	163.909297	0.034240	184.997211	127
i10	163.909297	0.034240	92.498605	107
i10	163.909297	0.034240	77.781746	127
i10	164.045578	0.034240	103.823246	43
i10	164.181859	0.034467	65.406391	127
i10	164.181859	0.034467	92.498605	110
i10	164.318367	0.034240	103.823246	43
i10	164.454649	0.034240	82.402241	127
i10	164.454649	0.034240	184.997211	127
i10	164.454649	0.034240	92.498605	110
i10	164.454649	0.034240	77.781746	127
i10	164.591156	0.034240	65.406391	98
i10	164.591156	0.034240	103.823246	46
i10	164.727438	0.034240	92.498605	107
i10	164.863719	0.034467	65.406391	127
i10	164.863719	0.034467	103.823246	43
i10	165.000227	0.034240	82.402241	127
i10	165.000227	0.034240	184.997211	127
i10	165.000227	0.034240	92.498605	104
i10	165.000227	0.034240	77.781746	127
i10	165.136508	0.034240	65.406391	95
i10	165.136508	0.034240	103.823246	40
i10	165.272789	0.034467	65.406391	127
i10	165.272789	0.034467	92.498605	101
i10	165.409297	0.034240	103.823246	40
i10	165.545578	0.034240	82.402241	127
i10	165.545578	0.034240	184.997211	127
i10	165.545578	0.034240	92.498605	107
i10	165.545578	0.034240	77.781746	127
i10	165.681859	0.034467	103.823246	40
i10	165.818367	0.034240	65.406391	127
i10	165.818367	0.034240	92.498605	110
i10	165.954649	0.034240	103.823246	40
i10	166.091156	0.034240	82.402241	127
i10	166.091156	0.034240	184.997211	127
i10	166.091156	0.034240	92.498605	107
i10	166.091156	0.034240	77.781746	127
i10	166.227438	0.034240	103.823246	43
i10	166.363719	0.034467	65.406391	127
i10	166.363719	0.034467	92.498605	110
i10	166.500227	0.034240	103.823246	43
i10	166.636508	0.034240	82.402241	127
i10	166.636508	0.034240	184.997211	127
i10	166.636508	0.034240	92.498605	110
i10	166.636508	0.034240	77.781746	127
i10	166.772789	0.034467	65.406391	98
i10	166.772789	0.034467	103.823246	46
i10	166.909297	0.034240	92.498605	107
i10	167.045578	0.034240	65.406391	81
i10	167.045578	0.034240	103.823246	43
i10	167.181859	0.034467	82.402241	127
i10	167.181859	0.034467	184.997211	127
i10	167.181859	0.034467	92.498605	104
i10	167.181859	0.034467	77.781746	127
i10	167.318367	0.034240	65.406391	95
i10	167.318367	0.034240	103.823246	40
i10	167.454649	0.034467	65.406391	127
i10	167.454649	0.034467	82.402241	115
i10	167.454649	0.034467	92.498605	101
i10	167.591156	0.034240	82.402241	107
i10	167.591156	0.034240	82.402241	107
i10	167.591156	0.034240	103.823246	40
i10	167.727438	0.034240	82.402241	127
i10	167.727438	0.034240	184.997211	127
i10	167.727438	0.034240	116.534366	113
i10	167.727438	0.034240	77.781746	127
i10	167.863719	0.034467	82.402241	107
i10	168.000227	0.034240	65.406391	127
i10	168.000227	0.034240	138.583497	99
i10	168.000227	0.034240	92.498605	110
i10	168.136508	0.034240	103.823246	40
i10	168.272789	0.034467	82.402241	127
i10	168.272789	0.034467	92.498605	107
i10	168.409297	0.034240	103.823246	43
i10	168.545578	0.034240	65.406391	127
i10	168.545578	0.034240	92.498605	110
i10	168.681859	0.034467	103.823246	43
i10	168.818367	0.034240	82.402241	127
i10	168.818367	0.034240	92.498605	110
i10	168.954649	0.034467	65.406391	98
i10	168.954649	0.034467	103.823246	46
i10	169.091156	0.034240	92.498605	107
i10	169.227438	0.034240	65.406391	127
i10	169.227438	0.034240	103.823246	43
i10	169.363719	0.034467	82.402241	127
i10	169.363719	0.034467	92.498605	104
i10	169.500227	0.034240	65.406391	95
i10	169.500227	0.034240	103.823246	40
i10	169.636508	0.034240	65.406391	127
i10	169.636508	0.034240	92.498605	101
i10	169.772789	0.034467	103.823246	40
i10	169.909297	0.034240	82.402241	127
i10	169.909297	0.034240	92.498605	107
i10	170.045578	0.034240	103.823246	40
i10	170.181859	0.034467	65.406391	127
i10	170.181859	0.034467	219.999999	107
i10	170.181859	0.034467	92.498605	110
i10	170.318367	0.034240	103.823246	40
i10	170.454649	0.034467	82.402241	127
i10	170.454649	0.034467	92.498605	107
i10	170.591156	0.034240	103.823246	43
i10	170.727438	0.034240	65.406391	127
i10	170.727438	0.034240	92.498605	110
i10	170.863719	0.034467	103.823246	43
i10	171.000227	0.034240	82.402241	127
i10	171.000227	0.034240	92.498605	110
i10	171.136508	0.034240	65.406391	98
i10	171.136508	0.034240	103.823246	46
i10	171.272789	0.034467	92.498605	107
i10	171.409297	0.034240	65.406391	127
i10	171.409297	0.034240	103.823246	43
i10	171.545578	0.034240	82.402241	127
i10	171.545578	0.034240	92.498605	104
i10	171.681859	0.034467	65.406391	95
i10	171.681859	0.034467	103.823246	40
i10	171.818367	0.034240	65.406391	127
i10	171.818367	0.034240	92.498605	101
i10	171.954649	0.034467	103.823246	40
i10	172.091156	0.034240	82.402241	127
i10	172.091156	0.034240	92.498605	107
i10	172.227438	0.034240	103.823246	40
i10	172.363719	0.034467	65.406391	127
i10	172.363719	0.034467	138.583497	91
i10	172.363719	0.034467	92.498605	110
i10	172.500227	0.034240	103.823246	40
i10	172.636508	0.034240	82.402241	127
i10	172.636508	0.034240	92.498605	107
i10	172.772789	0.034467	103.823246	43
i10	172.909297	0.034240	65.406391	127
i10	172.909297	0.034240	92.498605	110
i10	173.045578	0.034240	103.823246	43
i10	173.181859	0.034467	82.402241	127
i10	173.181859	0.034467	92.498605	110
i10	173.318367	0.034240	65.406391	98
i10	173.318367	0.034240	103.823246	46
i10	173.454649	0.034467	92.498605	107
i10	173.591156	0.034240	65.406391	127
i10	173.591156	0.034240	103.823246	43
i10	173.727438	0.034240	82.402241	127
i10	173.727438	0.034240	92.498605	104
i10	173.863719	0.034467	65.406391	95
i10	173.863719	0.034467	103.823246	40
i10	174.000227	0.034240	65.406391	127
i10	174.000227	0.034240	92.498605	101
i10	174.136508	0.034240	103.823246	40
i10	174.272789	0.034467	82.402241	127
i10	174.272789	0.034467	92.498605	107
i10	174.409297	0.034240	103.823246	40
i10	174.545578	0.034240	65.406391	127
i10	174.545578	0.034240	219.999999	99
i10	174.545578	0.034240	92.498605	110
i10	174.681859	0.034467	103.823246	40
i10	174.818367	0.034240	82.402241	127
i10	174.818367	0.034240	92.498605	107
i10	174.954649	0.034467	103.823246	43
i10	175.091156	0.034240	65.406391	127
i10	175.091156	0.034240	92.498605	110
i10	175.227438	0.034240	103.823246	43
i10	175.363719	0.034467	82.402241	127
i10	175.363719	0.034467	92.498605	110
i10	175.500227	0.034240	65.406391	98
i10	175.500227	0.034240	103.823246	46
i10	175.636508	0.034240	92.498605	107
i10	175.772789	0.034467	65.406391	81
i10	175.772789	0.034467	103.823246	43
i10	175.909297	0.034240	82.402241	127
i10	175.909297	0.034240	92.498605	104
i10	176.045578	0.034240	65.406391	95
i10	176.045578	0.034240	103.823246	40
i10	176.181859	0.034467	65.406391	127
i10	176.181859	0.034467	92.498605	101
i10	176.318367	0.034240	103.823246	40
i10	176.454649	0.034467	82.402241	127
i10	176.454649	0.034467	116.534366	113
i10	176.727438	0.034240	65.406391	127
i10	176.727438	0.034240	138.583497	79
i10	176.727438	0.034240	92.498605	110
i10	176.863719	0.034467	103.823246	40
i10	177.000227	0.034240	82.402241	127
i10	177.000227	0.034240	92.498605	107
i10	177.136508	0.034240	103.823246	43
i10	177.272789	0.034467	65.406391	127
i10	177.272789	0.034467	92.498605	110
i10	177.409297	0.034240	103.823246	43
i10	177.545578	0.034240	82.402241	127
i10	177.545578	0.034240	92.498605	110
i10	177.681859	0.034467	65.406391	98
i10	177.681859	0.034467	103.823246	46
i10	177.818367	0.034240	92.498605	107
i10	177.954649	0.034467	65.406391	127
i10	177.954649	0.034467	103.823246	43
i10	178.091156	0.034240	82.402241	127
i10	178.091156	0.034240	92.498605	104
i10	178.227438	0.034240	65.406391	95
i10	178.227438	0.034240	103.823246	40
i10	178.363719	0.034467	65.406391	127
i10	178.363719	0.034467	92.498605	101
i10	178.500227	0.034240	103.823246	40
i10	178.636508	0.034240	82.402241	127
i10	178.636508	0.034240	92.498605	107
i10	178.772789	0.034467	103.823246	40
i10	178.909297	0.034240	65.406391	127
i10	178.909297	0.034240	219.999999	99
i10	178.909297	0.034240	92.498605	110
i10	179.045578	0.034240	103.823246	40
i10	179.181859	0.034467	82.402241	127
i10	179.181859	0.034467	92.498605	107
i10	179.318367	0.034240	103.823246	43
i10	179.454649	0.034467	65.406391	127
i10	179.454649	0.034467	92.498605	110
i10	179.591156	0.034240	103.823246	43
i10	179.727438	0.034240	82.402241	127
i10	179.727438	0.034240	92.498605	110
i10	179.863719	0.034467	65.406391	98
i10	179.863719	0.034467	103.823246	46
i10	180.000227	0.034240	92.498605	107
i10	180.136508	0.034240	65.406391	81
i10	180.136508	0.034240	103.823246	43
i10	180.272789	0.034467	82.402241	127
i10	180.272789	0.034467	92.498605	104
i10	180.409297	0.034240	65.406391	95
i10	180.409297	0.034240	116.534366	93
i10	180.545578	0.034240	65.406391	127
i10	180.545578	0.034240	82.402241	115
i10	180.681859	0.034467	82.402241	107
i10	180.681859	0.034467	82.402241	107
i10	180.681859	0.034467	103.823246	40
i10	180.818367	0.034240	82.402241	127
i10	180.818367	0.034240	92.498605	113
i10	180.954649	0.034467	82.402241	107
i10	180.954649	0.034467	103.823246	40
i10	181.091156	0.034240	65.406391	127
i10	181.091156	0.034240	92.498605	110
i10	181.091156	0.113832	138.583497	107
i10	181.227438	0.034240	103.823246	40
i10	181.363719	0.034467	82.402241	127
i10	181.363719	0.034467	184.997211	127
i10	181.363719	0.034467	92.498605	107
i10	181.500227	0.034240	103.823246	43
i10	181.636508	0.034240	65.406391	127
i10	181.636508	0.034240	92.498605	110
i10	181.772789	0.034467	103.823246	43
i10	181.909297	0.034240	82.402241	127
i10	181.909297	0.034240	184.997211	127
i10	181.909297	0.034240	92.498605	110
i10	182.045578	0.034240	65.406391	98
i10	182.045578	0.034240	103.823246	46
i10	182.181859	0.034467	92.498605	107
i10	182.318367	0.034240	65.406391	127
i10	182.318367	0.034240	103.823246	43
i10	182.454649	0.034467	82.402241	127
i10	182.454649	0.034467	184.997211	127
i10	182.454649	0.034467	92.498605	104
i10	182.591156	0.034240	65.406391	95
i10	182.591156	0.034240	103.823246	40
i10	182.727438	0.034240	65.406391	127
i10	182.727438	0.034240	92.498605	101
i10	182.863719	0.034467	103.823246	40
i10	183.000227	0.034240	82.402241	127
i10	183.000227	0.034240	184.997211	127
i10	183.000227	0.034240	92.498605	107
i10	183.136508	0.034240	103.823246	40
i10	183.272789	0.034467	65.406391	127
i10	183.272789	0.034467	92.498605	110
i10	183.409297	0.034240	103.823246	40
i10	183.545578	0.034240	82.402241	127
i10	183.545578	0.034240	184.997211	127
i10	183.545578	0.034240	92.498605	107
i10	183.681859	0.034467	103.823246	43
i10	183.818367	0.034240	65.406391	127
i10	183.818367	0.034240	92.498605	110
i10	183.954649	0.034467	103.823246	43
i10	184.091156	0.034240	82.402241	127
i10	184.091156	0.034240	184.997211	127
i10	184.091156	0.034240	92.498605	110
i10	184.227438	0.034240	65.406391	98
i10	184.227438	0.034240	103.823246	46
i10	184.363719	0.034467	92.498605	107
i10	184.500227	0.034240	65.406391	127
i10	184.500227	0.034240	103.823246	43
i10	184.636508	0.034240	82.402241	127
i10	184.636508	0.034240	184.997211	127
i10	184.636508	0.034240	92.498605	104
i10	184.772789	0.034467	65.406391	95
i10	184.772789	0.034467	103.823246	40
i10	184.909297	0.034240	65.406391	127
i10	184.909297	0.034240	92.498605	101
i10	185.045578	0.034240	103.823246	40
i10	185.181859	0.034467	82.402241	127
i10	185.181859	0.034467	184.997211	127
i10	185.181859	0.034467	92.498605	107
i10	185.318367	0.034240	103.823246	40
i10	185.454649	0.034467	65.406391	127
i10	185.454649	0.034467	92.498605	110
i10	185.591156	0.034240	103.823246	40
i10	185.727438	0.034240	82.402241	127
i10	185.727438	0.034240	184.997211	127
i10	185.727438	0.034240	92.498605	107
i10	185.863719	0.034467	103.823246	43
i10	186.000227	0.034240	65.406391	127
i10	186.000227	0.034240	92.498605	110
i10	186.136508	0.034240	103.823246	43
i10	186.272789	0.034467	82.402241	127
i10	186.272789	0.034467	184.997211	127
i10	186.272789	0.034467	92.498605	110
i10	186.409297	0.034240	65.406391	98
i10	186.409297	0.034240	103.823246	46
i10	186.545578	0.034240	92.498605	107
i10	186.682086	0.034240	65.406391	127
i10	186.682086	0.034240	103.823246	43
i10	186.818367	0.034240	82.402241	127
i10	186.818367	0.034240	184.997211	127
i10	186.818367	0.034240	92.498605	104
i10	186.954649	0.034467	65.406391	95
i10	186.954649	0.034467	103.823246	40
i10	187.091156	0.034240	65.406391	127
i10	187.091156	0.034240	92.498605	101
i10	187.227438	0.034240	103.823246	40
i10	187.363719	0.034467	82.402241	127
i10	187.363719	0.034467	184.997211	127
i10	187.363719	0.034467	92.498605	107
i10	187.500227	0.034240	103.823246	40
i10	187.636508	0.034240	65.406391	127
i10	187.636508	0.034240	92.498605	110
i10	187.772789	0.034467	103.823246	40
i10	187.909297	0.034240	82.402241	127
i10	187.909297	0.034240	184.997211	127
i10	187.909297	0.034240	92.498605	107
i10	188.045578	0.034240	103.823246	43
i10	188.182086	0.034240	65.406391	127
i10	188.182086	0.034240	92.498605	110
i10	188.318367	0.034240	103.823246	43
i10	188.454649	0.034467	82.402241	127
i10	188.454649	0.034467	184.997211	127
i10	188.454649	0.034467	92.498605	110
i10	188.591156	0.034240	65.406391	98
i10	188.591156	0.034240	103.823246	46
i10	188.727438	0.034240	92.498605	107
i10	188.863719	0.034467	65.406391	127
i10	188.863719	0.034467	103.823246	43
i10	189.000227	0.034240	82.402241	127
i10	189.000227	0.034240	184.997211	127
i10	189.000227	0.034240	92.498605	104
i10	189.136508	0.034240	65.406391	95
i10	189.136508	0.034240	103.823246	40
i10	189.272789	0.034467	65.406391	127
i10	189.272789	0.034467	92.498605	101
i10	189.409297	0.034240	82.402241	127
i10	189.409297	0.034240	103.823246	40
i10	189.545578	0.034240	82.402241	107
i10	189.545578	0.034240	184.997211	127
i10	189.545578	0.034240	92.498605	107
i10	189.682086	0.034240	103.823246	40
i10	189.818367	0.034240	65.406391	127
i10	189.818367	0.034240	92.498605	110
i10	189.818367	0.113832	138.583497	107
i10	189.954649	0.034467	103.823246	40
i10	190.091156	0.034240	82.402241	127
i10	190.091156	0.034240	184.997211	127
i10	190.091156	0.034240	92.498605	107
i10	190.227438	0.034240	103.823246	43
i10	190.363719	0.034467	65.406391	127
i10	190.363719	0.034467	92.498605	110
i10	190.500227	0.034240	103.823246	43
i10	190.636508	0.034240	82.402241	127
i10	190.636508	0.034240	184.997211	127
i10	190.636508	0.034240	92.498605	110
i10	190.772789	0.034467	65.406391	98
i10	190.772789	0.034467	103.823246	46
i10	190.909297	0.034240	92.498605	107
i10	191.045578	0.034240	65.406391	127
i10	191.045578	0.034240	103.823246	43
i10	191.182086	0.034240	82.402241	127
i10	191.182086	0.034240	184.997211	127
i10	191.182086	0.034240	92.498605	104
i10	191.318367	0.034240	65.406391	95
i10	191.318367	0.034240	103.823246	40
i10	191.454649	0.034467	65.406391	127
i10	191.454649	0.034467	92.498605	101
i10	191.591156	0.034240	103.823246	40
i10	191.727438	0.034240	82.402241	127
i10	191.727438	0.034240	184.997211	127
i10	191.727438	0.034240	92.498605	107
i10	191.863719	0.034467	103.823246	40
i10	192.000227	0.034240	65.406391	127
i10	192.000227	0.034240	92.498605	110
i10	192.136508	0.034240	103.823246	40
i10	192.272789	0.034467	82.402241	127
i10	192.272789	0.034467	184.997211	127
i10	192.272789	0.034467	92.498605	107
i10	192.409297	0.034240	103.823246	43
i10	192.545578	0.034467	65.406391	127
i10	192.545578	0.034467	92.498605	110
i10	192.682086	0.034240	103.823246	43
i10	192.818367	0.034240	82.402241	127
i10	192.818367	0.034240	184.997211	127
i10	192.818367	0.034240	92.498605	110
i10	192.954649	0.034467	65.406391	98
i10	192.954649	0.034467	103.823246	46
i10	193.091156	0.034240	92.498605	107
i10	193.227438	0.034240	65.406391	127
i10	193.227438	0.034240	103.823246	43
i10	193.363719	0.034467	82.402241	127
i10	193.363719	0.034467	184.997211	127
i10	193.363719	0.034467	92.498605	104
i10	193.500227	0.034240	65.406391	95
i10	193.500227	0.034240	103.823246	40
i10	193.636508	0.034240	65.406391	127
i10	193.636508	0.034240	92.498605	101
i10	193.772789	0.034467	103.823246	40
i10	193.909297	0.034240	82.402241	127
i10	193.909297	0.034240	184.997211	127
i10	193.909297	0.034240	92.498605	107
i10	194.045578	0.034467	103.823246	40
i10	194.182086	0.034240	65.406391	127
i10	194.182086	0.034240	92.498605	110
i10	194.318367	0.034240	103.823246	40
i10	194.454649	0.034467	82.402241	127
i10	194.454649	0.034467	184.997211	127
i10	194.454649	0.034467	92.498605	107
i10	194.591156	0.034240	103.823246	43
i10	194.727438	0.034240	65.406391	127
i10	194.727438	0.034240	92.498605	110
i10	194.863719	0.034467	103.823246	43
i10	195.000227	0.034240	82.402241	127
i10	195.000227	0.034240	184.997211	127
i10	195.000227	0.034240	92.498605	110
i10	195.136508	0.034240	65.406391	98
i10	195.136508	0.034240	103.823246	46
i10	195.272789	0.034467	92.498605	107
i10	195.409297	0.034240	65.406391	127
i10	195.409297	0.034240	103.823246	43
i10	195.545578	0.034467	82.402241	127
i10	195.545578	0.034467	184.997211	127
i10	195.545578	0.034467	92.498605	104
i10	195.682086	0.034240	65.406391	95
i10	195.682086	0.034240	103.823246	40
i10	195.818367	0.034240	65.406391	127
i10	195.818367	0.034240	92.498605	101
i10	195.954649	0.034467	103.823246	40
i10	196.091156	0.034240	82.402241	127
i10	196.091156	0.034240	184.997211	127
i10	196.091156	0.034240	92.498605	107
i10	196.227438	0.034240	103.823246	40
i10	196.363719	0.034467	65.406391	127
i10	196.363719	0.034467	92.498605	110
i10	196.500227	0.034240	103.823246	40
i10	196.636508	0.034240	82.402241	127
i10	196.636508	0.034240	184.997211	127
i10	196.636508	0.034240	92.498605	107
i10	196.772789	0.034467	103.823246	43
i10	196.909297	0.034240	65.406391	127
i10	196.909297	0.034240	92.498605	110
i10	197.045578	0.034467	103.823246	43
i10	197.182086	0.034240	82.402241	127
i10	197.182086	0.034240	184.997211	127
i10	197.182086	0.034240	92.498605	110
i10	197.318367	0.034240	65.406391	98
i10	197.318367	0.034240	103.823246	46
i10	197.454649	0.034467	92.498605	107
i10	197.591156	0.034240	65.406391	81
i10	197.591156	0.034240	103.823246	43
i10	197.727438	0.034240	82.402241	127
i10	197.727438	0.034240	184.997211	127
i10	197.727438	0.034240	92.498605	104
i10	197.863719	0.034467	65.406391	95
i10	197.863719	0.034467	103.823246	40
i10	198.000227	0.034240	65.406391	127
i10	198.000227	0.034240	82.402241	115
i10	198.000227	0.034240	92.498605	101
i10	198.136508	0.034240	82.402241	107
i10	198.136508	0.034240	82.402241	107
i10	198.136508	0.034240	103.823246	40
i10	198.272789	0.034467	82.402241	127
i10	198.272789	0.034467	184.997211	127
i10	198.272789	0.034467	92.498605	107
i10	198.409297	0.034240	82.402241	107
i10	198.409297	0.034240	103.823246	40
i10	198.545578	0.034467	65.406391	127
i10	198.545578	0.034467	92.498605	110
i10	198.545578	0.113832	138.583497	107
i10	198.682086	0.034240	103.823246	40
i10	198.818367	0.034240	82.402241	127
i10	198.818367	0.034240	184.997211	127
i10	198.818367	0.034240	92.498605	107
i10	198.954649	0.034467	103.823246	43
i10	199.091156	0.034240	65.406391	127
i10	199.091156	0.034240	92.498605	110
i10	199.227438	0.034240	103.823246	43
i10	199.363719	0.034467	82.402241	127
i10	199.363719	0.034467	184.997211	127
i10	199.363719	0.034467	92.498605	110
i10	199.500227	0.034240	65.406391	98
i10	199.500227	0.034240	103.823246	46
i10	199.636508	0.034240	92.498605	107
i10	199.772789	0.034467	65.406391	127
i10	199.772789	0.034467	103.823246	43
i10	199.909297	0.034240	82.402241	127
i10	199.909297	0.034240	184.997211	127
i10	199.909297	0.034240	92.498605	104
i10	200.045578	0.034467	65.406391	95
i10	200.045578	0.034467	103.823246	40
i10	200.182086	0.034240	65.406391	127
i10	200.182086	0.034240	92.498605	101
i10	200.318367	0.034240	103.823246	40
i10	200.454649	0.034467	82.402241	127
i10	200.454649	0.034467	184.997211	127
i10	200.454649	0.034467	92.498605	107
i10	200.591156	0.034240	103.823246	40
i10	200.727438	0.034240	65.406391	127
i10	200.727438	0.034240	92.498605	110
i10	200.863719	0.034467	103.823246	40
i10	201.000227	0.034240	82.402241	127
i10	201.000227	0.034240	184.997211	127
i10	201.000227	0.034240	92.498605	107
i10	201.136508	0.034240	103.823246	43
i10	201.272789	0.034467	65.406391	127
i10	201.272789	0.034467	92.498605	110
i10	201.409297	0.034240	103.823246	43
i10	201.545578	0.034467	82.402241	127
i10	201.545578	0.034467	184.997211	127
i10	201.545578	0.034467	92.498605	110
i10	201.682086	0.034240	65.406391	98
i10	201.682086	0.034240	103.823246	46
i10	201.818367	0.034240	92.498605	107
i10	201.954649	0.034467	65.406391	127
i10	201.954649	0.034467	103.823246	43
i10	202.091156	0.034240	82.402241	127
i10	202.091156	0.034240	184.997211	127
i10	202.091156	0.034240	92.498605	104
i10	202.227438	0.034240	65.406391	95
i10	202.227438	0.034240	103.823246	40
i10	202.363719	0.034467	65.406391	127
i10	202.363719	0.034467	92.498605	101
i10	202.500227	0.034240	103.823246	40
i10	202.636508	0.034240	82.402241	127
i10	202.636508	0.034240	184.997211	127
i10	202.636508	0.034240	92.498605	107
i10	202.772789	0.034467	103.823246	40
i10	202.909297	0.034240	65.406391	127
i10	202.909297	0.034240	92.498605	110
i10	203.045578	0.034467	103.823246	40
i10	203.182086	0.034240	82.402241	127
i10	203.182086	0.034240	184.997211	127
i10	203.182086	0.034240	92.498605	107
i10	203.318367	0.034240	103.823246	43
i10	203.454649	0.034467	65.406391	127
i10	203.454649	0.034467	92.498605	110
i10	203.591156	0.034240	103.823246	43
i10	203.727438	0.034240	82.402241	127
i10	203.727438	0.034240	184.997211	127
i10	203.727438	0.034240	92.498605	110
i10	203.863719	0.034467	65.406391	98
i10	203.863719	0.034467	103.823246	46
i10	204.000227	0.034240	92.498605	107
i10	204.136508	0.034240	65.406391	127
i10	204.136508	0.034240	103.823246	43
i10	204.272789	0.034467	82.402241	127
i10	204.272789	0.034467	184.997211	127
i10	204.272789	0.034467	92.498605	104
i10	204.409297	0.034240	65.406391	95
i10	204.409297	0.034240	103.823246	40
i10	204.545578	0.034467	65.406391	127
i10	204.545578	0.034467	92.498605	101
i10	204.682086	0.034240	103.823246	40
i10	204.818367	0.034240	82.402241	127
i10	204.818367	0.034240	184.997211	127
i10	204.818367	0.034240	92.498605	107
i10	204.954649	0.034467	103.823246	40
i10	205.091156	0.034240	65.406391	127
i10	205.091156	0.034240	92.498605	110
i10	205.227438	0.034240	103.823246	40
i10	205.363719	0.034467	82.402241	127
i10	205.363719	0.034467	184.997211	127
i10	205.363719	0.034467	92.498605	107
i10	205.500227	0.034240	103.823246	43
i10	205.636508	0.034240	65.406391	127
i10	205.636508	0.034240	92.498605	110
i10	205.772789	0.034467	103.823246	43
i10	205.909297	0.034240	82.402241	127
i10	205.909297	0.034240	184.997211	127
i10	205.909297	0.034240	92.498605	110
i10	206.045578	0.034467	65.406391	98
i10	206.045578	0.034467	103.823246	46
i10	206.182086	0.034240	92.498605	107
i10	206.318367	0.034240	65.406391	127
i10	206.318367	0.034240	103.823246	43
i10	206.454649	0.034467	82.402241	127
i10	206.454649	0.034467	184.997211	127
i10	206.454649	0.034467	92.498605	104
i10	206.591156	0.034240	65.406391	95
i10	206.591156	0.034240	103.823246	40
i10	206.727438	0.034240	65.406391	127
i10	206.727438	0.034240	92.498605	101
i10	206.863719	0.034467	82.402241	127
i10	206.863719	0.034467	103.823246	40
i10	207.000227	0.034240	82.402241	107
i10	207.000227	0.034240	184.997211	127
i10	207.000227	0.034240	92.498605	107
i10	207.136508	0.034240	103.823246	40
i10	207.272789	0.034467	65.406391	127
i10	207.272789	0.034467	219.999999	107
i10	207.272789	0.034467	138.583497	99
i10	207.818367	0.034240	82.402241	127
i10	207.818367	0.034240	146.828242	99
i10	207.954649	0.034467	82.402241	107
i10	207.954649	0.034467	146.828242	116
i10	208.091156	0.034240	82.402241	127
i10	208.091156	0.034240	146.828242	113
i10	208.227438	0.034240	82.402241	113
i10	208.227438	0.034240	146.828242	110
i10	208.363719	0.034467	82.402241	127
i10	208.363719	0.034467	130.812782	107
i10	208.500227	0.034240	82.402241	116
i10	208.500227	0.034240	130.812782	104
i10	208.636508	0.034240	82.402241	127
i10	208.636508	0.034240	123.467342	101
i10	208.772789	0.034467	82.402241	116
i10	208.772789	0.034467	123.467342	97
i10	208.909297	0.034240	82.402241	127
i10	208.909297	0.034240	110.000000	94
i10	209.045578	0.034467	82.402241	113
i10	209.045578	0.034467	97.993331	91
i10	209.182  	0.034240	82.402241	127
i10	209.182 	0.034240	82.402241	127
i10	209.183 	0.034240	97.993331	88
i10	209.318367	0.034240	82.402241	104
i10	209.318367	0.034240	87.304595	85
i10	209.454649	0.057143	65.406391	127
i10	209.454649	0.084354	219.999999	107
i10	209.454649	0.091156	138.583497	99

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
