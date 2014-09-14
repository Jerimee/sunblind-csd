/* moogfleur.inc */
kfreq  = p4
kfiltq = p5/141
kfiltrate = 0.0002
kvibf  = 5
kvamp  = .01
;low volume is needed
asig moog .15, kfreq, kfiltq, kfiltrate, kvibf, kvamp, gimandpluk, giimpuls20, gisine
; Hans Mikelson cos pan left to right
kpan      linseg     0, p3, 1 ;move left to right
kpan      =  kpan*$M_PI_2 ;range 0-1 becomes 0-pi/2
kpanl     =  cos(kpan)
kpanr     =  sin(kpan)
;does not need outs?
;outs asig*kpanl, asig*kpanr

;instr 1 ; Moog Fleur
;kfreq  = p4
;kfiltq = p5/141
;kfiltrate = 0.0002
;kvibf  = 5
;kvamp  = .01
;low volume is needed
;asig moog .15, kfreq, kfiltq, kfiltrate, kvibf, kvamp, gimandpluk, giimpuls20, gisine
; Hans Mikelson cos pan left to right
;kpan      linseg     0, p3, 1 ;move left to right
;kpan  =  kpan*$M_PI_2 ;range 0-1 becomes 0-pi/2
;kpanl     =          cos(kpan)
;kpanr     =          sin(kpan)
;outs asig*kpanl, asig*kpanr
;endin ; end ins 1