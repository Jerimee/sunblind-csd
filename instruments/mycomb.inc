/* mycomb.inc */
;aIn   inleta "In" 

iamp = p4
;idur = p3
;id10 = p3*0.1
iXXXX = 4.364

; apply comb filter to input signal
krvt    linseg  0.1, 0.5, 1.2; reverb time envelope for comb filter

; loop time envelope for comb filter
; using an a-rate variable here will 
; produce better results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
alpt    expseg  	1,14,1,iXXXX,1,iXXXX,1,iXXXX,1,iXXXX,1,iXXXX,0.1,iXXXX,1,iXXXX,0.1,iXXXX,0.005,iXXXX,0.1,iXXXX,		0.005,iXXXX,1,iXXXX,0.9,iXXXX,0.1,iXXXX,0.005,iXXXX,0.1,iXXXX,1,iXXXX,1,iXXXX,1,iXXXX,1,iXXXX,	0.005,iXXXX,0.1,iXXXX,0.005,iXXXX,0.1,iXXXX,0.005,iXXXX,0.1,iXXXX,0.005,iXXXX,0.1,iXXXX,0.005,iXXXX,0.1,iXXXX,	1,iXXXX*14,0.8

aRes    vcomb   aIn, krvt, alpt, 0.1; comb filter
;outs     aRes*iamp, aRes*iamp; comb filtered audio sent to output
