/* sine_bass_wave.inc */
ifreq = (p4 > 15 ? p4 : cpspch(p4))

imyrandom random 0, 9
irdvd = imyrandom / 4
iroll = (60 * int(irdvd)) ; %20 chance output is 100, %40 50, %40 zero
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

outs aout, aout