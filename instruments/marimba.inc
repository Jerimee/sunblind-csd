/* marimba.inc */
  ifreq 	= p4
  ihrd 		= 0.1
  ipos 		= 0.561
  imp		= gimarmstk1
  kvibf		= 6.0
  kvamp		= 0.05
  ivibfn	= gisine
  idec		= 0.6
  idoubles = (p5/(127*2))*90
  itriples = (p5/(127*4))*90

  asig marimba 2, ifreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec, idoubles, itriples

  outs asig, asig