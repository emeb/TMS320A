
	.float 0.0
	.bfloat 0.0
	.double 0.0
	.efloat 0.0
;	.tfloat 0.0

	.float 1.0
	.bfloat 1.0
	.double 1.0
	.efloat 1.0
;	.tfloat 1.0

	.float 2.0
	.bfloat 2.0
	.double 2.0
	.efloat 2.0
;	.tfloat 2.0

	.float 1.5
	.bfloat 1.5
	.double 1.5
	.efloat 1.5
;	.tfloat 1.5

	.float 0.1
	.bfloat 0.1
	.double 0.1
	.efloat 0.1
;	.tfloat 0.1

	.float -0.1
	.bfloat -0.1
	.double -0.1
	.efloat -0.1
;	.tfloat -0.1

	.float -2.0
	.bfloat -2.0
	.double -2.0
	.efloat -2.0
;	.tfloat -2.0

	.float 0.5
	.bfloat 0.5
	.double 0.5
	.efloat 0.5
;	.tfloat 0.5

	.q12	1.0
	.q12	2.0
	.q12	1.5
	.q12	-1.5
	.q12	0.1

	.lq16	1.0
	.lq16	2.0
	.lq16	1.5
	.lq16	-1.5
	.lq16	0.1

a0	.set	1
a1	.set	a0*2
a2	.set	a1*2
a3	.set	a2*2
a4	.set	a3*2
a5	.set	a4*2
a6	.set	a5*2
a7	.set	a6*2
a8	.set	a7*2
a9	.set	a8*2
a10	.set	a9*2
a11	.set	a10*2
a12	.set	a11*2
a13	.set	a12*2
a14	.set	a13*2
a15	.set	a14*2
a16	.set	a15*2
a17	.set	a16*2
a18	.set	a17*2
a19	.set	a18*2
a20	.set	a19*2
a21	.set	a20*2

	ldpk	a0
	ldpk	a1
	ldpk	a2
	ldpk	a3
	ldpk	a4
	ldpk	a5
	ldpk	a6
	ldpk	a7
	ldpk	a8
	ldpk	a9
	ldpk	a10
	ldpk	a11
	ldpk	a12
	ldpk	a13
	ldpk	a14
	ldpk	a15
	.if	0
	ldpk	a16
	ldpk	a17
	ldpk	a18
	ldpk	a19
	.endif

	.end
