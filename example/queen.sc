?sN

# register assignments
# N g       	board size
# L g       	recursion level
# c l       	column availability (bitset)
# l l       	diag availability (bitset)
# r l       	adiag availability (bitset)
# R m    -> 	recursive search on level L
# r m   0->N	try each position 0..N-1 for current level
# @ m    -> 	start a new level of recursion
# q m    -> 	[q]
# p m   i->0	print i spaces
# P m ...-> 	print the board (board info is on stack)

[[ ]P1-d0<p]sp
[d0<p[Q
]Ps.z0<P]sP
[z0<PlN3*2+Q]s#

[lLlN=# lclllr&|&|St 0dlN>S s.Lts.]sR
[dltr&T0=@ 1+dlN>S]sS
[dlcr&sSc dllr&s1&<Sl dlrr&s1&>Sr
LL1+SL lRx LL1-SL Lcs.Lls.Lrs.]s@

0sc0sl0sr 0sL lRx
