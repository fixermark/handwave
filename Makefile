# You will need to set the path to the co2.scm source in your environment
CO2_PATH=../../programming-tools/co2/co2.scm
# Windows will use asm6.exe; linux uses asm6
ASM6=asm6.exe

default: handwave.nes

handwave.nes: handwave.co2 handwave.chr song.dat song.not
	racket ${CO2_PATH} --asm ${ASM6} -o handwave.nes handwave.co2

song.dat song.not &: first.sng
	racket -l errortrace -t song.scm -- -o song.dat -n song.not first.sng
