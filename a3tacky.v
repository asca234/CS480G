// basic sizes of things
`define WORD	[15:0]
`define	TAGWORD	[16:0]
`define	OP	[4:0]
`define	STATE	[4:0] // some state numbers are OPs
`define REGSIZE	[7:0]
`define MEMSIZE	[65535:0]
`define UPPER [15:8]
`define LOWER [7:0]

// field placements and values
`define	TAG	[16]    // type tag in registers
`define Op0	[15:11] // opcode field
`define	Reg0	[10:8]  // register number field
`define Op1	[7:3]   // second opcode
`define Reg1	[2:0]   // second register number, also size
`define	Imm8	[7:0]   // also used as size
`define	OpPack	[15:14]	// if not 2'b11, packed inst

// TACKY data type flag values
`define	TFLOAT	1'b1
`define	TINT	1'b0

// opcode values, also state numbers, per Dietz a2 solution.
`define OPa2r	5'b00000
`define OPr2a	5'b00001
`define OPadd	5'b00010
`define OPand	5'b00011
`define OPcvt	5'b00100
`define OPdiv	5'b00101
`define OPmul	5'b00110
`define OPnot	5'b00111
`define OPor	5'b01000
`define OPsh	5'b01001
`define OPslt	5'b01010
`define OPsub	5'b01011
`define OPxor	5'b01100

`define OPjr	5'b10000
`define OPlf	5'b10001
`define OPli	5'b10010
`define OPst	5'b10011

`define OPcf8	5'b11000
`define OPci8	5'b11001
`define OPjnz8	5'b11010
`define OPjz8	5'b11011

`define OPjp8	5'b11100
`define OPpre	5'b11101
`define OPsys	5'b11110
`define OPnop 5'b11111

`define NOPinst   16'hFFFF // simple instruction word to represent a no-op

//floating point library
// Floating point Verilog modules for CPE480
// Created February 19, 2019 by Henry Dietz, http://aggregate.org/hankd
// Distributed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/

// Field definitions
`define	WORD	[15:0]	// generic machine word size
`define	INT	signed [15:0]	// integer size
`define FLOAT	[15:0]	// half-precision float size
`define FSIGN	[15]	// sign bit
`define FEXP	[14:7]	// exponent
`define FFRAC	[6:0]	// fractional part (leading 1 implied)

// Constants
`define	FZERO	16'b0	  // float 0
`define F32767  16'h46ff  // closest approx to 32767, actually 32640
`define F32768  16'hc700  // -32768

// Count leading zeros, 16-bit (5-bit result) d=lead0s(s)
module lead0s(d, s);
output wire [4:0] d;
input wire `WORD s;
wire [4:0] t;
wire [7:0] s8;
wire [3:0] s4;
wire [1:0] s2;
assign t[4] = 0;
assign {t[3],s8} = ((|s[15:8]) ? {1'b0,s[15:8]} : {1'b1,s[7:0]});
assign {t[2],s4} = ((|s8[7:4]) ? {1'b0,s8[7:4]} : {1'b1,s8[3:0]});
assign {t[1],s2} = ((|s4[3:2]) ? {1'b0,s4[3:2]} : {1'b1,s4[1:0]});
assign t[0] = !s2[1];
assign d = (s ? t : 16);
endmodule

// Float set-less-than, 16-bit (1-bit result) torf=a<b
module fslt(torf, a, b);
output wire torf;
input wire `FLOAT a, b;
assign torf = (a `FSIGN && !(b `FSIGN)) ||
	      (a `FSIGN && b `FSIGN && (a[14:0] > b[14:0])) ||
	      (!(a `FSIGN) && !(b `FSIGN) && (a[14:0] < b[14:0]));
endmodule

// Floating-point addition, 16-bit r=a+b
module fadd(r, a, b);
output wire `FLOAT r;
input wire `FLOAT a, b;
wire `FLOAT s;
wire [8:0] sexp, sman, sfrac;
wire [7:0] texp, taman, tbman;
wire [4:0] slead;
wire ssign, aegt, amgt, eqsgn;
assign r = ((a == 0) ? b : ((b == 0) ? a : s));
assign aegt = (a `FEXP > b `FEXP);
assign texp = (aegt ? (a `FEXP) : (b `FEXP));
assign taman = (aegt ? {1'b1, (a `FFRAC)} : ({1'b1, (a `FFRAC)} >> (texp - a `FEXP)));
assign tbman = (aegt ? ({1'b1, (b `FFRAC)} >> (texp - b `FEXP)) : {1'b1, (b `FFRAC)});
assign eqsgn = (a `FSIGN == b `FSIGN);
assign amgt = (taman > tbman);
assign sman = (eqsgn ? (taman + tbman) : (amgt ? (taman - tbman) : (tbman - taman)));
lead0s m0(slead, {sman, 7'b0});
assign ssign = (amgt ? (a `FSIGN) : (b `FSIGN));
assign sfrac = sman << slead;
assign sexp = (texp + 1) - slead;
assign s = (sman ? (sexp ? {ssign, sexp[7:0], sfrac[7:1]} : 0) : 0);
endmodule

// Floating-point multiply, 16-bit r=a*b
module fmul(r, a, b);
output wire `FLOAT r;
input wire `FLOAT a, b;
wire [15:0] m; // double the bits in a fraction, we need high bits
wire [7:0] e;
wire s;
assign s = (a `FSIGN ^ b `FSIGN);
assign m = ({1'b1, (a `FFRAC)} * {1'b1, (b `FFRAC)});
assign e = (((a `FEXP) + (b `FEXP)) -127 + m[15]);
assign r = (((a == 0) || (b == 0)) ? 0 : (m[15] ? {s, e, m[14:8]} : {s, e, m[13:7]}));
endmodule

// Floating-point reciprocal, 16-bit r=1.0/a
// Note: requires initialized inverse fraction lookup table
module frecip(r, a);
output wire `FLOAT r;
input wire `FLOAT a;
reg [6:0] look[127:0];
initial $readmemh0(look);
assign r `FSIGN = a `FSIGN;
assign r `FEXP = 253 + (!(a `FFRAC)) - a `FEXP;
assign r `FFRAC = look[a `FFRAC];
endmodule

// Floating-point shift, 16 bit
// Shift +left,-right by integer
module fshift(r, f, i);
output wire `FLOAT r;
input wire `FLOAT f;
input wire `INT i;
assign r `FFRAC = f `FFRAC;
assign r `FSIGN = f `FSIGN;
assign r `FEXP = (f ? (f `FEXP + i) : 0);
endmodule

// Integer to float conversion, 16 bit
module i2f(f, i);
output wire `FLOAT f;
input wire `INT i;
wire [4:0] lead;
wire `WORD pos;
assign pos = (i[15] ? (-i) : i);
lead0s m0(lead, pos);
assign f `FFRAC = (i ? ({pos, 8'b0} >> (16 - lead)) : 0);
assign f `FSIGN = i[15];
assign f `FEXP = (i ? (128 + (14 - lead)) : 0);
endmodule

// Float to integer conversion, 16 bit
// Note: out-of-range values go to -32768 or 32767
module f2i(i, f);
output wire `INT i;
input wire `FLOAT f;
wire `FLOAT ui;
wire tiny, big;
fslt m0(tiny, f, `F32768);
fslt m1(big, `F32767, f);
assign ui = {1'b1, f `FFRAC, 16'b0} >> ((128+22) - f `FEXP);
assign i = (tiny ? 0 : (big ? 32767 : (f `FSIGN ? (-ui) : ui)));
endmodule



module processor(halt, reset, clk);
output reg halt;
input reset, clk;
reg `TAGWORD r `REGSIZE; //8 registers, each 17 bits (MSB is tag)
reg `WORD text `MEMSIZE; // inst. mem.
reg `WORD data `MEMSIZE; // data mem.
reg `WORD pc; //program counter
reg `WORD newPC;
reg `WORD ir[4:0]; //instruction word array, one for each stage
reg `Imm8 pre; //pre imm8 array, one for stage 1-4
reg `Imm8 preReg[4:0]// pre passed through stages
reg `TAGWORD OpRegA[4:0]; // operation register A passed from 1-4
reg `TAGWORD OpRegB[4:0]; // operation register B passed from 1-4
reg `TAGWORD AccRegA[4:0]; // accumulator (0) value passed from 1-4
reg `TAGWORD AccRegB[4:0]; // accumulator (1) value passed from 1-4
reg wait1, wait2pt1, wait2pt2 wait3pt1, wait3pt2, wait4; //flags to wait on the next stage; true means wait
reg setsReg `REGSIZE; //array of flags for if a register is about to be written into
reg setsPC; //flag for a changing program counter
reg setsPre; //flag for a changing pre register
reg jumpPC; // message from stage 4 to tell stage 1 to jump instead of increment pc
//floating point instances for ALU operations
wire `FLOAT addf, cvf, recipf, mulf, shf;
fadd myfadd(addf, a `WORD, ((op == `OPsub) ? (b `WORD ^ 16'h8000) : b `WORD));
i2f myi2f(cvf, b `WORD);
f2i myf2i(cvi, b `WORD);
frecip myfrecip(recipf, b `WORD);
fmul myfmul(mulf, a `WORD, ((op == `OPmul) ? b `WORD : recipf));
fshift myfshift(shf, a `WORD, b `WORD);
fslt myfslt(sltf, a `WORD, b `WORD);
newPC = pc;

//dependency helper subroutines. take 16bit instruction lines as input, looks at both fields
function checkRegFlags; //returns true if any regs read in argument instr. line have pending writes
	input `WORD line;
	reg retVal; //local variable
	begin
		retVal = 0;
		case (line `Op0) //case for accumulator 0 field
			`OPa2r:		retval = (retval || setsReg[line `Reg0]);
			`OPr2a:		retval = (retval || setsReg[0]);
			`OPadd:		retval = (retval || setsReg[0]);
			`OPand:		retval = (retval || setsReg[0]);
			`OPcvt:		retval = (retval || setsReg[0]);
			`OPdiv:		retval = (retval || setsReg[0]);
			`OPmul:		retval = (retval || setsReg[0]);
			`OPnot:		retval = (retval || setsReg[0]);
			`OPor:		retval = (retval || setsReg[0]);
			`OPsh:		retval = (retval || setsReg[0]);
			`OPslt:		retval = (retval || setsReg[0]);
			`OPsub:		retval = (retval || setsReg[0]);
			`OPxor:		retval = (retval || setsReg[0]);
			`OPjr:		retval = (retval || setsPC);
			`OPlf:		retval = (retval || setsReg[line `Reg0]);
			`OPli:		retval = (retval || setsReg[line `Reg0]);
			`OPst:		retval = (retval || setsMem);
			`OPcf8:		retval = (retval || setsReg[line `Reg0]);
			`OPci8:		retval = (retval || setsReg[line `Reg0]);
			`OPjnz8:	retval = (retval || setsPC);
			`OPjz8:		retval = (retval || setsPC);
			`OPjp8:		retval = (retval || setsPC);
			`OPpre:		retval = (retval || setsPre);
			`OPsys:		retval = retval; //might need a halting flag later, nop for now
			default:	retval = retval; //only case left is nop, so do no op
		endcase
		case (line `Op1) //case for accumulator 1 field
			`OPa2r:		retval = (retval || setsReg[line `Reg1]);
			`OPr2a:		retval = (retval || setsReg[1]);
			`OPadd:		retval = (retval || setsReg[1]);
			`OPand:		retval = (retval || setsReg[1]);
			`OPcvt:		retval = (retval || setsReg[1]);
			`OPdiv:		retval = (retval || setsReg[1]);
			`OPmul:		retval = (retval || setsReg[1]);
			`OPnot:		retval = (retval || setsReg[1]);
			`OPor:		retval = (retval || setsReg[1]);
			`OPsh:		retval = (retval || setsReg[1]);
			`OPslt:		retval = (retval || setsReg[1]);
			`OPsub:		retval = (retval || setsReg[1]);
			`OPxor:		retval = (retval || setsReg[1]);
			`OPjr:		retval = (retval || setsPC);
			`OPlf:		retval = (retval || setsReg[line `Reg1]);
			`OPli:		retval = (retval || setsReg[line `Reg1]);
			`OPst:		retval = (retval || setsMem);
			`OPcf8:		retval = (retval || setsReg[line `Reg1]);
			`OPci8:		retval = (retval || setsReg[line `Reg1]);
			`OPjnz8:	retval = (retval || setsPC);
			`OPjz8:		retval = (retval || setsPC);
			`OPjp8:		retval = (retval || setsPC);
			`OPpre:		retval = (retval || setsPre);
			`OPsys:		retval = retval; //might need a halting flag later, nop for now
			default:	retval = retval; //only case left is nop, so do no op
		endcase
		checkRegFlags = retVal;
	end
endfunction

task setRegFlags; //sets flags of all to-be-written-in regs in argument instr. line
	input `WORD line;
	begin
		case (line `Op0) //case for accumulator 0 field
			`OPa2r:		setsReg[line `Reg0] = 1;
			`OPr2a:		setsReg[0] = 1;
			`OPadd:		setsReg[0] = 1;
			`OPand:		setsReg[0] = 1;
			`OPcvt:		setsReg[0] = 1;
			`OPdiv:		setsReg[0] = 1;
			`OPmul:		setsReg[0] = 1;
			`OPnot:		setsReg[0] = 1;
			`OPor:		setsReg[0] = 1;
			`OPsh:		setsReg[0] = 1;
			`OPslt:		setsReg[0] = 1;
			`OPsub:		setsReg[0] = 1;
			`OPxor:		setsReg[0] = 1;
			`OPjr:		setsPC = 1;
			`OPlf:		setsReg[line `Reg0] = 1;
			`OPli:		setsReg[line `Reg0] = 1;
			`OPst:		setsMem = 1;
			`OPcf8:		setsReg[line `Reg0] = 1;
			`OPci8:		setsReg[line `Reg0] = 1;
			`OPjnz8:	setsPC = 1;
			`OPjz8:		setsPC = 1;
			`OPjp8:		setsPC = 1;
			`OPpre:		setsPre = 1;
			`OPsys:		setsPC = setsPC; //might need a halting flag later, nop for now
			default:	setsPC = setsPC; //only case left is nop, so do no op
		endcase
		case (line `Op1) //case for accumulator 1 field
			`OPa2r:		setsReg[line `Reg1] = 1;
			`OPr2a:		setsReg[1] = 1;
			`OPadd:		setsReg[1] = 1;
			`OPand:		setsReg[1] = 1;
			`OPcvt:		setsReg[1] = 1;
			`OPdiv:		setsReg[1] = 1;
			`OPmul:		setsReg[1] = 1;
			`OPnot:		setsReg[1] = 1;
			`OPor:		setsReg[1] = 1;
			`OPsh:		setsReg[1] = 1;
			`OPslt:		setsReg[1] = 1;
			`OPsub:		setsReg[1] = 1;
			`OPxor:		setsReg[1] = 1;
			`OPjr:		setsPC = 1;
			`OPlf:		setsReg[line `Reg1] = 1;
			`OPli:		setsReg[line `Reg1] = 1;
			`OPst:		setsMem = 1;
			`OPcf8:		setsReg[line `Reg1] = 1;
			`OPci8:		setsReg[line `Reg1] = 1;
			`OPjnz8:	setsPC = 1;
			`OPjz8:		setsPC = 1;
			`OPjp8:		setsPC = 1;
			`OPpre:		setsPre = 1;
			`OPsys:		setsPC = setsPC; //might need a halting flag later, nop for now
			default:	setsPC = setsPC; //only case left is nop, so do no op
		endcase
	end
endtask

task resetRegFlags; //resets flags of registers written into by argument instr. line
	input `WORD line;
	begin
		case (line `Op0) //case for accumulator 0 field
			`OPa2r:		setsReg[line `Reg0] = 0;
			`OPr2a:		setsReg[0] = 0;
			`OPadd:		setsReg[0] = 0;
			`OPand:		setsReg[0] = 0;
			`OPcvt:		setsReg[0] = 0;
			`OPdiv:		setsReg[0] = 0;
			`OPmul:		setsReg[0] = 0;
			`OPnot:		setsReg[0] = 0;
			`OPor:		setsReg[0] = 0;
			`OPsh:		setsReg[0] = 0;
			`OPslt:		setsReg[0] = 0;
			`OPsub:		setsReg[0] = 0;
			`OPxor:		setsReg[0] = 0;
			`OPjr:		setsPC = 0;
			`OPlf:		setsReg[line `Reg0] = 0;
			`OPli:		setsReg[line `Reg0] = 0;
			`OPst:		setsMem = 0;
			`OPcf8:		setsReg[line `Reg0] = 0;
			`OPci8:		setsReg[line `Reg0] = 0;
			`OPjnz8:	setsPC = 0;
			`OPjz8:		setsPC = 0;
			`OPjp8:		setsPC = 0;
			`OPpre:		setsPre = 0;
			`OPsys:		setsPC = setsPC; //might need a halting flag later, nop for now
			default:	setsPC = setsPC; //only case left is nop, so do no op
		endcase
		case (line `Op1) //case for accumulator 1 field
			`OPa2r:		setsReg[line `Reg1] = 0;
			`OPr2a:		setsReg[1] = 0;
			`OPadd:		setsReg[1] = 0;
			`OPand:		setsReg[1] = 0;
			`OPcvt:		setsReg[1] = 0;
			`OPdiv:		setsReg[1] = 0;
			`OPmul:		setsReg[1] = 0;
			`OPnot:		setsReg[1] = 0;
			`OPor:		setsReg[1] = 0;
			`OPsh:		setsReg[1] = 0;
			`OPslt:		setsReg[1] = 0;
			`OPsub:		setsReg[1] = 0;
			`OPxor:		setsReg[1] = 0;
			`OPjr:		setsPC = 0;
			`OPlf:		setsReg[line `Reg1] = 0;
			`OPli:		setsReg[line `Reg1] = 0;
			`OPst:		setsMem = 0;
			`OPcf8:		setsReg[line `Reg1] = 0;
			`OPci8:		setsReg[line `Reg1] = 0;
			`OPjnz8:	setsPC = 0;
			`OPjz8:		setsPC = 0;
			`OPjp8:		setsPC = 0;
			`OPpre:		setsPre = 0;
			`OPsys:		setsPC = setsPC; //might need a halting flag later, nop for now
			default:	setsPC = setsPC; //only case left is nop, so do no op
		endcase
	end
endtask

initial begin //initialize variables. TODO copy into a reset block
	wait1 = 0;
	wait2pt1 = 0;
  wait2pt2 = 0;
	wait3pt1 = 0;
  wait3pt2 = 0;
	wait4 = 0;
	for (i=0; i<8; i=i+1) begin
		setsReg[i] = 0;
	end
	setsPC = 0;
	setsPre = 0;
	setsMem = 0;
	pc = 0;
	pre[1] = ir[1] `Imm8; // pre at stage one is always 8 bit immediate from ir at stage one
	resetRegFlags();
end

//Stage 0: Instruction Fetch
always @(posedge clk) begin
    if (!wait1 && !checkRegFlags(text[pc])) begin //grab next instruction if no reason to halt 
        ir[0] <= text[pc]; //instruction reg stage 0
		setRegFlags(ir[0]); //set the flags once instr is read
		pc <= pc + 1;
    end
    else begin
        ir[0] <= `NOPinst // send a no-op through the system.
    end
end


//Stage 1: Reg. Read
always @(posedge clk) begin
  if (!wait2pt1 && wait2pt2) begin
    wait1 = 1;
      ir[1] <= ir[0];
      OpRegA[1] <= r[ir[0]`Reg0]; // loads value from specified register in first packed instruction
      OpRegB[1] <= r[ir[0] `Reg1]; // loads value from specified register in second packed instruction
      AccRegA[1] <= r[0]; // loads accumulator 0 stage 1
      AccRegB[1] <= r[1]; // loads accumulator 1 stage 1
      preReg[1] <= pre;
    

    wait1 = 0;
  end
  else begin
    //wait for dependencies
  end
end


//stage 2: computes floating point reciprical and read from data memory
//note that stage 2 consists of 2 always blocks, each one does the ops for one of the field.
always @(posedge clk) begin //accumulator 0 field
  if(!wait3pt1 && wait3pt2) begin // no-dependency flags
    wait2pt1 = 1;
    case (ir[1] `Op0) //checks for special instructions in field 0
      `OPli: r[rn] <= {`TINT, data[r[0] `WORD]};
      `OPlf: r[rn] <= {`TFLOAT, data[r[0] `WORD]};
      `OPst: data[r[rn]] <= r[0] `WORD;
      `OPdiv: if (AccRegA[1] `TAG == `TFLOAT) begin /*add float recip code here*/ end
      default: begin //for default case, pass on registers to next stage
        ir[2] <= ir[1];
        OpRegA[2] <= OpRegA[1];
        OpRegB[2] <= OpRegB[1];
        preReg[2] <= preReg[1];
        AccRegA[2] <= AccRegA[1];
        AccRegB[2] <= AccregB[1];
      end
    endcase

    wait2pt1 = 0;
  end else begin
  //do nothing and wait for depencencies to resolve
  end
end
always @(posedge clk) begin //accumulator 1 field
  if(!wait3pt1 && !wait3pt2) begin // no-dependency flags
    wait2pt2 = 1;
    case (ir[1] `Op1) //checks for special instructions in field 1
      `OPli: r[rn] <= {`TINT, data[r[0] `WORD]};
      `OPlf: r[rn] <= {`TFLOAT, data[r[0] `WORD]};
      `OPst: data[r[rn]] <= r[0] `WORD;
      `OPdiv: if(AccRegB[1] `TAG == `TFLOAT) begin /*add float recip code here*/ end
      default: begin //for default case, pass on registers to next stage
        ir[2] <= ir[1]
        OpRegA[2] <= OpRegA[1];
        OpRegB[2] <= OpRegB[1];
        AccRegA[2] <= AccRegA[1];
        AccRegB[2] <= AccregB[1];
      end
    endcase

    wait2pt2 = 0;
  end else begin
    //do nothing and wait for depencencies to resolve
  end
end


//stage 3: Main ALU phase, responsible for all operations except for li,lf,st& the frecip part of fdiv
always @(posedge clk) begin //outer dual ALU block
  t = AccRegA[3];
  a = AccRegA[2];
  rn = ir[2] `Reg0;
  preReg[3] <= preReg[2];

  b = OpRegA[2]; //maybe need to change this
  always @(posedge clk) begin //accumulator field 0
    if(!wait4) begin //no dependency issues
      wait3pt1 = 1;
      //main ALU stuff involving those big case statements
      case ({a `TAG, a `Op0})
        {`TINT, `OPr2a},
        {`TFLOAT, `OPr2a}: t = b;
        {`TINT, `OPadd}:   t = {`TINT, (a `WORD + b `WORD)};
        {`TFLOAT, `OPadd}: t = {`TFLOAT,  addf};
        {`TINT, `OPand},
        {`TFLOAT, `OPand}: t `WORD = a `WORD & b `WORD;
        {`TINT, `OPcvt},
        {`TFLOAT, `OPcvt}: t = ((b `TAG == `TFLOAT) ? {`TINT, cvi} : {`TFLOAT, cvf});
        {`TINT, `OPdiv}:   t = {`TINT, (a `WORD / b `WORD)};
        {`TFLOAT, `OPdiv}: t = {`TFLOAT, mulf};
        {`TINT, `OPmul}:   t = {`TINT, (a `WORD * b `WORD)};
        {`TFLOAT, `OPmul}: t = {`TFLOAT, mulf};
        {`TINT, `OPnot},
        {`TFLOAT, `OPnot}: t `WORD = ~(b `WORD);
        {`TINT, `OPor},
        {`TFLOAT, `OPor}:  t `WORD = (a `WORD | b `WORD);
        {`TINT, `OPsh}:    t = {`TINT, ((sb < 0) ? (sa >> -sb) : (sa << sb))};
        {`TFLOAT, `OPsh}:  t = {`TFLOAT, shf};
        {`TINT, `OPslt}:   t = {`TINT, (sa < sb)};
        {`TFLOAT, `OPslt}: t = {`TINT, 15'b0, sltf};
        {`TINT, `OPsub}:   t `WORD = sa - sb;
        {`TFLOAT, `OPsub}: t = {`TFLOAT, addf};
        {`TINT, `OPxor},
        {`TFLOAT, `OPxor}: t `WORD = (a `WORD ^ b `WORD);
        {`TINT, `OPcf8}:  begin r[rn] <= {`TFLOAT, preReg[1], ir `Imm8}; //s <= `Start; end
        {`TINT, `OPci8}:  begin r[rn] <= {`TINT, pre, ir `Imm8}; //s <= `Start; end
        {`TINT, `OPjnz8}: begin if (r[rn] `WORD) pc <= {pre, ir `Imm8}; //s <= `Start; end
        {`TINT, `OPjz8}:  begin if (!r[rn] `WORD) pc <= {pre, ir `Imm8}; //s <= `Start; end
        {`TINT, `OPjp8}:  begin pc <= {pre, ir `Imm8}; //s <= `Start; end
        {`TINT, `OPpre}:  begin pre <= ir `Imm8;// s <= `Start; end
        default: begin //pass registers to next stage if ALU not used
          ok = 0;
          ir[2] <= ir[1]
          OpRegA[2] <= OpRegA[1]
          OpRegB[2] <= OpRegB[1]
          AccRegA[2] <= AccRegA[1];
          AccRegB[2] <= AccregB[1];
        end
      endcase


      wait3pt1 = 0;
    end else begin
      //do nothing and wait for dependencies to resolve
    end
  end //end accumulator field 0

  //here is where the second always block will go to handle operations in field 1
  //should be a copy of the first alwasy block in stage 3, but pulling from ir[2] `op1 and outputting to s
  always @(posedge clk) begin //acumulator field 1
    if(!wait4) begin //no dependency issues
      wait3pt2 = 1;
      //main ALU stuff involving those big case statements
      case ({a `TAG, a `Op1})
        {`TINT, `OPr2a},
        {`TFLOAT, `OPr2a}: s = b;
        {`TINT, `OPadd}:   s = {`TINT, (a `WORD + b `WORD)};
        {`TFLOAT, `OPadd}: s = {`TFLOAT,  addf};
        {`TINT, `OPand},
        {`TFLOAT, `OPand}: s `WORD = a `WORD & b `WORD;
        {`TINT, `OPcvt},
        {`TFLOAT, `OPcvt}: s = ((b `TAG == `TFLOAT) ? {`TINT, cvi} : {`TFLOAT, cvf});
        {`TINT, `OPdiv}:   s = {`TINT, (a `WORD / b `WORD)};
        {`TFLOAT, `OPdiv}: s = {`TFLOAT, mulf};
        {`TINT, `OPmul}:   s = {`TINT, (a `WORD * b `WORD)};
        {`TFLOAT, `OPmul}: s = {`TFLOAT, mulf};
        {`TINT, `OPnot},
        {`TFLOAT, `OPnot}: s `WORD = ~(b `WORD);
        {`TINT, `OPor},
        {`TFLOAT, `OPor}:  s `WORD = (a `WORD | b `WORD);
        {`TINT, `OPsh}:    s = {`TINT, ((sb < 0) ? (sa >> -sb) : (sa << sb))};
        {`TFLOAT, `OPsh}:  s = {`TFLOAT, shf};
        {`TINT, `OPslt}:   s = {`TINT, (sa < sb)};
        {`TFLOAT, `OPslt}: s = {`TINT, 15'b0, sltf};
        {`TINT, `OPsub}:   s `WORD = sa - sb;
        {`TFLOAT, `OPsub}: s = {`TFLOAT, addf};
        {`TINT, `OPxor},
        {`TFLOAT, `OPxor}: s `WORD = (a `WORD ^ b `WORD);
        default: begin //pass registers to next stage if ALU not used
          ok = 0;
          ir[2] <= ir[1]
          OpRegA[2] <= OpRegA[1]
          OpRegB[2] <= OpRegB[1]
          AccRegA[2] <= AccRegA[1];
          AccRegB[2] <= AccregB[1];
        end
      endcase

      wait3pt2 = 0;
    end else begin
      //do nothing and wait for dependencies to resolve
    end
  end //end accumulator field 1
end //end outer dual ALU block


//Stage 4: Write to register or data memory
//for VLIW or packed instruction 1
always @(posedge clk) begin
  //write new values
  //call resetRegFlags(ir[3]) to reset write flags of all regs used

  if (ir[3] `Op0 == `OPa2r) begin 
    r[ir[3] `Reg0] <= AccRegA; // register gets Accumulator
  end 
  else if ((ir[3] `Op0 >= `OPr2a) && (ir[3] `Op0 <= `OPxor)) begin //write to acc
    r[0] <= OpRegA[3];
  end
  else if ((ir[3] `Op0 == `OPlf) || (ir[3] `Op0 == `OPli) begin
    r[ir[3] `Reg0] <= data[AccRegA[3]];
  end
  else if (ir[3] `Op0 == `OPst) begin
    data[ir[3] `Op0] <= AccRegA[3];
  end
  //start non packable instructions
  else if (ir[3] `Op0 == `OPcf8)begin
    r[ir[3] `Reg0] `TAG <= 1; //floating point
    r[ir[3] `Reg0] `UPPER <= preReg[3];
    r[ir[3] `Reg0] `LOWER <= ir[3] `LOWER;
  end
  else if (ir[3] `Op0 == `OPci8)begin
    r[ir[3] `Reg0] `TAG <= 0; //integer tag
    r[ir[3] `Reg0] `UPPER <= preReg[3];
    r[ir[3] `Reg0] `LOWER <= ir[3] `LOWER;
  end
  else if (ir[3] `Op0 == `OPjnz8)begin
    if (OpRegA[3] == 0) begin
    jumpPC[4] <= 1; //tell stage 1 to set pc to newPC instead of increment 
    newPC `UPPER <= preReg[3];
    newPC `LOWER <= ir[3] `LOWER;
    end
    else begin
      jumpPC <= 0; // dont set pc, only increment
    end
  end
  else if (ir[3] `Op0 == `OPjz8) begin
    if (OpRegA[3] != 0) begin
      jumpPC <= 1; //tell stage 1 to set pc to newPC instead of increment 
      newPC `UPPER <= preReg[3];
      newPC `LOWER <= ir[3] `LOWER;
    end
    else begin
      jumpPC <= 0; // dont set pc, only increment
    end
  end
  else if (ir[3] `Op0 == `OPjp8) begin
    jumpPC <= 1 ;// set pc next instruction
    newPC `UPPER <= preReg[3];
    newPC `LOWER <= ir[3] `LOWER;
  end
  else if (ir[3] `Op0 == `OPjr) begin
    jumpPC <= 1;
    newPC <= OpRegA `WORD; // only the lower 16 bits. ignores tag.
  end
  else if (ir[3] `Op0 == `OPpre) begin
    pre <= ir[3] `LOWER;
  end
  else (ir[3] `Op0 == `OPsys) begin
    halt <= 1;
  end

  //only reset flags of field 0 instrs by concatenating the nop bitcode for field 1 instr
  resetRegFlags({ir[3] `Op0, 8'hff});//only reset flags of field 0 instrs by concatenating the nop bitcode for field 1 instr
end //end stage 4 accumulator field 0

//stage 4 part 2; for packed instruction 2.
always @(posedge clk) begin
  if (ir[3] `Op1 == `OPa2r) begin 
    r[ir[3] `Reg1] <= AccRegB; // register gets Accumulator
  end 
  else if ((ir[3] `Op1 >= `OPr2a) && (ir[3] `Op1 <= `OPxor)) begin //write to acc
    r[1] <= OpRegB[3]; 
  end
  else if ((ir[3] `Op1== `OPlf) || (ir[3] `Op1 == `OPli) begin
    r[ir[3] `Reg1] <= data[AccRegB[3]];
  end
  else if (ir[3] `Op1 == `OPst) begin
    data[ir[3] `Op1] <= AccRegB[3];
  end

  //only reset flags of field 1 instrs by concatenating the nop bitcode for field0 instr
  resetRegFlags({8'hff, ir[3] `Op1});
end


endmodule //end of processor module


module testbench;
reg reset = 0;
reg clk = 0;
wire halted;
processor PE(halted, reset, clk);
initial begin
  $dumpfile;
  $dumpvars(1, PE.pc, PE.r[0], PE.r[1]); // would normally trace 0, PE
  #10 reset = 1;
  #10 reset = 0;
  while (!halted) begin
    #10 clk = 1;
    #10 clk = 0;
  end
  $finish;
end
endmodule