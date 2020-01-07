// basic sizes of things
`define WORD	[15:0] 
`define MYWORD [16:0]// 1 bit for int/float tag
`define FloatBit [16]
`define fiveBitOpcode1	[15:11]
`define fiveBitOpcode2	[7:3]
`define RS1		[10:8]
`define RS2		[2:0]
`define imm8	[7:0]//Jump commands will also utilize imm8
`define eightBitOpcode	[15:8]
`define REGSIZE [7:0] //8 registers
`define MEMSIZE [65535:0]
`define STATE [8:0]

// opcode values, also state numbers
`define OPsub	5'b00000//0
`define OPadd   5'b00001//1
`define OPmul   5'b00010//2
`define OPdiv   5'b00011//3
`define OPa2r   5'b00100//4special
`define OPr2a   5'b00101//5special
`define OPand   5'b00110//6
`define OPnot   5'b00111//7
`define OPor    5'b01000//8
`define OPxor   5'b01001//9
`define OPlf    5'b01010//10
`define OPli    5'b01011//11
`define OPst    5'b01100//12
`define OPsh    5'b01101//13
`define OPslt   5'b01110//14
`define OPcvt   5'b01111//15
`define OPjr    5'b10000//16
`define OPcf8   5'b10010//17
`define OPci8   5'b10011//18
`define OPsys   8'b11100001//225
`define OPpre   8'b11100010//226
`define OPjz8   8'b11100011//227
`define OPjnz8  8'b11100100//228
`define OPjp8   8'b11100101//229

//the 8 bit opcodes all start with the specificbit pattern 111, 
//this is unique to only 8 bit opcodes


// state numbers only
`define Start	8'b11111111
`define StartR0	8'b11111110
`define StartR1	8'b11111101
`define RunR0	8'b11111100
`define RunR1   8'b11111011




//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////FLOAT SHIZNIT

// Floating point Verilog modules for CPE480
// Created February 19, 2019 by Henry Dietz, http://aggregate.org/hankd
// Distributed under CC BY 4.0, https://creativecommons.org/licenses/by/4.0/

// Field definitions
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

// Floating-point subtraction, 16-bit r=a-b
module fsub(r, a, b);
output wire `FLOAT r;
input wire `FLOAT a, b;
fmul flipsign(r, b, -1);
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

// // Testing of floating point 
// module testbench2;
// reg `FLOAT a, b;
// reg `WORD r;
// wire `FLOAT addr,mulr, recr, shir, i2fr;
// wire `INT f2ir, i, j, ia, ib, addri;
// reg `WORD ref[1024:0];
// f2i myfa(ia, a);
// f2i myfb(ib, b);
// fadd myadd(addr, a, b);
// f2i myaddf(addri, addr);
// fmul mymul(mulr, a, b);
// frecip myrecip(recr, a);
// fshift myshift(shir, a, f2ir);
// f2i myf2i(f2ir, a);
// f2i myib(i, b);
// f2i myiadd(j, addr);
// i2f myi2f(i2fr, f2ir);
// initial begin
//   $readmemh1(ref);
//   r = 0;

//   while (ref[r] != 0) begin
//     a = ref[r]; b = ref[r+1];
//     #1 $display("Testing (int)%x = %d, (int)%x = %d", a, ia, b, ib);
//     if (addr != ref[r+2]) $display("%x + %x = %x # %x", a, b, addr, ref[r+2]);
//     if (mulr != ref[r+3]) $display("%x * %x = %x # %x", a, b, mulr, ref[r+3]);
//     if (recr != ref[r+4]) $display("1 / %x = %x # %x", a, recr, ref[r+4]);
//     r = r + 5;
//   end
// end
// endmodule

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////FLOAT SHIZNIT

module processor(halt, reset, clk);
output reg halt; 
input reset, clk;

reg `MYWORD regfile `REGSIZE;
reg `WORD prereg; //seperate register for the pre register
reg `WORD mainmem `MEMSIZE;
reg `WORD pc = 0;
reg `WORD ir;
reg `STATE s = `Start;
wire `FLOAT fsubreg0, faddreg0, fmulreg0, frecipreg0, fdivreg0, fshiftreg0, i2freg0, fsubreg1, faddreg1, fmulreg1, frecipreg1, fdivreg1, fshiftreg1, i2freg1;
wire `INT f2ireg0, f2ireg1;
wire fsltreg0, fsltreg1;
integer a;

fsub r0fsub(fsubreg0, regfile[0]`WORD, regfile[ir `RS1]`WORD);
fadd r0fadd(faddreg0, regfile[0]`WORD, regfile[ir `RS1]`WORD);
fmul r0fmul(fmulreg0, regfile[0]`WORD, regfile[ir `RS1]`WORD);
fmul r0fdiv(fdivreg0, regfile[0]`WORD, frecipreg0);
frecip r0frecip(frecipreg0, regfile[ir `RS1]`WORD);
fshift r0fshift(fshiftreg0, regfile[0]`WORD, regfile[ir `RS1]`WORD);
i2f r0i2f(i2freg0, regfile[ir `RS1]`WORD);
f2i r0f2i(f2ireg0, regfile[ir `RS1]`WORD);
fslt r0fslt(fsltreg0, regfile[0]`WORD, regfile[ir `RS1]`WORD);

fsub r1fsub(fsubreg1, regfile[1]`WORD, regfile[ir `RS2]`WORD);
fadd r1fadd(faddreg1, regfile[1]`WORD, regfile[ir `RS2]`WORD);
fmul r1fmul(fmulreg1, regfile[1]`WORD, regfile[ir `RS2]`WORD);
fmul r1fdiv(fdivreg1, regfile[1]`WORD, frecipreg1);
frecip r1frecip(frecipreg1, regfile[ir `RS2]`WORD);
fshift r1fshift(fshiftreg1, regfile[1]`WORD, regfile[ir `RS2]`WORD);
i2f r1i2f(i2freg1, regfile[ir `RS2]`WORD);
f2i r1f2i(f2ireg1, regfile[ir `RS2]`WORD);
fslt r1fslt(fsltreg1, regfile[1]`WORD, regfile[ir `RS2]`WORD);

always @(reset) begin
  halt = 0;
  pc = 0;
  s = `Start;
  $readmemh1(regfile); 
  $readmemh2(mainmem); 
end

always @(posedge clk) begin
  case (s)
    `Start: begin ir <= mainmem[pc]; s <= `StartR0; end
    `StartR0: begin
             pc <= pc + 1;            // bump pc
             $display("this is where the pc halted %d", pc);
             s <= `RunR0; // all instructions, state # is opcode
             end
    `StartR1: begin
   		 s <= `RunR1;
   		 end
   	`RunR1: begin
    	case(ir`fiveBitOpcode2)
    		`OPsub : begin regfile[1]`WORD <= 	regfile[ir `RS2] `FloatBit ? fsubreg1 : regfile[1]`WORD + regfile[ir `RS2]`WORD; s <= `Start; end 	//Not sure how to flip sign in floating point
			`OPadd : begin regfile[1]`WORD <=	regfile[ir `RS2] `FloatBit ? faddreg1 : regfile[1]`WORD + regfile[ir `RS2]`WORD; s <= `Start; end
			`OPmul : begin regfile[1]`WORD <=	regfile[ir `RS2] `FloatBit ? fmulreg1 : regfile[1]`WORD * regfile[ir `RS2]`WORD; s <= `Start; end
			`OPdiv : begin regfile[1]`WORD <= 	regfile[ir `RS2] `FloatBit ? fdivreg1 : regfile[1]`WORD / regfile[ir `RS2]`WORD; s <= `Start; end
			`OPa2r : begin regfile[ir`RS2] <=  	regfile[1]; 		   			s <= `Start; end
			`OPr2a : begin regfile[1]      <= 	regfile[ir`RS2]; 	   			s <= `Start; end
			`OPand : begin regfile[1]`WORD <= 	regfile[1]`WORD & regfile[ir `RS2]`WORD;	s <= `Start; end
			`OPnot : begin regfile[1]`WORD <= 	~(regfile[ir `RS2]`WORD);  			s <= `Start; end
			`OPor  : begin regfile[1]`WORD <= 	regfile[1]`WORD | regfile[ir `RS2]`WORD; 	s <= `Start; end
			`OPxor : begin regfile[1]`WORD <= 	regfile[1]`WORD ^ regfile[ir `RS2]`WORD; 	s <= `Start; end
			`OPlf  : begin regfile[1]      <= 	{1'b1, regfile[ir`RS2]`WORD}; 		   	s <= `Start; end
			`OPli  : begin regfile[1]      <= 	{1'b0, regfile[ir`RS2]`WORD}; 		   	s <= `Start; end
			`OPst  : begin regfile[ir`RS2]`WORD <=  regfile[1]`WORD; 		   		s <= `Start; end
			`OPsh  : begin regfile[1]`WORD <= 	regfile[ir `RS2] `FloatBit ? fshiftreg1 : regfile[1]`WORD << regfile[ir `RS2]`WORD; s <= `Start; end
			`OPslt : begin regfile[1]`WORD <=	regfile[ir `RS2] `FloatBit ? fsltreg1 : ((regfile[1]`WORD < regfile[ir `RS2]`WORD) ? 1 : 0); s <= `Start; end
			`OPcvt : begin regfile[1]`FloatBit <= regfile[ir `RS2] `FloatBit ? 1'b0 : 1'b1; regfile[1]      <=	regfile[ir `RS2] `FloatBit ? f2ireg1 : i2freg1; s <= `Start; end
			`OPjr  : begin pc <=  regfile[ir `RS1]`WORD;  end
			default: halt <= 1; 
    endcase
    end
	`RunR0: begin  
    	case(ir`fiveBitOpcode1)
			`OPsub : begin regfile[0]`WORD <= 	regfile[ir `RS1] `FloatBit ? fsubreg0 : regfile[0]`WORD + regfile[ir `RS1]`WORD; s <= `StartR1; end 	//Above
			`OPadd : begin regfile[0]`WORD <= 	regfile[ir `RS1] `FloatBit ? faddreg0 : regfile[0]`WORD + regfile[ir `RS1]`WORD; s <= `StartR1; end
			`OPmul : begin regfile[0]`WORD <= 	regfile[ir `RS1] `FloatBit ? fmulreg0 : regfile[0]`WORD * regfile[ir `RS1]`WORD; s <= `StartR1; end
			`OPdiv : begin regfile[0]`WORD <=	regfile[ir `RS1] `FloatBit ? fdivreg0 : regfile[0]`WORD / regfile[ir `RS1]`WORD; s <= `StartR1; end
			`OPa2r : begin regfile[ir`RS1] <=  	regfile[0]; 		   			s <= `StartR1; end
			`OPr2a : begin regfile[0]      <= 	regfile[ir`RS1]; 	   			s <= `StartR1; end
			`OPand : begin regfile[0]`WORD <= 	regfile[0]`WORD & regfile[ir `RS1]`WORD;	s <= `StartR1; end
			`OPnot : begin regfile[0]`WORD <= 	~(regfile[ir `RS1]`WORD);   			s <= `StartR1; end
			`OPor  : begin regfile[0]`WORD <= 	regfile[0]`WORD | regfile[ir `RS1]`WORD;	s <= `StartR1; end
			`OPxor : begin regfile[0]`WORD <= 	regfile[0]`WORD ^ regfile[ir `RS1]`WORD;	s <= `StartR1; end
			`OPlf  : begin regfile[0]      <= 	{1'b1, regfile[ir`RS1]`WORD};			s <= `StartR1; end
			`OPli  : begin regfile[0]      <= 	{1'b0, regfile[ir`RS1]`WORD};			s <= `StartR1; end
			`OPst  : begin regfile[ir`RS1]`WORD <=  regfile[0]`WORD; 		   		s <= `StartR1; end
			`OPsh  : begin regfile[0]`WORD <=	regfile[ir `RS1] `FloatBit ? fshiftreg0 : regfile[0]`WORD << regfile[ir `RS1]`WORD; s <= `StartR1; end
			`OPslt : begin regfile[0]`WORD <=	regfile[ir `RS1] `FloatBit ? fsltreg0 : ((regfile[0]`WORD < regfile[ir `RS1]`WORD) ? 1 : 0); s <= `StartR1; end
			`OPcvt : begin regfile[0]`FloatBit <= regfile[ir `RS2] `FloatBit ? 1'b0 : 1'b1; regfile[0]      <=	regfile[ir `RS2] `FloatBit ? f2ireg0 : i2freg0; s <= `StartR1; end
			`OPjr  : begin pc <= regfile[ir `RS1]`WORD;  end
			
			default: case(ir `eightBitOpcode) 
				`OPsys : begin halt <= 1; end	//sys only sets halt
 				`OPpre : begin prereg <= ir`imm8; 		s<=`Start;	end
				`OPjz8 : begin pc <= 	{prereg,ir`imm8};		s<=`Start;		end
				`OPjnz8: begin pc <= 	{prereg,ir`imm8};		s<=`Start;			end
				`OPjp8 : begin pc <= 	{prereg,ir`imm8};		s<=`Start;			end
				default: s <= `StartR1; 
				endcase
			endcase
		end
    default: halt <= 1;
  endcase
end
endmodule


//testbench borrowed from dietz assignment spec page
module testbench;
reg reset = 0;
reg clk = 0;
wire halted;
processor PE(halted, reset, clk);
initial begin
  #10 reset = 1;
  #10 reset = 0;
  while (!halted) begin
    #10 clk = 1;
    #10 clk = 0;
  end
  $finish;
  
end
endmodule


