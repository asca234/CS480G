// Size of common uses
`define word		[15:0]
`define opcode1		[15:11]
`define reg1		[10:8]
`define opcode2	 	[7:3]
`define reg2		[2:0]
`define immediate	[7:0]
`define floatorint	[16]
`define regID		[2:0]
`define pre8bits	[15:8]
`define ALUbit	    [15]

`define state		[4:0]
`define REGSIZE		[16:0]
`define MEMSIZE		[65535:0]
`define CACHE_LINE [63:0]
`define CACHE_WORD [15:0]

//BEGIN modified slowmem64 - 16bit word read and write, still stores in lines as per spec
`define SM_LINEADDR [15:0]
`define SM_LINE [63:0]
`define SM_LINES [16383:0]
`define SM_MEMDELAY 4

//BEGIN arbiter module
`define ARB_WORD [15:0] //word size for addr in
`define ARB_DATA [16:0] //1 ready bit + data word for returned data out



// ALU Operations
`define OPadd		5'b00000
`define OPsub		5'b00001
`define	OPdiv		5'b00010
`define OPmul		5'b00011
`define OPand		5'b00100
`define OPnot		5'b00101
`define OPor		5'b00110
`define OPxor		5'b00111
`define OPsh		5'b01000
`define OPcvt		5'b01001
`define OPslt		5'b01010

// Load and Store Operations
`define OPli		5'b10001
`define OPlf		5'b10010
`define OPst		5'b10011

// Change from Float to Int
`define OPcf8		5'b10101
`define OPci8		5'b10110

// Jumps
`define OPjnz8		5'b10111
`define OPjp8		5'b11000
`define OPjr		5'b11001
`define OPjz8		5'b11010

// Swaps
`define OPa2r		5'b11100
`define OPr2a		5'b11101

// Pre & Sys
`define OPpre		5'b11110
`define OPsys		5'b11111

// NoOp
`define OPnop      	5'b11011

//Floating Point Modules

// Field definitions
`define WORD [15:0]	// generic machine word size
`define	INT	signed [15:0]	// integer size
`define FLOAT	[15:0]	// half-precision float size
`define FSIGN	[15]	// sign bit
`define FEXP	[14:7]	// exponent
`define FFRAC	[6:0]	// fractional part (leading 1 implied)
`define TYPEBIT [16] //17th bit that determines int or float

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
	assign torf = (a `FSIGN && !(b `FSIGN))||
				  (a `FSIGN && b `FSIGN && (a[14:0] > b[14:0]))||
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
	initial $readmemh2(look);
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

//ALU Module

module ALUprime(accOut, regOut, accIn, regIn, opP); //accIn is accumulator
input wire [16:0] accIn, regIn;
input wire [4:0] opP;
output reg [16:0] accOut, regOut;

wire `FLOAT recip;
frecip freciprocal(recip, {regIn [15:0]});
 
always @ (*)
begin
	if (accIn `floatorint) begin
	case(opP)
		`OPdiv: begin regOut <= recip; accOut <= accIn; end //if it is a float div, set up operand 2 as a reciprocal
		default: begin regOut <= regIn; accOut <= accIn; end //general case, just pass through the operands as normal
	endcase 
	end
	else begin
		regOut <= regIn; 
		accOut <= accIn;
	end
end endmodule

module ALU(temp, inAcc, inReg, op);
input wire [16:0]  inAcc, inReg;
input wire [4:0] op;
wire floslt;
wire `FLOAT floadd, flomul, flodiv, flosub, flocvt, intcvt, flosh, i2fw;
wire `INT addw, subw, divw, mulw, intsh, andw, notw, orw, xorw, f2iw;
output reg [16:0] temp = 0;

fadd floatadd(floadd, inAcc`FLOAT, inReg`FLOAT);
fadd floatsub(flosub, inAcc`FLOAT, {!inReg[15],inReg[14:0]});
fmul floatmul(flomul, inAcc`FLOAT, inReg`FLOAT);
fmul fdiv(flodiv, inAcc`FLOAT, inReg`FLOAT);
i2f floatcvt(flocvt, inAcc`FLOAT);
f2i integercvt(intcvt, inAcc`INT);
fshift floatshift(flosh, inAcc`FLOAT, inReg`INT);
fslt floatsetless(floslt, inAcc`FLOAT, inReg`FLOAT);

always @ (*)
begin
case(inAcc`TYPEBIT)
	1'b0: 
	begin
		case(op)
			`OPadd: begin temp = {inAcc`TYPEBIT, inAcc `WORD + inReg `WORD}; end //add
			`OPsub: begin temp = {inAcc`TYPEBIT, inAcc `WORD - inReg `WORD}; end //sub
			`OPdiv: begin temp = {inAcc`TYPEBIT, inAcc `WORD / inReg `WORD}; end //div
			`OPmul: begin temp = {inAcc`TYPEBIT, inAcc `WORD * inReg `WORD}; end //mul
			`OPand: begin temp = {inAcc`TYPEBIT, inAcc `WORD & inReg `WORD}; end //and
			`OPnot: begin temp = {inAcc`TYPEBIT, !(inReg `WORD)}; end //not inReg is the default r register
			`OPor: begin temp = {inAcc`TYPEBIT, inAcc `WORD | inReg `WORD}; end //or
			`OPxor: begin temp = {inAcc`TYPEBIT, inAcc `WORD ^ inReg `WORD}; end //xor
			`OPsh: begin temp = {inAcc`TYPEBIT, ((~inReg[15])?(inAcc `WORD >> inReg `WORD):(inAcc<<!inReg`WORD))}; end
			`OPcvt: begin temp = {!inAcc`TYPEBIT, flocvt}; end
			`OPslt: begin temp = ((inAcc<inReg)?(1'b1):(1'b0)); end
			default: begin temp = inAcc;  end
		endcase
	end
    1'b1: 
	begin
		case(op)
		  `OPadd: begin temp = {inAcc`TYPEBIT, floadd}; end //add
		  `OPsub: begin temp = {inAcc`TYPEBIT, flosub}; end //sub
		  `OPdiv: begin temp = {inAcc`TYPEBIT, flodiv}; end //div; remove divide module and just make the instantiation a multiply module
		  `OPmul: begin temp = {inAcc`TYPEBIT, flomul}; end //mul
		  `OPand: begin temp = {inAcc`TYPEBIT, inAcc`WORD & inReg`WORD}; end //and
		  `OPnot: begin temp = {inAcc`TYPEBIT, !inReg`WORD}; end //not
		  `OPor: begin temp = {inAcc`TYPEBIT, inAcc`WORD | inReg`WORD}; end //or
		  `OPxor: begin temp = {inAcc`TYPEBIT, inAcc`WORD ^ inReg`WORD}; end //xor
		  `OPsh: begin temp = {inAcc`TYPEBIT, flosh`WORD}; end //sh
		  `OPcvt: begin temp = {!inAcc`TYPEBIT, intcvt`WORD}; end //cvt to int
		  `OPslt: begin temp = floslt; end //slt
		  default: begin temp = inAcc; end
		endcase
    end
endcase
end
endmodule

module slowmem64(mfc, rdata, addr, wdata, rnotw, strobe, clk);
output reg mfc;
output reg `SM_LINEADDR rdata;
input `SM_LINEADDR addr;
input `SM_LINEADDR wdata;
input rnotw, strobe, clk;
reg [7:0] pend;
reg `SM_LINEADDR raddr;
reg `SM_LINE m `SM_LINES;

initial begin
  pend <= 0;
  // put your memory initialization code here
end

always @(posedge clk) begin
  if (strobe && rnotw) begin
    // new read request
    raddr <= addr;
    pend <= `SM_MEMDELAY;
  end else begin
    if (strobe && !rnotw) begin
      // do write
      m[addr] <= wdata;
    end

    // pending read?
    if (pend) begin
      // write satisfies pending read
      if ((raddr == addr) && strobe && !rnotw) begin
        rdata <= wdata;
        mfc <= 1;
        pend <= 0;
      end else if (pend == 1) begin
        // finally ready
        rdata <= m[raddr];
        mfc <= 1;
        pend <= 0;
      end else begin
        pend <= pend - 1;
      end
    end else begin
      // return invalid data
      rdata <= 16'hxxxx;
      mfc <= 0;
    end
  end
end
endmodule //END modified slowmem64



//wrapper for slowmem to arbitrate access; writes are instantaneous, reads sequential
module arbiter(core0DataOut, core1DataOut, core0ReadAddr, core1ReadAddr, core0WriteAddr, core1WriteAddr, core0WriteData, core1WriteData, clk);
  
  output `ARB_DATA core0DataOut, core1DataOut;
  input `ARB_WORD core0ReadAddr, core1ReadAddr, core0WriteAddr, core1WriteAddr, core0WriteData, core1WriteData;
  input clk;
  wire mfc;
  wire `ARB_WORD rdata;
  reg strobe, rnotw, sel;
  reg `ARB_WORD addr, wdata;
  
  //modified slowmem64 to allow read/write-by-word instead of by line
  slowmem64 ram(mfc, rdata, addr, wdata, rnotw, strobe, clk);

  //"multiplexers" to determine which core gets the requested data from a read
  //prepends the ready bit from mfc via concatenation
  //when not selected, data doesn't change but ready bit does
  assign core0DataOut = (~sel) ? {mfc, rdata} : {1'b0, core0DataOut `ARB_WORD}; 
  assign core1DataOut = sel ? {mfc, rdata} : {1'b0, core1DataOut `ARB_WORD};

  initial begin
    strobe <= 1'b0;
    sel <= 1'b0;
  end

  //these always blocks allow instantaneous writes to wrapped slowmem
  always@(core0WriteAddr) begin
    addr <= core0WriteAddr;
    wdata <= core0WriteData;
    rnotw <= 1'b0;
    strobe <= #1 1'b1; //wait 1 cycle to enable strobe to ensure values above are set
    strobe <= #1 1'b0; //toggle strobe for 1 cycle to execute the write
  end
  always@(core1WriteAddr) begin
    addr <= core1WriteAddr;
    wdata <= core1WriteData;
    rnotw <= 1'b0;
    strobe <= #1 1'b1; //wait 1 cycle to enable strobe to ensure values above are set
    strobe <= #1 1'b0; //toggle strobe for 1 cycle to execute the write
  end
  
  //these always blocks detect the first read request to come in, set the slowmem ins and outs,
  //set the sel reg for muxes above, and latch these values until MFC
  always@(core0ReadAddr) begin
    if (mfc) begin //last read done
      addr <= core0ReadAddr;
      rnotw <= 1'b1;
      sel <= 1'b0;
      strobe <= #1 1'b1;//same as write blocks
      strobe <= #1 1'b0; //mfc will be 0 here, preventing other reads
    end
  end
  always@(core1ReadAddr) begin
    if (mfc) begin //last read done
      addr <= core1ReadAddr;
      rnotw <= 1'b1;
      sel <= 1'b1;
      strobe <= #1 1'b1;//same as write blocks
      strobe <= #1 1'b0; //mfc will be 0 here, preventing other reads
    end
  end
endmodule //END arbiter


module cache(outgoing, miss, RW, enableBit, incoming, address); //idk if compiles
  output reg `CACHE_WORD outgoing;
  output reg miss;
  input enableBit, RW;
  //address is 16 bit with top 10 for the tag line, 4 in the middle selecting line, selecting line and bottom 2 parsing line
  input `CACHE_WORD incoming, address;
  reg `CACHE_LINE cache `CACHE_WORD;
  reg `CACHE_LINE line;
  reg [9:0] tagLine `CACHE_LINE;

  always@(negedge RW) begin //write
    line <= cache[address[5:2]]; //take the line
    tagLine[address[5:0]] <= address[15:6];
    case(address[1:0])
      2'b00: line[6'b001111:6'b000000] <= incoming;
      2'b01: line[6'b011111:6'b010000] <= incoming;
      2'b10: line[6'b101111:6'b100000] <= incoming;
      2'b11: line[6'b111111:6'b110000] <= incoming;
    endcase
    cache[address[5:2]]<=line;
  end

  always@(posedge RW) begin //read
    if(tagLine[address[5:0]] == address[15:6]) begin
      if(enableBit) begin
        case(address[1:0])
          2'b00: outgoing <= line[6'b001111:6'b000000];
          2'b01: outgoing <= line[6'b011111:6'b010000];
          2'b10: outgoing <= line[6'b101111:6'b100000];
          2'b11: outgoing <= line[6'b111111:6'b110000];
          endcase
          miss <= 1'b0;
      end
      else assign miss = 1'b1;
    end
    else miss <= 1'b1;
  end

endmodule



// core implementation
module processor(halt, read_s_address_out, update_proc_value_out, update_proc_address_out, write_slow_mem_address, write_slow_mem_val, counter_out, clk, reset, read_s_address_in, update_proc_val_in, update_proc_address_in, initPC, initSP);

reg [16:0] regfile `REGSIZE;
reg `word mainmem `MEMSIZE;
wire [16:0] ALU0output, ALU0input;
wire [16:0] ALU1output, ALU1input;
reg isInterlocks = 0, pcGo = 0; // have a bit to flag if there is an interlock
reg [3:0] sysflag = 0; //flag to see if system is called
reg [1:0] pausebit = 0;
                           
reg [7:0] pre = 0;

output reg halt;

input clk, reset;
output reg [15:0] read_s_address_out;
reg readybit;
output reg [15:0] write_slow_mem_address, write_slow_mem_val, update_proc_value_out, update_proc_address_out;
output [15:0] counter_out;
input [16:0] read_s_address_in;
input [15:0] initPC, initSP, update_proc_val_in, update_proc_address_in;


//Op Storage
reg `state s00op, s01op, s10op, s11op, s20op, s21op, s30op, s31op;
reg `regID reg00 = 0, reg01 = 0, reg10 = 0, reg11 = 0, reg20 = 0, reg21 = 0,  reg30 = 0, reg31 = 0;
wire `REGSIZE reg2P0acc, reg2P0reg, reg2P1acc, reg2P1reg;
reg `REGSIZE reg10value = 0, reg11value = 0, reg20value = 0, reg21value = 0, stage2acc1 = 0, stage2reg1 = 0, stage2acc2 = 0, stage2reg2 = 0, stage2res1 = 0, stage2res2 = 0;

assign ALU0input = regfile[0];
assign ALU1input = regfile[1];
ALUprime ALU0P(reg2P0acc, reg2P0reg, ALU0input, reg20value, s20op);
ALUprime ALU1P(reg2P1acc, reg2P1reg, ALU1input, reg21value, s21op);
ALU ALU0(ALU0output, stage2acc1, stage2reg1, s20op);
ALU ALU1(ALU1output, stage2acc1, stage2reg1, s21op);   	 

reg `word pc = 0;
reg `word ir, irtemp = 0, newpc;
reg `state s2;
reg `immediate imm0, imm1, imm2;
	//reg `state s = `start;
integer a;
reg isJump;

// cache registers inputs and outputs
wire [15:0] read_from_cache;
wire miss;
reg RW;
reg enablebit; 
reg [15:0] write_to_cache; 
reg [15:0] cache_address;



assign counter_out = pc;


cache cac(read_from_cache, miss, RW, enablebit, write_to_cache, cache_address);
                                  
always @(posedge reset) 
begin
// set Op Storage to Noops; key: s00 = stage 0, instruction slot 0 (rather than 1)
    	// set Op Storage to Noops; key: s00 = stage 0, instruction slot 0 (rather than 1)
	s00op = `OPnop;
	s01op = `OPnop;
	s10op = `OPnop;
	s11op = `OPnop;
	s20op = `OPnop;
	s21op = `OPnop;
	s30op = `OPnop;
	s31op = `OPnop;
	imm0 = 0;
	imm1 = 0;
	imm2 = 0;
	halt = 0;
	pc = 0;
	//s = `start;
	pc = initPC;
	pausebit = 0; 
	enablebit <= 1; //cache stuff gets initialized here
	RW <= 0;
	write_to_cache = 0;
	cache_address <= 0;
	readybit <= read_s_address_in[16];


// DOUBLE CHECK, ask Adrian
	$readmemh1(mainmem); //"program" code from vmem1
	regfile[7][15:0] = initSP;


//$display("Registers: %d %d %d %d %d %d %d %d",regfile[0],regfile[1],regfile[2],regfile[3],regfile[4],regfile[5],regfile[6],regfile[7]);
end

//PC Control
always @(*)
begin
	if(s10op == `OPjnz8 && (regfile[reg10] != 0)) newpc <= {pre, imm1};
	else if (s10op == `OPjp8) newpc <= {pre, imm1};
	else if ((s10op == `OPjz8) && (regfile[reg20] == 0)) newpc <= {pre, imm1};
	else if (s10op == `OPjr) newpc <= regfile[reg10];
	else newpc <= pc + 1;
end

//Interlock Check and load IR from Mem/PC
always @(*) 
begin
		ir = mainmem[pc];
	isInterlocks = (pcGo? (((ir [15] == 0) && ((ir `reg1 == 0) || (ir `reg2 == 0) || (ir `reg1 == 1) || (ir `reg2 == 1))) ||
               	(((s10op == `OPli) || (s10op == `OPlf) || (s10op == `OPa2r) || (s10op == `OPcf8) || (s10op == `OPci8)) && ((ir `reg1 == reg10) || (ir `reg2 == reg10))) ||
               	(((s11op == `OPli) || (s11op == `OPlf) || (s11op == `OPa2r) || (s11op == `OPcf8) || (s11op == `OPci8)) && ((ir `reg1 == reg11) || (ir `reg2 == reg11)))) : 0);

//Get stuff from cache --read
if(irtemp `opcode1 == `OPli || irtemp `opcode1 == `OPlf) begin 
  RW = 1'b1;
  cache_address <= ir `reg2;
  //wait for cache
  if (miss == 1'b1) begin
	pausebit <= 1'b1;
	read_s_address_out = cache_address;
end
else ir `reg1 <= read_from_cache;
end
if (pausebit == 1'b1 && readybit == 1'b1) begin 
           // wait for slowMem
           ir `reg1 <= read_s_address_in[15:0]; 
           pausebit <= 1'b0;
end
// get stuff from cache. If cache misses, send request to big_proc which is read_s_address_out
//if request sent to big proc, start pause flag. Continue pause flag until it’s done.
//read_s_address_out requests things, if requested, set read_s_address_out to 0
// once address is in, read_s_address, 	 	 	 	
// use these to update other processor: 
//update_proc_value_out, update_proc_address_out

end

//Instruction Fetch (Stage 0)
always @(posedge clk) if(!halt) 
begin
if(pausebit == 1'b0)
begin
	if (s00op != `OPsys) irtemp <= ir;
	if (isInterlocks) begin
    	pcGo <= 0;
$display("Interlock set");
	end
	if (s00op == `OPsys || sysflag[0] == 1) begin //flag to check if system called
    	sysflag[0] = 1;
    	s01op <= `OPnop;
$display("Op Sys: %h %h %h %h %h %h", ir, irtemp, s00op, s01op, reg00, reg01);
	end
	else begin
    	if ((irtemp `ALUbit == 0) || (irtemp `opcode1 == `OPli) || (irtemp `opcode1 == `OPlf) || (irtemp `opcode1 == `OPst)) begin
        	s00op <= ((isInterlocks || isJump) ? `OPnop : ir `opcode1);
        	s01op <= ((isInterlocks || isJump) ? `OPnop : ir `opcode2);
        	reg00 <= ((isInterlocks || isJump) ?  0 : ir `reg1);
        	reg01 <= ((isInterlocks || isJump) ?  0 : ir `reg2);
$display("Op Fetch: %h %h %h %h %h %h", ir, irtemp, s00op, s01op, reg00, reg01);
    	end
    	else begin
        	s00op <= ((isInterlocks || isJump) ? `OPnop : ir `opcode1);
        	s01op <= `OPnop;
        	reg00 <= ((isInterlocks || isJump) ?  0 : ir `reg1);
        	reg01 <= 0;
        	imm0 <= ir `immediate;
$display("Span: %h %h %h %h %h %h", ir, irtemp, s00op, s01op, reg00, reg01);
    	end
    	pc <= ((isInterlocks || sysflag[0]) ? pc : newpc);
$display("PC: %d", pc);
    	if (pc == newpc) begin
        	pcGo = 1;
$display("Interlock reset");
    	end
	end
end
end

//Register Read (Stage 1)
always @(posedge clk) begin
 if(!halt) begin

 if(pausebit == 1'b0) begin
/* 	if (sysflag[0] == 1) begin//flag to check if system called
    	sysflag[1] = 1;
    	s10op <= `OPnop;
    	s11op <= `OPnop;
	end
	else begin */
    	s10op <= s00op;
    	s11op <= s01op;
    	reg10value <= regfile[reg10];
    	reg11value <= regfile[reg01];
    	reg10 <= reg00;
    	reg11 <= reg01;
    	imm1 <= imm0;
  end
end
end 

//ALU (Stage 2 and 3)
always @ (posedge clk) if(!halt)
begin
if(pausebit == 1'b0)
begin
	/* if (sysflag[1] == 1) begin//flag to check if system called
    	sysflag[2] = 1;
    	s20op <= `OPnop;
    	s21op <= `OPnop;
	end
	else begin */
	s20op <= s10op;
	s21op <= s11op;
	reg20 <= reg10;
	reg21 <= reg11;
	imm2 <= imm1;
    	if(s20op[4] != 0) begin
    	case (s20op)
        	`OPci8: begin regfile[reg20] <= {1'b0, pre, imm1}; end
        	`OPcf8: begin regfile[reg20] <= {1'b1, pre, imm1}; end
        	`OPpre: begin pre  <= imm1; end
        	//`OPsys: begin halt <= 1; end
    	endcase
    	end
    	else begin
        	if(s20op == `OPst) begin
            	mainmem[regfile[0]] = regfile[reg20];
        	end    
        	if(s21op == `OPst) begin
            	mainmem[regfile[1]] = regfile[reg21];
        	end
        	if(s20op == `OPa2r) begin
            	regfile[reg20] <= regfile[0];
        	end    
        	if(s21op == `OPa2r) begin
            	regfile[reg20] <= regfile[0];
        	end
        	if(s20op == `OPr2a) begin
            	regfile[0] <= regfile[reg20];    
        	end    
        	if(s21op == `OPr2a) begin
            	regfile[1] <= regfile[reg21];
        	end
        	else begin
            	reg20value <= reg10value;
            	reg21value <= reg11value;
            	stage2acc1 <= reg2P0acc;
            	stage2reg1 <= reg2P0reg;
            	stage2acc2 <= reg2P1acc;
            	stage2reg2 <= reg2P1reg;
            	stage2res1 <= ALU0output;
            	stage2res2 <= ALU1output;


end
end
end
end

//Register Write
always @ (posedge clk) if(!halt) begin

if(pausebit == 1'b0)
begin

// update proc value in goes here
// cache write goes HERE
// write slow mem address, write slow mem value
//write to cache and slowMem; update other processor

if (update_proc_address_in != 1'b0)
begin
 RW <= 1'b0;
cache_address <= update_proc_address_in;
write_to_cache <= update_proc_val_in;
update_proc_address_out <= 1'b0;
end

   if(irtemp `opcode1 == `OPst) begin
     RW <=0; //cache
     cache_address <= ir `reg1;
     write_slow_mem_address <= ir `reg1;
     write_to_cache <= ir `reg2;
     write_slow_mem_val <= ir `reg2; //<- & ^may need to store in register that is connected to the wire
//that connects to input to cache
     //may want to cut these V and just have an update bit for top proc to handle
     update_proc_address_out <= ir `reg1;
     update_proc_value_out <= ir `reg2;
   end		


	if ((sysflag[0] == 1) && (s00op == `OPsys) && (s10op == `OPsys) && (s20op == `OPsys)&&(s30op == `OPsys)) begin //flag to check if system called
    	halt <= 1;
$display("Finish: %d %d", regfile[0], regfile[1]);
$display("Halted");
	end
	else begin
	s30op <= s20op;
	s31op <= s21op;
	reg30 <= reg20;
	reg31 <= reg21;
$display("OP store: %h %h %h %h", s30op, s31op, reg30, reg31);
    	if(s30op == `OPlf) begin
        	regfile[reg30] <= mainmem[regfile[0]];
        	regfile[reg30]`floatorint <= 1;
        	if(s21op == `OPlf) begin
            	regfile[reg31] <= mainmem[regfile[1]];
            	regfile[reg31][16] <= 1;
        	end
        	else if(s31op == `OPli) begin
            	regfile[reg31] <= mainmem[regfile[1]];
            	regfile[reg31]`floatorint <= 0;
        	end
        	else if (s31op == `OPnop) regfile[0] <= regfile[0];
        	else regfile[1] <= reg21value;
    	end
    	else if(s30op == `OPli) begin
        	regfile[reg30] <= mainmem[regfile[0]];
        	regfile[reg30]`floatorint <= 0;
        	if(s31op == `OPlf) begin
            	regfile[reg21] <= mainmem[1];
            	regfile[reg21]`floatorint <= 1;
        	end
        	else if(s21op == `OPli) begin
            	regfile[reg31] <= mainmem[1];
            	regfile[reg31]`floatorint <= 0;
        	end
        	else if (s31op == `OPnop) regfile[0] <= regfile[0];
        	else regfile[1] <= reg21value;
    	end
    	else if ((s30op[4] == 0) && (s31op[4] == 0)) begin
        	regfile[0] <= stage2res1;
        	regfile[1] <= stage2res2;
    	end
    	else if(s31op == `OPlf) begin
        	regfile[reg31] <= mainmem[1];
        	regfile[reg31]`floatorint <= 1;
        	if(s30op == `OPlf)
        	begin
            	regfile[reg30] <= mainmem[0];
            	regfile[reg30]`floatorint <= 1;
        	end
        	else if(s30op == `OPli)
            	begin
            	regfile[reg30] <= mainmem[0];
            	regfile[reg30]`floatorint <= 0;
            	end
        	else if (s31op == `OPnop) regfile[0] <= regfile[0];
        	else regfile[0] <= reg20value;
    	end
    	else if(s31op == `OPli) begin
        	regfile[reg31] <= mainmem[1];
        	regfile[reg31]`floatorint <= 0;
        	if(s30op == `OPlf)
        	begin
            	regfile[reg30] <= mainmem[0];
            	regfile[reg30]`floatorint <= 1;
        	end
        	else if(s30op == `OPli)
            	begin
            	regfile[reg30] <= mainmem[0];
            	regfile[reg30]`floatorint <= 0;
            	end
        	else if (s31op == `OPnop) regfile[0] <= regfile[0];
        	else regfile[0] <= reg20value;
    	end
    	else regfile[0] <= regfile[0];
	end
end
end
endmodule

//BEGIN full system module; connects all component modules, accepts testbench stimulus
module big_processor(halted, pc0, pc1, reset, clk);
  output halted;
  output [15:0] pc0, pc1;
  input reset, clk;

  //wires to connect modules here
  wire halt_0, halt_1;
  wire [16:0] core0ReadData, core1ReadData;
  wire [15:0] core0ReadAddr, core1ReadAddr, core0WriteAddr, core1WriteAddr,
      core0WriteData, core1WriteData, core0UpdateData, core0UpdateAddr,
	  core1UpdateData, core1UpdateAddr;
  
  //only halt the system when both cores have halted
  assign halted = halt_0 && halt_1;

  //modules
  //arbiter has 2 outputs, rest inputs; arbiter contains slowmem instance
  arbiter arb(core0ReadData, core1ReadData, core0ReadAddr, core1ReadAddr, 
      core0WriteAddr, core1WriteAddr, core0WriteData, core1WriteData, clk);
      
  //each core contains the relevant cache instance
  processor core0(halt_0, core0ReadAddr, core0UpdateData,
      core0UpdateAddr, core0WriteAddr, core0WriteData, pc0,
      //outputs above, inputs below
      clk, reset, core0ReadData, core1UpdateData, core1UpdateAddr,
      16'h0000, 16'hffff);
       
  processor core1(halt_1, core1ReadAddr, core1UpdateData,
      core1UpdateAddr, core1WriteAddr, core1WriteData, pc1,
      //outputs above, inputs below
      clk, reset, core1ReadData, core0UpdateData, core0UpdateAddr,
      16'h8000, 16'hbfff);
endmodule //END full system


//BEGIN testbench module; stimulus to big_processor
module testbench;
  reg reset = 0;
  reg clk = 0;
  wire halted;
  wire [15:0] counter0, counter1;
  big_processor P(halted, counter0, counter1, reset, clk);
  initial begin
    #10 reset = 1; //start with a reset to initialize internal vars
    #10 reset = 0;
    while (!halted) begin
      #10 clk = 1;
      #10 clk = 0;
    end
    $display("!HALTED!");
    $display("core0 PC: ", counter0);
    $display("core1 PC: ", counter1);
    $finish;
  end
endmodule //END testbench
