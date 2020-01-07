# CS480G
Team Projects I worked on in Advanced Microcomputer Architecture @ UK, Dr. Dietz

This course was centered on teams of students building and iterating upon a processor architecture in Verilog. For each new assignment, the team members were shuffled, so that we were always working with new people, and experience from each previous team's implementations could be synthesized into the next implementation.

Included Files:

tacky.aik - assembler specification for Dr. Dietz's AIK tool(http://aggregate.org/EE480/assembler.html), used to specify an assembly language

a2final.v - first processor implementation, includes floating-point arithmetic(implementation provided by Dr. Dietz)

a2TextSuite.txt - simple verilog program to test the processor in a2final.v

a3tacky.v - second processor implementation: twin-accumulator pipelined processor with RAW and WAR dependency handling

a3TestSuite.txt - edit of a2TextSuite.txt to add tests for RAW/WAR dependencies

tacky.v - final processor implementation, includes function load and store instructions, both to/from a fast cache and a slow RAM.

testProgram.tacky - final testing suite, added tests for cache and RAM load/stores, and dependencies for each
