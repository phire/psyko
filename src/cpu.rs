
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

struct Icache {

}

impl Icache {
    fn read(&self, address: u32, bios: &Bios) -> u32 {
        return bios.read32(address);
    }
}


enum ALU {
    ADD,
    ADDU,
    SUB,
    SUBU,
    AND,
    OR,
    XOR,
    NOR,
    SLL,
    SRL,
    SRA,
    SLT,
    SLTU,
}

#[derive(Copy, Clone)]
enum MemMode {
    NOP,
    LB,
    LH,
    LWL,
    LW,
    LBU,
    LHU,
    LWR,
    SB,
    SH,
    SWL,
    SW,
    SWR,
}

struct Cpu {
    regs : [u32; 32],
    pc : u32,
    hi : u32,
    lo : u32,

    // stage 1 result (Instruction Fetch)
    fetched_instruction : u32,

    // stage 2 result (Read Register)
    temp_a : u32,
    temp_b : u32,
    input_a : u32,
    input_b : u32,
    input_store : u32,
    dest : u32,
    alu_mode : ALU,
    mem_mode : MemMode,
    do_branch : bool,

    // stage 3 result (ALU)
    alu_result : u32,
    store_data : u32,
    dest_2 : u32,
    mem_mode_2 : MemMode,

    // stage 4 result (Memory)
    mem_result : u32,
    dest_3 : u32,
    write_back_mask : u32,

    icache : Icache,
    debug_rotate : usize,
}

impl Cpu {
    fn read8(&self, addr:u32) -> u32 {
        let shift = addr & 3;
        return self.read(addr & 0xffffffc, 0x1 << shift) >> (shift*8);
    }
    fn read16(&self, addr:u32) -> u32 {
        let shift = addr & 2;
        return self.read(addr & 0xffffffd, 0x3 << shift) >> (shift*8);
    }
    fn read32(&self, addr:u32) -> u32 {
        return self.read(addr, 0xf);
    }
    fn read(&self, addr:u32, select:u8) -> u32 {
        return 0;
    }

    fn write8(&self, addr:u32, data:u32) {
        let shift = addr & 3;
        self.write(addr & 0xffffffc, data << (shift*8), 0x1 << shift);
    }
    fn write16(&self, addr:u32, data:u32) {
        let shift = addr & 2;
        self.write(addr & 0xffffffd, data << (shift*8), 0x3 << shift);
    }
    fn write32(&self, addr:u32, data:u32) {
        self.write(addr, data, 0xf);
    }
    fn write(&self, addr:u32, data:u32, select:u8) {
    }

    fn run_pipeline_cycle(&mut self, bios : &Bios) {
        // This function runs each of the 5 stages.
        // In hardware all the stages would be run in parallel, but in software
        // we run the stages sequenctally in reverse order.


        let mut debug = ["".to_string(), "".to_string(), "".to_string(), "".to_string(), "".to_string()];

        // Write Back Stage
        // =========================================================
        if self.dest_3 != 0 {
            if self.write_back_mask == 0xf {
                self.regs[self.dest_3 as usize] = self.mem_result;
            } else { // Unaligned loads
                let old = self.regs[self.dest_3 as usize];
                // TODO: can we generate these masks with faster code than a switch table?
                self.regs[self.dest_3 as usize] = match self.write_back_mask {
                    0x1 => old & 0xffffff00 | self.mem_result & 0x000000ff,
                    0x3 => old & 0xffff0000 | self.mem_result & 0x0000ffff,
                    0x7 => old & 0xff000000 | self.mem_result & 0x00ffffff,
                    0xe => old & 0x000000ff | self.mem_result & 0xffffff00,
                    0xc => old & 0x0000ffff | self.mem_result & 0xffff0000,
                    0x8 => old & 0x00ffffff | self.mem_result & 0xff000000,
                    _ => unreachable!()
                };
            }
            debug[4] = format!("                  => r{:}", self.dest_3);
        }


        // Memory Stage
        // ======================================================
        // The address was calculated in the last cycle.
        self.write_back_mask = 0xf;

        // TODO: Alignment exceptions
        // TODO: Stalls
        // TODO: Check unaligned instruction correctness
        self.mem_result = match self.mem_mode_2 {
            MemMode::NOP => self.alu_result,
            MemMode::LB => self.read8(self.alu_result) as i8 as u32,
            MemMode::LH => self.read16(self.alu_result) as i16 as u32,
            MemMode::LWL => { self.write_back_mask = (0x78 >> (self.alu_result & 3)) & 0xf; self.read32(self.alu_result) },
            MemMode::LW => self.read32(self.alu_result),
            MemMode::LBU => self.read8(self.alu_result),
            MemMode::LHU => self.read16(self.alu_result),
            MemMode::LWR => { self.write_back_mask = 0xf >> (self.alu_result & 3); self.read32(self.alu_result) },
            MemMode::SB => { self.write8(self.alu_result, self.store_data); 0 },
            MemMode::SH => { self.write16(self.alu_result, self.store_data); 0 },
            MemMode::SW => { self.write32(self.alu_result, self.store_data); 0},
            MemMode::SWL => { self.write(self.alu_result & 0xfffffffc, self.store_data, (0x78 >> (self.alu_result & 3)) & 0xf); 0},
            MemMode::SWR => { self.write(self.alu_result & 0xfffffffc, self.store_data, 0xf >> (self.alu_result & 3)); 0},
        };

        debug[3] = match self.mem_mode_2 {
            MemMode::NOP if self.dest_2 == 0 => format!(""), 
            MemMode::NOP => format!("              = {:08x}", self.alu_result),
            MemMode::SB | MemMode::SH | MemMode::SW | MemMode::SWL | MemMode::SWR
              => format!("  {:08x} => [{:08x}]", self.store_data, self.alu_result & 0xfffffffc),
            _ => format!(" [{:08x}]", self.alu_result & 0xfffffffc),
        };

        self.dest_3 = self.dest_2;


        // ALU Stage
        // ===============================================
        let data_a = match self.input_a {
            0 => 0,
            1 ... 15 if self.input_a == self.dest_2 => self.alu_result, // Bypass network
            1 ... 15 => self.regs[self.input_a as usize],
            19 => self.temp_a,
            20 => self.temp_b,
            _ => unreachable!(),
        };
        let data_b = match self.input_b {
            0 => 0,
            1 ... 15 if self.input_b == self.dest_2 => self.alu_result, // Bypass network
            1 ... 15 => self.regs[self.input_b as usize],
            19 => self.temp_a,
            20 => self.temp_b,
            _ => unreachable!(),
        };

        self.alu_result = match self.alu_mode {
            ALU::ADD  => data_a + data_b, // TODO: trap on overflow
            ALU::ADDU => data_a.wrapping_add(data_b),
            ALU::SUB  => data_a - data_b, // TODO: trap on overflow
            ALU::SUBU => data_a.wrapping_sub(data_b),
            ALU::AND  => data_a & data_b,
            ALU::OR   => data_a | data_b,
            ALU::XOR  => data_a ^ data_b,
            ALU::NOR  => !(data_a | data_b),
            ALU::SLL  => data_a.wrapping_shl(data_b & 0x1f),
            ALU::SRL  => data_a.wrapping_shr(data_b & 0x1f),
            ALU::SRA  => (data_a as i32).wrapping_shr(data_b & 0x1f) as u32,
            ALU::SLT  => ((data_a as i32) < (data_b as i32)) as u32,
            ALU::SLTU => (data_a < data_b) as u32,
        };
        self.dest_2 = self.dest;

        debug[2] = match self.alu_mode {
            ALU::ADD | ALU::ADDU => format!(" {:x} + {:x} ", data_a, data_b),
            ALU::SUB | ALU::SUBU => format!(" {:x} - {:x} ", data_a, data_b),
            ALU::AND => format!(" {:x} & {:x} ", data_a, data_b),
            ALU::OR  => format!(" {:x} | {:x} ", data_a, data_b),
            ALU::XOR => format!(" {:x} ^ {:x} ", data_a, data_b),
            ALU::NOR => format!(" ~({:x} | {:x})", data_a, data_b),
            ALU::SLL => format!(" {:x} << {:}", data_a, data_b),
            ALU::SRL => format!(" {:x} >> {:}", data_a, data_b),
            ALU::SRA => format!(" {:x} >>> {:}", data_a, data_b),
            ALU::SLT | ALU::SLTU => format!(" {:>8x} < {:>8x}", data_a, data_b),
        };

        match self.mem_mode {  // Hide nop alu operations
            MemMode::NOP if self.dest == 0 => { debug[2] = "".to_string(); },
            _ => {}
        };

        if self.do_branch {
            debug[2] = format!(" branch to {:08x}", self.alu_result);
            let old_pc = self.pc;
            self.pc = self.alu_result;
            self.do_branch = false;
            if self.dest_2 > 0 { // Write returm address?
                // Note: is there a spare +8 adder, or should this go through the ALU?
                self.alu_result = old_pc + 8 
            }
        }

        self.store_data = match self.input_store {
            0 => 0,
            1 ... 15 if self.input_store == self.dest_2 => self.alu_result, // Bypass network
            1 ... 15 => self.regs[self.input_store as usize],
            _ => unreachable!(),
        };
        self.mem_mode_2 = self.mem_mode;
        self.mem_mode = MemMode::NOP;


        // Read Stage (instruction decoding)
        // =============================================
        let opcode = self.fetched_instruction >> 26;
        let rs = (self.fetched_instruction >> 21) & 0x1f;
        let rt = (self.fetched_instruction >> 16) & 0x1f;
        let rd = (self.fetched_instruction >> 11) & 0x1f;
        let imm5 = (self.fetched_instruction >> 6) & 0x1f;
        let imm16 = self.fetched_instruction & 0xffff;
        let imm26 = self.fetched_instruction & 0x3ffffff;

        debug[1] = format!("     opcode: {:x}", self.fetched_instruction);
        match opcode {
            0x0 => {
                let subop = self.fetched_instruction & 0x3f;

                match subop {
                    0 | 2 | 3 | 4 | 6 | 7 => { // Shifts
                        self.input_a = rt;
                        self.input_b = match subop & 0x4 {
                            0 => { self.temp_a = imm5; 19 }, // shift-imm
                            4 => rs, // shift-reg
                            _ => unreachable!()
                        };
                        self.alu_mode = match subop & 0x3 {
                            0 => ALU::SLL,
                            2 => ALU::SRL,
                            3 => ALU::SRA,
                            _ => unreachable!()
                        };
                        self.dest = rd;
                    },
                    0x8 | 0x9 => {
                        self.input_a = rs;
                        self.input_b = 0;
                        self.alu_mode = ALU::OR;
                        self.do_branch = true;
                        if subop == 9 { // Calculate return address?
                            self.dest = rd;
                        } else {
                            self.dest = 0;
                        }
                    },
                    0x30 ... 0x38 | 0x3a | 0x3b => { // reg ALU ops
                        self.input_a = rs;
                        self.input_b = rt;
                        self.alu_mode = match subop & 0x7 {
                            0 => ALU::ADD,
                            1 => ALU::ADDU,
                            2 => ALU::SUB,
                            3 => ALU::SUBU,
                            4 => ALU::AND,
                            5 => ALU::OR,
                            6 => ALU::XOR,
                            7 => ALU::NOR,
                            10 => ALU::SLT,
                            11 => ALU::SLTU,
                            _ => unreachable!()
                        };
                        self.dest = rd;
                    },
                    _ => { panic!("Unimplemented Opcode") }
                };
            },
            0x1 => { // bltz/bgez
                self.temp_a = self.pc;
                self.temp_b = (imm16 as i16 as u32) << 2;
                self.input_a = 19;
                self.input_b = 20;
                self.alu_mode = ALU::ADDU;
                unimplemented!();
                //self.do_branch = match rt {
                //    0x0 => (self.read_reg(rs) as i32) < 0,
                //    0x1 => (self.read_reg(rs) as i32) >= 0,
                //    _ => { panic!("Unimplemented conditional call Opcode") }
                //};
                self.dest = 0;
            },
            0x2 | 0x3 => {
                self.temp_a = self.pc & 0xfffffff0;
                self.temp_b = imm26 << 2;
                self.input_a = 19;
                self.input_b = 20;
                self.alu_mode = ALU::OR;
                self.do_branch = true;
                if opcode == 3 { // Calculate return address?
                    self.dest = 31;
                } else {
                    self.dest = 0;
                }
            },
            0x4 ... 0x7 => {
                self.temp_a = self.pc;
                self.temp_b = (imm16 as i16 as u32) << 2;
                self.input_a = 19;
                self.input_b = 20;
                self.alu_mode = ALU::ADDU;
                unimplemented!();
                /* TODO: work out how to implement this.
                self.do_branch = match opcode {
                    4 => self.read_reg(rs) == self.read_reg(rt),
                    5 => self.read_reg(rs) != self.read_reg(rt),
                    6 => self.read_reg(rs) as i32 <= 0,
                    7 => self.read_reg(rs) as i32 > 0,
                    _ => unreachable!(),
                };*/
                self.dest = 0;
            },
            0x8 ... 0xe => {
                self.temp_a = if opcode > 0xb {
                    imm16 } else {
                    imm16 as i16 as u32
                };
                self.input_a = rs;
                self.input_b = 19;
                self.alu_mode = match opcode {
                    0x8 => ALU::ADD,
                    0x9 => ALU::ADDU,
                    0xa => ALU::SLT,
                    0xb => ALU::SLTU,
                    0xc => ALU::AND,
                    0xd => ALU::OR,
                    0xe => ALU::XOR,
                    _ => unreachable!()
                };
                self.dest = rt;
            },
            0x20 ... 0x2f => {
                self.temp_a = imm16;
                self.input_a = rs;
                self.input_b = 19;
                self.alu_mode = ALU::ADDU;
                if opcode & 0x8 == 0 { // Load              
                    self.dest = rt;
                } else { // Store
                    self.input_store = rt;
                    self.dest = 0;
                }
                self.mem_mode = match opcode {
                    0x20 => MemMode::LB,
                    0x21 => MemMode::LH,
                    0x22 => MemMode::LWL,
                    0x23 => MemMode::LW,
                    0x24 => MemMode::LBU,
                    0x25 => MemMode::LHU,
                    0x26 => MemMode::LWR,
                    0x28 => MemMode::SB,
                    0x29 => MemMode::SH,
                    0x2a => MemMode::SWL,
                    0x2b => MemMode::SW,
                    0x2e => MemMode::SWR,
                    _ => panic!("Unimplemented memory Opcode") 
                };
            },
            0xf => { // load shifted immediate
                self.temp_a = imm16;
                self.temp_b = 16;
                self.input_a = 19;
                self.input_b = 20;
                self.alu_mode = ALU::SLL;
                self.dest = rt;
            }
            _ => { panic!("Unimplemented Opcode") }
        }


        // Instruction Fetch Stage
        // =============================================
        debug[0] = format!(" {:08x}:", self.pc);
        self.fetched_instruction = self.icache.read(self.pc, bios); // TODO: takes 1 cycle to generate a result
        self.pc = self.pc + 4;

        println!("{:25}|{:25}|{:25}|{:25}|{:25}", debug[(self.debug_rotate + 4) % 5],
                                                  debug[(self.debug_rotate + 3) % 5], 
                                                  debug[(self.debug_rotate + 2) % 5], 
                                                  debug[(self.debug_rotate + 1) % 5], 
                                                  debug[(self.debug_rotate + 0) % 5]);
        self.debug_rotate = (self.debug_rotate + 1) % 5;
    }

    pub fn run(&mut self, cycles : u32, bios : &Bios) -> u32 {
        let mut count = cycles;
        while count > 0 {
            self.run_pipeline_cycle(bios);
            count = count - 1;
        }
        return cycles;
    }

    pub fn new() -> Cpu {
        return Cpu {
            regs : [0; 32],
            pc : 0xbfc00000,
            hi : 0,
            lo : 0,
            fetched_instruction : 0,
            temp_a : 0,
            temp_b : 0,
            input_a : 0,
            input_b : 0,
            input_store : 0,
            dest : 0,
            do_branch : false,
            alu_mode: ALU::OR,
            mem_mode: MemMode::NOP,
            alu_result : 0,
            dest_2 : 0,
            store_data : 0,
            mem_result : 0,
            write_back_mask: 0xf,
            mem_mode_2 : MemMode::NOP,
            dest_3 : 0,
            icache : Icache {},
            debug_rotate : 1,
        }
    }
}

struct Bios {
    data: Vec<u8>,
    mask: u32,
}

impl Bios {
    fn new() -> Bios {
        // Hardcoded path for now
        let path = Path::new("Scph5502.bin");

        let mut f = match File::open(&path) {
            Err(e) => panic!("couldn't open {}: {}", path.display(), e.description()),
            Ok(file) => file,
        };
        let mut buffer = vec![];

        f.read_to_end(&mut buffer);

        assert!((buffer.len() & (buffer.len() - 1)) == 0, "bios length isn't a power of 2");

        return Bios {data: buffer.to_vec(), mask: (buffer.len() - 1) as u32};
    }

    fn read32(&self, address: u32) -> u32 {
        let offset = (address & self.mask) as usize;

        // Cause we can totally read 32bits in a single cycle
        return (self.data[offset] as u32) |
               (self.data[offset+1] as u32) << 8 |
               (self.data[offset+2] as u32) << 16 |
               (self.data[offset+3] as u32) << 24;
    }
}

// CPU domain: CPU, RAM, BIOS, GPU DMA, 
pub struct CpuDomain {
    cpu : Cpu,
    bios : Bios,
}

impl CpuDomain {
    pub fn new() -> CpuDomain {
        CpuDomain {cpu : Cpu::new(), bios : Bios::new()}
    }

    pub fn run(&mut self, cycles : u32) -> u32 {
        return self.cpu.run(cycles, &self.bios);
    }
}
