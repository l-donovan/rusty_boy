use crate::mem;

// Our target cycle rate is ~4m Hz
// This is somewhere in the neighborhood of 700,000 instructions/sec,
// which is great because we're hitting about 35,000,000 instructions/sec.

const CYCLE_RATE: usize = 4_194_304;
const SPEED_MULTIPLIER: f32 = 1.0;

pub struct CPU {
    mem: mem::MappedMemory,
    pc: u16,
    sp: u16,
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            mem: mem::MappedMemory::new(),
            pc: 0,
            sp: 0,
            af: 0,
            bc: 0,
            de: 0,
            hl: 0,
        }
    }

    pub fn boot(&mut self) {
        // Map 256 Byte GB bootstrap ROM to 0x0000
        // Start reading at 0x0000

        // The boostrap does the following:
        // - Initialize RAM
        // - Initialize sound
        // - Copy Nintendo logo from cartridge ROM to display RAM
        // - Draw logo at top edge of screen
        // - Scroll logo and play sound
        // - DRM logo verification or hang (no way)
        // - Cartridge ROM header checksum verification or hang
        // - Remove GB bootstrap ROM from memory map
    }

    // Memory manipulation aliases

    pub fn mem_at(&self, addr: u16) -> u8 {
        self.mem.mem_at(addr)
    }

    pub fn mem_set(&self, addr: u16, val: u8) {
        self.mem.mem_set(addr, val)
    }

    // Instruction handlers

    fn inst_ld_sp_d16(&mut self) {
        trace!("LD SP,d16");
        let val = u16::from_le_bytes([self.mem_at(self.pc + 1), self.mem_at(self.pc + 2)]);
        self.sp = val;
        trace!("SP <- {:#06x}", self.sp);
        self.pc += 3;
    }

    fn inst_ld_hld_a(&mut self) {
        trace!("LD (HL-),A");
        // *HL = A
        self.mem_set(self.hl, self.af.to_be_bytes()[0]);
        // HL = HL - 1
        self.hl -= 1;
        trace!("HL <- {:#06x}", self.hl);
        self.pc += 1;
    }

    fn inst_ld_hli_a(&mut self) {
        trace!("LD (HL+),A");
        // *HL = A
        self.mem_set(self.hl, self.af.to_be_bytes()[0]);
        // HL = HL + 1
        self.hl += 1;
        trace!("HL <- {:#06x}", self.hl);
        self.pc += 1;
    }

    fn inst_ld_hl_d16(&mut self) {
        trace!("LD HL,d16");
        let val = u16::from_le_bytes([self.mem_at(self.pc + 1), self.mem_at(self.pc + 2)]);
        self.hl = val;
        trace!("HL <- {:#06x}", self.hl);
        self.pc += 3;
    }

    fn inst_xor_a(&mut self) {
        trace!("XOR A");
        self.set_a(0);
        self.pc += 1;
    }

    fn inst_jr_nz_r8(&mut self) {
        trace!("JR NZ,r8");

        // If the Z register is not set, jump
        if (self.af.to_be_bytes()[1] >> 7) == 0 {
            let jmp_offset = self.mem_at(self.pc + 1) as i8;
            self.pc = ((self.pc as i16) + (jmp_offset as i16)) as u16;
        }

        self.pc += 2;
    }

    fn inst_cb_bit_7_h(&mut self) {
        trace!("BIT 7,H");

        let new_bit = (!((self.hl.to_be_bytes()[0] & 0b10000000) >> 7) & 0b1) as u16;
        self.af = (self.af & !(1u16 << 7)) | (new_bit << 7);
        trace!("AF <- {:#06x}", self.af);

        self.pc += 1;
    }

    fn inst_ld_c_d8(&mut self) {
        trace!("LD C,d8");
        let val = self.mem_at(self.pc + 1);
        self.set_c(val);
        trace!("BC <- {:#06x}", self.bc);
        self.pc += 2;
    }

    fn inst_ld_a_d8(&mut self) {
        trace!("LD A,d8");
        let val = self.mem_at(self.pc + 1);
        self.set_a(val);
        trace!("AF <- {:#06x}", self.af);
        self.pc += 2;
    }

    fn inst_ld_cp_a(&mut self) {
        trace!("LD (C),A");
        let c_val = self.get_c();
        let addr = 0xff00 | u16::from(c_val);
        self.mem_set(addr, self.get_a());
        self.pc += 1;
    }

    fn inst_ld_hlp_a(&mut self) {
        trace!("LD (HL),A");
        self.mem_set(self.hl, self.get_a());
        self.pc += 1;
    }

    fn inst_ld_a16p_a(&mut self) {
        trace!("LD (a16),A");
        let val = u16::from_le_bytes([self.mem_at(self.pc + 1), self.mem_at(self.pc + 2)]);
        self.mem_set(val, self.get_a());
        self.pc += 3;
    }

    fn inst_inc_c(&mut self) {
        trace!("INC C");
        self.set_c(self.get_c() + 1);
        self.pc += 1;
    }

    fn inst_inc_de(&mut self) {
        trace!("INC DE");
        self.de += 1;
        self.pc += 1;
    }

    fn inst_inc_hl(&mut self) {
        trace!("INC HL");
        self.hl += 1;
        self.pc += 1;
    }

    fn inst_ld_a8p_a(&mut self) {
        trace!("LD (a8),A");
        let offset = self.mem_at(self.pc + 1);
        let a_val = self.af.to_be_bytes()[0];
        let addr = 0xff00 | u16::from(offset);
        self.mem_set(addr, a_val);
        self.pc += 2;
    }

    fn inst_ld_de_d16(&mut self) {
        trace!("LD DE,d16");
        let val = u16::from_le_bytes([self.mem_at(self.pc + 1), self.mem_at(self.pc + 2)]);
        self.de = val;
        trace!("DE <- {:#06x}", self.de);
        self.pc += 3;
    }

    fn inst_ld_a_dep(&mut self) {
        trace!("LD A,(DE)");
        let val = self.mem_at(self.de);
        self.set_a(val);
        self.pc += 1;
    }

    fn inst_call_a16(&mut self) {
        trace!("CALL a16");
        let addr = u16::from_le_bytes([self.mem_at(self.pc + 1), self.mem_at(self.pc + 2)]);
        self.sp_push(self.pc + 3);
        self.pc = addr;
    }

    fn inst_ld_c_a(&mut self) {
        trace!("LD C,A");
        self.set_c(self.get_a());
        self.pc += 1;
    }

    fn inst_ld_a_e(&mut self) {
        trace!("LD A,E");
        self.set_a(self.get_e());
        self.pc += 1;
    }

    fn inst_ld_b_d8(&mut self) {
        trace!("LD B,d8");
        let val = self.mem_at(self.pc + 1);
        self.set_b(val);
        self.pc += 2;
    }

    fn inst_push_bc(&mut self) {
        trace!("PUSH BC");
        self.sp_push(self.bc);
        self.pc += 1;
    }

    fn inst_push_de(&mut self) {
        trace!("PUSH DE");
        self.sp_push(self.de);
        self.pc += 1;
    }

    fn inst_push_hl(&mut self) {
        trace!("PUSH HL");
        self.sp_push(self.hl);
        self.pc += 1;
    }

    fn inst_push_af(&mut self) {
        trace!("PUSH AF");
        self.sp_push(self.af);
        self.pc += 1;
    }

    fn inst_cb_rl_c(&mut self) {
        // TODO: This is nightmare code
        trace!("RL C");

        let carry_flag_pos = 4;
        let flags = self.af.to_be_bytes()[1];
        let old_carry = (flags >> carry_flag_pos) & 1;

        let c_val = u16::from(self.bc.to_be_bytes()[1]) << 1;
        let flag_mask = c_val.to_be_bytes()[0] << carry_flag_pos;
        let shifted_c_val = c_val.to_be_bytes()[1];
        let new_c_val = shifted_c_val | old_carry;

        self.af = (self.af & !(1u16 << carry_flag_pos)) | u16::from(flag_mask);
        self.bc = (self.bc & 0xff00) | u16::from(new_c_val);
        self.pc += 1;
    }

    fn inst_rla(&mut self) {
        // TODO: This is nightmare code
        trace!("RLA");

        let carry_flag_pos = 4;
        let flags = self.get_f();
        let old_carry = (flags >> carry_flag_pos) & 1;

        let a_val = u16::from(self.get_a()) << 1;
        let flag_mask = a_val.to_be_bytes()[0] << carry_flag_pos;
        let shifted_a_val = a_val.to_be_bytes()[1];
        let new_a_val = shifted_a_val | old_carry;
        let new_f_val = (flags & !(1u8 << carry_flag_pos)) | flag_mask;

        self.set_a(new_a_val);
        self.set_f(new_f_val);

        self.pc += 1;
    }

    fn inst_pop_bc(&mut self) {
        trace!("POP BC");
        self.bc = self.sp_pop();
        self.pc += 1;
    }

    fn inst_dec_b(&mut self) {
        trace!("DEC B");
        let b_val = self.get_b() - 1;
        self.set_b(b_val);
        self.set_flag(7, b_val == 0);
        self.pc += 1;
    }

    fn inst_dec_c(&mut self) {
        trace!("DEC C");
        let c_val = self.get_c() - 1;
        self.set_c(c_val);
        self.set_flag(7, c_val == 0);
        self.pc += 1;
    }

    fn inst_dec_d(&mut self) {
        trace!("DEC D");
        let d_val = self.get_d() - 1;
        self.set_d(d_val);
        self.set_flag(7, d_val == 0);
        self.pc += 1;
    }

    fn inst_dec_e(&mut self) {
        trace!("DEC E");
        let e_val = self.get_e() - 1;
        self.set_e(e_val);
        self.set_flag(7, e_val == 0);
        self.pc += 1;
    }

    fn inst_dec_h(&mut self) {
        trace!("DEC H");
        let h_val = self.get_h() - 1;
        self.set_h(h_val);
        self.set_flag(7, h_val == 0);
        self.pc += 1;
    }

    fn inst_dec_l(&mut self) {
        trace!("DEC L");
        let l_val = self.get_l() - 1;
        self.set_l(l_val);
        self.set_flag(7, l_val == 0);
        self.pc += 1;
    }

    fn inst_dec_a(&mut self) {
        trace!("DEC A");
        let a_val = self.get_a() - 1;
        self.set_a(a_val);
        self.set_flag(7, a_val == 0);
        self.pc += 1;
    }

    fn inst_ret(&mut self) {
        trace!("RET");
        self.pc = self.sp_pop();
    }

    fn inst_cp_d8(&mut self) {
        trace!("CP d8");
        let val = self.mem_at(self.pc + 1);
        let result = self.get_a() - val;
        self.set_flag(7, result == 0);
        self.pc += 2;
    }

    // Stack manipulation

    fn sp_push(&mut self, val: u16) {
        let val_bytes = val.to_be_bytes();

        self.mem_set(self.sp, val_bytes[0]);
        self.sp -= 1;
        self.mem_set(self.sp, val_bytes[1]);
        self.sp -= 1;
    }

    fn sp_pop(&mut self) -> u16 {
        self.sp += 1;
        let b0 = self.mem_at(self.sp);
        self.sp += 1;
        let b1 = self.mem_at(self.sp);
        return u16::from_be_bytes([b1, b0]);
    }

    // Register manipulation

    fn set_a(&mut self, val: u8) {
        self.af &= 0x00ff;
        self.af |= u16::from(val) << 8;
    }

    fn get_a(&self) -> u8 {
        return self.af.to_be_bytes()[0];
    }

    fn get_f(&self) -> u8 {
        return self.af.to_be_bytes()[1];
    }

    fn set_b(&mut self, val: u8) {
        self.bc &= 0x00ff;
        self.bc |= u16::from(val) << 8;
    }

    fn get_b(&self) -> u8 {
        return self.bc.to_be_bytes()[0];
    }

    fn set_c(&mut self, val: u8) {
        self.bc &= 0xff00;
        self.bc |= u16::from(val);
    }

    fn get_c(&self) -> u8 {
        return self.bc.to_be_bytes()[1];
    }

    fn set_d(&mut self, val: u8) {
        self.de &= 0x00ff;
        self.de |= u16::from(val) << 8;
    }

    fn get_d(&self) -> u8 {
        return self.de.to_be_bytes()[0];
    }

    fn set_e(&mut self, val: u8) {
        self.de &= 0xff00;
        self.de |= u16::from(val);
    }

    fn get_e(&self) -> u8 {
        return self.de.to_be_bytes()[1];
    }

    fn set_h(&mut self, val: u8) {
        self.hl &= 0x00ff;
        self.hl |= u16::from(val) << 8;
    }

    fn get_h(&self) -> u8 {
        return self.hl.to_be_bytes()[0];
    }

    fn set_l(&mut self, val: u8) {
        self.hl &= 0xff00;
        self.hl |= u16::from(val);
    }

    fn get_l(&self) -> u8 {
        return self.hl.to_be_bytes()[1];
    }

    fn set_f(&mut self, val: u8) {
        self.af &= 0xff00;
        self.af |= u16::from(val);
    }

    fn set_flag(&mut self, idx: u8, val: bool) {
        let a_val = self.af.to_be_bytes()[0];
        let flags = self.af.to_be_bytes()[1];
        let uval = val as u8;

        let new_f_val = (flags & !(1u8 << idx)) | (uval << idx);

        self.af = u16::from_be_bytes([a_val, new_f_val]);
    }

    pub fn cycle(&mut self) {
        let inst = self.mem_at(self.pc);

        let inst_handler: fn(&mut CPU) = match inst {
            0x05 => CPU::inst_dec_b,
            0x06 => CPU::inst_ld_b_d8,
            0x0c => CPU::inst_inc_c,
            0x0d => CPU::inst_dec_c,
            0x0e => CPU::inst_ld_c_d8,
            0x11 => CPU::inst_ld_de_d16,
            0x13 => CPU::inst_inc_de,
            0x15 => CPU::inst_dec_d,
            0x17 => CPU::inst_rla,
            0x1a => CPU::inst_ld_a_dep,
            0x1d => CPU::inst_dec_e,
            0x20 => CPU::inst_jr_nz_r8,
            0x21 => CPU::inst_ld_hl_d16,
            0x22 => CPU::inst_ld_hli_a,
            0x23 => CPU::inst_inc_hl,
            0x25 => CPU::inst_dec_h,
            0x2d => CPU::inst_dec_l,
            0x31 => CPU::inst_ld_sp_d16,
            0x32 => CPU::inst_ld_hld_a,
            0x3d => CPU::inst_dec_a,
            0x3e => CPU::inst_ld_a_d8,
            0x4f => CPU::inst_ld_c_a,
            0x7b => CPU::inst_ld_a_e,
            0x77 => CPU::inst_ld_hlp_a,
            0xaf => CPU::inst_xor_a,
            0xc1 => CPU::inst_pop_bc,
            0xc5 => CPU::inst_push_bc,
            0xc9 => CPU::inst_ret,
            0xcd => CPU::inst_call_a16,
            0xd5 => CPU::inst_push_de,
            0xe0 => CPU::inst_ld_a8p_a,
            0xe2 => CPU::inst_ld_cp_a,
            0xe5 => CPU::inst_push_hl,
            0xea => CPU::inst_ld_a16p_a,
            0xf5 => CPU::inst_push_af,
            0xfe => CPU::inst_cp_d8,
            0xcb => {
                self.pc += 1;
                self.cycle_cb();
                return;
            }
            _ => {
                error!("Unknown instruction {:#04x}", inst);
                return;
            }
        };

        inst_handler(self);
    }

    fn cycle_cb(&mut self) {
        let inst = self.mem_at(self.pc);

        let inst_handler: fn(&mut CPU) = match inst {
            0x11 => CPU::inst_cb_rl_c,
            0x7c => CPU::inst_cb_bit_7_h,
            _ => {
                error!("Unknown instruction {:#04x}", 0xcb00 | u16::from(inst));
                return;
            }
        };

        inst_handler(self);
    }
}
