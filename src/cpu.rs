use crate::mem::MappedMemory;
use crate::util;
use winit::event_loop::EventLoop;

// Our target cycle rate is ~4m Hz
// This is somewhere in the neighborhood of 700,000 instructions/sec,
// which is great because we're hitting about 35,000,000 instructions/sec.

const CYCLE_RATE: usize = 4_194_304;
const SPEED_MULTIPLIER: f32 = 1.0;

const REG_B: u8 = 0b000;
const REG_C: u8 = 0b001;
const REG_D: u8 = 0b010;
const REG_E: u8 = 0b011;
const REG_H: u8 = 0b100;
const REG_L: u8 = 0b101;
const REG_HLP: u8 = 0b110;
const REG_A: u8 = 0b111;

pub struct CPU {
    pub pc: u16,
    sp: u16,
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
}

impl CPU {
    pub fn new(event_loop: &EventLoop<()>) -> CPU {
        CPU {
            pc: 0,
            sp: 0,
            af: 0,
            bc: 0,
            de: 0,
            hl: 0,
        }
    }

    // Memory manipulation aliases

    pub fn mem_unmap_bootstrap(&self, mem: &mut MappedMemory) {
        mem.unmap_bootstrap();
    }

    // Instruction handlers

    fn inst_nop(&mut self, mem: &mut MappedMemory) {
        trace!("NOP");
        self.pc += 1;
    }

    fn inst_ld_sp_d16(&mut self, mem: &mut MappedMemory) {
        trace!("LD SP,d16");
        let val = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        self.sp = val;
        trace!("SP <- {:#06x}", self.sp);
        self.pc += 3;
    }

    fn inst_ld_hld_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD (HL-),A");
        // *HL = A
        mem.set(self.hl, self.af.to_be_bytes()[0]);
        // HL = HL - 1
        self.hl -= 1;
        trace!("HL <- {:#06x}", self.hl);
        self.pc += 1;
    }

    fn inst_ld_hli_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD (HL+),A");
        // *HL = A
        mem.set(self.hl, self.af.to_be_bytes()[0]);
        // HL = HL + 1
        self.hl += 1;
        trace!("HL <- {:#06x}", self.hl);
        self.pc += 1;
    }

    fn inst_ld_hl_d16(&mut self, mem: &mut MappedMemory) {
        trace!("LD HL,d16");
        let val = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        self.hl = val;
        trace!("HL <- {:#06x}", self.hl);
        self.pc += 3;
    }

    fn inst_xor_a(&mut self, mem: &mut MappedMemory) {
        trace!("XOR A");
        self.set_a(0);
        self.pc += 1;
    }

    fn inst_jr_nz_r8(&mut self, mem: &mut MappedMemory) {
        trace!("JR NZ,r8");

        // If the Z flag is not set, jump
        if (self.af.to_be_bytes()[1] >> 7) == 0 {
            let jmp_offset = mem.at(self.pc + 1) as i8;
            self.pc = ((self.pc as i16) + (jmp_offset as i16)) as u16;
        }

        self.pc += 2;
    }

    fn inst_jr_z_r8(&mut self, mem: &mut MappedMemory) {
        trace!("JR Z,r8");

        // If the Z flag is set, jump
        if (self.af.to_be_bytes()[1] >> 7) == 1 {
            let jmp_offset = mem.at(self.pc + 1) as i8;
            self.pc = ((self.pc as i16) + (jmp_offset as i16)) as u16;
        }

        self.pc += 2;
    }

    fn inst_jr_r8(&mut self, mem: &mut MappedMemory) {
        trace!("JR r8");
        let jmp_offset = mem.at(self.pc + 1) as i8;
        self.pc = ((self.pc as i16) + (jmp_offset as i16)) as u16;
        self.pc += 2;
    }

    fn inst_cb_bit_7_h(&mut self, mem: &mut MappedMemory) {
        trace!("BIT 7,H");

        let new_bit = (!((self.hl.to_be_bytes()[0] & 0b10000000) >> 7) & 0b1) as u16;
        self.af = (self.af & !(1u16 << 7)) | (new_bit << 7);

        self.pc += 1;
    }

    fn inst_cb_res_x_x(&mut self, mem: &mut MappedMemory) {
        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let bit = (inst >> 3) & 0b111;
        self.update_reg(mem, reg, |v: u8| util::set_bit(v, bit, false));
        self.pc += 1;
    }

    fn inst_ld_c_d8(&mut self, mem: &mut MappedMemory) {
        trace!("LD C,d8");
        let val = mem.at(self.pc + 1);
        self.set_c(val);
        self.pc += 2;
    }

    fn inst_ld_d_d8(&mut self, mem: &mut MappedMemory) {
        trace!("LD D,d8");
        let val = mem.at(self.pc + 1);
        self.set_d(val);
        self.pc += 2;
    }

    fn inst_ld_a_d8(&mut self, mem: &mut MappedMemory) {
        trace!("LD A,d8");
        let val = mem.at(self.pc + 1);
        self.set_a(val);
        self.pc += 2;
    }

    fn inst_ld_e_d8(&mut self, mem: &mut MappedMemory) {
        trace!("LD E,d8");
        let val = mem.at(self.pc + 1);
        self.set_e(val);
        self.pc += 2;
    }

    fn inst_ld_l_d8(&mut self, mem: &mut MappedMemory) {
        trace!("LD L,d8");
        let val = mem.at(self.pc + 1);
        self.set_l(val);
        self.pc += 2;
    }

    fn inst_ld_cp_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD (C),A");
        let c_val = self.get_c();
        let addr = 0xff00 | u16::from(c_val);
        mem.set(addr, self.get_a());
        self.pc += 1;
    }

    fn inst_add_a_hlp(&mut self, mem: &mut MappedMemory) {
        trace!("ADD A,(HL)");
        let a_val = self.get_a() + mem.at(self.hl);
        self.set_a(a_val);
        self.pc += 1;
    }

    fn inst_ld_hlp_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD (HL),A");
        mem.set(self.hl, self.get_a());
        self.pc += 1;
    }

    fn inst_ld_a16p_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD (a16),A");
        let val = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        mem.set(val, self.get_a());
        self.pc += 3;
    }

    fn inst_inc_b(&mut self, mem: &mut MappedMemory) {
        trace!("INC B");
        self.set_b(self.get_b() + 1);
        self.pc += 1;
    }

    fn inst_inc_c(&mut self, mem: &mut MappedMemory) {
        trace!("INC C");
        self.set_c(self.get_c() + 1);
        self.pc += 1;
    }

    fn inst_inc_d(&mut self, mem: &mut MappedMemory) {
        trace!("INC D");
        self.set_d(self.get_d() + 1);
        self.pc += 1;
    }

    fn inst_inc_h(&mut self, mem: &mut MappedMemory) {
        trace!("INC H");
        self.set_h(self.get_h() + 1);
        self.pc += 1;
    }

    fn inst_inc_de(&mut self, mem: &mut MappedMemory) {
        trace!("INC DE");
        self.de += 1;
        self.pc += 1;
    }

    fn inst_inc_hl(&mut self, mem: &mut MappedMemory) {
        trace!("INC HL");
        self.hl += 1;
        self.pc += 1;
    }

    fn inst_ld_a8p_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD (a8),A");
        let offset = mem.at(self.pc + 1);
        let a_val = self.af.to_be_bytes()[0];
        let addr = 0xff00 | u16::from(offset);
        mem.set(addr, a_val);
        self.pc += 2;
    }

    fn inst_ld_a_a8p(&mut self, mem: &mut MappedMemory) {
        trace!("LD A,(a8)");
        let offset = mem.at(self.pc + 1);
        let addr = 0xff00 | u16::from(offset);
        let val = mem.at(addr);
        self.set_a(val);
        trace!("A <- {:#04x}", val);
        self.pc += 2;
    }

    fn inst_ld_de_d16(&mut self, mem: &mut MappedMemory) {
        trace!("LD DE,d16");
        let val = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        self.de = val;
        trace!("DE <- {:#06x}", self.de);
        self.pc += 3;
    }

    fn inst_ld_a_dep(&mut self, mem: &mut MappedMemory) {
        trace!("LD A,(DE)");
        let val = mem.at(self.de);
        self.set_a(val);
        self.pc += 1;
    }

    fn inst_call_a16(&mut self, mem: &mut MappedMemory) {
        trace!("CALL a16");
        let addr = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        self.sp_push(mem, self.pc + 3);
        self.pc = addr;
    }

    fn inst_jp_a16(&mut self, mem: &mut MappedMemory) {
        trace!("JP a16");
        let addr = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        self.pc = addr;
    }

    fn inst_ld_c_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD C,A");
        self.set_c(self.get_a());
        self.pc += 1;
    }

    fn inst_ld_a_b(&mut self, mem: &mut MappedMemory) {
        trace!("LD A,B");
        self.set_a(self.get_b());
        self.pc += 1;
    }

    fn inst_ld_a_h(&mut self, mem: &mut MappedMemory) {
        trace!("LD A,H");
        self.set_a(self.get_h());
        self.pc += 1;
    }

    fn inst_ld_a_l(&mut self, mem: &mut MappedMemory) {
        trace!("LD A,L");
        self.set_a(self.get_l());
        self.pc += 1;
    }

    fn inst_ld_d_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD D,A");
        self.set_d(self.get_a());
        self.pc += 1;
    }

    fn inst_ld_h_a(&mut self, mem: &mut MappedMemory) {
        trace!("LD H,A");
        self.set_h(self.get_a());
        self.pc += 1;
    }

    fn inst_ld_a_e(&mut self, mem: &mut MappedMemory) {
        trace!("LD A,E");
        self.set_a(self.get_e());
        self.pc += 1;
    }

    fn inst_ld_b_d8(&mut self, mem: &mut MappedMemory) {
        trace!("LD B,d8");
        let val = mem.at(self.pc + 1);
        self.set_b(val);
        self.pc += 2;
    }

    fn inst_push_bc(&mut self, mem: &mut MappedMemory) {
        trace!("PUSH BC");
        self.sp_push(mem, self.bc);
        self.pc += 1;
    }

    fn inst_push_de(&mut self, mem: &mut MappedMemory) {
        trace!("PUSH DE");
        self.sp_push(mem, self.de);
        self.pc += 1;
    }

    fn inst_push_hl(&mut self, mem: &mut MappedMemory) {
        trace!("PUSH HL");
        self.sp_push(mem, self.hl);
        self.pc += 1;
    }

    fn inst_push_af(&mut self, mem: &mut MappedMemory) {
        trace!("PUSH AF");
        self.sp_push(mem, self.af);
        self.pc += 1;
    }

    fn inst_cb_rl_c(&mut self, mem: &mut MappedMemory) {
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

    fn inst_rla(&mut self, mem: &mut MappedMemory) {
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

    fn inst_pop_bc(&mut self, mem: &mut MappedMemory) {
        trace!("POP BC");
        self.bc = self.sp_pop(mem);
        self.pc += 1;
    }

    fn inst_dec_b(&mut self, mem: &mut MappedMemory) {
        trace!("DEC B");
        let b_val = self.get_b() - 1;
        self.set_b(b_val);
        self.set_flag(7, b_val == 0);
        self.pc += 1;
    }

    fn inst_dec_c(&mut self, mem: &mut MappedMemory) {
        trace!("DEC C");
        let c_val = self.get_c() - 1;
        self.set_c(c_val);
        self.set_flag(7, c_val == 0);
        self.pc += 1;
    }

    fn inst_dec_d(&mut self, mem: &mut MappedMemory) {
        trace!("DEC D");
        let d_val = self.get_d() - 1;
        self.set_d(d_val);
        self.set_flag(7, d_val == 0);
        self.pc += 1;
    }

    fn inst_dec_e(&mut self, mem: &mut MappedMemory) {
        trace!("DEC E");
        let e_val = self.get_e() - 1;
        self.set_e(e_val);
        self.set_flag(7, e_val == 0);
        self.pc += 1;
    }

    fn inst_dec_h(&mut self, mem: &mut MappedMemory) {
        trace!("DEC H");
        let h_val = self.get_h() - 1;
        self.set_h(h_val);
        self.set_flag(7, h_val == 0);
        self.pc += 1;
    }

    fn inst_dec_l(&mut self, mem: &mut MappedMemory) {
        trace!("DEC L");
        let l_val = self.get_l() - 1;
        self.set_l(l_val);
        self.set_flag(7, l_val == 0);
        self.pc += 1;
    }

    fn inst_dec_a(&mut self, mem: &mut MappedMemory) {
        trace!("DEC A");
        let a_val = self.get_a() - 1;
        self.set_a(a_val);
        self.set_flag(7, a_val == 0);
        self.pc += 1;
    }

    fn inst_ret(&mut self, mem: &mut MappedMemory) {
        trace!("RET");
        self.pc = self.sp_pop(mem);
    }

    fn inst_cp_d8(&mut self, mem: &mut MappedMemory) {
        trace!("CP d8");
        let val = mem.at(self.pc + 1);
        let result = self.get_a() - val;
        self.set_flag(7, result == 0);
        self.pc += 2;
    }

    fn inst_cp_hlp(&mut self, mem: &mut MappedMemory) {
        trace!("CP (HL)");
        let val = mem.at(self.hl);
        let result = self.get_a() - val;
        self.set_flag(7, result == 0);
        self.pc += 1;
    }

    fn inst_sub_b(&mut self, mem: &mut MappedMemory) {
        trace!("SUB B");
        let a_val = self.get_a() - self.get_b();
        self.set_a(a_val);
        self.set_flag(7, a_val == 0);
        self.pc += 1;
    }

    // Stack manipulation

    fn sp_push(&mut self, mem: &mut MappedMemory, val: u16) {
        let val_bytes = val.to_be_bytes();

        mem.set(self.sp, val_bytes[0]);
        self.sp -= 1;
        mem.set(self.sp, val_bytes[1]);
        self.sp -= 1;
    }

    fn sp_pop(&mut self, mem: &mut MappedMemory) -> u16 {
        self.sp += 1;
        let b0 = mem.at(self.sp);
        self.sp += 1;
        let b1 = mem.at(self.sp);
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

    pub fn cycle(&mut self, mem: &mut MappedMemory) {
        let inst = mem.at(self.pc);

        let inst_handler: fn(&mut CPU, &mut MappedMemory) = match inst {
            0x00 => CPU::inst_nop,
            0x04 => CPU::inst_inc_b,
            0x05 => CPU::inst_dec_b,
            0x06 => CPU::inst_ld_b_d8,
            0x0c => CPU::inst_inc_c,
            0x0d => CPU::inst_dec_c,
            0x0e => CPU::inst_ld_c_d8,
            0x11 => CPU::inst_ld_de_d16,
            0x13 => CPU::inst_inc_de,
            0x14 => CPU::inst_inc_d,
            0x15 => CPU::inst_dec_d,
            0x16 => CPU::inst_ld_d_d8,
            0x17 => CPU::inst_rla,
            0x18 => CPU::inst_jr_r8,
            0x1a => CPU::inst_ld_a_dep,
            0x1d => CPU::inst_dec_e,
            0x1e => CPU::inst_ld_e_d8,
            0x20 => CPU::inst_jr_nz_r8,
            0x21 => CPU::inst_ld_hl_d16,
            0x22 => CPU::inst_ld_hli_a,
            0x23 => CPU::inst_inc_hl,
            0x24 => CPU::inst_inc_h,
            0x25 => CPU::inst_dec_h,
            0x28 => CPU::inst_jr_z_r8,
            0x2d => CPU::inst_dec_l,
            0x2e => CPU::inst_ld_l_d8,
            0x31 => CPU::inst_ld_sp_d16,
            0x32 => CPU::inst_ld_hld_a,
            0x3d => CPU::inst_dec_a,
            0x3e => CPU::inst_ld_a_d8,
            0x4f => CPU::inst_ld_c_a,
            0x57 => CPU::inst_ld_d_a,
            0x67 => CPU::inst_ld_h_a,
            0x7b => CPU::inst_ld_a_e,
            0x77 => CPU::inst_ld_hlp_a,
            0x78 => CPU::inst_ld_a_b,
            0x7c => CPU::inst_ld_a_h,
            0x7d => CPU::inst_ld_a_l,
            0x86 => CPU::inst_add_a_hlp,
            0x90 => CPU::inst_sub_b,
            0xaf => CPU::inst_xor_a,
            0xbe => CPU::inst_cp_hlp,
            0xc1 => CPU::inst_pop_bc,
            0xc3 => CPU::inst_jp_a16,
            0xc5 => CPU::inst_push_bc,
            0xc9 => CPU::inst_ret,
            0xcd => CPU::inst_call_a16,
            0xd5 => CPU::inst_push_de,
            0xe0 => CPU::inst_ld_a8p_a,
            0xe2 => CPU::inst_ld_cp_a,
            0xe5 => CPU::inst_push_hl,
            0xea => CPU::inst_ld_a16p_a,
            0xf0 => CPU::inst_ld_a_a8p,
            0xf5 => CPU::inst_push_af,
            0xfe => CPU::inst_cp_d8,
            0xcb => {
                self.pc += 1;
                self.cycle_cb(mem);
                return;
            }
            _ => {
                error!("Unknown instruction {:#04x}", inst);
                return;
            }
        };

        inst_handler(self, mem);
    }

    fn update_reg<F: Fn(u8) -> u8>(&mut self, mem: &mut MappedMemory, reg: u8, handler: F) {
        match reg {
            REG_A => self.set_a(handler(self.get_a())),
            REG_B => self.set_b(handler(self.get_b())),
            REG_C => self.set_c(handler(self.get_c())),
            REG_D => self.set_d(handler(self.get_d())),
            REG_E => self.set_e(handler(self.get_e())),
            REG_H => self.set_h(handler(self.get_h())),
            REG_HLP => {
                let val = mem.at(self.hl);
                mem.set(self.hl, handler(val));
            }
            _ => {}
        }
    }

    fn cycle_cb(&mut self, mem: &mut MappedMemory) {
        let inst = mem.at(self.pc);

        let inst_handler: fn(&mut CPU, &mut MappedMemory) = match inst {
            0x11 => CPU::inst_cb_rl_c,
            0x7c => CPU::inst_cb_bit_7_h,
            0x87 => CPU::inst_cb_res_x_x,
            _ => {
                error!("Unknown instruction {:#04x}", 0xcb00 | u16::from(inst));
                return;
            }
        };

        inst_handler(self, mem);
    }
}
