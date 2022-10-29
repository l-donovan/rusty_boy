use crate::mem::MappedMemory;
use crate::util;
use bitmatch::bitmatch;
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

enum Flag {
    Carry = 4,
    HalfCarry = 5,
    Subtract = 6,
    Zero = 7,
}

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

    // Instruction handlers

    fn inst_nop(&mut self) -> u8 {
        trace!("NOP");
        self.pc += 1;
        return 4;
    }

    // 0xcb Instructions

    fn inst_cb_rlc_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("RLC X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let v = self.get_reg(mem, reg);

        let new_val = v.rotate_left(1);
        let new_carry = util::bit_is_set(v, 7);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, new_carry);

        self.set_reg(mem, reg, new_val);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_rrc_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("RRC X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let v = self.get_reg(mem, reg);

        let new_val = v.rotate_right(1);
        let new_carry = util::bit_is_set(v, 0);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, new_carry);

        self.set_reg(mem, reg, new_val);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_sla_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("SLA X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let v = self.get_reg(mem, reg);

        let new_val = v << 1;
        let new_carry = util::bit_is_set(v, 7);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, new_carry);

        self.set_reg(mem, reg, new_val);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_sra_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("SRA X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let v = self.get_reg(mem, reg);

        let new_val = v >> 1;
        let new_carry = util::bit_is_set(v, 0);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, new_carry);

        // This is an arithmetic shift so the MSB is sticky
        self.set_reg(mem, reg, util::set_bit(new_val, 7, util::bit_is_set(v, 7)));

        self.pc += 1;

        return 8;
    }

    fn inst_cb_srl_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("SRL X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let v = self.get_reg(mem, reg);

        let new_val = v >> 1;
        let new_carry = util::bit_is_set(v, 0);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, new_carry);

        self.set_reg(mem, reg, new_val);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_rl_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("RL X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let v = self.get_reg(mem, reg);

        let carry_bit = self.get_flag(4);
        let shifted_reg = ((u16::from(v) << 1) & 0xff) as u8;
        let new_carry = util::bit_is_set(v, 7);
        let new_val = util::set_bit(shifted_reg, 0, carry_bit);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, new_carry);

        self.set_reg(mem, reg, new_val);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_rr_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("RR X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let v = self.get_reg(mem, reg);

        let carry_bit = self.get_flag(4);
        let shifted_reg = ((u16::from(v) >> 1) & 0xff) as u8;
        let new_carry = util::bit_is_set(v, 0);
        let new_val = util::set_bit(shifted_reg, 7, carry_bit);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, new_carry);

        self.set_reg(mem, reg, new_val);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_swap_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("SWAP X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let reg_val = self.get_reg(mem, reg);
        let lower_bits = reg_val & 0x0f;
        let upper_bits = reg_val & 0xf0;
        let new_val = (lower_bits << 4) | (upper_bits >> 4);

        self.set_flag(7, new_val == 0);
        self.set_flag(6, false);
        self.set_flag(5, false);
        self.set_flag(4, false);

        self.set_reg(mem, reg, new_val);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_bit_x_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("BIT X,X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let bit = (inst >> 3) & 0b111;
        let reg_val = self.get_reg(mem, reg);
        let is_zero = (reg_val >> bit) & 0b1 == 0;

        self.set_flag(7, is_zero);
        self.set_flag(6, false);
        self.set_flag(5, true);

        self.pc += 1;

        return 8;
    }

    fn inst_cb_res_x_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("RES X,X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let bit = (inst >> 3) & 0b111;

        self.update_reg(mem, reg, |v: u8| util::set_bit(v, bit, false));

        self.pc += 1;

        return 8;
    }

    fn inst_cb_set_x_x(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("SET X,X");

        let inst = mem.at(self.pc);
        let reg = inst & 0b111;
        let bit = (inst >> 3) & 0b111;

        self.update_reg(mem, reg, |v: u8| util::set_bit(v, bit, true));

        self.pc += 1;

        return 8;
    }

    fn inst_ld_x_d8(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("LD X,d8");
        let val = mem.at(self.pc + 1);
        self.set_reg(mem, reg, val);
        self.pc += 2;

        return 8;
    }

    fn inst_rst_xxh(&mut self, mem: &mut MappedMemory, mode: u8) -> u8 {
        let addr = match mode {
            0 => 0x00,
            1 => 0x08,
            2 => 0x10,
            3 => 0x18,
            4 => 0x20,
            5 => 0x28,
            6 => 0x30,
            7 => 0x38,
            _ => {
                self.pc += 1;
                return 16;
            }
        };

        self.sp_push(mem, self.pc + 3);
        self.pc = addr;

        return 16;
    }

    // TODO
    fn inst_rxa(&mut self, mem: &mut MappedMemory, mode: u8) -> u8 {
        trace!("RXA");

        match mode {
            0 => {
                // RLCA
                error!("TODO");
            }
            1 => {
                // RRCA
                error!("TODO");
            }
            2 => {
                // RLA
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
            }
            3 => {
                // RRA
                error!("TODO");
            }
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 4;
            }
        }

        self.pc += 1;

        return 4;
    }

    fn inst_jr_x_r8(&mut self, mem: &mut MappedMemory, condition: u8) -> u8 {
        trace!("JR X,r8");

        let should_jump = match condition {
            0 => !self.get_flag(Flag::Zero as u8),
            1 => self.get_flag(Flag::Zero as u8),
            2 => !self.get_flag(Flag::Carry as u8),
            3 => self.get_flag(Flag::Carry as u8),
            4 => true, // This just gives us a shortcut to a JR r8 instruction
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 2;
                return 8;
            }
        };

        if !should_jump {
            self.pc += 2;
            return 8;
        }

        let jump_offset = mem.at(self.pc + 1) as i8;
        self.pc = ((self.pc as i16) + (jump_offset as i16)) as u16;
        self.pc += 2;

        return 12;
    }

    fn inst_jp_x_a16(&mut self, mem: &mut MappedMemory, condition: u8) -> u8 {
        trace!("JP X,a16");

        let addr = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);

        let should_jump = match condition {
            0 => !self.get_flag(Flag::Zero as u8),
            1 => self.get_flag(Flag::Zero as u8),
            2 => !self.get_flag(Flag::Carry as u8),
            3 => self.get_flag(Flag::Carry as u8),
            4 => true, // This just gives us a shortcut to a JP a16 instruction
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 3;
                return 12;
            }
        };

        if !should_jump {
            self.pc += 3;
            return 12;
        }

        self.pc = addr;

        return 16;
    }

    fn inst_add_a_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("ADD A,X");

        let old_val = self.get_a();
        let rhs = self.get_reg(mem, reg);
        let half_carry = (old_val & 0xf).wrapping_add(rhs) > 0x0f;
        let (new_val, did_overflow) = old_val.overflowing_add(rhs);

        self.set_a(new_val);

        // Z0HC
        self.set_flag(Flag::Zero as u8, new_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 1;

        return 4;
    }

    fn inst_add_sp_r8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("ADD SP,r8");

        // TODO

        self.pc += 2;

        return 16;
    }

    fn inst_ld_hl_spr8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("LD HL,SP+r8");

        // TODO

        self.pc += 2;

        return 12;
    }

    fn inst_add_a_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("ADD A,d8");

        let old_val = self.get_a();
        let rhs = mem.at(self.pc + 1);
        let half_carry = (old_val & 0xf).wrapping_add(rhs) > 0x0f;
        let (new_val, did_overflow) = old_val.overflowing_add(rhs);

        self.set_a(new_val);

        // Z0HC
        self.set_flag(Flag::Zero as u8, new_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 2;

        return 8;
    }

    fn inst_add_hl_xx(&mut self, reg: u8) -> u8 {
        trace!("ADD HL,XX");

        let old_val = self.hl;
        let rhs = match reg {
            0 => self.bc,
            1 => self.de,
            2 => self.hl,
            3 => self.sp,
            _ => {
                self.pc += 1;
                return 8;
            }
        };

        let half_carry = (old_val & 0xf).wrapping_add(rhs) > 0x0f;
        let (new_val, did_overflow) = old_val.overflowing_add(rhs);

        self.hl = new_val;

        // -0HC
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 1;

        return 8;
    }

    fn inst_adc_a_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("ADC A,X");

        let old_val = self.get_a();
        let carry = self.get_flag(Flag::Carry as u8) as u8;
        let rhs = self.get_reg(mem, reg) + carry;
        let half_carry = (old_val & 0xf).wrapping_add(rhs & 0xf) > 0x0f;
        let (new_val, did_overflow) = old_val.overflowing_add(rhs);

        self.set_a(new_val);

        // Z0HC
        self.set_flag(Flag::Zero as u8, new_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 1;

        return 4;
    }

    fn inst_adc_a_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("ADC A,d8");

        let old_val = self.get_a();
        let carry = self.get_flag(Flag::Carry as u8) as u8;
        let rhs = mem.at(self.pc + 1) + carry;
        let half_carry = (old_val & 0xf).wrapping_add(rhs & 0xf) > 0x0f;
        let (new_val, did_overflow) = old_val.overflowing_add(rhs);

        self.set_a(new_val);

        // Z0HC
        self.set_flag(Flag::Zero as u8, new_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 2;

        return 8;
    }

    fn inst_ldh_a8pa(&mut self, mem: &mut MappedMemory, mode: u8) -> u8 {
        trace!("LDH (a8)-A");

        let val = mem.at(self.pc + 1);
        let addr = 0xff00 | u16::from(val);

        match mode {
            0 => {
                let a_val = self.get_a();
                mem.set(addr, a_val);
            }
            1 => {
                let mem_val = mem.at(addr);
                self.set_a(mem_val);
            }
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 2;
                return 12;
            }
        };

        self.pc += 2;

        return 12;
    }

    fn inst_ld_cpa(&mut self, mem: &mut MappedMemory, mode: u8) -> u8 {
        trace!("LD (C)-A");

        let val = self.get_c();
        let addr = 0xff00 | u16::from(val);

        match mode {
            0 => {
                let a_val = self.get_a();
                mem.set(addr, a_val);
            }
            1 => {
                let mem_val = mem.at(addr);
                self.set_a(mem_val);
            }
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 2;
                return 8;
            }
        };

        self.pc += 2;

        return 8;
    }

    fn inst_ld_a16pa(&mut self, mem: &mut MappedMemory, mode: u8) -> u8 {
        trace!("LD (a16)-A");

        let addr = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);

        match mode {
            0 => {
                let a_val = self.get_a();
                mem.set(addr, a_val);
            }
            1 => {
                let mem_val = mem.at(addr);
                self.set_a(mem_val);
            }
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 3;
                return 16;
            }
        };

        self.pc += 3;

        return 16;
    }

    fn inst_ld_a16p_sp(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("LD (a16),SP");

        let addr = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        let bytes = self.sp.to_le_bytes();
        mem.set(addr, bytes[0]);
        mem.set(addr + 1, bytes[1]);

        self.pc += 3;

        return 20;
    }

    fn inst_ld_xx_d16(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("LD XX,d16");

        let val = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);

        let target: &mut u16 = match reg {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => &mut self.hl,
            3 => &mut self.sp,
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 3;
                return 12;
            }
        };

        *target = val;

        self.pc += 3;

        return 12;
    }

    fn inst_ld_sp_hl(&mut self) -> u8 {
        trace!("LD SP,HL");

        self.sp = self.hl;
        self.pc += 1;

        return 8;
    }

    fn inst_ld_xxp_a(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("LD (XX),A");

        let mut inc = false;
        let mut dec = false;

        let a_val = self.get_a();

        let target: &mut u16 = match reg {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => {
                inc = true;
                &mut self.hl
            }
            3 => {
                dec = true;
                &mut self.hl
            }
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 8;
            }
        };

        mem.set(*target, a_val);

        if inc {
            *target = target.wrapping_add(1);
        } else if dec {
            *target = target.wrapping_sub(1);
        }

        self.pc += 1;

        return 8;
    }

    fn inst_inc_xx(&mut self, reg: u8) -> u8 {
        trace!("INC XX");

        let target: &mut u16 = match reg {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => &mut self.hl,
            3 => &mut self.sp,
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 8;
            }
        };

        *target = target.wrapping_add(1);

        self.pc += 1;

        return 8;
    }

    fn inst_ld_a_xxp(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("LD A,(XX)");

        let mut inc = false;
        let mut dec = false;

        let target: &mut u16 = match reg {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => {
                inc = true;
                &mut self.hl
            }
            3 => {
                dec = true;
                &mut self.hl
            }
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 8;
            }
        };

        let val = mem.at(*target);

        if inc {
            *target = target.wrapping_add(1);
        } else if dec {
            *target = target.wrapping_sub(1);
        }

        drop(target);

        self.set_a(val);

        self.pc += 1;

        return 8;
    }

    fn inst_dec_xx(&mut self, reg: u8) -> u8 {
        trace!("DEC XX");

        let target: &mut u16 = match reg {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => &mut self.hl,
            3 => &mut self.sp,
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 8;
            }
        };

        *target = target.wrapping_sub(1);

        self.pc += 1;

        return 8;
    }

    fn inst_call_x_a16(&mut self, mem: &mut MappedMemory, mode: u8) -> u8 {
        trace!("CALL X,a16");

        let should_call = match mode {
            0 => !self.get_flag(Flag::Zero as u8),
            1 => self.get_flag(Flag::Zero as u8),
            2 => !self.get_flag(Flag::Carry as u8),
            3 => self.get_flag(Flag::Carry as u8),
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 3;
                return 12;
            }
        };

        if !should_call {
            self.pc += 3;
            return 12;
        }

        let addr = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        self.sp_push(mem, self.pc + 3);
        self.pc = addr;

        return 24;
    }

    fn inst_call_a16(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("CALL a16");

        let addr = u16::from_le_bytes([mem.at(self.pc + 1), mem.at(self.pc + 2)]);
        self.sp_push(mem, self.pc + 3);

        self.pc = addr;

        return 24;
    }

    fn inst_ld_x_x(&mut self, mem: &mut MappedMemory, target: u8, source: u8) -> u8 {
        if target == REG_HLP && source == REG_HLP {
            // This is a special case where the instruction is actually HALT.
            // This purposefully does not increment the PC as it waits for the next INT.
            trace!("HALT");

            return 4;
        }

        trace!("LD X,X");

        let source_val = self.get_reg(mem, source);
        self.set_reg(mem, target, source_val);

        self.pc += 1;

        return 4;
    }

    fn inst_push_xx(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("PUSH XX");

        let target: u16 = match reg {
            0 => self.bc,
            1 => self.de,
            2 => self.hl,
            3 => self.af,
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 16;
            }
        };

        self.sp_push(mem, target);

        self.pc += 1;

        return 16;
    }

    fn inst_pop_xx(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("POP XX");

        let val = self.sp_pop(mem);

        let target: &mut u16 = match reg {
            0 => &mut self.bc,
            1 => &mut self.de,
            2 => &mut self.hl,
            3 => &mut self.af,
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 12;
            }
        };

        *target = val;

        self.pc += 1;

        return 12;
    }

    fn inst_inc_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("INC X");

        let old_val = self.get_reg(mem, reg);
        let half_carry = (old_val & 0xf).wrapping_add(1) > 0x0f;
        let (new_val, _did_overflow) = old_val.overflowing_add(1);

        self.set_reg(mem, reg, new_val);

        // Z0H-
        self.set_flag(Flag::Zero as u8, new_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, half_carry);

        self.pc += 1;

        return 4;
    }

    fn inst_dec_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("DEC X");

        let old_val = self.get_reg(mem, reg);
        let half_carry = (old_val & 0xf).wrapping_sub(1) > 0x10;
        let (new_val, _did_overflow) = old_val.overflowing_sub(1);

        self.set_reg(mem, reg, new_val);

        // Z1H-
        self.set_flag(Flag::Zero as u8, new_val == 0);
        self.set_flag(Flag::Subtract as u8, true);
        self.set_flag(Flag::HalfCarry as u8, half_carry);

        self.pc += 1;

        return 4;
    }

    fn inst_ret(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("RET");

        self.pc = self.sp_pop(mem);

        return 16;
    }

    fn inst_ret_x(&mut self, mem: &mut MappedMemory, condition: u8) -> u8 {
        trace!("RET X");

        let should_return = match condition {
            0 => !self.get_flag(Flag::Zero as u8),
            1 => self.get_flag(Flag::Zero as u8),
            2 => !self.get_flag(Flag::Carry as u8),
            3 => self.get_flag(Flag::Carry as u8),
            _ => {
                error!("UNKNOWN MODE");
                self.pc += 1;
                return 8;
            }
        };

        if !should_return {
            self.pc += 1;
            return 8;
        }

        self.pc = self.sp_pop(mem);

        return 20;
    }

    fn inst_cp_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("CP X");

        let a_val = self.get_a();
        let op_val = self.get_reg(mem, reg);

        if a_val == op_val {
            self.set_flag(Flag::Zero as u8, true);
        } else {
            self.set_flag(Flag::Zero as u8, false);
        }

        if a_val < op_val {
            self.set_flag(Flag::Carry as u8, true);
        } else {
            self.set_flag(Flag::Carry as u8, false);
        }

        self.pc += 1;

        return 4;
    }

    fn inst_cp_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("CP d8");

        let a_val = self.get_a();
        let op_val = mem.at(self.pc + 1);

        if a_val == op_val {
            self.set_flag(Flag::Zero as u8, true);
        } else {
            self.set_flag(Flag::Zero as u8, false);
        }

        if a_val < op_val {
            self.set_flag(Flag::Carry as u8, true);
        } else {
            self.set_flag(Flag::Carry as u8, false);
        }

        self.pc += 2;

        return 8;
    }

    fn inst_sub_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("SUB X");

        let reg_val = self.get_reg(mem, reg);
        let old_val = self.get_a();
        let half_carry = (old_val & 0xf).wrapping_sub(reg_val & 0xf) > 0x10;
        let (a_val, did_overflow) = old_val.overflowing_sub(reg_val);

        self.set_a(a_val);

        // Z1HC
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, true);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 1;

        return 4;
    }

    fn inst_sub_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("SUB d8");

        let rhs = mem.at(self.pc + 1);
        let old_val = self.get_a();
        let half_carry = (old_val & 0xf).wrapping_sub(rhs & 0xf) > 0x10;
        let (a_val, did_overflow) = old_val.overflowing_sub(rhs);

        self.set_a(a_val);

        // Z1HC
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, true);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 2;

        return 8;
    }

    fn inst_sbc_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("SBC A,X");

        let carry = self.get_flag(Flag::Carry as u8) as u8;
        let rhs = self.get_reg(mem, reg) + carry;
        let old_val = self.get_a();
        let half_carry = (old_val & 0xf).wrapping_sub(rhs & 0xf) > 0x10;
        let (a_val, did_overflow) = old_val.overflowing_sub(rhs);

        self.set_a(a_val);

        // Z1HC
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, true);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 1;

        return 4;
    }

    fn inst_sbc_a_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("SBC A,d8");

        let carry = self.get_flag(Flag::Carry as u8) as u8;
        let rhs = mem.at(self.pc + 1) + carry;
        let old_val = self.get_a();
        let half_carry = (old_val & 0xf).wrapping_sub(rhs & 0xf) > 0x10;
        let (a_val, did_overflow) = old_val.overflowing_sub(rhs);

        self.set_a(a_val);

        // Z1HC
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, true);
        self.set_flag(Flag::HalfCarry as u8, half_carry);
        self.set_flag(Flag::Carry as u8, did_overflow);

        self.pc += 2;

        return 8;
    }

    fn inst_and_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("AND X");

        let rhs = self.get_reg(mem, reg);
        let old_val = self.get_a();
        let a_val = old_val & rhs;

        self.set_a(a_val);

        // Z010
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, true);
        self.set_flag(Flag::Carry as u8, false);

        self.pc += 1;

        return 4;
    }

    fn inst_and_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("AND d8");

        let rhs = mem.at(self.pc + 1);
        let old_val = self.get_a();
        let a_val = old_val & rhs;

        self.set_a(a_val);

        // Z010
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, true);
        self.set_flag(Flag::Carry as u8, false);

        self.pc += 2;

        return 8;
    }

    fn inst_xor_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("XOR X");

        let rhs = self.get_reg(mem, reg);
        let old_val = self.get_a();
        let a_val = old_val ^ rhs;

        self.set_a(a_val);

        // Z000
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, false);
        self.set_flag(Flag::Carry as u8, false);

        self.pc += 1;

        return 4;
    }

    fn inst_xor_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("XOR d8");

        let rhs = mem.at(self.pc + 1);
        let old_val = self.get_a();
        let a_val = old_val ^ rhs;

        self.set_a(a_val);

        // Z000
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, false);
        self.set_flag(Flag::Carry as u8, false);

        self.pc += 2;

        return 8;
    }

    fn inst_or_x(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        trace!("OR X");

        let rhs = self.get_reg(mem, reg);
        let old_val = self.get_a();
        let a_val = old_val | rhs;

        self.set_a(a_val);

        // Z000
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, false);
        self.set_flag(Flag::Carry as u8, false);

        self.pc += 1;

        return 4;
    }

    fn inst_or_d8(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("OR d8");

        let rhs = mem.at(self.pc + 1);
        let old_val = self.get_a();
        let a_val = old_val | rhs;

        self.set_a(a_val);

        // Z000
        self.set_flag(Flag::Zero as u8, a_val == 0);
        self.set_flag(Flag::Subtract as u8, false);
        self.set_flag(Flag::HalfCarry as u8, false);
        self.set_flag(Flag::Carry as u8, false);

        self.pc += 2;

        return 8;
    }

    fn inst_reti(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("RETI");

        self.pc += 1;

        return 16;
    }

    fn inst_jp_hlp(&mut self, mem: &mut MappedMemory) -> u8 {
        trace!("JP (HL)");
        self.pc = self.hl;
        return 4;
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

    fn get_flag(&mut self, idx: u8) -> bool {
        util::bit_is_set(self.get_f(), idx)
    }

    fn get_reg(&mut self, mem: &mut MappedMemory, reg: u8) -> u8 {
        match reg {
            REG_A => self.get_a(),
            REG_B => self.get_b(),
            REG_C => self.get_c(),
            REG_D => self.get_d(),
            REG_E => self.get_e(),
            REG_H => self.get_h(),
            REG_L => self.get_l(),
            REG_HLP => mem.at(self.hl),
            _ => 0,
        }
    }

    fn set_reg(&mut self, mem: &mut MappedMemory, reg: u8, val: u8) {
        match reg {
            REG_A => self.set_a(val),
            REG_B => self.set_b(val),
            REG_C => self.set_c(val),
            REG_D => self.set_d(val),
            REG_E => self.set_e(val),
            REG_H => self.set_h(val),
            REG_L => self.set_l(val),
            REG_HLP => mem.set(self.hl, val),
            _ => {}
        };
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

    #[bitmatch]
    pub fn cycle(&mut self, mem: &mut MappedMemory) -> u8 {
        let inst = mem.at(self.pc);

        #[bitmatch]
        match inst {
            "00000000" => self.inst_nop(),
            "00ii0001" => self.inst_ld_xx_d16(mem, i),
            "00ii0010" => self.inst_ld_xxp_a(mem, i),
            "00ii0011" => self.inst_inc_xx(i),
            "00rrr100" => self.inst_inc_x(mem, r),
            "00rrr101" => self.inst_dec_x(mem, r),
            "00rrr110" => self.inst_ld_x_d8(mem, r),
            "000ii111" => self.inst_rxa(mem, i),
            "00001000" => self.inst_ld_a16p_sp(mem),
            "00ii1001" => self.inst_add_hl_xx(i),
            "00ii1010" => self.inst_ld_a_xxp(mem, i),
            "00ii1011" => self.inst_dec_xx(i),
            "00011000" => self.inst_jr_x_r8(mem, 4),
            "001ii000" => self.inst_jr_x_r8(mem, i),
            "01iiijjj" => self.inst_ld_x_x(mem, i, j),
            "10000rrr" => self.inst_add_a_x(mem, r),
            "10001rrr" => self.inst_adc_a_x(mem, r),
            "10010rrr" => self.inst_sub_x(mem, r),
            "10011rrr" => self.inst_sbc_x(mem, r),
            "10100rrr" => self.inst_and_x(mem, r),
            "10101rrr" => self.inst_xor_x(mem, r),
            "10110rrr" => self.inst_or_x(mem, r),
            "10111rrr" => self.inst_cp_x(mem, r),
            "110ii000" => self.inst_ret_x(mem, i),
            "11ii0001" => self.inst_pop_xx(mem, i),
            "110ii010" => self.inst_jp_x_a16(mem, i),
            "11000011" => self.inst_jp_x_a16(mem, 4),
            "110ii100" => self.inst_call_x_a16(mem, i),
            "11ii0101" => self.inst_push_xx(mem, i),
            "11000110" => self.inst_add_a_d8(mem),
            "11iii111" => self.inst_rst_xxh(mem, i),
            "11001001" => self.inst_ret(mem),
            "11001101" => self.inst_call_a16(mem),
            "11001110" => self.inst_adc_a_d8(mem),
            "11010110" => self.inst_sub_d8(mem),
            "11011001" => self.inst_reti(mem),
            "11011110" => self.inst_sbc_a_d8(mem),
            "111i0000" => self.inst_ldh_a8pa(mem, i),
            "111i0010" => self.inst_ld_cpa(mem, i),
            "11100110" => self.inst_and_d8(mem),
            "11101000" => self.inst_add_sp_r8(mem),
            "11101001" => self.inst_jp_hlp(mem),
            "111i1010" => self.inst_ld_a16pa(mem, i),
            "11101110" => self.inst_xor_d8(mem),
            "11110110" => self.inst_or_d8(mem),
            "11111000" => self.inst_ld_hl_spr8(mem),
            "11111001" => self.inst_ld_sp_hl(),
            "11111110" => self.inst_cp_d8(mem),
            "11001011" => {
                self.pc += 1;
                self.cycle_cb(mem)
            }
            _ => {
                error!("Unknown instruction 0x{:02x}", inst);
                self.inst_nop()
            }
        }
    }

    fn cycle_cb(&mut self, mem: &mut MappedMemory) -> u8 {
        let inst = mem.at(self.pc);

        match inst {
            0x00..=0x07 => self.inst_cb_rlc_x(mem),
            0x08..=0x0f => self.inst_cb_rrc_x(mem),
            0x10..=0x17 => self.inst_cb_rl_x(mem),
            0x18..=0x1f => self.inst_cb_rr_x(mem),
            0x20..=0x27 => self.inst_cb_sla_x(mem),
            0x28..=0x2f => self.inst_cb_sra_x(mem),
            0x30..=0x37 => self.inst_cb_swap_x(mem),
            0x38..=0x3f => self.inst_cb_srl_x(mem),
            0x40..=0x7f => self.inst_cb_bit_x_x(mem),
            0x80..=0xbf => self.inst_cb_res_x_x(mem),
            0xc0..=0xff => self.inst_cb_set_x_x(mem),
        }
    }
}
