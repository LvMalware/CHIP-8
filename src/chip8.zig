const std = @import("std");
const c = @cImport({
    @cInclude("SDL2/SDL.h");
});

const Self = @This();

pub const frequency = std.time.ns_per_s / 60; // 60Hz

// Opcode reference: https://en.wikipedia.org/wiki/CHIP-8
//
// Memory map:
//
// 0x000-0x1FF - Chip 8 interpreter (contains font set in emu)
// 0x050-0x0A0 - Used for the built in 4x5 pixel font set (0-F)
// 0x200-0xFFF - Program ROM and work RAM
//

pub const width = 64;
pub const height = 32;

pub const fontset = [_]u8{
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
};

const Window = struct {
    window: *c.SDL_Window,
    renderer: *c.SDL_Renderer,
};

V: [16]u8, // registers V0-VF
I: u16, // Index register
pc: u16, // program counter
sp: u16, // stack pointer
key: [16]u8, // HEX-based keypad
gfx: [height * width]u8, // pixel matrix for graphics
draw: bool, // a flag to know when we need to update the screen
quit: bool,
scale: c_int = 15, // scale factor for pixels: 1 chip8 pixels = scale pixels on your screen
stack: [16]u16, // stack
memory: [4096]u8, // RAM memory (4K total)
screen: ?Window,
delay_timer: u8, // timer at 60Hz, counts down to 0
sound_timer: u8, // timer at 60Hz, counts down to 0, plays sound until reaching 0

pub fn init() Self {
    var chip8 = std.mem.zeroInit(Self, .{
        .pc = 0x200,
    });

    std.mem.copyForwards(u8, chip8.memory[0..], &fontset);

    return chip8;
}

pub fn deinit(self: *Self) void {
    if (self.screen) |screen| {
        defer self.screen = null;
        c.SDL_DestroyWindow(screen.window);
        c.SDL_DestroyRenderer(screen.renderer);
        c.SDL_Quit();
    }
}

pub fn loadGame(self: *Self, path: []const u8) !void {
    // std.debug.print("Loading game from {s} ...\n", .{path});
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    _ = try file.readAll(self.memory[0x200..]);
}

fn fetch(self: *Self) u16 {
    // std.debug.print("Fetching instruction at position 0x{x:0>4}\n", .{self.pc});
    return std.mem.nativeToBig(u16, std.mem.bytesToValue(u16, self.memory[self.pc..][0..2]));
}

pub fn nextCycle(self: *Self) !void {
    // std.debug.print("Next cycle...\n", .{});
    const opcode = self.fetch();
    // std.debug.print("Opcode: 0x{x:0>4}\n", .{opcode});
    const op: u4 = @truncate(opcode >> 12);
    const val = opcode & 0x0fff;
    // std.debug.print("\x1b[2J", .{});
    // std.debug.print("I = {d}\n", .{self.I});
    // std.debug.print("PC = {d}\n", .{self.pc});
    // for (self.V, 0..) |v, i| {
    //    std.debug.print("V{x} = {d}\n", .{ i, v });
    // }

    switch (op) {
        0x0 => {
            if (opcode == 0x00E0) {
                // 0x00E0: clear screen
                // std.debug.print("CLS\n", .{});
                @memset(self.gfx[0..], 0);
                self.draw = true;
            } else if (opcode == 0x00EE) {
                // 0x00EE: "ret" instruction, returns to last address on the stack
                // std.debug.print("RET\n", .{});
                self.pc = self.stack[self.sp - 1];
                self.sp -= 1;
            } else {
                //0x0NNN: SYS addr, ignored on modern computers
            }
            self.pc += 2;
        },
        0x1 => {
            // 0x1NNN: jump to instruction at address 0xNNN
            self.pc = val;
            // std.debug.print("JMP 0x{x}\n", .{val});
        },
        0x2 => {
            // 0x2NNN: "call" the subroutine at address 0xNNN
            self.stack[self.sp] = self.pc;
            self.sp += 1;
            self.pc = val;
            // std.debug.print("CALL 0x{x}\n", .{val});
        },
        0x3 => {
            // 0x3XNN: skip the next instruction if register Vx == NN
            // std.debug.print("JRE V{d}, 0x{x}\n", .{ val >> 8, val & 0xff });
            if (self.V[val >> 8] == val & 0x00ff) {
                self.pc += 4;
            } else {
                self.pc += 2;
            }
        },
        0x4 => {
            // 0x4XNN: skip the next instruction if register Vx != NN
            // std.debug.print("JRNE V{d}, 0x{x}\n", .{ val >> 8, val & 0xff });
            if (self.V[val >> 8] != val & 0x00ff) {
                self.pc += 4;
            } else {
                self.pc += 2;
            }
        },
        0x5 => {
            // 0x5XY0: skip the next instruction if register Vx == Vy
            // std.debug.print("JRRE V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0xf });
            if (self.V[val >> 8] == self.V[(val >> 4) & 0x000f]) {
                self.pc += 4;
            } else {
                self.pc += 2;
            }
        },
        0x6 => {
            // 0x6XNN: set register Vx to NN
            // std.debug.print("MOV V{d}, 0x{x}\n", .{ val >> 8, val & 0xff });
            self.V[val >> 8] = @truncate(val & 0x00ff);
            self.pc += 2;
        },
        0x7 => {
            // 0x7XNN: add NN to register Vx without carry flag
            self.V[val >> 8] +%= @truncate(val & 0x00ff);
            self.pc += 2;
            // std.debug.print("ADDI V{d}, 0x{x}\n", .{ val >> 8, val & 0xff });
        },
        0x8 => {
            // operations between registers
            switch (val & 0x000f) {
                0x0 => {
                    // 0x8XY0: assign register Vx = Vy
                    self.V[val >> 8] = self.V[(val >> 4) & 0x000f];
                    // std.debug.print("MOV V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0x0f });
                },
                0x1 => {
                    // 0x8XY1: bit-wise OR register Vx |= Vy
                    self.V[val >> 8] |= self.V[(val >> 4) & 0x000f];
                    // std.debug.print("OR V{d}, V{d}", .{ val >> 8, (val >> 4) & 0x0f });
                },
                0x2 => {
                    // 0x8XY2: bit-wise AND register Vx |= Vy
                    self.V[val >> 8] &= self.V[(val >> 4) & 0x000f];
                    // std.debug.print("AND V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0x0f });
                },
                0x3 => {
                    // 0x8XY3: bit-wise XOR register Vx |= Vy
                    self.V[val >> 8] ^= self.V[(val >> 4) & 0x000f];
                    // std.debug.print("XOR V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0x0f });
                },
                0x4 => {
                    // 0x8XY4: add register Vx += Vy, setting the carry flag (VF)
                    const add = @addWithOverflow(self.V[val >> 8], self.V[(val >> 4) & 0x000f]);
                    self.V[val >> 8] = add[0];
                    self.V[0xf] = add[1];
                    // std.debug.print("ADDC V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0x0f });
                },
                0x5 => {
                    // 0x8XY5: subtract register Vx -= Vy, setting the carry flag (VF)
                    const sub = @subWithOverflow(self.V[val >> 8], self.V[(val >> 4) & 0x000f]);
                    self.V[val >> 8] = sub[0];
                    self.V[0xf] = 1 - sub[1];
                    // std.debug.print("SUBC V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0x0f });
                },
                0x6 => {
                    // 0x8XY6: Shifts register Vx to the right by 1, store the least significant bit into VF
                    self.V[0xf] = self.V[val >> 8] & 0x1;
                    self.V[val >> 8] >>= 1;
                    // std.debug.print("SHR V{d}\n", .{val >> 8});
                },
                0x7 => {
                    // 0x8XY7: subtract register Vx = Vy - Vx, setting the underflow flag (VF)
                    const sub = @subWithOverflow(self.V[(val >> 4) & 0x000f], self.V[val >> 8]);
                    self.V[val >> 8] = sub[0];
                    self.V[0xf] = 1 - sub[1];
                    // std.debug.print("SUBR V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0x0f });
                },
                0xE => {
                    // 0x8XYE: Left shift register Vx by 1, store the most significant bit into VF
                    self.V[0xf] = self.V[val >> 8] >> 7;
                    self.V[val >> 8] <<= 1;
                    // std.debug.print("SHL V{d}\n", .{val >> 8});
                },
                else => return error.InvalidInstruction,
            }
            self.pc += 2;
        },
        0x9 => {
            // 0x9XY0: skip the next instruction if Vx != Vy
            if (self.V[val >> 8] != self.V[(val >> 4) & 0x000f]) {
                self.pc += 4;
            } else {
                self.pc += 2;
            }
            // std.debug.print("JRNE V{d}, V{d}\n", .{ val >> 8, (val >> 4) & 0xf });
        },
        0xA => {
            // 0xANNN: set the Index register (I) to 0xNNN
            self.I = val & 0x0fff;
            self.pc += 2;
            // std.debug.print("MOV I, 0x{x}\n", .{val & 0x0fff});
        },
        0xB => {
            // 0xBNNN: jump to address V0 + NNN
            self.pc = self.V[0] + val;
            // std.debug.print("JMP V0[0x{x}]\n", .{val});
        },
        0xC => {
            // 0xCXNN: Sets Vx to rand() & NN (bit-wise AND between a random number and NN)
            self.V[val >> 8] = @truncate(val & std.crypto.random.int(u8));
            self.pc += 2;
            // std.debug.print("RND V{d}, 0x{x}\n", .{ val >> 8, val & 0xff });
        },
        0xD => {
            // 0xDXYN: draw a sprite at coordinates (Vx, Vy) with a width of 8 pixels and height of N pixels
            const Vx = self.V[val >> 8];
            const Vy = self.V[(val >> 4) & 0x000f];

            //std.debug.print("Drawing sprite at ({d}, {d}) with width of {d} pixels\n", .{ Vx, Vy, val & 0xf });
            // std.debug.print("DRAW V{d}, V{d}, 0x{x}\n", .{ val >> 8, (val >> 4) & 0xf, val & 0xf });
            self.V[0xf] = 0;

            for (0..val & 0x000f) |y| {
                const pixel = self.memory[self.I + y];
                for (0..8) |x| {
                    const bit: u8 = @as(u8, 0x80) >> @as(u3, @truncate(x));
                    if ((pixel & bit) != 0) {
                        const posX = (Vx + x) % width;
                        const posY = (Vy + y) % height;
                        const index = posX + posY * width;
                        if (self.gfx[index] == 1) self.V[0xf] = 1;
                        self.gfx[index] ^= 1;
                    }
                }
            }
            self.pc += 2;
            self.draw = true;
        },
        0xE => {
            switch (val & 0xff) {
                0x9E => {
                    // 0xEX9E: skip next instruction if key() == Vx (key stored in Vx is pressed)
                    if (self.key[self.V[(val >> 8) & 0x0f]] != 0) {
                        self.pc += 4;
                    } else {
                        self.pc += 2;
                    }
                    // std.debug.print("JKE V{d}\n", .{val >> 8});
                },
                0xA1 => {
                    // 0xEX9E: skip next instruction if key() != Vx (key stored in Vx is not pressed)
                    if (self.key[self.V[(val >> 8) & 0x0f]] == 0) {
                        self.pc += 4;
                    } else {
                        self.pc += 2;
                    }
                    // std.debug.print("JKNE V{d}\n", .{val >> 8});
                },
                else => return error.InvalidInstruction,
            }
        },
        0xF => {
            switch (val & 0xff) {
                0x07 => {
                    self.V[val >> 8] = self.delay_timer;
                    // std.debug.print("VDT V{d}\n", .{val >> 8});
                },
                0x0A => {
                    if (std.mem.indexOfScalar(u8, &self.key, 1)) |k| {
                        self.V[val >> 8] = @truncate(k);
                    } else return;
                    self.pc += 2;
                },
                0x15 => {
                    self.delay_timer = self.V[val >> 8];
                    // std.debug.print("DTV V{d}\n", .{val >> 8});
                },
                0x18 => {
                    self.sound_timer = self.V[val >> 8];
                    // std.debug.print("STV V{d}\n", .{val >> 8});
                },
                0x1E => {
                    const add = @addWithOverflow(self.I, self.V[val >> 8]);
                    self.I = add[0];
                    self.V[0xf] = 1 - add[1];
                    // std.debug.print("MOV I, V{d}\n", .{val >> 8});
                },
                0x29 => {
                    // 0xFX29: set I to the location of the sprite for character in Vx
                    self.I = self.V[val >> 8] * 5;
                    // std.debug.print("SPI V{d}\n", .{val >> 8});
                },
                0x33 => {
                    // 0xFX33: store the binary coded decimal representation of Vx at address I onwards
                    self.memory[self.I] = self.V[val >> 8] / 100;
                    self.memory[self.I + 1] = (self.V[val >> 8] / 10) % 10;
                    self.memory[self.I + 2] = (self.V[val >> 8] % 100) % 10;
                    // std.debug.print("BIN V{d}\n", .{val >> 8});
                },
                0x55 => {
                    // 0xFX55: store the value of registers from V0 to Vx into the memory addresses starting at I (leaving I unmodified)
                    for (0..val >> 8) |i| {
                        self.memory[self.I + i] = self.V[i];
                    }
                    // std.debug.print("STORE {x}\n", .{val >> 8});
                },
                0x65 => {
                    // 0xFX65: restore the values of registers from V0 to Vx, saved into memory addresses starting at I

                    for (0..val >> 8) |i| {
                        self.V[i] = self.memory[self.I + i];
                    }
                    // std.debug.print("LOAD {x}\n", .{val >> 8});
                },
                else => return error.InvalidInstruction,
            }
            self.pc += 2;
        },
    }

    if (self.delay_timer > 0)
        self.delay_timer -= 1;

    if (self.sound_timer > 0) {
        if (self.sound_timer == 1) {
            self.beep(50);
        }
        self.sound_timer -= 1;
    }
}

pub fn drawGraphics(self: *Self) void {
    if (!self.draw) return;
    defer self.draw = false;

    var y: c_int = 0;
    var x: c_int = 0;
    for (0..self.gfx.len) |i| {
        if (i > 0 and i % width == 0) {
            y += self.scale;
            x = 0;
        }
        const rect: c.SDL_Rect = .{
            .x = x,
            .y = y,
            .w = self.scale,
            .h = self.scale,
        };
        if (self.gfx[i] == 0) {
            _ = c.SDL_SetRenderDrawColor(self.screen.?.renderer, 0x00, 0x00, 0x00, 255);
        } else {
            _ = c.SDL_SetRenderDrawColor(self.screen.?.renderer, 0xff, 0xff, 0xff, 255);
        }
        _ = c.SDL_RenderFillRect(self.screen.?.renderer, &rect);
        x += self.scale;
    }

    _ = c.SDL_RenderPresent(self.screen.?.renderer);
}

pub fn setupGraphics(self: *Self) !void {
    // std.debug.print("Setting up graphics...\n", .{});
    if (self.screen != null) return;
    errdefer self.deinit();
    if (c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_AUDIO) < 0) return error.SDLInit;

    const window = c.SDL_CreateWindow(
        "Chip8",
        c.SDL_WINDOWPOS_UNDEFINED,
        c.SDL_WINDOWPOS_UNDEFINED,
        width * self.scale,
        height * self.scale,
        c.SDL_WINDOW_SHOWN,
    ) orelse return error.SDLCreateWindow;

    const renderer = c.SDL_CreateRenderer(
        window,
        -1,
        c.SDL_RENDERER_ACCELERATED,
    ) orelse return error.SDLCreateRenderer;

    _ = c.SDL_SetHint(c.SDL_HINT_RENDER_SCALE_QUALITY, "linear");

    self.screen = .{
        .window = window,
        .renderer = renderer,
    };
}

fn callback(userdata: ?*anyopaque, stream: [*c]u8, len: c_int) callconv(.C) void {
    const fstream: [*]f32 = @ptrCast(@alignCast(stream));
    const oscilator: *Oscilator = @ptrCast(@alignCast(userdata.?));
    var i: usize = 0;
    while (i < @divFloor(len, @sizeOf(f32))) : (i += 1) {
        fstream[i] = oscilator.next();
    }
}

const Oscilator = struct {
    step: f32 = (2.0 * std.math.pi) / (44100.0 / 440.0),
    volume: f32 = 0.5,
    current: f32 = 0,
    pub fn next(self: *Oscilator) f32 {
        defer self.current += self.step;
        return @sin(self.current) * self.volume;
    }
};

pub fn beep(self: *Self, time: u32) void {
    _ = .{self};
    if (c.SDL_GetAudioDeviceName(c.SDL_GetNumAudioDevices(0) - 1, 0)) |name| {
        var oscilator = Oscilator{};
        const desired: c.SDL_AudioSpec = .{
            .format = c.AUDIO_F32,
            .channels = 1,
            .freq = 44100,
            .samples = 4096,
            .callback = callback,
            .userdata = &oscilator,
        };

        var spec: c.SDL_AudioSpec = undefined;
        const dev = c.SDL_OpenAudioDevice(name, 0, &desired, &spec, 0);
        if (dev == 0) return;
        c.SDL_PauseAudioDevice(dev, 0);
        c.SDL_Delay(time);
        c.SDL_PauseAudioDevice(dev, 1);
    }
}

pub fn setKeys(self: *Self) void {
    // std.debug.print("Setting keys...\n", .{});
    var e: c.SDL_Event = undefined;
    while (c.SDL_PollEvent(&e) != 0) {
        switch (e.type) {
            c.SDL_QUIT => self.quit = true,
            c.SDL_KEYDOWN => {
                switch (e.key.keysym.sym) {
                    c.SDLK_1 => self.key[0x1] = 1,
                    c.SDLK_2 => self.key[0x2] = 1,
                    c.SDLK_3 => self.key[0x3] = 1,
                    c.SDLK_4 => self.key[0xC] = 1,
                    c.SDLK_q => self.key[0x4] = 1,
                    c.SDLK_w => self.key[0x5] = 1,
                    c.SDLK_e => self.key[0x6] = 1,
                    c.SDLK_r => self.key[0xD] = 1,
                    c.SDLK_a => self.key[0x7] = 1,
                    c.SDLK_s => self.key[0x8] = 1,
                    c.SDLK_d => self.key[0x9] = 1,
                    c.SDLK_f => self.key[0xE] = 1,
                    c.SDLK_z => self.key[0xA] = 1,
                    c.SDLK_x => self.key[0x0] = 1,
                    c.SDLK_c => self.key[0xB] = 1,
                    c.SDLK_v => self.key[0xF] = 1,
                    c.SDLK_ESCAPE => self.quit = true,
                    else => continue,
                }
            },
            c.SDL_KEYUP => {
                switch (e.key.keysym.sym) {
                    c.SDLK_1 => self.key[0x1] = 0,
                    c.SDLK_2 => self.key[0x2] = 0,
                    c.SDLK_3 => self.key[0x3] = 0,
                    c.SDLK_4 => self.key[0xC] = 0,
                    c.SDLK_q => self.key[0x4] = 0,
                    c.SDLK_w => self.key[0x5] = 0,
                    c.SDLK_e => self.key[0x6] = 0,
                    c.SDLK_r => self.key[0xD] = 0,
                    c.SDLK_a => self.key[0x7] = 0,
                    c.SDLK_s => self.key[0x8] = 0,
                    c.SDLK_d => self.key[0x9] = 0,
                    c.SDLK_f => self.key[0xE] = 0,
                    c.SDLK_z => self.key[0xA] = 0,
                    c.SDLK_x => self.key[0x0] = 0,
                    c.SDLK_c => self.key[0xB] = 0,
                    c.SDLK_v => self.key[0xF] = 0,
                    else => continue,
                }
            },
            else => continue,
        }
    }
    // std.debug.print("Keypad: {x}\n", .{self.key});
}
