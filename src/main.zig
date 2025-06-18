const std = @import("std");
const Chip8 = @import("chip8.zig");

pub fn main() !void {
    var chip = Chip8.init();
    defer chip.deinit();

    try chip.loadGame("games/ufo.c8");
    try chip.setupGraphics();

    var timer = try std.time.Timer.start();

    while (!chip.quit) {
        try chip.nextCycle();
        chip.setKeys();
        chip.drawGraphics();
        const interval = timer.lap();
        if (interval < Chip8.frequency) {
            const ns = Chip8.frequency - interval;
            //std.debug.print("Sleeping for {d} ns\n", .{ns});
            std.time.sleep(ns);
        }
    }
}
