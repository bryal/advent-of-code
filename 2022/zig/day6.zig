const std = @import("std");

pub fn main() anyerror!void {
    var buf: [4096]u8 = undefined;
    var input = try std.fs.cwd().readFile("../inputs/day6.txt", &buf);
    const packet = find_marker(input, 4);
    const message = find_marker(input, 14);
    std.debug.print("part 1: {d}\npart 2: {d}\n", .{ packet, message });
}

fn find_marker(input: []u8, n: usize) usize {
    var i: usize = n - 1;
    outer: while (i < input.len) : (i += 1) {
        for (input[i - (n - 1) .. i]) |c1, j| {
            for (input[i + 1 - (n - 1) + j .. i + 1]) |c2| {
                if (c1 == c2) continue :outer;
            }
        }
        return i + 1;
    }
    return 0;
}
