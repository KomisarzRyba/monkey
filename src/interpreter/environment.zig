const std = @import("std");
const Allocator = std.mem.Allocator;

const object = @import("object/object.zig");

allocator: Allocator,
store: std.StringHashMap(object.Object),
outer: ?*Self = null,

const Self = @This();

pub fn init(allocator: Allocator) Self {
    return Self{
        .allocator = allocator,
        .store = .init(allocator),
    };
}

pub fn init_enclosed(allocator: Allocator, outer: *Self) Self {
    return Self{
        .allocator = allocator,
        .store = .init(allocator),
        .outer = outer,
    };
}

pub fn deinit(self: *Self) void {
    self.store.deinit();
}

pub fn get(self: *Self, name: []const u8) ?object.Object {
    const value = self.store.get(name);
    if (value) |v| return v;
    if (self.outer) |outer| return outer.get(name);
    return null;
}

pub fn set(self: *Self, name: []const u8, value: object.Object) !void {
    // this fails when setting y inside the closure.
    // store seems to be not initialized
    try self.store.put(name, value);
}
