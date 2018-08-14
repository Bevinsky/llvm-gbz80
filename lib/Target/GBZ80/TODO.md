# Todo

Need to add some hooks/flags containing info about what flags instrs
produce. e.g., does the instruction produce a valid zero flag at all?
Also need hooks to detect users of flags as well.

Remat of more? What can we safely remat?

## Optimizations

Analyze comparisons post-isel and attempt to swap operands of CPs if profitable.
This should minimize register use for when we have swapped a constant RHS into
the LHS of a comparison.

---

Check for flag-producing instructions leading into CP (like INC, DEC, etc) and
try to use them as flag-producer instead, especially for Z and NZ (C and NC are
harder)

---

After PEI, try to find stack stores that dominate stack loads. (there's another
condition here that I haven't figured out yet) These should be convertible into
PUSH-POP pairs if this is safe (won't invalidate anything inside the region).

---

Eliminate unneeded PUSH-POP pairs between stack loads/stores when possible.

---

Expand ANDs with masks that have more than one bit clear, and ORs with sets of
bits. This should be profitable up to 4 bits for i16 and 2 bits for i8.

## Patterns and expansions

```
SEXT C -> BC
  LD B, 0xFF
  BIT 7, C
  JR NZ, .Over
  LD B, 0x00
.Over
Worst case: 8B, 8C
```
---
```
ZEXT C -> BC
LD B, 0
```
---
```
LD R, [XY]
    LD R, [HL]
    (1, 2)
      or
    LD A, [BC/DE]
    LD R, A
    (2, 3)

LD R, [XY+-]
    LD A, [HL+-]
    LD R, A
    (2, 3)
      or
    LD A, [BC/DE]
    INC/DEC BC/DE
    LD R, A
    (3, 5)

LD [XY], R
    LD [HL], R
    (1, 2)
      or
    LD A, R
    LD [BC/DE], A
    (2, 3)

LD [XY+-], R
    LD A, R
    LD [HL+-], A
    (2, 3)
      or
    LD A, R
    LD [BC/DE], A
    INC/DEC BC/DE
    (3, 5)
```
---
Load of i16. Clobbers the pointer.

Perhaps correct this with hld afterward?

```
ld a, [hli]
ld h, [hl]
ld l, a

ld a, [hli]
ld h, [hl]
ld l, a

ld a, [hli]
ld h, [hl]
ld l, a
...
```
---
Add of i8 to i16

```
    ld hl, value
    add hl, reg16
    ld HIGH(reg16), h
    ld LOW(reg16), l
```

```
    ld a, value
    add a, LOW(reg16)
    ld LOW(reg16), a
    jr nc, .noCarry
    inc HIGH(reg16)
.noCarry
```

```
    ld a, value
    add a, LOW(reg16)
    ld LOW(reg16), a
    adc a, HIGH(reg16)
    sub a, LOW(reg16)
    ld HIGH(reg16), a
```
---
signed hl <= de

```
    ld a, d
    xor h
    and $80
    jr nz, .differentSigns
    ; Unsigned comparison with `jr .no` 
.differentSigns
    ; a = $80
    and d ; Filter D's sign bit
    jr z, .yes
.no
```