# Wave2 Assembler

Assembler for the [Wave2](https://github.com/Meisaka/MeiVM2/) architecture.

### See also:
- https://github.com/Meisaka/MeiVM2/
- https://github.com/zeb-hicks/waverune
- https://marketplace.visualstudio.com/items?itemName=Nimphious.wave2-assembly

---
### Contents

- [Example](#example)
- [Instruction Set](#instruction-set)
- [Install](#install)
- [Usage](#usage)

<a name="example"></a>

## Example

This example program cycles the user's ship colour through a list of colours stored in the constant register.

<div style="font-family: monospace; padding: 1em; border-radius: 0.5em; background: #202224; color: #ddd; white-space-collapse: preserve;"><span style="color: #6c6;">; This section describes the memory layout of the constant registers</span>
<span style="color: #ee9;">.memory</span>

<span style="color: #a84;">000f 00ff 00f0 0ff0
0f00 ff00 f000 0f0f
f0f0 fff0 f00f 0fff
f0ff ff0f 7777 ffff</span>

<span style="color: #6c6;">; This section contains the code that is inserted at 0x40</span>
<span style="color: #ee9;">.code</span>

<span style="color: #59f;">set</span> r1, $0f          <span style="color: #6c6;">; Colour count</span>
<span style="color: #59f;">set</span> r4, $39c         <span style="color: #6c6;">; Ship colour</span>
<span style="color: #fff;">:loop</span>

<span style="color: #59f;">mov</span> r1.x, <span style="color: #e83;">[r0.x]</span>     <span style="color: #6c6;">; Read colour from constants</span>
<span style="color: #59f;">mov</span> <span style="color: #e83;">[r0.x+]</span>, r1.x    <span style="color: #6c6;">; Increment r0 and also copy the colour</span>

<span style="color: #59f;">mov</span> r2.x, r0.x       <span style="color: #6c6;">; Make a copy of the counter</span>
<span style="color: #59f;">ge.w</span> r2, r2, r1      <span style="color: #6c6;">; If counter > colour count</span>
<span style="color: #59f;">sub.w</span> ri, ri, r2     <span style="color: #6c6;">; (skips next if false)</span>
<span style="color: #59f;">sub.w</span> r0, r0, r0     <span style="color: #6c6;">; then reset counter</span>

<span style="color: #59f;">set</span> r6, <span style="color: #0ff;">$04ff</span>        <span style="color: #6c6;">; Wait duration</span>
<span style="color: #59f;">slp.w</span> r6             <span style="color: #6c6;">; Sleep for duration</span>
<span style="color: #59f;">mov</span> <span style="color: #e83;">[r4.x]</span>, r1.x     <span style="color: #6c6;">; Set ship colour</span>
<span style="color: #59f;">jmp</span> <span style="color: #fff;">:loop</span></div>

<a name="instruction-set"></a>

## Instruction Set

- N = Number                   <sup>e.g. `42`</sup>
- C = Constant register        <sup>e.g. `c3`</sup>
- R = General Purpose Register <sup>e.g. `r2`</sup>
- P = Pointer                  <sup>e.g. `[r0.x]`</sup>
- I = Increment Pointer        <sup>e.g. `[ri.x+]`</sup>
- L = Label                    <sup>e.g. `:Loop_Start`</sup>

<table>
    <th>
        <td>Mnemonics</td>
        <td>Operands</td>
        <td>Description</td>
        <td>Notes / Examples</td>
    </th>
    <tr>
        <td>Halt</td>
        <td>
            <code>hlt|halt</code>
        </td>
        <td></td>
        <td>Halts the process</td>
        <td></td>
    </tr>
    <tr>
        <td>Nop</td>
        <td>
            <code>nop</code>
        </td>
        <td></td>
        <td>No-op.</td>
        <td>A single-tick sleep. Under the hood this is a sleep for 0 ticks.</td>
    </tr>
    <tr>
        <td>Skip</td>
        <td>
            <code>skip|skip1</code><br/>
            <code>skip2</code><br/>
            <code>skip3</code><br/>
            <code>skip4</code>
        </td>
        <td></td>
        <td>
            Skip increments the program counter by the specified amount.<br/>
            <br/>
            This can be useful for flow control in certian situations.
        </td>
        <td></td>
    </tr>
    <tr>
        <td>Sleep</td>
        <td>
            <code>slp|sleep</code>
        </td>
        <td>
            <code>N</code><br/>
            <code>C</code><br/>
            <code>R</code><br/>
        </td>
        <td>
            Pauses execution for a specified duration.<br/>
            <br/>
            When specifying a number, the number must be between 0 and 255.<br/>
            When specifying a register, the instruction requires an additional specifier for the byte or word from which to select the duration from the target register.
        </td>
        <td>
            <code>slp.w r0</code> sleeps for the duration specified by the word <code>r0.x</code><br/>
            <code>slp.h r3</code> sleeps for the duration specified by the high byte of the word <code>r3.x</code><br/>
            <br/>
            The valid specifiers are <code>.w</code> for word, <code>.h</code> for high byte, and <code>.l</code> for low byte.
        </td>
    </tr>
    <tr>
        <td>Move</td>
        <td>
            <code>mov|move</code>
        </td>
        <td>
            <code>R, R</code><br/>
            <code>R, C</code><br/>
            <code>R, P</code><br/>
            <code>R, I</code><br/>
            <code>P, R</code><br/>
            <code>P, C</code><br/>
            <code>I, R</code><br/>
            <code>I, C</code><br/>
        </td>
        <td>
            Copy a value between registers or pointers.<br/>
            <br/>
            Registers can either be unswizzled, or have matching swizzles. Different swizzles are not supported.<br/>
            <br/>
            Moves to pointers are encoded as store instructions.<br/>
            Moves from pointers are encoded as load instructions.
        </td>
        <td>
            <code>mov r0, c1</code> Copies <code>c1</code>'s value into <code>r0</code><br/>
            <code>mov [c4.x], r5.x</code> Stores the first word of <code>r5</code> into the address of the pointer in the first word of <code>c4</code><br/>
        </td>
    </tr>
    <tr>
        <td>Select</td>
        <td>
            <code>wmov|wmove</code><br/>
            <code>wswap</code><br/>
            <code>wadd</code><br/>
            <code>wsub</code>
        </td>
        <td>
            <code>R, R</code><br/>
            <code>R, C</code>
        </td>
        <td>Performs specified operations on a an arbitrary source word to the first word in the destination.</td>
        <td>
            <code>wmove r0, c4.y</code> Copies <code>c4.y</code> into <code>r0.x</code><br/>
            <code>wadd r2, c1.w</code> Adds <code>c1.w</code> to <code>r2.x</code>
        </td>
    </tr>
    <tr>
        <td>Swizzle</td>
        <td>
            <code>swi</code><br/>
            <code>swizzle</code>
        </td>
        <td>
            <code>R</code>
        </td>
        <td>Swizzles the words within a register arbitrarily.<br/>Using the <code>.[xyzw]</code> notation the words in a register can be arbitrarily reassigned/copied.</td>
        <td>
            <code>swizzle r4.wwww</code> Sets every word in <code>r4</code> to its fourth word. (<code>r4.w</code>)<br/>
            <code>swizzle r0.yyzz</code> Given <code>r0</code> is <code>[1, 2, 3, 4]</code>, sets <code>r0</code> to be <code>[1, 1, 2, 2]</code>
        </td>
    </tr>
    <tr>
        <td>Math</td>
        <td>
            <code>add</code><br/>
            <code>adds</code><br/>
            <code>ado|addo|addover</code><br/>
            <code>sub</code><br/>
            <code>subs</code><br/>
            <code>suo|subo|subover</code><br/>
            <br/>
            <code>eq|equ</code><br/>
            <code>ne|neq</code><br/>
            <code>lt</code><br/>
            <code>le|lte</code><br/>
            <code>gt</code><br/>
            <code>ge|gte</code><br/>
            <code>carry</code><br/>
        </td>
        <td>
            <code>R, R, R</code><br/>or<br/>
            <code>R, R, C</code><br/>or<br/>
            <code>R, C, R</code><br/>
        </td>
        <td>
            Math instructions take three operands in the order destination, left, right. The destination register must be writeable (not constant) and this same register must be at least one of the other two operands. The remaining operand can be a constant register.<br/>
            <br/>
            Additionally, math instructions require a width specifier <code>.b</code> or <code>.w</code> to control whether the operation is performed between bytes or words, respectively.
        </td>
        <td></td>
    </tr>
    <tr>
        <td>Bitwise</td>
        <td>
            <code>all</code><br/>
            <code>one</code><br/>
            <code>swp|swap</code><br/>
            <code>nsrc|notsrc</code><br/>
            <code>ndst|notdst</code><br/>
            <code>sand|srcandnotdest</code><br/>
            <code>nsad|notsrcanddest</code><br/>
            <code>sond|srcornotdest</code><br/>
            <code>nsod|notsrcordest</code><br/>
            <code>and</code><br/>
            <code>or</code><br/>
            <code>xor</code><br/>
            <code>nand</code><br/>
            <code>nor</code><br/>
            <code>xnor</code><br/>
        </td>
        <td>
            <code>R</code><br/>
            <code>R, R</code><br/>
            <code>R, C</code><br/>
        </td>
        <td>
            Bitwise operations are all binary operand except for <code>all</code> and <code>one</code> which are unary operations taking only one writeable register as an operand.<br/>
            <br/>
            Bitwise operations operate on all words of the specified registers.
        </td>
        <td>
            Some examples include:<br/>
            <code>and r0, r4</code> → <code>r0 = r0 & r4</code><br/>
            <code>xor r1, c0</code> → <code>r1 = r1 ^ c0</code><br/>
            <code>srcandnotdest ri, c2</code> → <code>ri = ri & !c2</code>
        </td>
    </tr>
    <tr>
        <td>Shift</td>
        <td>
            <code>asl|lsl</code><br/>
            <code>rol</code><br/>
            <code>asr</code><br/>
            <code>lsr</code><br/>
            <code>ror</code><br/>
        </td>
        <td>
            <code>R, R</code><br/>
            <code>R, N</code>
        </td>
        <td>
            Math instructions take three operands in the order destination, left, right. The destination register must be writeable (not constant) and this same register must be at least one of the other two operands. The remaining operand can be a constant register.<br/>
            <br/>
            Additionally, shift instructions require a width specifier <code>.b</code> or <code>.w</code> to control whether the operation is performed at the byte or word level, respectively.
        </td>
        <td></td>
    </tr>
    <tr>
        <td>Special</td>
        <td>
            <code>hadd</code><br/>
            <code>mul|mults|multisat</code><br/>
            <code>mlo|multl|multlow</code><br/>
            <code>mhi|multh|multhigh</code><br/>
            <code>div|divide</code><br/>
            <code>rdiv|rdivide</code><br/>
        </td>
        <td>
            <code>R, R</code><br/>
            <code>R, C</code>
        </td>
        <td>
            Special advanced arithmetic instructions.<br/>
            <br/>
            These instructions are all SIMD and operate on all words in a specified register.
        </td>
        <td></td>
    </tr>
    <tr>
        <td>Set</td>
        <td>
            <code>set|set1</code><br/>
            <code>set2</code><br/>
            <code>set3</code><br/>
            <code>set4</code>
        </td>
        <td>
            <code>R, N</code><br/>
            <code>R, L</code>
        </td>
        <td>
            Set is a shorthand for an advanced usage of the <code>move</code> instruction in which the incremented program counter is used as a pointer to the raw value stored in the proceding memory addresses.<br/>
            <br/>
            The second operand can be a literal value or a label.<br/>
        </td>
        <td>
            Literal values will simply be stored in the proceding memory addresses directly.<br/>
            Labels will store the jump address of the specified label into the proceding memory address.
        </td>
    </tr>
    <tr>
        <td>Jump</td>
        <td>
            <code>jmp</code>
        </td>
        <td>
            <code>N</code><br/>
            <code>L</code>
        </td>
        <td>Jump is a shorthand for storing literal values directly into the program counter. Similar to Set except we load the value into <code>ri.x</code> specifically.</td>
        <td>Effectively equivalent to writing <code>set ri, *</code> but a little more idiomatic.</td>
    </tr>
</table>

<a name="install"></a>

## Install

Make sure rust is installed, and then run:
```
cargo install --git https://github.com/zeb-hicks/wave2_assembler
```

<a name="usage"></a>

## Usage

```
waveasm [OPTIONS] <INPUT>

Arguments:
  <INPUT>  Input file path

Options:
  -b, --binary                 Output as Wave2 binary format
  -o, --output <OUTPUT>        Output file path, only logs to stdout if not set
  -l, --log-level <LOG_LEVEL>  Log level, valid values are: OFF, ERROR, WARN, INFO, DEBUG, TRACE [default: WARN]
  -h, --help                   Print help
```
