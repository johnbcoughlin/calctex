/* $Id: dviop.h,v 1.1 2001/05/27 19:53:07 jfine Exp $ */
/*  -*- linux-c -*- */
/*
email:jfine@active-tex.demon.co.uk

Maybe this should be called dviopc.h.  But most of the dviopc are in
fact dviops: they have no operand

$Log: dviop.h,v $
Revision 1.1  2001/05/27 19:53:07  jfine
Initial revision


*/

/*

A dvi file consists of a sequence of dvi operations.  A dvi operation
consists of a code followed by an operand, which may be empty.  Some
operations have a string of some sort as part of its operand.  The
length of such operations is variable.

This file, following Don Knuth's example in dvitype, provides symbolic
names for the dvi opcodes.  The names are exactly as given by Don,
except that "dviop_" has been added as a prefix.  Thus, "dviop_bop" is
the begin page operation.  This gives us a good chance of avoiding
name clashes with other source files.

A trick allows the programmer to write
      case dviop_set_char:
in a switch statement, and by so doing catch dviop_set_char_1 through
to dviop_set_char_127.  Don's "sixty_four_cases()" macro is a similar
trick.

The 256 dvi operations are divided into classes in various ways.  Here
is the division according to size.
	switch (op) {
	case dviop_misc:
	case dviop_set_char:
	case dviop_fnt_num:
	case dviop_size1:
	case dviop_size2:
	case dviop_size3:
	case dviop_size4:
	case dviop_size5:
	case dviop_size9:
	case dviop_var_size:
	case dviop_undefined:
	default:
	}

Some comments.  1) The categories misc, set_char and fnt_num are used
in every division.  2) Misc consists of the dvi operations bop, eop,
post and post_post.  The cannot appear within a page.  3) Not every
byte-code is a dvi operation.  Those that are not are undefined.

*/

#ifndef _DVI_OP_H
#define _DVI_OP_H

/* symbolic names for the dvi operations */
enum {
	dviop_set_char_0 = 0,
	dviop_set1 = 128, dviop_set2, dviop_set3, dviop_set4,
	dviop_set_rule,
	dviop_put1, dviop_put2, dviop_put3, dviop_put4,
	dviop_put_rule, dviop_nop, dviop_bop, dviop_eop,
	dviop_push, dviop_pop,
	dviop_right1, dviop_right2, dviop_right3, dviop_right4,
	dviop_w0, dviop_w1, dviop_w2, dviop_w3, dviop_w4,
	dviop_x0, dviop_x1, dviop_x2, dviop_x3, dviop_x4,
	dviop_down1, dviop_down2, dviop_down3, dviop_down4,
	dviop_y0, dviop_y1, dviop_y2, dviop_y3, dviop_y4,
	dviop_z0, dviop_z1, dviop_z2, dviop_z3, dviop_z4,
	dviop_fnt_num_0,
	dviop_fnt1 = 235, dviop_fnt2, dviop_fnt3, dviop_fnt4,
	dviop_xxx1, dviop_xxx2, dviop_xxx3, dviop_xxx4,
	dviop_fnt_def1, dviop_fnt_def2, dviop_fnt_def3, dviop_fnt_def4,
	dviop_pre, dviop_post, dviop_post_post
};

/* operations that cannot appear within a page */
#define dviop_misc \
dviop_pre: case dviop_bop: case dviop_eop: \
case dviop_post: case dviop_post_post

/* set a single character, in the current font */
#define dviop_set_char \
0: case 1: case 2: case 3: case 4: case 5: case 6: case 7: \
case 8: case 9: case 10: case 11: case 12: case 13: case 14: case 15: \
case 16: case 17: case 18: case 19: case 20: case 21: case 22: case 23: \
case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31: \
case 32: case 33: case 34: case 35: case 36: case 37: case 38: case 39: \
case 40: case 41: case 42: case 43: case 44: case 45: case 46: case 47: \
case 48: case 49: case 50: case 51: case 52: case 53: case 54: case 55: \
case 56: case 57: case 58: case 59: case 60: case 61: case 62: case 63: \
case 64: case 65: case 66: case 67: case 68: case 69: case 70: case 71: \
case 72: case 73: case 74: case 75: case 76: case 77: case 78: case 79: \
case 80: case 81: case 82: case 83: case 84: case 85: case 86: case 87: \
case 88: case 89: case 90: case 91: case 92: case 93: case 94: case 95: \
case 96: case 97: case 98: case 99: case 100: case 101: case 102: case 103: \
case 104: case 105: case 106: case 107: case 108: case 109: case 110: case 111: \
case 112: case 113: case 114: case 115: case 116: case 117: case 118: case 119: \
case 120: case 121: case 122: case 123: case 124: case 125: case 126: case 127

/* change the current font */
#define dviop_fnt_num \
171: case 172: case 173: case 174: case 175: case 176: case 177: case 178: \
case 179: case 180: case 181: case 182: case 183: case 184: case 185: case 186: \
case 187: case 188: case 189: case 190: case 191: case 192: case 193: case 194: \
case 195: case 196: case 197: case 198: case 199: case 200: case 201: case 202: \
case 203: case 204: case 205: case 206: case 207: case 208: case 209: case 210: \
case 211: case 212: case 213: case 214: case 215: case 216: case 217: case 218: \
case 219: case 220: case 221: case 222: case 223: case 224: case 225: case 226: \
case 227: case 228: case 229: case 230: case 231: case 232: case 233: case 234

/* operand is empty */
#define dviop_size1 \
dviop_nop: case dviop_push: case dviop_pop: \
case dviop_w0: case dviop_x0: case dviop_y0: case dviop_z0

/* operand is a single byte */
#define dviop_size2 \
dviop_set1: case dviop_put1: case dviop_right1: case dviop_down1: \
case dviop_w1: case dviop_x1: case dviop_y1: case dviop_z1: \
case dviop_fnt1

/* operand is two bytes */
#define dviop_size3 \
dviop_set2: case dviop_put2: case dviop_right2: case dviop_down2: \
case dviop_w2: case dviop_x2: case dviop_y2: case dviop_z2: \
case dviop_fnt2

/* operand is three bytes */
#define dviop_size4 \
dviop_set3: case dviop_put3: case dviop_right3: case dviop_down3: \
case dviop_w3: case dviop_x3: case dviop_y3: case dviop_z3: \
case dviop_fnt3

/* operand is four bytes, i.e. a quad */
#define dviop_size5 \
dviop_set4: case dviop_put4: case dviop_right4: case dviop_down4: \
case dviop_w4: case dviop_x4: case dviop_y4: case dviop_z4: \
case dviop_fnt4

/* operand is two quads */
#define dviop_size9 \
dviop_set_rule: case dviop_put_rule

/* operand's size is variable */
#define dviop_var_size \
dviop_xxx1: case dviop_xxx2: case dviop_xxx3: case dviop_xxx4: \
case dviop_fnt_def1: case dviop_fnt_def2: case dviop_fnt_def3: case dviop_fnt_def4

/* undefined operations */
#define dviop_undefined \
250: case 251: case 252: case 253: case 254: case 255

#endif /* _DVI_DVIOP_H */
