local Tokenizer = require('tokenizer').Tokenizer

local function test(source, expected)
  local buf = source .. '\000'
  local tokenizer = Tokenizer.init(buf)
  for _, expected_tag in ipairs(expected) do
    local tok = tokenizer:next()
    if tok.tag ~= expected_tag then
      error("expected " .. expected_tag .. ", found " .. tok.tag)
    end
  end

  local tok = tokenizer:next()
  assert(tok.tag == "eof")
end

test("const c = self.buffer[self.index];", {"keyword_const", "identifier", "equal", "identifier", "period", "identifier", "l_bracket", "identifier", "period", "identifier", "r_bracket", "semicolon",})
test("a...z,", {"identifier", "ellipsis3", "identifier", "comma",})
test("start..end", {"identifier", "ellipsis2", "identifier",})
test("foo //", {"identifier",})
test("foo // line comment", {"identifier",})

do -- "keywords"
  test("test const else", { "keyword_test", "keyword_const", "keyword_else" })
end

do -- "line comment followed by top-level comptime"
  test([[// line comment
comptime {}]],
  {
    "keyword_comptime",
    "l_brace",
    "r_brace",
  })
end

do -- "unknown length pointer and then c pointer"
  test("[*]u8\n[*c]u8",
  {
    "l_bracket",
    "asterisk",
    "r_bracket",
    "identifier",
    "l_bracket",
    "asterisk",
    "identifier",
    "r_bracket",
    "identifier",
  })
end

do -- "code point literal with hex escape"
  test(
    [['\x1b']]
  , {"char_literal"});
  test(
    [['\x1']]
  , {"char_literal"});
end

do -- "newline in char literal"
  test(
    [['
']]
  , { "invalid", "invalid" })
end

do -- "newline in string literal"
  test(
    [["
"]]
  , { "invalid", "invalid" })
end

do -- "code point literal with unicode escapes"
  -- Valid unicode escapes
  test(
    [['\u{3}']]
  , {"char_literal"})
  test(
    [['\u{01}']]
  , {"char_literal"})
  test(
    [['\u{2a}']]
  , {"char_literal"})
  test(
    [['\u{3f9}']]
  , {"char_literal"})
  test(
    [['\u{6E09aBc1523}']]
  , {"char_literal"})
  test(
    [["\u{440}"]]
  , {"string_literal"})

  -- Invalid unicode escapes
  test(
    [['\u']]
  , {"char_literal"})
  test(
    [['\u{{']]
  , {"char_literal"})
  test(
    [['\u{}']]
  , {"char_literal"})
  test(
    [['\u{s}']]
  , {"char_literal"})
  test(
    [['\u{2z}']]
  , {"char_literal"})
  test(
    [['\u{4a']]
  , {"char_literal"})

  -- Test old-style unicode literals
  test(
    [['\u0333']]
  , {"char_literal"})
  test(
    [['\U0333']]
  , {"char_literal"})
end

do -- "code point literal with unicode code point"
  test(
    [['💩']]
  , {"char_literal"})
end

do -- "float literal e exponent" {
  test("a = 4.94065645841246544177e-324;\n", {
    "identifier",
    "equal",
    "number_literal",
    "semicolon",
  })
end

do -- "float literal p exponent" {
  test("a = 0x1.a827999fcef32p+1022;\n", {
    "identifier",
    "equal",
    "number_literal",
    "semicolon",
  })
end

do -- "chars" {
  test("'c'", {"char_literal"})
end

do -- "invalid token characters" {
  test("#", {"invalid"})
  test("`", {"invalid"})
  test("'c", {"invalid"})
  test("'", {"invalid"})
  test("''", {"char_literal"})
  test("'\n'", {"invalid", "invalid"})
end

do -- "invalid literal/comment characters" {
  test('"'..string.char(0x00)..'"', {"invalid"})
  test("//"..string.char(0x00), {"invalid"})
  test("//"..string.char(0x1f), {"invalid"})
  test("//"..string.char(0x7f), {"invalid"})
end

do -- "utf8" {
  test("//"..string.char(0xc2, 0x80), {})
  test("//"..string.char(0xf4, 0x8f, 0xbf, 0xbf), {})
end

do -- "invalid utf8" {
  test("//"..string.char(0x80), {})
  test("//"..string.char(0xbf), {})
  test("//"..string.char(0xf8), {})
  test("//"..string.char(0xff), {})
  test("//"..string.char(0xc2, 0xc0), {})
  test("//"..string.char(0xe0), {})
  test("//"..string.char(0xf0), {})
  test("//"..string.char(0xf0, 0x90, 0x80, 0xc0), {})
end

do -- "illegal unicode codepoints"
  -- unicode newline characters.U+0085, U+2028, U+2029
  test("//"..string.char(0xc2, 0x84), {})
  test("//"..string.char(0xc2, 0x85), {})
  test("//"..string.char(0xc2, 0x86), {})
  test("//"..string.char(0xe2, 0x80, 0xa7), {})
  test("//"..string.char(0xe2, 0x80, 0xa8), {})
  test("//"..string.char(0xe2, 0x80, 0xa9), {})
  test("//"..string.char(0xe2, 0x80, 0xaa), {})
end

do -- "string identifier and builtin fns"
  test([[const @"if" = @import("std");]],
  {
    "keyword_const",
    "identifier",
    "equal",
    "builtin",
    "l_paren",
    "string_literal",
    "r_paren",
    "semicolon",
  })
end

do -- "pipe and then invalid"
  test("||=", {
    "pipe_pipe",
    "equal",
  })
end

do -- "line comment and doc comment"
  test("//", {})
  test("// a / b", {})
  test("// /", {})
  test("/// a", {"doc_comment"})
  test("///", {"doc_comment"})
  test("////", {})
  test("//!", {"container_doc_comment"})
  test("//!!", {"container_doc_comment"})
end

do -- "line comment followed by identifier"
  test([[    Unexpected,
    // another
    Another,]],
  {
    "identifier",
    "comma",
    "identifier",
    "comma",
  })
end

do -- "UTF-8 BOM is recognized and skipped"
  test(string.char(0xEF, 0xBB, 0xBF).."a;\n", {
    "identifier",
    "semicolon",
  });
end

do -- "correctly parse pointer assignment"
  test("b.*=3;\n", {
    "identifier",
    "period_asterisk",
    "equal",
    "number_literal",
    "semicolon",
  });
end

do -- "correctly parse pointer dereference followed by asterisk"
  test("\"b\".* ** 10", {
    "string_literal",
    "period_asterisk",
    "asterisk_asterisk",
    "number_literal",
  });

  test("(\"b\".*)** 10", {
    "l_paren",
    "string_literal",
    "period_asterisk",
    "r_paren",
    "asterisk_asterisk",
    "number_literal",
  });

  test("\"b\".*** 10", {
    "string_literal",
    "invalid_periodasterisks",
    "asterisk_asterisk",
    "number_literal",
  });
end

do -- "range literals"
  test("0...9", { "number_literal", "ellipsis3", "number_literal" });
  test("'0'...'9'", { "char_literal", "ellipsis3", "char_literal" });
  test("0x00...0x09", { "number_literal", "ellipsis3", "number_literal" });
  test("0b00...0b11", { "number_literal", "ellipsis3", "number_literal" });
  test("0o00...0o11", { "number_literal", "ellipsis3", "number_literal" });
end

do -- "number literals decimal"
  test("0", {"number_literal"});
  test("1", {"number_literal"});
  test("2", {"number_literal"});
  test("3", {"number_literal"});
  test("4", {"number_literal"});
  test("5", {"number_literal"});
  test("6", {"number_literal"});
  test("7", {"number_literal"});
  test("8", {"number_literal"});
  test("9", {"number_literal"});
  test("1..", { "number_literal", "ellipsis2" });
  test("0a", {"number_literal"});
  test("9b", {"number_literal"});
  test("1z", {"number_literal"});
  test("1z_1", {"number_literal"});
  test("9z3", {"number_literal"});

  test("0_0", {"number_literal"});
  test("0001", {"number_literal"});
  test("01234567890", {"number_literal"});
  test("012_345_6789_0", {"number_literal"});
  test("0_1_2_3_4_5_6_7_8_9_0", {"number_literal"});

  test("00_", {"number_literal"});
  test("0_0_", {"number_literal"});
  test("0__0", {"number_literal"});
  test("0_0f", {"number_literal"});
  test("0_0_f", {"number_literal"});
  test("0_0_f_00", {"number_literal"});
  test("1_,", { "number_literal", "comma" });

  test("0.0", {"number_literal"});
  test("1.0", {"number_literal"});
  test("10.0", {"number_literal"});
  test("0e0", {"number_literal"});
  test("1e0", {"number_literal"});
  test("1e100", {"number_literal"});
  test("1.0e100", {"number_literal"});
  test("1.0e+100", {"number_literal"});
  test("1.0e-100", {"number_literal"});
  test("1_0_0_0.0_0_0_0_0_1e1_0_0_0", {"number_literal"});

  test("1.", { "number_literal", "period" });
  test("1e", {"number_literal"});
  test("1.e100", {"number_literal"});
  test("1.0e1f0", {"number_literal"});
  test("1.0p100", {"number_literal"});
  test("1.0p-100", {"number_literal"});
  test("1.0p1f0", {"number_literal"});
  test("1.0_,", { "number_literal", "comma" });
  test("1_.0", {"number_literal"});
  test("1._", {"number_literal"});
  test("1.a", {"number_literal"});
  test("1.z", {"number_literal"});
  test("1._0", {"number_literal"});
  test("1.+", { "number_literal", "period", "plus" });
  test("1._+", { "number_literal", "plus" });
  test("1._e", {"number_literal"});
  test("1.0e", {"number_literal"});
  test("1.0e,", { "number_literal", "comma" });
  test("1.0e_", {"number_literal"});
  test("1.0e+_", {"number_literal"});
  test("1.0e-_", {"number_literal"});
  test("1.0e0_+", { "number_literal", "plus" });
end

do -- "number literals binary"
  test("0b0", {"number_literal"});
  test("0b1", {"number_literal"});
  test("0b2", {"number_literal"});
  test("0b3", {"number_literal"});
  test("0b4", {"number_literal"});
  test("0b5", {"number_literal"});
  test("0b6", {"number_literal"});
  test("0b7", {"number_literal"});
  test("0b8", {"number_literal"});
  test("0b9", {"number_literal"});
  test("0ba", {"number_literal"});
  test("0bb", {"number_literal"});
  test("0bc", {"number_literal"});
  test("0bd", {"number_literal"});
  test("0be", {"number_literal"});
  test("0bf", {"number_literal"});
  test("0bz", {"number_literal"});

  test("0b0000_0000", {"number_literal"});
  test("0b1111_1111", {"number_literal"});
  test("0b10_10_10_10", {"number_literal"});
  test("0b0_1_0_1_0_1_0_1", {"number_literal"});
  test("0b1.", { "number_literal", "period" });
  test("0b1.0", {"number_literal"});

  test("0B0", {"number_literal"});
  test("0b_", {"number_literal"});
  test("0b_0", {"number_literal"});
  test("0b1_", {"number_literal"});
  test("0b0__1", {"number_literal"});
  test("0b0_1_", {"number_literal"});
  test("0b1e", {"number_literal"});
  test("0b1p", {"number_literal"});
  test("0b1e0", {"number_literal"});
  test("0b1p0", {"number_literal"});
  test("0b1_,", { "number_literal", "comma" });
end

do -- "number literals octal"
  test("0o0", {"number_literal"});
  test("0o1", {"number_literal"});
  test("0o2", {"number_literal"});
  test("0o3", {"number_literal"});
  test("0o4", {"number_literal"});
  test("0o5", {"number_literal"});
  test("0o6", {"number_literal"});
  test("0o7", {"number_literal"});
  test("0o8", {"number_literal"});
  test("0o9", {"number_literal"});
  test("0oa", {"number_literal"});
  test("0ob", {"number_literal"});
  test("0oc", {"number_literal"});
  test("0od", {"number_literal"});
  test("0oe", {"number_literal"});
  test("0of", {"number_literal"});
  test("0oz", {"number_literal"});

  test("0o01234567", {"number_literal"});
  test("0o0123_4567", {"number_literal"});
  test("0o01_23_45_67", {"number_literal"});
  test("0o0_1_2_3_4_5_6_7", {"number_literal"});
  test("0o7.", { "number_literal", "period" });
  test("0o7.0", {"number_literal"});

  test("0O0", {"number_literal"});
  test("0o_", {"number_literal"});
  test("0o_0", {"number_literal"});
  test("0o1_", {"number_literal"});
  test("0o0__1", {"number_literal"});
  test("0o0_1_", {"number_literal"});
  test("0o1e", {"number_literal"});
  test("0o1p", {"number_literal"});
  test("0o1e0", {"number_literal"});
  test("0o1p0", {"number_literal"});
  test("0o_,", { "number_literal", "comma" });
end

do -- "number literals hexadecimal"
  test("0x0", {"number_literal"});
  test("0x1", {"number_literal"});
  test("0x2", {"number_literal"});
  test("0x3", {"number_literal"});
  test("0x4", {"number_literal"});
  test("0x5", {"number_literal"});
  test("0x6", {"number_literal"});
  test("0x7", {"number_literal"});
  test("0x8", {"number_literal"});
  test("0x9", {"number_literal"});
  test("0xa", {"number_literal"});
  test("0xb", {"number_literal"});
  test("0xc", {"number_literal"});
  test("0xd", {"number_literal"});
  test("0xe", {"number_literal"});
  test("0xf", {"number_literal"});
  test("0xA", {"number_literal"});
  test("0xB", {"number_literal"});
  test("0xC", {"number_literal"});
  test("0xD", {"number_literal"});
  test("0xE", {"number_literal"});
  test("0xF", {"number_literal"});
  test("0x0z", {"number_literal"});
  test("0xz", {"number_literal"});

  test("0x0123456789ABCDEF", {"number_literal"});
  test("0x0123_4567_89AB_CDEF", {"number_literal"});
  test("0x01_23_45_67_89AB_CDE_F", {"number_literal"});
  test("0x0_1_2_3_4_5_6_7_8_9_A_B_C_D_E_F", {"number_literal"});

  test("0X0", {"number_literal"});
  test("0x_", {"number_literal"});
  test("0x_1", {"number_literal"});
  test("0x1_", {"number_literal"});
  test("0x0__1", {"number_literal"});
  test("0x0_1_", {"number_literal"});
  test("0x_,", { "number_literal", "comma" });

  test("0x1.0", {"number_literal"});
  test("0xF.0", {"number_literal"});
  test("0xF.F", {"number_literal"});
  test("0xF.Fp0", {"number_literal"});
  test("0xF.FP0", {"number_literal"});
  test("0x1p0", {"number_literal"});
  test("0xfp0", {"number_literal"});
  test("0x1.0+0xF.0", { "number_literal", "plus", "number_literal" });

  test("0x1.", { "number_literal", "period" });
  test("0xF.", { "number_literal", "period" });
  test("0x1.+0xF.", { "number_literal", "period", "plus", "number_literal", "period" });
  test("0xff.p10", {"number_literal"});

  test("0x0123456.789ABCDEF", {"number_literal"});
  test("0x0_123_456.789_ABC_DEF", {"number_literal"});
  test("0x0_1_2_3_4_5_6.7_8_9_A_B_C_D_E_F", {"number_literal"});
  test("0x0p0", {"number_literal"});
  test("0x0.0p0", {"number_literal"});
  test("0xff.ffp10", {"number_literal"});
  test("0xff.ffP10", {"number_literal"});
  test("0xffp10", {"number_literal"});
  test("0xff_ff.ff_ffp1_0_0_0", {"number_literal"});
  test("0xf_f_f_f.f_f_f_fp+1_000", {"number_literal"});
  test("0xf_f_f_f.f_f_f_fp-1_00_0", {"number_literal"});

  test("0x1e", {"number_literal"});
  test("0x1e0", {"number_literal"});
  test("0x1p", {"number_literal"});
  test("0xfp0z1", {"number_literal"});
  test("0xff.ffpff", {"number_literal"});
  test("0x0.p", {"number_literal"});
  test("0x0.z", {"number_literal"});
  test("0x0._", {"number_literal"});
  test("0x0_.0", {"number_literal"});
  test("0x0_.0.0", { "number_literal", "period", "number_literal" });
  test("0x0._0", {"number_literal"});
  test("0x0.0_", {"number_literal"});
  test("0x0_p0", {"number_literal"});
  test("0x0_.p0", {"number_literal"});
  test("0x0._p0", {"number_literal"});
  test("0x0.0_p0", {"number_literal"});
  test("0x0._0p0", {"number_literal"});
  test("0x0.0p_0", {"number_literal"});
  test("0x0.0p+_0", {"number_literal"});
  test("0x0.0p-_0", {"number_literal"});
  test("0x0.0p0_", {"number_literal"});
end

do -- "multi line string literal with only 1 backslash"
  test("x \\\n;", { "identifier", "invalid", "semicolon" });
end

do -- "invalid builtin identifiers"
  test("@()", {"invalid"});
  test("@0()", {"invalid"});
end

do -- "invalid token with unfinished escape right before eof"
  test("\"\\", {"invalid"});
  test("'\\", {"invalid"});
  test("'\\u", {"invalid"});
end

do -- "saturating operators" {
  test("<<", {"angle_bracket_angle_bracket_left"});
  test("<<|", {"angle_bracket_angle_bracket_left_pipe"});
  test("<<|=", {"angle_bracket_angle_bracket_left_pipe_equal"});

  test("*", {"asterisk"});
  test("*|", {"asterisk_pipe"});
  test("*|=", {"asterisk_pipe_equal"});

  test("+", {"plus"});
  test("+|", {"plus_pipe"});
  test("+|=", {"plus_pipe_equal"});

  test("-", {"minus"});
  test("-|", {"minus_pipe"});
  test("-|=", {"minus_pipe_equal"});
end

do -- "null byte before eof" {
  test("123 \000 456", { "number_literal", "invalid" });
  test("//\000", {"invalid"});
  test("\\\\\000", {"invalid"});
  test("\000", {"invalid"});
  test("// NUL\000\n", {"invalid"});
  test("///\000\n", { "doc_comment", "invalid" });
  test("/// NUL\000\n", { "doc_comment", "invalid" });
end

do -- "invalid tabs and carriage returns" {
  -- "Inside Line Comments and Documentation Comments, Any TAB is rejected by
  -- the grammar since it is ambiguous how it should be rendered."
  -- https://github.com/ziglang/zig-spec/issues/38
  test("//\t", {"invalid"});
  test("// \t", {"invalid"});
  test("///\t", {"invalid"});
  test("/// \t", {"invalid"});
  test("//!\t", {"invalid"});
  test("//! \t", {"invalid"});

  -- "Inside Line Comments and Documentation Comments, CR directly preceding
  -- NL is unambiguously part of the newline sequence. It is accepted by the
  -- grammar and removed by zig fmt, leaving only NL. CR anywhere else is
  -- rejected by the grammar."
  -- https://github.com/ziglang/zig-spec/issues/38
  test("//\r", {"invalid"});
  test("// \r", {"invalid"});
  test("///\r", {"invalid"});
  test("/// \r", {"invalid"});
  test("//\r ", {"invalid"});
  test("// \r ", {"invalid"});
  test("///\r ", {"invalid"});
  test("/// \r ", {"invalid"});
  test("//\r\n", {});
  test("// \r\n", {});
  test("///\r\n", {"doc_comment"});
  test("/// \r\n", {"doc_comment"});
  test("//!\r", {"invalid"});
  test("//! \r", {"invalid"});
  test("//!\r ", {"invalid"});
  test("//! \r ", {"invalid"});
  test("//!\r\n", {"container_doc_comment"});
  test("//! \r\n", {"container_doc_comment"});

  -- The control characters TAB and CR are rejected by the grammar inside multi-line string literals,
  -- except if CR is directly before NL.
  -- https://github.com/ziglang/zig-spec/issues/38
  test("\\\\\r", {"invalid"});
  test("\\\\\r ", {"invalid"});
  test("\\\\ \r", {"invalid"});
  test("\\\\\t", {"invalid"});
  test("\\\\\t ", {"invalid"});
  test("\\\\ \t", {"invalid"});
  test("\\\\\r\n", {"multiline_string_literal_line"});

  -- "TAB used as whitespace is...accepted by the grammar. CR used as
  -- whitespace, whether directly preceding NL or stray, is...accepted by the
  -- grammar."
  -- https://github.com/ziglang/zig-spec/issues/38
  test("\tpub\tswitch\t", { "keyword_pub", "keyword_switch" });
  test("\rpub\rswitch\r", { "keyword_pub", "keyword_switch" });
end
