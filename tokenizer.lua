local keywords_list = {
  "addrspace",
  "align",
  "allowzero",
  "and",
  "anyframe",
  "anytype",
  "asm",
  "break",
  "callconv",
  "catch",
  "comptime",
  "const",
  "continue",
  "defer",
  "else",
  "enum",
  "errdefer",
  "error",
  "export",
  "extern",
  "fn",
  "for",
  "if",
  "inline",
  "noalias",
  "noinline",
  "nosuspend",
  "opaque",
  "or",
  "orelse",
  "packed",
  "pub",
  "resume",
  "return",
  "linksection",
  "struct",
  "suspend",
  "switch",
  "test",
  "threadlocal",
  "try",
  "union",
  "unreachable",
  "usingnamespace",
  "var",
  "volatile",
  "while",
}
local keywords = {}
for i, keyword in ipairs(keywords_list) do
  keywords[keyword] = "keyword_" .. keyword
end

local Tokenizer = {}
Tokenizer.__index = Tokenizer

-- buffer must be NUL-terminated
function Tokenizer.init(buffer)
  assert(buffer:byte(#buffer) == 0)
  local self = setmetatable({}, Tokenizer)
  self.buffer = buffer
  self.index = 1
  -- Skip the UTF-8 BOM if present.
  if self.buffer:sub(1, 3) == string.char(0xEF, 0xBB, 0xBF) then
    self.index = self.index + 3
  end
  return self
end

local function isWhitespace(c)
  return c == (' '):byte() or c == ('\n'):byte() or c == ('\t'):byte() or c == ('\r'):byte()
end

local function isIdentifierStart(c)
  return (c >= ('a'):byte() and c <= ('z'):byte()) or (c >= ('A'):byte() and c <= ('Z'):byte()) or c == ('_'):byte()
end

local function isDigit(c)
  return (c >= ('0'):byte() and c <= ('9'):byte())
end

local function isIdentifierContinuation(c)
  return isIdentifierStart(c) or isDigit(c)
end

local function isControl(c)
  return (c >= 0x01 and c <= 0x09) or (c >= 0x0B and c <= 0x0C) or (c >= 0x0E and c <= 0x1F) or c == 0x7F
end

local function isNumberContinuation(c)
  return isDigit(c) or c == ('_'):byte() or
    (c >= ('a'):byte() and c <= ('d'):byte()) or (c >= ('A'):byte() and c <= ('D'):byte()) or
    (c >= ('f'):byte() and c <= ('o'):byte()) or (c >= ('F'):byte() and c <= ('O'):byte()) or
    (c >= ('q'):byte() and c <= ('z'):byte()) or (c >= ('Q'):byte() and c <= ('Z'):byte())
end

function Tokenizer:start(result)
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index == #self.buffer then
      result.tag = "eof"
      result.start = self.index
      result["end"] = self.index
      return result
    end
    return self:invalid(result)
  elseif isWhitespace(c) then
    self.index = self.index + 1
    result.start = self.index
    return self:start(result)
  elseif c == ('"'):byte() then
    result.tag = "string_literal"
    return self:string_literal(result)
  elseif c == ("'"):byte() then
    result.tag = "char_literal"
    return self:char_literal(result)
  elseif isIdentifierStart(c) then
    result.tag = "identifier"
    return self:identifier(result)
  elseif c == ('@'):byte() then
    return self:saw_at_sign(result)
  elseif c == ('='):byte() then
    return self:equal(result)
  elseif c == ('!'):byte() then
    return self:bang(result)
  elseif c == ('|'):byte() then
    return self:pipe(result)
  elseif c == ('('):byte() then
    result.tag = "l_paren"
    self.index = self.index + 1
    return
  elseif c == (')'):byte() then
    result.tag = "r_paren"
    self.index = self.index + 1
    return
  elseif c == ('['):byte() then
    result.tag = "l_bracket"
    self.index = self.index + 1
    return
  elseif c == (']'):byte() then
    result.tag = "r_bracket"
    self.index = self.index + 1
    return
  elseif c == (';'):byte() then
    result.tag = "semicolon"
    self.index = self.index + 1
    return
  elseif c == (','):byte() then
    result.tag = "comma"
    self.index = self.index + 1
    return
  elseif c == ('?'):byte() then
    result.tag = "question_mark"
    self.index = self.index + 1
    return
  elseif c == (':'):byte() then
    result.tag = "colon"
    self.index = self.index + 1
    return
  elseif c == ('%'):byte() then
    return self:percent(result)
  elseif c == ('*'):byte() then
    return self:asterisk(result)
  elseif c == ('+'):byte() then
    return self:plus(result)
  elseif c == ('<'):byte() then
    return self:angle_bracket_left(result)
  elseif c == ('>'):byte() then
    return self:angle_bracket_right(result)
  elseif c == ('^'):byte() then
    return self:caret(result)
  elseif c == ('\\'):byte() then
    result.tag = "multiline_string_literal_line"
    return self:backslash(result)
  elseif c == ('{'):byte() then
    result.tag = "l_brace"
    self.index = self.index + 1
    return
  elseif c == ('}'):byte() then
    result.tag = "r_brace"
    self.index = self.index + 1
    return
  elseif c == ('~'):byte() then
    result.tag = "tilde"
    self.index = self.index + 1
    return
  elseif c == ('.'):byte() then
    return self:period(result)
  elseif c == ('-'):byte() then
    return self:minus(result)
  elseif c == ('/'):byte() then
    return self:slash(result)
  elseif c == ('&'):byte() then
    return self:ampersand(result)
  elseif isDigit(c) then
    result.tag = "number_literal"
    self.index = self.index + 1
    return self:int(result)
  else
    return self:invalid(result)
  end
end

function Tokenizer:expect_newline(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index == #self.buffer then
      result.tag = "invalid"
      return
    end
    return self:invalid(result)
  elseif c == ('\n'):byte() then
    self.index = self.index + 1
    result.start = self.index
    return self:start(result)
  else
    return self:invalid(result)
  end
end

function Tokenizer:invalid(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index == #self.buffer then
      result.tag = "invalid"
      return
    end
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  end
  return self:invalid(result)
end

function Tokenizer:saw_at_sign(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 or c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif c == ('"'):byte() then
    result.tag = "identifier"
    return self:string_literal(result)
  elseif isIdentifierStart(c) then
    result.tag = "builtin"
    return self:builtin(result)
  else
    return self:invalid(result)
  end
end

function Tokenizer:ampersand(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "ampersand_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "ampersand"
    return
  end
end

function Tokenizer:asterisk(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "asterisk_equal"
    self.index = self.index + 1
    return
  elseif c == ('*'):byte() then
    result.tag = "asterisk_asterisk"
    self.index = self.index + 1
    return
  elseif c == ('%'):byte() then
    return self:asterisk_percent(result)
  elseif c == ('|'):byte() then
    return self:asterisk_pipe(result)
  else
    result.tag = "asterisk"
    return
  end
end

function Tokenizer:asterisk_percent(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "asterisk_percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "asterisk_percent"
    return
  end
end

function Tokenizer:asterisk_pipe(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "asterisk_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "asterisk_pipe"
    return
  end
end

function Tokenizer:percent(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "percent"
    return
  end
end

function Tokenizer:plus(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "plus_equal"
    self.index = self.index + 1
    return
  elseif c == ('+'):byte() then
    result.tag = "plus_plus"
    self.index = self.index + 1
    return
  elseif c == ('%'):byte() then
    return self:plus_percent(result)
  elseif c == ('|'):byte() then
    return self:plus_pipe(result)
  else
    result.tag = "plus"
    return
  end
end

function Tokenizer:plus_percent(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "plus_percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "plus_percent"
    return
  end
end

function Tokenizer:plus_pipe(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "plus_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "plus_pipe"
    return
  end
end

function Tokenizer:caret(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "caret_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "caret"
    return
  end
end

function Tokenizer:identifier(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if isIdentifierContinuation(c) then
    return self:identifier(result)
  else
    local slice = self.buffer:sub(result.start, self.index - 1)
    local keyword_tag = keywords[slice]
    if keyword_tag then
      result.tag = keyword_tag
    end
    return
  end
end

function Tokenizer:builtin(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if isIdentifierContinuation(c) then
    return self:builtin(result)
  else
    return
  end
end

function Tokenizer:backslash(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    result.tag = "invalid"
    return
  elseif c == ('\\'):byte() then
    return self:multiline_string_literal_line(result)
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  else
    return self:invalid(result)
  end
end

function Tokenizer:string_literal(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index ~= #self.buffer then
      return self:invalid(result)
    end
    result.tag = "invalid"
    return
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif c == ('\\'):byte() then
    return self:string_literal_backslash(result)
  elseif c == ('"'):byte() then
    self.index = self.index + 1
    return
  elseif isControl(c) then
    return self:invalid(result)
  else
    return self:string_literal(result)
  end
end

function Tokenizer:string_literal_backslash(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 or c == ('\n'):byte() then
    result.tag = "invalid"
    return
  else
    return self:string_literal(result)
  end
end

function Tokenizer:char_literal(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index ~= #self.buffer then
      return self:invalid(result)
    end
    result.tag = "invalid"
    return
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif c == ('\\'):byte() then
    return self:char_literal_backslash(result)
  elseif c == ("'"):byte() then
    self.index = self.index + 1
    return
  elseif isControl(c) then
    return self:invalid(result)
  else
    return self:char_literal(result)
  end
end

function Tokenizer:char_literal_backslash(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index ~= #self.buffer then
      return self:invalid(result)
    end
    result.tag = "invalid"
    return
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif isControl(c) then
    return self:invalid(result)
  else
    return self:char_literal(result)
  end
end

function Tokenizer:multiline_string_literal_line(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index ~= #self.buffer then
      return self:invalid(result)
    end
    return
  elseif c == ('\n'):byte() then
    return
  elseif c == ('\r'):byte() then
    if self.buffer:byte(self.index + 1) == ('\n'):byte() then
      return
    else
      return self:invalid(result)
    end
  elseif isControl(c) then
    return self:invalid(result)
  else
    return self:multiline_string_literal_line(result)
  end
end

function Tokenizer:bang(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "bang_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "bang"
    return
  end
end

function Tokenizer:pipe(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "pipe_equal"
    self.index = self.index + 1
    return
  elseif c== ('|'):byte() then
    result.tag = "pipe_pipe"
    self.index = self.index + 1
    return
  else
    result.tag = "pipe"
    return
  end
end

function Tokenizer:equal(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "equal_equal"
    self.index = self.index + 1
    return
  elseif c == ('>'):byte() then
    result.tag = "equal_angle_bracket_right"
    self.index = self.index + 1
    return
  else
    result.tag = "equal"
    return
  end
end

function Tokenizer:minus(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('>'):byte() then
    result.tag = "arrow"
    self.index = self.index + 1
    return
  elseif c == ('='):byte() then
    result.tag = "minus_equal"
    self.index = self.index + 1
    return
  elseif c == ('%'):byte() then
    return self:minus_percent(result)
  elseif c == ('|'):byte() then
    return self:minus_pipe(result)
  else
    result.tag = "minus"
    return
  end
end

function Tokenizer:minus_percent(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "minus_percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "minus_percent"
    return
  end
end

function Tokenizer:minus_pipe(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "minus_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "minus_pipe"
    return
  end
end

function Tokenizer:angle_bracket_left(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('<'):byte() then
    return self:angle_bracket_angle_bracket_left(result)
  elseif c == ('='):byte() then
    result.tag = "angle_bracket_left_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_left"
    return
  end
end

function Tokenizer:angle_bracket_angle_bracket_left(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "angle_bracket_angle_bracket_left_equal"
    self.index = self.index + 1
    return
  elseif c == ('|'):byte() then
    return self:angle_bracket_angle_bracket_left_pipe(result)
  else
    result.tag = "angle_bracket_angle_bracket_left"
    return
  end
end

function Tokenizer:angle_bracket_angle_bracket_left_pipe(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "angle_bracket_angle_bracket_left_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_angle_bracket_left_pipe"
    return
  end
end

function Tokenizer:angle_bracket_right(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('>'):byte() then
    return self:angle_bracket_angle_bracket_right(result)
  elseif c == ('='):byte() then
    result.tag = "angle_bracket_right_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_right"
    return
  end
end

function Tokenizer:angle_bracket_angle_bracket_right(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('='):byte() then
    result.tag = "angle_bracket_angle_bracket_right_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_angle_bracket_right"
    return
  end
end

function Tokenizer:period(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('.'):byte() then
    return self:period_2(result)
  elseif c == ('*'):byte() then
    return self:period_asterisk(result)
  else
    result.tag = "period"
    return
  end
end

function Tokenizer:period_2(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('.'):byte() then
    result.tag = "ellipsis3"
    self.index = self.index + 1
    return
  else
    result.tag = "ellipsis2"
    return
  end
end

function Tokenizer:period_asterisk(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('*'):byte() then
    result.tag = "invalid_periodasterisks"
    return
  else
    result.tag = "period_asterisk"
    return
  end
end

function Tokenizer:slash(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('/'):byte() then
    return self:line_comment_start(result)
  elseif c == ('='):byte() then
    result.tag = "slash_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "slash"
    return
  end
end

function Tokenizer:line_comment_start(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index ~= #self.buffer then
      return self:invalid(result)
    end
    result.tag = "eof"
    result.start = self.index
    result["end"] = self.index
    return
  elseif c == ('/'):byte() then
    return self:doc_comment_start(result)
  elseif c == ('!'):byte() then
    result.tag = "container_doc_comment"
    return self:doc_comment(result)
  elseif c == ('\r'):byte() then
    return self:expect_newline(result)
  elseif c == ('\n'):byte() then
    self.index = self.index + 1
    result.start = self.index
    return self:start(result)
  elseif isControl(c) then
    return self:invalid(result)
  else
    return self:line_comment(result)
  end
end

function Tokenizer:doc_comment_start(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 or c == ('\n'):byte() then
    result.tag = "doc_comment"
    return
  elseif c == ('\r'):byte() then
    if self.buffer:byte(self.index + 1) == ('\n'):byte() then
      result.tag = "doc_comment"
      return
    else
      return self:invalid(result)
    end
  elseif c == ('/'):byte() then
    return self:line_comment(result)
  elseif isControl(c) then
    return self:invalid(result)
  else
    result.tag = "doc_comment"
    return self:doc_comment(result)
  end
end

function Tokenizer:line_comment(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 then
    if self.index ~= #self.buffer then
      return self:invalid(result)
    end
    result.tag = "eof"
    result.start = self.index
    result["end"] = self.index
    return result
  elseif c == ('\r'):byte() then
    return self:expect_newline(result)
  elseif c == ('\n'):byte() then
    self.index = self.index + 1
    result.start = self.index
    return self:start(result)
  elseif isControl(c) then
    return self:invalid(result)
  else
    return self:line_comment(result)
  end
end

function Tokenizer:doc_comment(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == 0 or c == ('\n'):byte() then
    return
  elseif c == ('\r'):byte() then
    if self.buffer:byte(self.index + 1) == ('\n'):byte() then
      return
    else
      return self:invalid(result)
    end
  elseif isControl(c) then
    return self:invalid(result)
  else
    return self:doc_comment(result)
  end
end

function Tokenizer:int(result)
  local c = self.buffer:byte(self.index)
  if c == ('.'):byte() then
    return self:int_period(result)
  elseif isNumberContinuation(c) then
    self.index = self.index + 1
    return self:int(result)
  elseif c == ('e'):byte() or c == ('E'):byte() or c == ('p'):byte() or c == ('P'):byte() then
    return self:int_exponent(result)
  else
    return
  end
end

function Tokenizer:int_exponent(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('-'):byte() or c == ('+'):byte() then
    self.index = self.index + 1
    return self:float(result)
  else
    return self:int(result)
  end
end

function Tokenizer:int_period(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if isNumberContinuation(c) then
    return self:float(result)
  elseif c == ('e'):byte() or c == ('E'):byte() or c == ('p'):byte() or c == ('P'):byte() then
    return self:float_exponent(result)
  else
    self.index = self.index - 1
    return
  end
end

function Tokenizer:float(result)
  local c = self.buffer:byte(self.index)
  if isNumberContinuation(c) then
    self.index = self.index + 1
    return self:float(result)
  elseif c == ('e'):byte() or c == ('E'):byte() or c == ('p'):byte() or c == ('P'):byte() then
    return self:float_exponent(result)
  else
    return
  end
end

function Tokenizer:float_exponent(result)
  self.index = self.index + 1
  local c = self.buffer:byte(self.index)
  if c == ('-'):byte() or c == ('+'):byte() then
    self.index = self.index + 1
    return self:float(result)
  else
    return self:float(result)
  end
end

function Tokenizer:next(result)
  result = result or {}
  result.start = self.index
  local tok = self:start(result)
  if tok then
    return tok
  end
  result["end"] = self.index - 1
  return result
end

-- if `source` does not have a NUL-terminator, then one is appended
local function getTokens(source)
  if source:byte(#source) ~= 0 then
    source = source .. '\000'
  end
  local tokenizer = Tokenizer.init(source)
  local tokens = {}
  while true do
    local tok = tokenizer:next()
    table.insert(tokens, tok)
    if tok.tag == "eof" then break end
  end
  return tokens
end

local lexemes = {
  invalid = false,
  identifier = false,
  string_literal = false,
  multiline_string_literal_line = false,
  char_literal = false,
  eof = false,
  builtin = false,
  number_literal = false,
  doc_comment = false,
  container_doc_comment = false,

  invalid_periodasterisks = ".**",
  bang = "!",
  pipe = "|",
  pipe_pipe = "||",
  pipe_equal = "|=",
  equal = "=",
  equal_equal = "==",
  equal_angle_bracket_right = "=",
  bang_equal = "!=",
  l_paren = "(",
  r_paren = ")",
  semicolon = ";",
  percent = "%",
  percent_equal = "%=",
  l_brace = "{",
  r_brace = "}",
  l_bracket = "[",
  r_bracket = "]",
  period = ".",
  period_asterisk = ".*",
  ellipsis2 = "..",
  ellipsis3 = "...",
  caret = "^",
  caret_equal = "^=",
  plus = "+",
  plus_plus = "++",
  plus_equal = "+=",
  plus_percent = "+%",
  plus_percent_equal = "+%=",
  plus_pipe = "+|",
  plus_pipe_equal = "+|=",
  minus = "-",
  minus_equal = "-=",
  minus_percent = "-%",
  minus_percent_equal = "-%=",
  minus_pipe = "-|",
  minus_pipe_equal = "-|=",
  asterisk = "*",
  asterisk_equal = "*=",
  asterisk_asterisk = "**",
  asterisk_percent = "*%",
  asterisk_percent_equal = "*%=",
  asterisk_pipe = "*|",
  asterisk_pipe_equal = "*|=",
  arrow = "->",
  colon = ":",
  slash = "/",
  slash_equal = "/=",
  comma = ",",
  ampersand = "&",
  ampersand_equal = "&=",
  question_mark = "?",
  angle_bracket_left = "<",
  angle_bracket_left_equal = "<=",
  angle_bracket_angle_bracket_left = "<<",
  angle_bracket_angle_bracket_left_equal = "<<=",
  angle_bracket_angle_bracket_left_pipe = "<<|",
  angle_bracket_angle_bracket_left_pipe_equal = "<<|=",
  angle_bracket_right = ">",
  angle_bracket_right_equal = ">=",
  angle_bracket_angle_bracket_right = ">>",
  angle_bracket_angle_bracket_right_equal = ">>=",
  tilde = "~",
  keyword_addrspace = "addrspace",
  keyword_align = "align",
  keyword_allowzero = "allowzero",
  keyword_and = "and",
  keyword_anyframe = "anyframe",
  keyword_anytype = "anytype",
  keyword_asm = "asm",
  keyword_break = "break",
  keyword_callconv = "callconv",
  keyword_catch = "catch",
  keyword_comptime = "comptime",
  keyword_const = "const",
  keyword_continue = "continue",
  keyword_defer = "defer",
  keyword_else = "else",
  keyword_enum = "enum",
  keyword_errdefer = "errdefer",
  keyword_error = "error",
  keyword_export = "export",
  keyword_extern = "extern",
  keyword_fn = "fn",
  keyword_for = "for",
  keyword_if = "if",
  keyword_inline = "inline",
  keyword_noalias = "noalias",
  keyword_noinline = "noinline",
  keyword_nosuspend = "nosuspend",
  keyword_opaque = "opaque",
  keyword_or = "or",
  keyword_orelse = "orelse",
  keyword_packed = "packed",
  keyword_pub = "pub",
  keyword_resume = "resume",
  keyword_return = "return",
  keyword_linksection = "linksection",
  keyword_struct = "struct",
  keyword_suspend = "suspend",
  keyword_switch = "switch",
  keyword_test = "test",
  keyword_threadlocal = "threadlocal",
  keyword_try = "try",
  keyword_union = "union",
  keyword_unreachable = "unreachable",
  keyword_var = "var",
  keyword_volatile = "volatile",
  keyword_while = "while",
}

return {
  Tokenizer = Tokenizer,
  getTokens = getTokens,
  lexemes = lexemes,
  isWhitespace = isWhitespace,
}