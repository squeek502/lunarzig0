local keywords_list = {
  "addrspace",
  "align",
  "allowzero",
  "and",
  "anyframe",
  "anytype",
  "asm",
  "async",
  "await",
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

-- State functions:
-- On token emission. return: nil
-- On state change/continue, return: state
local states = {}

states.start = function(self, c, result, current_state)
  if c == 0 then
    if self.index == #self.buffer then
      result.tag = "eof"
      result.loc = {
        start = self.index,
        ["end"] = self.index,
      }
      result.finished = true
      return
    end
    return states.invalid
  elseif isWhitespace(c) then
    result.loc.start = self.index + 1
  elseif c == ('"'):byte() then
    result.tag = "string_literal"
    return states.string_literal
  elseif c == ("'"):byte() then
    result.tag = "char_literal"
    return states.char_literal
  elseif isIdentifierStart(c) then
    result.tag = "identifier"
    return states.identifier
  elseif c == ('@'):byte() then
    return states.saw_at_sign
  elseif c == ('='):byte() then
    return states.equal
  elseif c == ('!'):byte() then
    return states.bang
  elseif c == ('|'):byte() then
    return states.pipe
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
    return states.percent
  elseif c == ('*'):byte() then
    return states.asterisk
  elseif c == ('+'):byte() then
    return states.plus
  elseif c == ('<'):byte() then
    return states.angle_bracket_left
  elseif c == ('>'):byte() then
    return states.angle_bracket_right
  elseif c == ('^'):byte() then
    return states.caret
  elseif c == ('\\'):byte() then
    result.tag = "multiline_string_literal_line"
    return states.backslash
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
    return states.period
  elseif c == ('-'):byte() then
    return states.minus
  elseif c == ('/'):byte() then
    return states.slash
  elseif c == ('&'):byte() then
    return states.ampersand
  elseif isDigit(c) then
    result.tag = "number_literal"
    return states.int
  else
    return states.invalid
  end

  return current_state
end

states.expect_newline = function(self, c, result, current_state)
  if c == 0 then
    if self.index == #self.buffer then
      result.tag = "invalid"
      return
    end
    return states.invalid
  elseif c == ('\n'):byte() then
    result.loc.start = self.index + 1
    return states.start
  else
    return states.invalid
  end
end

states.invalid = function(self, c, result, current_state)
  if c == 0 then
    if self.index == #self.buffer then
      result.tag = "invalid"
      return
    end
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  end
  return current_state
end

states.saw_at_sign = function(self, c, result, current_state)
  if c == 0 or c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif c == ('"'):byte() then
    result.tag = "identifier"
    return states.string_literal
  elseif isIdentifierStart(c) then
    result.tag = "builtin"
    return states.builtin
  else
    return states.invalid
  end
end

states.ampersand = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "ampersand_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "ampersand"
    return
  end
end

states.asterisk = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "asterisk_equal"
    self.index = self.index + 1
    return
  elseif c == ('*'):byte() then
    result.tag = "asterisk_asterisk"
    self.index = self.index + 1
    return
  elseif c == ('%'):byte() then
    return states.asterisk_percent
  elseif c == ('|'):byte() then
    return states.asterisk_pipe
  else
    result.tag = "asterisk"
    return
  end
end

states.asterisk_percent = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "asterisk_percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "asterisk_percent"
    return
  end
end

states.asterisk_pipe = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "asterisk_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "asterisk_pipe"
    return
  end
end

states.percent = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "percent"
    return
  end
end

states.plus = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "plus_equal"
    self.index = self.index + 1
    return
  elseif c == ('+'):byte() then
    result.tag = "plus_plus"
    self.index = self.index + 1
    return
  elseif c == ('%'):byte() then
    return states.plus_percent
  elseif c == ('|'):byte() then
    return states.plus_pipe
  else
    result.tag = "plus"
    return
  end
end

states.plus_percent = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "plus_percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "plus_percent"
    return
  end
end

states.plus_pipe = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "plus_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "plus_pipe"
    return
  end
end

states.caret = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "caret_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "caret"
    return
  end
end

states.identifier = function(self, c, result, current_state)
  if isIdentifierContinuation(c) then
    return current_state
  else
    local slice = self.buffer:sub(result.loc.start, self.index - 1)
    local keyword_tag = keywords[slice]
    if keyword_tag then
      result.tag = keyword_tag
    end
    return
  end
end

states.builtin =  function(self, c, result, current_state)
  if isIdentifierContinuation(c) then
    return current_state
  else
    return
  end
end

states.backslash = function(self, c, result, current_state)
  if c == 0 then
    result.tag = "invalid"
    return
  elseif c == ('\\'):byte() then
    return states.multiline_string_literal_line
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  else
    return states.invalid
  end
end

states.string_literal = function(self, c, result, current_state)
  if c == 0 then
    if self.index ~= #self.buffer then
      return states.invalid
    end
    result.tag = "invalid"
    return
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif c == ('\\'):byte() then
    return states.string_literal_backslash
  elseif c == ('"'):byte() then
    self.index = self.index + 1
    return
  elseif isControl(c) then
    return states.invalid
  else
    return current_state
  end
end

states.string_literal_backslash = function(self, c, result, current_state)
  if c == 0 or c == ('\n'):byte() then
    result.tag = "invalid"
    return
  else
    return states.string_literal
  end
end

states.char_literal = function(self, c, result, current_state)
  if c == 0 then
    if self.index ~= #self.buffer then
      return states.invalid
    end
    result.tag = "invalid"
    return
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif c == ('\\'):byte() then
    return states.char_literal_backslash
  elseif c == ("'"):byte() then
    self.index = self.index + 1
    return
  elseif isControl(c) then
    return states.invalid
  else
    return current_state
  end
end

states.char_literal_backslash = function(self, c, result, current_state)
  if c == 0 then
    if self.index ~= #self.buffer then
      return states.invalid
    end
    result.tag = "invalid"
    return
  elseif c == ('\n'):byte() then
    result.tag = "invalid"
    return
  elseif isControl(c) then
    return states.invalid
  else
    return states.char_literal
  end
end

states.multiline_string_literal_line = function(self, c, result, current_state)
  if c == 0 then
    if self.index ~= #self.buffer then
      return states.invalid
    end
    return
  elseif c == ('\n'):byte() then
    self.index = self.index + 1
    return
  elseif c == ('\r'):byte() then
    if self.buffer:byte(self.index + 1) == ('\n'):byte() then
      self.index = self.index + 2
      return
    else
      return states.invalid
    end
  elseif isControl(c) then
    return states.invalid
  else
    return current_state
  end
end

states.bang = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "bang_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "bang"
    return
  end
end

states.pipe = function(self, c, result, current_state)
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

states.equal = function(self, c, result, current_state)
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

states.minus = function(self, c, result, current_state)
  if c == ('>'):byte() then
    result.tag = "arrow"
    self.index = self.index + 1
    return
  elseif c == ('='):byte() then
    result.tag = "minus_equal"
    self.index = self.index + 1
    return
  elseif c == ('%'):byte() then
    return states.minus_percent
  elseif c == ('|'):byte() then
    return states.minus_pipe
  else
    result.tag = "minus"
    return
  end
end

states.minus_percent = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "minus_percent_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "minus_percent"
    return
  end
end

states.minus_pipe = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "minus_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "minus_pipe"
    return
  end
end

states.angle_bracket_left = function(self, c, result, current_state)
  if c == ('<'):byte() then
    return states.angle_bracket_angle_bracket_left
  elseif c == ('='):byte() then
    result.tag = "angle_bracket_left_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_left"
    return
  end
end

states.angle_bracket_angle_bracket_left = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "angle_bracket_angle_bracket_left_equal"
    self.index = self.index + 1
    return
  elseif c == ('|'):byte() then
    return states.angle_bracket_angle_bracket_left_pipe
  else
    result.tag = "angle_bracket_angle_bracket_left"
    return
  end
end

states.angle_bracket_angle_bracket_left_pipe = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "angle_bracket_angle_bracket_left_pipe_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_angle_bracket_left_pipe"
    return
  end
end

states.angle_bracket_right = function(self, c, result, current_state)
  if c == ('>'):byte() then
    return states.angle_bracket_angle_bracket_right
  elseif c == ('='):byte() then
    result.tag = "angle_bracket_right_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_right"
    return
  end
end

states.angle_bracket_angle_bracket_right = function(self, c, result, current_state)
  if c == ('='):byte() then
    result.tag = "angle_bracket_angle_bracket_right_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "angle_bracket_angle_bracket_right"
    return
  end
end

states.period = function(self, c, result, current_state)
  if c == ('.'):byte() then
    return states.period_2
  elseif c == ('*'):byte() then
    return states.period_asterisk
  else
    result.tag = "period"
    return
  end
end

states.period_2 = function(self, c, result, current_state)
  if c == ('.'):byte() then
    result.tag = "ellipsis3"
    self.index = self.index + 1
    return
  else
    result.tag = "ellipsis2"
    return
  end
end

states.period_asterisk = function(self, c, result, current_state)
  if c == ('*'):byte() then
    result.tag = "invalid_periodasterisks"
    return
  else
    result.tag = "period_asterisk"
    return
  end
end

states.slash = function(self, c, result, current_state)
  if c == ('/'):byte() then
    return states.line_comment_start
  elseif c == ('='):byte() then
    result.tag = "slash_equal"
    self.index = self.index + 1
    return
  else
    result.tag = "slash"
    return
  end
end

states.line_comment_start = function(self, c, result, current_state)
  if c == 0 then
    if self.index ~= #self.buffer then
      return states.invalid
    end
    result.tag = "eof"
    result.loc = {
      start = self.index,
      ["end"] = self.index,
    }
    result.finished = true
    return
  elseif c == ('/'):byte() then
    return states.doc_comment_start
  elseif c == ('!'):byte() then
    result.tag = "container_doc_comment"
    return states.doc_comment
  elseif c == ('\r'):byte() then
    return states.expect_newline
  elseif c == ('\n'):byte() then
    result.loc.start = self.index + 1
    return states.start
  elseif isControl(c) then
    return states.invalid
  else
    return states.line_comment
  end
end

states.doc_comment_start = function(self, c, result, current_state)
  if c == 0 or c == ('\n'):byte() then
    result.tag = "doc_comment"
    return
  elseif c == ('\r'):byte() then
    if self.buffer:byte(self.index + 1) == ('\n'):byte() then
      self.index = self.index + 1
      result.tag = "doc_comment"
      return
    else
      return states.invalid
    end
  elseif c == ('/'):byte() then
    return states.line_comment
  elseif isControl(c) then
    return states.invalid
  else
    result.tag = "doc_comment"
    return states.doc_comment
  end
end

states.line_comment = function(self, c, result, current_state)
  if c == 0 then
    if self.index ~= #self.buffer then
      return states.invalid
    end
    result.tag = "eof"
    result.loc = {
      start = self.index,
      ["end"] = self.index,
    }
    result.finished = true
    return
  elseif c == ('\r'):byte() then
    return states.expect_newline
  elseif c == ('\n'):byte() then
    result.loc.start = self.index + 1
    return states.start
  elseif isControl(c) then
    return states.invalid
  else
    return current_state
  end
end

states.doc_comment = function(self, c, result, current_state)
  if c == 0 or c == ('\n'):byte() then
    return
  elseif c == ('\r'):byte() then
    if self.buffer:byte(self.index + 1) == ('\n'):byte() then
      self.index = self.index + 1
      return
    else
      return states.invalid
    end
  elseif isControl(c) then
    return states.invalid
  else
    return current_state
  end
end

states.int = function(self, c, result, current_state)
  if c == ('.'):byte() then
    return states.int_period
  elseif isNumberContinuation(c) then
    return current_state
  elseif c == ('e'):byte() or c == ('E'):byte() or c == ('p'):byte() or c == ('P'):byte() then
    return states.int_exponent
  else
    return
  end
end

states.int_exponent = function(self, c, result, current_state)
  if c == ('-'):byte() or c == ('+'):byte() then
    return states.float
  else
    self.index = self.index - 1
    return states.int
  end
end

states.int_period = function(self, c, result, current_state)
  if isNumberContinuation(c) then
    return states.float
  elseif c == ('e'):byte() or c == ('E'):byte() or c == ('p'):byte() or c == ('P'):byte() then
    return states.float_exponent
  else
    self.index = self.index - 1
    return
  end
end

states.float = function(self, c, result, current_state)
  if isNumberContinuation(c) then
    return current_state
  elseif c == ('e'):byte() or c == ('E'):byte() or c == ('p'):byte() or c == ('P'):byte() then
    return states.float_exponent
  else
    return
  end
end

states.float_exponent = function(self, c, result, current_state)
  if c == ('-'):byte() or c == ('+'):byte() then
    return states.float
  else
    self.index = self.index - 1
    return states.float
  end
end

local function getStateName(state)
  for name, fn in pairs(states) do
    if fn == state then
      return name
    end
  end
end

function Tokenizer:next()
  local state = states.start
  local result = {
    tag = nil,
    loc = {
      start = self.index,
      ["end"] = nil,
    },
    -- If set to true, then loc.end is not set automatically.
    -- Before returning from `next`, this key is removed.
    finished = false,
  }
  while state do
    local c = self.buffer:byte(self.index)
    state = state(self, c, result, state)
    if not state then
      if not result.finished then
        result.loc["end"] = self.index - 1
      end
      break
    end
    self.index = self.index + 1
  end
  result.finished = nil
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

return {
  Tokenizer = Tokenizer,
  getTokens = getTokens,
}