local Enum = {
  safety = true,
}

function Enum.fromTable(tbl, safety)
  local effective_safety = safety or Enum.safety
  if not effective_safety then return tbl end

  return setmetatable({}, {
    __index = function(_, k)
      local v = tbl[k]
      if v ~= nil then return v end
      error("invalid key: "..tostring(k))
    end,
    __newindex = function(_, k, v) error("cannot modify enum") end,
    __metatable = false,
  })
end

function Enum.fromArray(arr, safety)
  local tbl = {}
  for i, v in ipairs(arr) do
    tbl[v] = i
  end
  return Enum.fromTable(tbl, safety)
end

return Enum
