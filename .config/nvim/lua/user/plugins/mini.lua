local default_spec = { event = "VeryLazy", config = true, }

local function mini(name, spec)
  local ret = {
    'echasnovski/mini.'..name,
    version = false,
  }
  for k, v in pairs(spec or default_spec) do
    ret[k] = v
  end
  return ret
end

return {
  mini("ai"),
  mini("align"),
  mini("bracketed"),
  mini("bufremove"),
  mini("comment"),
  mini("jump"),
  mini("move", {
    event = "VeryLazy",
    opts = { mappings = { line_left = '', line_right = '', } }
  }),
  mini("splitjoin"),
  mini("trailspace"),
}
