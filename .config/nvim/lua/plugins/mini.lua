local function mini(name, opts)
  return {
    "echasnovski/mini." .. name,
    version = false,
    config = function()
      require("mini." .. name).setup(opts)
    end,
  }
end

return {
  mini("align"),
  mini("splitjoin"),
  mini("trailspace"),
}
