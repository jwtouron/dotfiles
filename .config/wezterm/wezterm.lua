---@type table<any, any>
local config = {
  adjust_window_size_when_changing_font_size = false,

  disable_default_key_bindings = true,

  hide_tab_bar_if_only_one_tab = true,

  use_fancy_tab_bar = false,

  warn_about_missing_glyphs = false,

  window_padding = {
    left = 0, right = 0, top = 0, bottom = 0
  },
}

local keys = require 'keys'
config.keys = keys.keys
config.key_tables = keys.key_tables

local ok, custom = pcall(require, "custom")
if ok then
  for k, v in pairs(custom or {}) do
    config[k] = v
  end
end

return config

