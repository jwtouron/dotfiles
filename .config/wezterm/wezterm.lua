local wezterm = require 'wezterm'
local act = wezterm.action

local keys = {
  -- General
  { key = 'd', mods =  'CTRL|SHIFT', action = act.ShowDebugOverlay },
  { key = '|', mods =  'CTRL|SHIFT', action = act.ClearScrollback('ScrollbackOnly') },
  -- { key = 'Colon', mods =  'CTRL|SHIFT', action = act.ActivateCommandPalette },

  -- Tabs
  { key = '{', mods = 'CTRL|SHIFT', action = act.ActivateTabRelative(-1) },
  { key = '}', mods = 'CTRL|SHIFT', action = act.ActivateTabRelative(1) },

  -- Panes
  { key = '"', mods = 'CTRL|SHIFT', action = act.SplitVertical({domain="CurrentPaneDomain"})},
  { key = '%', mods = 'CTRL|SHIFT', action = act.SplitHorizontal({domain="CurrentPaneDomain"})},
  { key = 'R', mods = 'CTRL|SHIFT', action = act.RotatePanes('Clockwise') },
  { key = 'Backspace', mods =  'CTRL|SHIFT', action = act.CloseCurrentPane({ confirm = false }) },
  { key = 'p', mods =  'CTRL|SHIFT', action = act.PaneSelect({ alphabet = "1234567890" }) },
  { key = 'h', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection('Left') },
  { key = 'j', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection('Down') },
  { key = 'k', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection('Up') },
  { key = 'l', mods = 'CTRL|SHIFT', action = act.ActivatePaneDirection('Right') },

  { key = '-', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '0', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '1', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '2', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '3', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '4', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '5', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '6', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '7', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '8', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '9', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '=', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '[', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
  { key = ']', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
  { key = 'c', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 'f', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 'k', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 'm', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 'n', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 'r', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 't', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 'v', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = 'w', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '{', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '{', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
  { key = '}', mods = 'SUPER',       action = act.DisableDefaultAssignment },
  { key = '}', mods = 'SHIFT|SUPER', action = act.DisableDefaultAssignment },
}

return {
  adjust_window_size_when_changing_font_size = false,
  background = {
    {
      source = { Color = "black" },
      opacity = 0.9,
      height = '100%',
      width = '100%',
    }
  },
  font_size = 11,
  font = wezterm.font('monospace'),
  hide_tab_bar_if_only_one_tab = true,
  keys = keys,
  use_fancy_tab_bar = false,
}
