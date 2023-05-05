local function f(func)
  local Mark = nil
  local UI = nil
  return function()
    if not Mark then
        local mark = require("harpoon.mark")
        local ui = require("harpoon.ui")
        Mark = mark
        UI = ui
    end
    return func(Mark, UI)
  end
end

return {
  "ThePrimeagen/harpoon",
  keys = {
    { "<M-a>", f(function(mark, _) mark.add_file() end) },
    { "<M-m>", f(function(_, ui) ui.toggle_quick_menu() end) },
    { "<M-l>", f(function(_, ui) ui.nav_next() end) },
    { "<M-h>", f(function(_, ui) ui.nav_prev() end) },
    { "<M-1>", f(function(_, ui) ui.nav_file(1) end) },
    { "<M-2>", f(function(_, ui) ui.nav_file(2) end) },
    { "<M-3>", f(function(_, ui) ui.nav_file(3) end) },
    { "<M-4>", f(function(_, ui) ui.nav_file(4) end) },
    { "<M-5>", f(function(_, ui) ui.nav_file(5) end) },
    { "<M-6>", f(function(_, ui) ui.nav_file(6) end) },
  },
}
