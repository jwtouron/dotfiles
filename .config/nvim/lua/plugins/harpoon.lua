local function desc(s)
  return "Harpoon: "..s
end

local keys = {
  { "<M-a>", function(mark, _)  mark.add_file() end,     desc("Add File") },
  { "<M-m>", function(_, ui) ui.toggle_quick_menu() end, desc("Quick Menu") },
  { "<M-l>", function(_, ui) ui.nav_next() end,          desc("Nav Next") },
  { "<M-h>", function(_, ui) ui.nav_prev() end,          desc("Nav Prev") },
  { "<M-1>", function(_, ui) ui.nav_file(1) end,         desc("Nav File 1") },
  { "<M-2>", function(_, ui) ui.nav_file(2) end,         desc("Nav File 2") },
  { "<M-3>", function(_, ui) ui.nav_file(3) end,         desc("Nav File 3") },
  { "<M-4>", function(_, ui) ui.nav_file(4) end,         desc("Nav File 4") },
  { "<M-5>", function(_, ui) ui.nav_file(5) end,         desc("Nav File 5") },
  { "<M-6>", function(_, ui) ui.nav_file(6) end,         desc("Nav File 1") },
}

return {
  "ThePrimeagen/harpoon",
  keys = function()
    local result = {}
    for _, key in ipairs(keys) do
      table.insert(result, { key[1], nil, desc = key[3] })
    end
    return result
  end,
  config = function()
    require("harpoon").setup()
    local mark = require("harpoon.mark")
    local ui = require("harpoon.ui")

    for _, key in ipairs(keys) do
      vim.keymap.set("n", key[1], function()
        key[2](mark, ui)
      end, { desc = key[3] })
    end
  end
}
