vim.api.nvim_create_user_command(
  "ReadDate",
  "read !date '+\\%Y-\\%m-\\%d'",
  { desc = "Insert the current date as YYYY-MM-DD below the current line." }
)

for _, cmd in ipairs({ "Cdf", "CDF" }) do
  vim.api.nvim_create_user_command(
    cmd,
    [[execute 'cd' expand('%:p:h')]],
    { desc = "cd to the directory of the current file" }
  )
end

-- Random Color Schemes

local function random_color_scheme()
  local color_schemes = vim.fn.getcompletion("", "color")
  if color_schemes then
    local idx = math.random(1, #color_schemes)
    local color_scheme = color_schemes[idx]
    vim.cmd.colorscheme(color_scheme)
    print("Color scheme set: " .. color_scheme)
  end
end

vim.api.nvim_create_user_command("RandomColorScheme", random_color_scheme, {})
