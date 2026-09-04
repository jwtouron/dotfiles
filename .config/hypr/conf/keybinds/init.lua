local function move_into_or_out_of_group(direction)
  return function()
    local w = hl.get_active_window()
    if w == nil then return end

    if w.group then
      local group = w.group
      hl.dispatch(hl.dsp.window.move({ out_of_group = direction }))
      if group.size == 1 then
        hl.dispatch(hl.dsp.group.toggle({ window = group.current }))
      end
    else
      hl.dispatch(hl.dsp.window.move({ into_or_create_group = direction }))
    end
  end
end

local function smart_focus(direction)
  local is_backward = direction == "left" or direction == "up"

  return function()
    local w = hl.get_active_window()
    if w == nil then return end

    if not w.group then
      hl.dispatch(hl.dsp.focus({ direction = direction }))
      return
    end

    local at_edge
    if is_backward then
      at_edge = w.group.current_index == 1
    else
      at_edge = w.group.current_index == w.group.size
    end

    if at_edge then
      hl.dispatch(hl.dsp.focus({ direction = direction }))
      return
    end

    local step = is_backward
      and hl.dsp.group.prev()
      or hl.dsp.group.next()
    hl.dispatch(step)
  end
end

local function focus_or_cycle(direction)
  return function()
    local workspace = hl.get_active_workspace()
    if hl.get_active_special_workspace() then
      workspace = hl.get_active_special_workspace()
    end

    if not workspace then
      return
    end

    local monocle_dirs = { up = true, left = true, right = false, down = false }

    if workspace.tiled_layout == "dwindle" then
      hl.dispatch(hl.dsp.focus({ direction = direction }))
    elseif workspace.tiled_layout == "monocle" then
      hl.dispatch(hl.dsp.window.cycle_next({ next = monocle_dirs[direction], tiled = true }))
    end
  end
end

hl.bind("SUPER + Return", hl.dsp.exec_cmd("kitty"))
hl.bind("SUPER + Q", hl.dsp.window.close())
hl.bind("SUPER + SHIFT + Q", hl.dsp.window.kill())
hl.bind("SUPER + SHIFT + S", hl.dsp.exec_cmd("noctalia msg panel-toggle session"))
hl.bind("SUPER + SHIFT + f", hl.dsp.window.float({ action = "toggle" }))
hl.bind("SUPER + Space", hl.dsp.exec_cmd("noctalia msg panel-toggle launcher"))
hl.bind("SUPER + SHIFT + P", hl.dsp.window.pseudo())

hl.bind("SUPER + f", hl.dsp.window.fullscreen({ mode = "fullscreen" }))

hl.bind("SUPER + h", focus_or_cycle("left"))
hl.bind("SUPER + l", focus_or_cycle("right"))
hl.bind("SUPER + k", focus_or_cycle("up"))
hl.bind("SUPER + j", focus_or_cycle("down"))

hl.bind("SUPER + SHIFT + h", hl.dsp.window.move({ direction = "left" }))
hl.bind("SUPER + SHIFT + l", hl.dsp.window.move({ direction = "right" }))
hl.bind("SUPER + SHIFT + k", hl.dsp.window.move({ direction = "up" }))
hl.bind("SUPER + SHIFT + j", hl.dsp.window.move({ direction = "down" }))

hl.bind("SUPER + ALT + h", move_into_or_out_of_group("left"))
hl.bind("SUPER + ALT + l", move_into_or_out_of_group("right"))
hl.bind("SUPER + ALT + k", move_into_or_out_of_group("up"))
hl.bind("SUPER + ALT + j", move_into_or_out_of_group("down"))

hl.bind("SUPER + i", hl.dsp.window.cycle_next({ next = false, tiled = true }))
hl.bind("SUPER + o", hl.dsp.window.cycle_next({ tiled = true }))

hl.bind("SUPER + n", hl.dsp.group.next())
hl.bind("SUPER + p", hl.dsp.group.prev())

hl.bind("SUPER + SHIFT + n", hl.dsp.group.move_window({ forward = true }))
hl.bind("SUPER + SHIFT + p", hl.dsp.group.move_window({ forward = false }))

hl.bind("SUPER + g", hl.dsp.group.toggle())

local num_workspaces = 6
for i = 1, num_workspaces do
  local key = i
  hl.bind("SUPER + " .. key,         hl.dsp.focus({ workspace = i}))
  hl.bind("SUPER + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

hl.bind("SUPER + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind("SUPER + mouse:273", hl.dsp.window.resize(), { mouse = true })

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.05-"),      { locked = true, repeating = true })
hl.bind("XF86AudioMute",        hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),     { locked = true, repeating = true })
hl.bind("XF86AudioMicMute",     hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),   { locked = true, repeating = true })

hl.bind("XF86AudioNext",  hl.dsp.exec_cmd("playerctl next"),       { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay",  hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev",  hl.dsp.exec_cmd("playerctl previous"),   { locked = true }) -- Resizing

hl.bind("SUPER + R", hl.dsp.submap("resize")) hl.define_submap("resize", function()
  hl.bind("h", hl.dsp.window.resize({ x = 50, y = 0, relative = true}), { repeating = true })
  hl.bind("l", hl.dsp.window.resize({ x = -50, y = 0, relative = true}), { repeating = true })
  hl.bind("k", hl.dsp.window.resize({ x = 0, y = 50, relative = true}), { repeating = true })
  hl.bind("j", hl.dsp.window.resize({ x = 0, y = -50, relative = true}), { repeating = true })
  hl.bind("escape", hl.dsp.submap("reset"))
  hl.bind("catchall", function() end)
end)

hl.bind("SUPER + semicolon", function ()
  local layouts     = { "dwindle", "monocle" }
  local workspace   = hl.get_active_workspace()
  if hl.get_active_special_workspace() then
    workspace = hl.get_active_special_workspace()
  end

  local next_layout = "dwindle"

  if not workspace then
    return
  end

  for i = 1, #layouts do
    if layouts[i] == workspace.tiled_layout then
      local next_layout_idx = (i % #layouts) + 1
      next_layout = layouts[next_layout_idx]
      break
    end
  end

  if workspace.special then
    hl.workspace_rule({ workspace = tostring(workspace.name), layout = next_layout })
  else
    hl.workspace_rule({ workspace = tostring(workspace.id), layout = next_layout })
  end
end)
