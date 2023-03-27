local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local volume_widget = require('awesome-wm-widgets.volume-widget.volume')

local function dim(color)
  if not color or not gears.string.startswith(color, '#') then
    return color
  end
  local r = string.sub(color, 2, 3)
  local g = string.sub(color, 4, 5)
  local b = string.sub(color, 6, 7)
  local f = function(x)
    return tostring(math.floor(tonumber(x, 16) / 4))
  end
  return '#' .. f(r) .. f(g) .. f(b)
end

local function make_textclock()
  local tc = wibox.widget.textclock("%a, %d %b, %H:%M", 5)
  local cal = awful.widget.calendar_popup.month({ start_sunday = true })
  cal:attach(tc, "tr", { on_hover = true })
  return wibox.container.margin(tc, 0, 0, 5, 5)
end

local function make_weather_widget()
  local w = wibox.widget.textbox()
  gears.timer {
    timeout = 1800,
    call_now = true,
    autostart = true,
    callback = function()
      local cmd = "loc=$(cat ~/.cache/wttr 2>/dev/null | tr -d '[[:space:]]');" ..
                  'if [ -n "$loc" ]; then loc="/$loc" fi;' ..
                  'curl -Ssk "https://wttr.in${loc}?0&T&Q" 2>/dev/null | cut -c 16- | head -2 | xargs echo'
      awful.spawn.easy_async_with_shell(cmd, function(stdout) w.text = stdout end)
    end
  }
  local ww = wibox.container.margin(
    wibox.layout.fixed.horizontal(wibox.widget.textbox(' '), w),
    0, 0, 5, 5)
  ww:connect_signal("button::press", function(self, lx, ly, button)
    if button == 1 then
      awful.spawn.with_shell('xterm -class xterm-popup -geometry 125x45 -e ~/.config/polybar/weather-click.sh')
    end
  end)
  return ww
end

local function make_cpu_widget()
  local w = wibox.widget {
    widget = wibox.widget.progressbar,
    forced_width = 25,
    max_value = 100,
  }
  gears.timer {
    timeout = 1,
    call_now = true,
    autostart = true,
    callback = function()
      local cmd = "read cpu a b c previdle rest < /proc/stat;" ..
                  "prevtotal=$((a+b+c+previdle));" ..
                  "sleep 0.5;" ..
                  "read cpu a b c idle rest < /proc/stat;" ..
                  "total=$((a+b+c+idle));" ..
                  "echo $((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))"
      awful.spawn.easy_async_with_shell(cmd, function(stdout)
        local used = tonumber(stdout) or 0
        w.value = used
        if used >= 90 then
          w.color = 'red'
        elseif used >= 70 then
          w.color = 'yellow'
        else
          w.color = beautiful.progressbar_fg or 'green'
        end
      end)
    end
  }
  return wibox.container.margin(
    wibox.layout.fixed.horizontal( wibox.widget.textbox(' '), w),
    0, 0, 5, 5)
end

local function make_mem_widget()
  local w = wibox.widget {
    widget = wibox.widget.progressbar,
    forced_width = 25,
    max_value = 100,
  }
  gears.timer {
    timeout = 1,
    call_now = true,
    autostart = true,
    callback = function()
      local cmd = "free -t | awk '/Total/ {printf \"%d\", $3 / ($3 + $4) * 100.0}'"
      awful.spawn.easy_async_with_shell(cmd, function(stdout)
        local used = tonumber(stdout) or 0
        w.value = used
        if used >= 90 then
          w.color = 'red'
        elseif used >= 70 then
          w.color = 'yellow'
        else
          w.color = beautiful.progressbar_fg or 'green'
        end
      end)
    end
  }
  return wibox.container.margin(
    wibox.layout.fixed.horizontal( wibox.widget.textbox(' '), w),
    10, 0, 5, 5)
end

local self = {}

self.init = function()
  -- Create a wibox for each screen and add it
  local taglist_buttons = gears.table.join(
                      awful.button({ }, 1, function(t) t:view_only() end),
                      awful.button({ modkey }, 1, function(t)
                                                if client.focus then
                                                    client.focus:move_to_tag(t)
                                                end
                                            end),
                      awful.button({ }, 3, awful.tag.viewtoggle),
                      awful.button({ modkey }, 3, function(t)
                                                if client.focus then
                                                    client.focus:toggle_tag(t)
                                                end
                                            end),
                      awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                      awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                  )

  function set_wallpaper(s)
      local cmd = "find /usr/share/backgrounds /usr/share/wallpapers ~/.local/share/wallpapers -type f \\( -name '*.jpg' -o -name '*.png' \\) 2>/dev/null | shuf -n1"
      awful.spawn.easy_async_with_shell(cmd, function(wallpaper)
          wallpaper = string.gsub(wallpaper, '^%s*(.-)%s*$', '%1')
          gears.wallpaper.maximized(wallpaper, s, true)
      end)
  end

  awful.screen.connect_for_each_screen(function(s)
      -- Wallpaper
      set_wallpaper(s)

      -- Each screen has its own tag table.
      awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(gears.table.join(
                             awful.button({ }, 1, function () awful.layout.inc( 1) end),
                             awful.button({ }, 3, function () awful.layout.inc(-1) end),
                             awful.button({ }, 4, function () awful.layout.inc( 1) end),
                             awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist {
          screen  = s,
          filter  = awful.widget.taglist.filter.all,
          buttons = taglist_buttons
      }

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "top", screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
          layout = wibox.layout.align.horizontal,
          { -- Left widgets
              layout = wibox.layout.fixed.horizontal,
              s.mytaglist,
              wibox.container.margin(s.mylayoutbox, 5, 0, 5, 5),
              s.mypromptbox,
          },
          nil,
          { -- Right widgets
              layout = wibox.layout.fixed.horizontal,
              spacing = 25,
              volume_widget(),
              {
                make_cpu_widget(),
                make_mem_widget(),
                layout = wibox.layout.fixed.horizontal,
              },
              make_weather_widget(),
              make_textclock(),
              wibox.container.margin(wibox.widget.systray(), 0, 5, 5, 5),
          },
      }
  end)
end

return self
