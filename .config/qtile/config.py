from libqtile import bar, hook, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from shutil import which
import os
import re
import subprocess


mod = "mod4"

terminal = os.environ.get("TERMINAL") or which('st') or which('xterm') or ''

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.spawn("sh -c ~/.config/qtile/powermenu.sh")),
    Key([mod], "r", lazy.spawn("fzfmenu_run")),
    # My custom keys
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod, "shift"], "f", lazy.window.toggle_floating()),
    Key([mod], "Semicolon", lazy.spawn("sh -c ~/.local/bin/launch.sh")),
    Key([mod], "period", lazy.next_screen(), desc='Move focus to next monitor'),
    Key([mod], "comma", lazy.prev_screen(), desc='Move focus to previous monitor'),
    # Function keys
    Key([mod], "f5", lazy.spawn("st -c st-popup -e ~/.local/bin/fzfmount", shell=True)),
    Key([mod], "f6", lazy.spawn("st -c st-popup -e ~/.local/bin/fzfumount", shell=True)),
    Key([mod], "f7", lazy.spawn("feh-random-background.sh")),
    # Volume keys
    Key([mod], "bracketright", lazy.spawn("sh -c '~/.local/bin/pactl.sh inc 5'")),
    Key([mod, "shift"], "bracketright", lazy.spawn("sh -c '~/.local/bin/pactl.sh inc 1'")),
    Key([mod], "bracketleft", lazy.spawn("sh -c '~/.local/bin/pactl.sh dec 5'")),
    Key([mod, "Shift"], "bracketleft", lazy.spawn("sh -c '~/.local/bin/pactl.sh dec 1'")),
    Key([mod], "backslash", lazy.spawn("sh -c '~/.local/bin/pactl.sh mute'")),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.MonadTall(
        border_focus='#cccccc',
        border_normal='#333333',
        margin=5,
        new_client_position='top',  # default: 'after_current'
    ),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(highlight_method='line'),
                widget.Spacer(),
                widget.WindowName(),
                widget.Spacer(),
                widget.PulseVolume(
                    background='#83c092',
                    fmt=" <span weight='bold'>{}</span>",
                    foreground='#000',
                    step=5,
                ),
                widget.Spacer(5),
                widget.CPU(
                    background='#d699b6',
                    foreground='#000',
                    fmt=" <span weight='bold'>{}</span>",
                    format="{load_percent}%",
                ),
                # Memory
                widget.GenPollText(
                    background='#d699b6',
                    foreground='#000',
                    func=lambda: subprocess.check_output("~/.config/qtile/memory.sh", shell=True).decode('utf-8').strip(),
                    update_interval=1,
                ),
                # Updates
                widget.GenPollText(
                    background='#d699b6',
                    foreground='#000',
                    func=lambda: subprocess.check_output("~/.config/qtile/updates.sh", shell=True).decode('utf-8').strip(),
                    update_interval=21600,  # 6 hours
                ),
                widget.Spacer(5),
                # Weather (Wttr)
                widget.GenPollText(
                    background='#7fbbb3',
                    foreground='#000000',
                    func=lambda: subprocess.check_output("~/.config/qtile/weather.sh", shell=True).decode('utf-8').strip(),
                    mouse_callbacks={
                        'Button1': lazy.spawn('xterm -class xterm-popup -geometry 125x45 -e ~/.config/polybar/weather-click.sh', shell=True)
                    },
                    update_interval=1800,
                ),
                widget.Spacer(5),
                widget.Clock(
                    background='#a7c080',
                    foreground='#000',
                    format="<span weight='bold'>%a, %d %b, %H:%M</span>",
                    mouse_callbacks={
                        "Button1": lazy.spawn("yad --calendar --undecorated --no-buttons --mouse --close-on-unfocus")
                    },
                ),
                widget.Spacer(5),
                widget.Systray(background='#333'),
            ],
            24,
            border_width=3,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class=re.compile("st-popup")),
        Match(wm_class=re.compile("xterm-popup")),
        Match(wm_class="Yad"),
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"


@hook.subscribe.startup_once
def init_once():
    subprocess.call("pgrep cbatticon || cbatticon &", shell=True)
    subprocess.call("pgrep nm-applet || nm-applet &", shell=True)
    subprocess.call("pgrep picom || picom &", shell=True)
    subprocess.run("feh-random-background.sh", shell=True)
