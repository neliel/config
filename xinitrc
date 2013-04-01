## .xinitrc

setxkbmap fr
xsetroot -cursor_name left_ptr
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
urxvtd -q -o -f
spacefm -d &
. ~/.fehbg &
xmonad

#EOF
