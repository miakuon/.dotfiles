sleep 0.2
scrot -s -q 1 -e 'pngquant -f --ext ".png" $f; xclip -selection clipboard -t image/png -i $f' "/home/mia/Pictures/Screenshots/%Y.%m.%d-%T.png"
