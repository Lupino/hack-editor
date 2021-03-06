#!/bin/sh
set -e

if [ ! -z "$EDITOR_DATA" ]; then
    DATA="$EDITOR_DATA"
else
    DATA="/data"
fi

echo "Using '$DATA' as data directory"

if [ -f "$DATA/.editorrc" ]; then
    . $DATA/.editorrc
fi

if [ -d "$DATA/.vim" ]; then
    ln -s $DATA/.vim/vimrc /root/.vimrc
    ln -s $DATA/.vim /root/.vim
fi

if [ -x "$DATA/bin/setup.sh" ]; then
    $DATA/bin/setup.sh
fi

if [ ! -f "$DATA/public/js/config.js" ]; then
    mkdir -p $DATA/public/js
    cp /app/public/js/config.sample.js $DATA/public/js/config.js
fi

if [ "$1" = "hack-editor" ]; then
    exec "$@" --source $DATA
else
    exec "$@"
fi
