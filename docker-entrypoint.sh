#!/bin/sh
set -e

if [ ! -z "$EDITOR_DATA" ]; then
    DATA="$EDITOR_DATA"
else
    DATA="/data"
fi

echo "Using '$DATA' as data directory"

if [ -f "$DATA/bin/activate" ]; then
    . $DATA/bin/activate
fi

if [ -d "$DATA/.vim" ]; then
    ln -s $DATA/.vim/vimrc /root/.vimrc
    ln -s $DATA/.vim /root/.vim
fi

if [ -x "$DATA/bin/setup.sh" ]; then
    $DATA/bin/setup.sh
fi

if [ ! -f "$DATA/config.js" ]; then
    cp /app/public/config.sample.js $DATA/config.js
fi

exec "$@" --source $DATA
