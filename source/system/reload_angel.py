import os

os.system("ps aux | grep angel | grep -v grep | awk '{print $2}' | xargs kill -HUP")
