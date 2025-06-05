# Tut

## Deps

```bash
sudo pacman -S cronie
sudo systemctl enable cronie.service --now
crontab -e # to edit/add cron jobs (automatically refreshing on close editor)
crontab -l # to check list of active jobs
```

## Crontab

```bash

SHELL=/bin/bash
#MAILTO=shtanga.net@gmail.com
PATH=/home/serhii/.cargo/bin:/usr/local/bin:/usr/bin:/home/serhii/.ghcup/bin:/home/serhii/.local/bin

0 6 * * * touch ~/dotfiles/cron/cron.logs.txt && ~/dotfiles/cron/scripts/hypr/redshift day >> ~/dotfiles/cron/cron.logs.txt 2>&1
0 21 * * * touch ~/dotfiles/cron/cron.logs.txt && ~/dotfiles/cron/scripts/hypr/redshift night >> ~/dotfiles/cron/cron.logs.txt 2>&1
```

```bash
# just to test first run (every minute/every second minute)

* * * * * touch /tmp/test.cron.txt && echo "date is: $(date)" >> /tmp/test.cron.txt

* * * * * touch ~/dotfiles/cron/cron.logs.txt && ~/dotfiles/cron/scripts/hypr/redshift day >> ~/dotfiles/cron/cron.logs.txt 2>&1
*/2 * * * * touch ~/dotfiles/cron/cron.logs.txt && ~/dotfiles/cron/scripts/hypr/redshift night >> ~/dotfiles/cron/cron.logs.txt 2>&1

10 22 * * * touch /tmp/test.cron.txt && echo "abc: date is: $(date)" >> /tmp/test.cron.txt

```

### Links

[crontab.guru](https://crontab.guru)
[small tut](https://opensource.com/article/17/11/how-use-cron-linux)
