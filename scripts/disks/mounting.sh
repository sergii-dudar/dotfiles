# in case ntfs disks
sudo pacman -S ntfs-3g
lsblk # for example we want mount /dev/sdb2
sudo mkdir -p /mnt/data250
sudo ntfs-3g /dev/sdb2 /mnt/data250

# creating symlink in home dir to quick access
sudo ln -s /mnt /home/serhii/mnt