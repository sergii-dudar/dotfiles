# 1
sudo pacman -S wqy-zenhei noto-fonts-cjk adobe-source-han-sans-otc-fonts

# 2
sudo nvim /etc/locale.gen
# uncomment:
# zh_CN.UTF-8 UTF-8
# zh_TW.UTF-8 UTF-8

# 3
sudo locale-gen

# 4
sudo nvim /etc/locale.conf
# Add or modify the following lines:
LANG=en_US.UTF-8
LC_CTYPE=zh_CN.UTF-8