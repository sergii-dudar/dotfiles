cd ~/tools && \
git clone https://github.com/rvaiya/keyd && \
cd keyd && \
make && sudo make install && \
echo 'key installed, creating config symlink to /etc/keyd/default.conf' && \
sudo ln -s ~/dotfiles/nonhome/keyd/default.conf /etc/keyd/default.conf && \
echo 'enabling systemctl key service' && \
sudo systemctl enable keyd && sudo systemctl start keyd && \
echo "keyd successfully installed and started!"