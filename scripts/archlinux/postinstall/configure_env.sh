# install ohmyzsh, starship prompt
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
curl -sS https://starship.rs/install.sh | sh

# change default bash to zsh
sudo chsh -s "$(which zsh)" serhii
echo 'SHELL was changed to zsh, need make logout to apply changes'

# sdkman
curl -s "https://get.sdkman.io" | bash
# sdk list gradle
# sdk list java
# sdk list maven
sdk install gradle 8.10
sdk install maven 3.8.8
sdk install java 21.0.4-oracle

# install dotfiles my custom configurations
cd ~/ && \
git clone https://github.com/sergii-dudar/dotfiles.git && \
cd dotfiles && \
stow alacritty && \
stow awesome && \
stow fastfetch && \
stow kitty && \
stow lazygit && \
stow git && \
stow nvim && \
stow qtile && \
stow ranger && \
stow rofi && \
stow starship && \
stow zellij && \

# we need remove stock .zshrc to be able to override from dotfiles
rm .zshrc && \
stow zsh

# Installing extensions and plugins

# ohmyzsh
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"

# neovim
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# alacritty
mkdir -p ~/.config/alacritty/themes && \
git clone https://github.com/alacritty/alacritty-theme ~/.config/alacritty/themes

# ranger
git clone https://github.com/alexanderjeurissen/ranger_devicons ~/.config/ranger/plugins/ranger_devicons
cd ~/.config/ranger/plugins && git clone https://github.com/maximtrp/ranger-archives.git

# tmux (with little tricks because of customized power-line)
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
cp ~/dotfiles/tmux/.tmux.conf ~/.tmux.conf
echo 'Please open tmux and make "prefix + I" to install plugins, then execute tmux/post_script_tmux.sh'

